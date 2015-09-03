;;; cider-resolve.el --- Resolve clojure symbols according to current nREPL connection

;; Copyright © 2015 Artur Malabarba

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The ns cache is a dict of namespaces stored in the connection buffer. This
;; file offers functions to easily get information about variables from this
;; cache, given the variable's name and the file's namespace.
;;
;; Below is a typical entry on this cache dict. Note that clojure.core symbols
;; are excluded from the refers to save space.
;;
;; "cider.nrepl.middleware.track-state"
;; (dict "aliases"
;;       (dict "cljs" "cider.nrepl.middleware.util.cljs"
;;             "misc" "cider.nrepl.middleware.util.misc"
;;             "set" "clojure.set")
;;       "interns" (dict "assoc-state"
;;                       (dict "arglists"
;;                             (("response"
;;                               (dict "as" "msg" "keys"
;;                                     ("session")))))
;;                       "filter-core"
;;                       (dict "arglists"
;;                             (("refers")))
;;                       "make-transport"
;;                       (dict "arglists"
;;                             (((dict "as" "msg" "keys"
;;                                     ("transport")))))
;;                       "ns-as-map"
;;                       (dict "arglists"
;;                             (("ns")))
;;                       "ns-cache"
;;                       (dict)
;;                       "relevant-meta"
;;                       (dict "arglists"
;;                             (("var")))
;;                       "update-vals"
;;                       (dict "arglists"
;;                             (("m" "f")))
;;                       "wrap-tracker"
;;                       (dict "arglists"
;;                             (("handler"))))
;;       "refers" (dict "set-descriptor!" "#'clojure.tools.nrepl.middleware/set-descriptor!"))

;;; Code:

(require 'nrepl-client)
(require 'cider-interaction)
(require 'cider-repl)

(defun cider-resolve--get-in (&rest keys)
  "Return (nrepl-dict-get-in cider-repl-ns-cache keys)."
  (when cider-connections
    (nrepl-dict-get-in
     (with-current-buffer (cider-current-connection)
       cider-repl-ns-cache)
     keys)))

(defun cider-resolve-alias (ns alias)
  "Return the namespace that ALIAS refers to in namespace NS.
If it doesn't point anywhere, returns ALIAS."
  (or (cider-resolve--get-in ns "aliases" alias)
      alias))

(defun cider-resolve-var (ns var)
  "Return a dict of the metadata of a clojure var VAR in namespace NS.
VAR is a string.
Return nil only if VAR cannot be resolved."
  (let* ((prefix-regexp "\\`\\([^/]+\\)/")
         (var-ns (when (string-match prefix-regexp var)
                   (cider-resolve-alias ns (match-string 1 var))))
         (name (replace-regexp-in-string prefix-regexp "" var)))
    (or
     (cider-resolve--get-in (or var-ns ns) "interns" name)
     (unless var-ns
       ;; If the var had no prefix, it might be referred.
       (-if-let (referal (cider-resolve--get-in ns "refers" name))
           (cider-resolve-var ns referal)
         ;; Or it might be from core.
         (unless (equal ns "clojure.core")
           (cider-resolve-var "clojure.core" name)))))))

(defun cider-resolve-var-ns (ns var)
  "Return a string of the namespace of a clojure var VAR.
VAR is a string. NS is the current namespace.
Return nil only if VAR cannot be resolved."
  (let ((prefix-regexp "\\`\\([^/]+\\)/"))
    (-if-let ((var-ns (when (string-match prefix-regexp var)
                        (cider-resolve-alias ns (match-string 1 var)))))
        var-ns
      (if (cider-resolve--get-in ns "interns" var)
          ns
        ;; If the var was not interned, it might be referred.
        (-if-let (referal (cider-resolve--get-in ns "refers" var))
            (replace-regexp-in-string "/.*\\'" "" referal)
          ;; Or it might be from core.
          (unless (equal ns "clojure.core")
            (when (cider-resolve-var "clojure.core" var)
              "clojure.core")))))))

;;; Dynamic font locking
(defcustom cider-font-lock-dynamically '(macro core)
  "Specifies how much dynamic font-locking CIDER should use.
Dynamic font-locking this refers to applying syntax highlighting to vars
defined in the currently active nREPL connection. This is done in addition
to `clojure-mode's usual (static) font-lock, so even if you set this
variable to nil you'll still see basic syntax highlighting.

The value is a list of symbols, each one indicates a different type of var
that should be font-locked:
   `macro' (default): Any defined macro gets the `font-lock-builtin-face'.
   `function': Any defined function gets the `font-lock-function-face'.
   `var': Any non-local var gets the `font-lock-variable-face'.
   `core' (default): Any symbol from clojure.core (face depends on type).

The value can also be t, which means to font-lock as much as possible."
  :type '(choice (set :tag "Fine-tune font-locking"
                      (const :tag "Any defined macro" macro)
                      (const :tag "Any defined function" function)
                      (const :tag "Any defined var" var)
                      (const :tag "Any symbol from clojure.core" core))
                 (const :tag "Font-lock as much as possible" t))
  :group 'cider
  :package-version '(cider . "0.10.0"))

(defun cider--valid-macro-place-p (pos)
  "Return non-nil if POS points to a valid place for a macro.
This is either after a `(' or after a `#''.
Because you cannot take the value of macros in Clojure, a lone symbol like
`ns' is guaranteed to not be a macro."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (forward-char -1)
      (or (eq (char-after) ?\()
          (and (eq (char-after) ?\')
               (eq (char-before) ?\#))))))

(defun cider-matched-symbol-face-spec (n face)
  "Return a face specification for font-locking.
If (match-string N) is an instrumented symbol, return the list
    (face (FACE cider-instrumented-face))
otherwise, return (face FACE)."
  (let* ((fl-types (if (eq cider-font-lock-dynamically t)
                       '(macro function var core)
                     cider-font-lock-dynamically))
         (var (match-string n))
         (ns (cider-current-ns))
         (is-core-and-font-lock (and (memq 'core fl-types)
                                     (equal (cider-resolve-var-ns ns var) "clojure.core")))
         (meta (cider-resolve-var ns var))
         (spec (append (when face (list face))
                       (when (nrepl-dict-get meta "cider-instrumented")
                         '(cider-instrumented-face))
                       (when fl-types
                         ;; Is it a macro, function, or var? And do we want to
                         ;; font-lock that much?
                         (cond
                          ((nrepl-dict-get meta "macro")
                           (when (or is-core-and-font-lock
                                     (memq 'macro fl-types))
                             (when (cider--valid-macro-place-p (match-beginning n))
                               '(font-lock-keyword-face))))

                          ((nrepl-dict-get meta "arglists")
                           (when (or is-core-and-font-lock
                                     (memq 'function fl-types))
                             '(font-lock-function-name-face)))

                          (meta
                           (when (or is-core-and-font-lock
                                     (memq 'var fl-types))
                             '(font-lock-variable-name-face))))))))
    (when spec
      (list 'face spec))))

(provide 'cider-resolve)
;;; cider-resolve.el ends here
