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
  (let* ((decoration-level (font-lock-value-in-major-mode font-lock-maximum-decoration))
         (var (match-string n))
         (meta (cider-resolve-var (cider-current-ns) var))
         (spec (append (when face (list face))
                       (when (nrepl-dict-get meta "cider-instrumented")
                         '(cider-instrumented-face))
                       (when decoration-level
                         (unless (and (numberp decoration-level)
                                      (< decoration-level 2))
                           ;; Is it a macro, function, or var? And do we want to
                           ;; font-lock that much?
                           (cond
                            ((nrepl-dict-get meta "macro")
                             (when (cider--valid-macro-place-p (match-beginning n))
                               '(font-lock-keyword-face)))
                            ((nrepl-dict-get meta "arglists")
                             (unless (and (numberp decoration-level)
                                          (< decoration-level 3))
                               '(font-lock-function-name-face)))
                            (meta
                             (unless (and (numberp decoration-level)
                                          (< decoration-level 4))
                               '(font-lock-variable-name-face)))))))))
    (when spec
      (list 'face spec))))

(provide 'cider-resolve)
;;; cider-resolve.el ends here
