;;; gripe.el --- Emacs wrapper for https://github.com/bfontaine/grape -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Rafael Nicdao
;;
;; Author: Rafael Nicdao <https://github.com/anonimito>
;; Maintainer: Rafael Nicdao <nicdaoraf@gmail.com>
;; Created: February 04, 2021
;; Modified: February 04, 2021
;; Version: 0.0.1
;; Keywords: clojure, grape, grep, search, pattern, wrapper
;; Homepage: https://github.com/anonimitoraf/gripe
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs wrapper for https://github.com/bfontaine/grape.
;;  As of now, only `ivy' is the only supported completion interface.
;;  To use:
;;  * `M-x gripe-find'
;;  * Look for the search file/directory path
;;  * Enter a valid search pattern (See https://github.com/bfontaine/grape#command-line)
;;
;;; Code:

(require 'gripe-common)
(require 'gripe-ivy)

(defgroup gripe nil
  "Wrapper for syntax-aware Clojure pattern search/grep alternative."
  :group 'convenience)

(defcustom gripe-completion nil
  "Decidees which completion interface to use to pick within gripe results."
  ;; Support for helm, ido, selctrum upcoming
  :type '(choice
          (const :tag "Ivy" ivy))
  :group 'gripe)

(defun gripe--render-grape-output (grape-output)
  "Renders the GRAPE-OUTPUT."
  (let ((gripe-ast (gripe--make-grape-output-ast (split-string grape-output "\n"))))
    (cond ((equal gripe-completion 'ivy) (gripe--ivy gripe-ast))
          (t (user-error (concat "Invalid value for gripe-completion :"
                                 (prin1-to-string gripe-completion)))))))
;;;###autoload
(defun gripe-find (file-or-dir-path pattern)
  "Find occurrences of PATTERN within a FILE-OR-DIR-PATH.
For the format of PATTERN, see https://github.com/bfontaine/grape#command-line"
  (interactive
   (list (read-file-name "File/directory to search: ")
         (read-string "Pattern to search: ")))
  (gripe--async-shell-command-to-string (concat "grape --unindent '" pattern "' "  file-or-dir-path)
                                        #'gripe--render-grape-output))

(provide 'gripe)
;;; gripe.el ends here
