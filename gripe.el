;;; gripe.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Rafael Nicdao
;;
;; Author: Rafael Nicdao <https://github.com/anonimito>
;; Maintainer: Rafael Nicdao <nicdaoraf@gmail.com>
;; Created: February 04, 2021
;; Modified: February 04, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/anonimitoraf/gripe
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'gripe-common)
(require 'gripe-ivy)

;; For now, just ivy. I've yet to learn how `defcustom' and whatnot works
(setq gripe-completion 'ivy)

(defun gripe--render-grape-output (grape-output)
  "Renders the GRAPE-OUTPUT."
  (let ((gripe-ast (gripe--make-grape-output-ast (split-string grape-output "\n"))))
    (cond ((equal gripe-completion 'ivy) (gripe--ivy gripe-ast)))))

(defun gripe-find (file-or-dir-path pattern)
  "Find occurrences of a pattern within a file/directory.

* FILE-OR-DIR-PATH - Path to search
* PATTERN - Pattern to search for. (See https://github.com/bfontaine/grape#command-line)"
  (interactive
   (list (read-file-name "File/directory to search: ")
         (read-string "Pattern to search: ")))
  (gripe--async-shell-command-to-string (concat "grape --unindent '" pattern "' "  file-or-dir-path)
                                        #'gripe--render-grape-output))

(provide 'gripe)
;;; gripe.el ends here
