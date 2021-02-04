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

(defun gripe-find (file-or-dir-path pattern)
  "Find occurrences of a pattern within a file/directory.

* FILE-OR-DIR-PATH - Path to search
* PATTERN - Pattern to search for. (See https://github.com/bfontaine/grape#command-line)"
  (interactive
   (list (read-file-name "File/directory to search: ")
         (read-string "Pattern to search: ")))
  (let ((buffer (generate-new-buffer "*gripe-output*")))
    (async-shell-command (concat "grape --unindent '" pattern "' "  file-or-dir-path)
                         "*gripe-output*"
                         "*gripe-error*")))

(provide 'gripe)
;;; gripe.el ends here
