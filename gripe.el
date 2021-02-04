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

;; Taken from https://stackoverflow.com/a/23078813
(defun async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
  (lexical-let
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun gripe-find (file-or-dir-path pattern)
  "Find occurrences of a pattern within a file/directory.

* FILE-OR-DIR-PATH - Path to search
* PATTERN - Pattern to search for. (See https://github.com/bfontaine/grape#command-line)"
  (interactive
   (list (read-file-name "File/directory to search: ")
         (read-string "Pattern to search: ")))
  (async-shell-command-to-string (concat "grape --unindent '" pattern "' "  file-or-dir-path)
                          (lambda (output)
                            (set-buffer (get-buffer-create "*gripe-output*"))
                            (erase-buffer)
                            (insert output)
                            (display-buffer "*gripe-output*" t))))

(provide 'gripe)
;;; gripe.el ends here
