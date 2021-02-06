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

(require 'cl-lib)

;; --- cl-lib stuff -----------------------------------------

(cl-defstruct pattern-line-occurrence line-number)
(cl-defstruct pattern-file-occurrence
  file-path
  line-numbers)

;; ---------------------------------------------------------

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

(defun gripe--make-grape-output-ast (lines)
  "Parse lines into an AST.
* LINES - TODO"
  (let* ((ast '())
         ;; Returns non-nil if the particular line is the start of an occurrence.
         ;; The heuristic is the fact that the start of the occurrences start with a number.
         (line-occurrence? (lambda (s) (string-match "^\\([[:digit:]]+\\):.*" s)))
         (line-continuation? (lambda (s) (string-match "^\s+.*$" s))))
    (dolist (line lines)
      (cond
       ;; Check for line occurrences
       ((funcall line-occurrence? line)
        (let* ((occ-line-number (match-string 1 line))
               (line-occ (make-pattern-line-occurrence :line-number occ-line-number))
               (file-occ (car (last ast))))
          (setf (pattern-file-occurrence-line-numbers file-occ)
                (append (pattern-file-occurrence-line-numbers file-occ)
                        (list line-occ)))))
       ;; Check for line occurrence continuations. Skip them (for now)
       ((or (funcall line-continuation? line)
            ;; Or blank lines
            (length< (string-trim line) 1)) nil)
       ;; Otheriwse, we assume it's a new file occurrence
       (t (setq ast (append ast (list (make-pattern-file-occurrence :file-path line
                                                                    :line-numbers '())))))))
    ;; Return the AST
    ast))

(defun gripe--display-grape-output (output)
  "Display grape output.

* OUTPUT - grape output to display"
  (let* ((ast (gripe--make-grape-output-ast (split-string output "\n"))))
    (set-buffer (get-buffer-create "*gripe-output*"))
    (erase-buffer)
    (dolist (file-occ ast)
      (insert (concat "File occ: " (pattern-file-occurrence-file-path file-occ)) "\n")
      (dolist (line-occ (pattern-file-occurrence-line-numbers file-occ))
        (insert (concat "Line occ: " (pattern-line-occurrence-line-number line-occ) "\n")))
      (insert "\n------------------------\n"))
    (display-buffer "*gripe-output*" t)))

(defun gripe-find (file-or-dir-path pattern)
  "Find occurrences of a pattern within a file/directory.

* FILE-OR-DIR-PATH - Path to search
* PATTERN - Pattern to search for. (See https://github.com/bfontaine/grape#command-line)"
  (interactive
   (list (read-file-name "File/directory to search: ")
         (read-string "Pattern to search: ")))
  (async-shell-command-to-string (concat "grape --unindent '" pattern "' "  file-or-dir-path)
                                 #'gripe--display-grape-output))

(provide 'gripe)
;;; gripe.el ends here
