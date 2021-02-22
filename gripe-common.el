;;; gripe-common.el --- Internal common utilities for gripe -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Rafael Nicdao
;;
;; Author: Rafael Nicdao <https://github.com/anonimito>
;; Maintainer: Rafael Nicdao <nicdaoraf@gmail.com>
;; Created: February 08, 2021
;; Modified: February 08, 2021
;; Homepage: https://github.com/anonimito/gripe-common
;; Package-Requires: ((emacs "24.3") (cl-lib "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Internal common utilities for gripe
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct gripe--occ-line line-number)
(cl-defstruct gripe--occ-file
  file-path
  line-numbers)

;; Taken from https://stackoverflow.com/a/23078813
(defun gripe--async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
  (let*
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback))
    (set-process-sentinel
     (start-process "gripe-async" output-buffer shell-file-name shell-command-switch command)
     (lambda (process _signal)
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
  "Parse grape's output (i.e. LINES) into an AST."
  (let* ((ast '())
         ;; Returns non-nil if the particular line is the start of an occurrence.
         ;; The heuristic is the fact that the start of the occurrences start with a number.
         (line-occurrence? (lambda (s) (string-match "^\\([[:digit:]]+\\):.*" s)))
         ;; Continuations of an occurrence start with space/s
         (line-continuation? (lambda (s) (string-match "^\s+.*$" s))))
    (dolist (line lines)
      (cond
       ;; Check for line occurrences
       ((funcall line-occurrence? line)
        (let* ((occ-line-number (match-string 1 line))
               (line-occ (make-gripe--occ-line :line-number occ-line-number))
               (file-occ (car (last ast))))
          (setf (gripe--occ-file-line-numbers file-occ)
                (append (gripe--occ-file-line-numbers file-occ)
                        (list line-occ)))))
       ;; Check for line occurrence continuations. Skip them (for now)
       ((or (funcall line-continuation? line)
            ;; Or blank lines
            (length< (string-trim line) 1)) nil)
       ;; Otheriwse, we assume it's a new file occurrence
       (t (setq ast (append ast (list (make-gripe--occ-file :file-path line
                                                            :line-numbers '())))))))
    ast))

(defun gripe--path-relative-from-project-root (full-path)
  "Return the relative file path of FULL-PATH from the project's root."
  (file-relative-name full-path
                      (locate-dominating-file full-path "project.clj")))

(provide 'gripe-common)
;;; gripe-common.el ends here
