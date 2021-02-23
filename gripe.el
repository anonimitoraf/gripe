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
;; Package-Requires: ((emacs "24.3") (cl-lib "1.0"))
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

(require 'cl-lib)
(require 'subr-x)

;;; Configuration

(defgroup gripe nil
  "Wrapper for syntax-aware Clojure pattern search/grep alternative."
  :group 'convenience)

(defcustom gripe-completion nil
  "Decides which completion package to use for rendering the gripe results.
Currently, only `ivy' is supported.
Support for `helm', `ido' and `selectrum' are planned.
If this is nil, the first completion package found is used, in this order:
- selectrum
- ivy
- helm
- ido"
  :type '(choice
          (const :tag "Ivy" ivy))
  :group 'gripe)

;;; Core
;; This is shared across the ivy, helm, ido, selectrum implementations

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

(defun gripe--render-grape-output (grape-output)
  "Renders the GRAPE-OUTPUT."
  (let ((gripe-ast (gripe--make-grape-output-ast (split-string grape-output "\n"))))
    (cond ((equal gripe-completion 'ivy) (gripe--ivy gripe-ast))
          ;; No user preference specified, use the first supported completion pkg found
          ;; in this order:
          ;; - selectrum - TODO
          ;; - ivy
          ;; - helm - TODO
          ;; - ido - TODO
          (t (cond ((featurep 'ivy) (setq gripe-completion 'ivy)
                    (user-error (concat "Supported completion packages: (ivy). None found"))))))))

;;; Ivy-specific code

(defvar gripe--ivy-highlight-removal-timer nil)

(defun gripe--ivy-go-to-occurrence (selected)
  "Go to the file and line number of the SELECTED grape occurrence."
  (let* (;; val is of shape '("path:line-num" ("path", "line-num"))
         (val (car (cdr selected)))
         (full-file-path (car val))
         (line-number (car (cdr val))))
    (when (file-exists-p full-file-path)
      (find-file full-file-path)
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (isearch-highlight (+ (line-beginning-position) (current-indentation))
                         (line-end-position))
      (unless gripe--ivy-highlight-removal-timer
        (setq gripe--ivy-highlight-removal-timer
              ;; Remove highlight after a while
              (run-at-time "5 sec" nil (lambda ()
                                         (isearch-highlight 0 0)
                                         (setq gripe--ivy-highlight-removal-timer nil))))))))

(defun gripe--ivy (gripe-ast)
  "Navigate through gripe results with ivy.
* GRIPE-AST - The output of `gripe--make-grape-output-ast'"
  (interactive)
  (progn
    (require 'ivy)
    (ivy-read "Preview pattern match: "
              (apply #'append ; Flatten the list of lists
                     (cl-map 'list
                             (lambda (occ-file)
                               (cl-map 'list
                                       (lambda (occ-line)
                                         (let* ((candidate-key (concat (gripe--path-relative-from-project-root
                                                                        (gripe--occ-file-file-path occ-file))
                                                                       (gripe--occ-line-line-number occ-line)))
                                                ;; grape's file path has a trailing ":" which we want to remove
                                                (candidate-val (list (replace-regexp-in-string
                                                                      "\\(.*\\):$" "\\1"
                                                                      (gripe--occ-file-file-path occ-file))
                                                                     (gripe--occ-line-line-number occ-line))))
                                           (list candidate-key candidate-val)))
                                       (gripe--occ-file-line-numbers occ-file)))
                             gripe-ast))
              :require-match t
              :update-fn 'auto
              :action #'gripe--ivy-go-to-occurrence)))

;;; Public interface

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
