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
;; Package-Requires: ((emacs "24.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs wrapper for https://github.com/bfontaine/grape.
;;  As of now, only `ivy', `helm', `selectrum' are supported for traversing results.
;;  To use:
;;  * `M-x gripe-find'
;;  * Look for the search file/directory path
;;  * Enter a valid search pattern (See https://github.com/bfontaine/grape#command-line)
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C O N F I G U R A T I O N ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup gripe nil
  "Wrapper for syntax-aware Clojure pattern search/grep alternative."
  :group 'convenience)

(defcustom gripe-completion nil
  "Decides which completion package to use for rendering the gripe results.
Possible values: `'ivy', `'helm', `'selectrum'.
Support for `'selectrum' is planned.
If this is nil, the first completion package found is used, in this order:
- ivy
- helm
- selectrum"
  :type '(choice
          (const :tag "ivy" ivy)
          (const :tag "helm" helm)
          (const :tag "selectrum" selectrum))
  :group 'gripe)

(defcustom gripe-highlight-duration nil
  "How long (in sec) the highlight is active when previewing a result.
Defaults to 1 sec."
  :type 'number
  :group 'gripe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C O R E ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is shared across the completion packages' implementations

(cl-defstruct gripe--occ-line line-number preview)
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
  (let* ((output-buffer (generate-new-buffer " *temp*"))
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

(defun gripe--parse-grape-output (lines)
  "Parse grape's output (i.e. LINES) into an a flat list of file paths and line numbers."
  (let* ((parsed '())
         ;; Returns non-nil if the particular line is the start of an occurrence.
         ;; The heuristic is the fact that the start of the occurrences start with a number.
         (line-occurrence? (lambda (s) (string-match "^\\([[:digit:]]+\\):\\(.*\\)" s)))
         ;; Continuations of an occurrence start with space/s
         (line-continuation? (lambda (s) (string-match "^\s+.*$" s))))
    (dolist (line lines)
      (cond
       ;; Check for line occurrences
       ((funcall line-occurrence? line)
        (let* ((occ-line-number (match-string 1 line))
               (occ-line-preview (match-string 2 line))
               (line-occ (make-gripe--occ-line :line-number occ-line-number
                                               :preview occ-line-preview))
               (file-occ (car (last parsed))))
          (setf (gripe--occ-file-line-numbers file-occ)
                (append (gripe--occ-file-line-numbers file-occ)
                        (list line-occ)))))
       ;; Check for line occurrence continuations. Skip them (for now)
       ((or (funcall line-continuation? line)
            ;; Or blank lines
            (length< (string-trim line) 1)) nil)
       ;; Otheriwse, we assume it's a new file occurrence
       (t (setq parsed (append parsed (list (make-gripe--occ-file :file-path line
                                                                  :line-numbers '())))))))
    parsed))

(defun gripe--path-relative-from-project-root (full-path)
  "Return the relative file path of FULL-PATH from the project's root."
  (file-relative-name full-path
                      (locate-dominating-file full-path "project.clj")))

(defun gripe--render-grape-output (grape-output)
  "Renders the GRAPE-OUTPUT."
  (let ((parsed (gripe--parse-grape-output (split-string grape-output "\n"))))
    (cond ((equal gripe-completion 'ivy) (gripe--ivy parsed))
          ((equal gripe-completion 'helm) (gripe--helm parsed))
          ((equal gripe-completion 'selectrum) (gripe--selectrum parsed))
          ;; No user preference specified, use the first supported completion pkg found:
          ((featurep 'ivy) (gripe--ivy parsed))
          ((featurep 'helm) (gripe--helm parsed))
          ((featurep 'selectrum) (gripe--selectrum parsed))
          (t (user-error (concat "Supported completion packages: (ivy, helm, selectrum). None found"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C O M M O N - H E L P E R S ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gripe--flatten-list-1 (list-of-lists)
  "Flatten LIST-OF-LISTS once (i.e. depth = 1)."
  (apply #'append list-of-lists))

(defun gripe--make-candidates (gripe-parsed-output)
  "Transform GRIPE-PARSED-OUTPUT into a list of '(file-line-num-string (file-name line-num))."
  (gripe--flatten-list-1 (cl-map 'list
                                 (lambda (occ-file)
                                   (cl-map 'list
                                           (lambda (occ-line)
                                             (let* ((candidate-key (concat (gripe--path-relative-from-project-root
                                                                            (gripe--occ-file-file-path occ-file))
                                                                           (gripe--occ-line-line-number occ-line)
                                                                           " "
                                                                           (gripe--occ-line-preview occ-line)))
                                                    ;; grape's file path has a trailing ":" which we want to remove
                                                    (candidate-val (list (replace-regexp-in-string
                                                                          "\\(.*\\):$" "\\1"
                                                                          (gripe--occ-file-file-path occ-file))
                                                                         (gripe--occ-line-line-number occ-line))))
                                               (list candidate-key candidate-val)))
                                           (gripe--occ-file-line-numbers occ-file)))
                                 gripe-parsed-output)))

(defvar gripe--highlight-removal-timer nil)

(defun gripe--go-to-occurrence (selected)
  "Go to the file and line number of the SELECTED grape occurrence.

SELECTED is expected to be of shape '(\"{path}\" \"{line}\")"
  (let* ((full-file-path (car selected))
         (line-number (car (cdr selected))))
    (when (file-exists-p full-file-path)
      (find-file full-file-path)
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (isearch-highlight (+ (line-beginning-position) (current-indentation))
                         (line-end-position))
      (unless gripe--highlight-removal-timer
        (setq gripe--highlight-removal-timer
              ;; Remove highlight after a while
              (run-at-time (concat (number-to-string (or gripe-highlight-duration 1)) " sec")
                           nil
                           (lambda ()
                             (isearch-highlight 0 0)
                             (setq gripe--highlight-removal-timer nil))))))))

(defun gripe--show-config-on-user-error ()
  "Renders user configuration as a portion of the user error."
  (concat "Gripe configuration:\n"
          "  gripe-completion: " (prin1-to-string gripe-completion) "\n"
          "  gripe-highlight-duration: " (prin1-to-string gripe-highlight-duration) "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; S E L E C T R U M ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gripe--on-selectrum-selection (selected-key lookup)
  "Process a selectrum selection.
Takes in the SELECTED-KEY which is used to look up the
selected value in LOOKUP"
  (let* ((selected-entry (cl-remove-if-not
                          (lambda (entry)
                            (equal (car entry) selected-key))
                          lookup))
         (selected-val (car (cdr (car selected-entry)))))
    (gripe--go-to-occurrence selected-val)))

(defun gripe--selectrum (gripe-parsed-output)
  "Navigate through gripe results with selectrum.
* GRIPE-PARSED-OUTPUT - The output of `gripe--parse-grape-output'"
  (let* ((lookup (gripe--make-candidates gripe-parsed-output))
         (selected-key (if (fboundp 'selectrum-completing-read)
                           (selectrum-completing-read "Go to a pattern occurrence: " lookup nil t)
                         (user-error (concat "The function `selectrum-completing-read' is missing. "
                                             "Is `selectrum' installed?\n"
                                             (gripe--show-config-on-user-error))))))
    (gripe--on-selectrum-selection selected-key lookup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; H E L M ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gripe--make-helm-source (gripe-parsed-output)
  "Create a helm soure from GRIPE-PARSED-OUTPUT."
  (progn
    (if (fboundp 'helm-build-sync-source)
        (helm-build-sync-source "Pattern occurrences"
          :match (lambda (_candidate) t)
          :candidates (gripe--make-candidates gripe-parsed-output)
          ;; For some reason, helm returns a list for the
          ;; supposedly single selected candidate
          :action '(("Preview" . (lambda (multi-selected)
                                   (gripe--go-to-occurrence (car multi-selected))))))
      (user-error (concat "The function `helm-build-sync-source' is missing. Is `helm' installed?\n"
                          (gripe--show-config-on-user-error))))))

(defun gripe--helm (gripe-parsed-output)
  "Navigate through gripe results with helm.
* GRIPE-PARSED-OUTPUT - The output of `gripe--parse-grape-output'"
  (if (fboundp 'helm)
      (helm :sources (gripe--make-helm-source gripe-parsed-output)
            :buffer "*helm gripe*")
    (user-error (concat "The function `helm' is missing. Is `helm' installed?\n"
                        (gripe--show-config-on-user-error)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; I V Y ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gripe--ivy (gripe-parsed-output)
  "Navigate through gripe results with ivy.
* GRIPE-PARSED-OUTPUT - The output of `gripe--parse-grape-output'"
  (interactive)
  (progn
    (require 'ivy)
    (if (fboundp 'ivy-read)
        (ivy-read "Preview pattern match: " (gripe--make-candidates gripe-parsed-output)
                  :require-match t
                  :update-fn 'auto
                  ;; This returns '("{path}:{line}" ("{path}" "{line}")).
                  ;; We want to pass in only ("{path}" "{line}")
                  :action (lambda (selected-key-val)
                            (gripe--go-to-occurrence (car (cdr selected-key-val)))))
      (user-error (concat "The function `ivy-read' is missing. Is `ivy' installed?\n"
                          (gripe--show-config-on-user-error))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; P U B L I C - I N T E R F A C E ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun gripe-find (file-or-dir-path pattern)
  "Find occurrences of PATTERN within a FILE-OR-DIR-PATH.
For the format of PATTERN, see https://github.com/bfontaine/grape#command-line"
  (interactive
   (list (read-file-name "File/directory to search: ")
         (read-string "Pattern to search: ")))
  ;; TODO Check that grape exists
  (gripe--async-shell-command-to-string (concat "grape --unindent '" pattern "' "  file-or-dir-path)
                                        #'gripe--render-grape-output))

(provide 'gripe)
;;; gripe.el ends here
