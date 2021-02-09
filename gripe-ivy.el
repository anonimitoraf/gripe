;;; gripe-ivy.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Rafael Nicdao
;;
;; Author: Rafael Nicdao <https://github.com/anonimito>
;; Maintainer: Rafael Nicdao <nicdaoraf@gmail.com>
;; Created: February 07, 2021
;; Modified: February 07, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/anonimito/gripe-ivy
;; Package-Requires: ((emacs "24.3") (ivy "0.8.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'cl-lib)
(require 'ivy)
(require 'gripe-common)

(defun gripe--ivy-go-to-occurrence (selected)
  "Go to the file and line number of the SELECTED grape occurrence."
  (let* (;; val is of shape '("path:line-num" ("path", "line-num"))
         (val (car (cdr selected)))
         (full-file-path (car val))
         (line-number (car (cdr val))))
    (when (file-exists-p full-file-path)
      (find-file full-file-path)
      (goto-line (string-to-number line-number)))))

(defun gripe--ivy (gripe-ast)
  "Navigate through gripe results with ivy.
* GRIPE-AST - The output of `gripe--make-grape-output-ast'"
  (interactive)
  (ivy-read "Preview pattern match: "
            (gripe--flatten-list-1
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
            :action #'gripe--ivy-go-to-occurrence))

(provide 'gripe-ivy)
;;; gripe-ivy.el ends here