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

(defun gripe--ivy-preview-occurrence ()
  "Preview a gripe occurrence."
  (message (ivy-state-current ivy-last)))

(defun gripe--ivy (gripe-ast)
  "Navigate through gripe results with ivy.
* GRIPE-AST - The output of `gripe--make-grape-output-ast'"
  (interactive)
  (ivy-read "Preview pattern match: "
            (flatten-list
             (cl-map 'list
                     (lambda (occ-file)
                       (cl-map 'list
                               (lambda (occ-line)
                                 (concat (gripe--path-relative-from-project-root
                                          (gripe--occ-file-file-path occ-file))
                                         (gripe--occ-line-line-number occ-line)))
                               (gripe--occ-file-line-numbers occ-file)))
                     gripe-ast))
            :require-match t
            :update-fn #'gripe--ivy-preview-occurrence))

(provide 'gripe-ivy)
;;; gripe-ivy.el ends here
