;;; evil-ts.el --- Add actions to evil using treesit. -*- lexical-binding: t; -*-

;; Version: 0.0.02
;; URL: https://github.com/foxfriday/evil-ts
;; Package-Requires: ((emacs "29") (evil "1"))

;;; Commentary:
;; This package has the minor mode evil-ts-mode. Activating the minor mode
;; add some actions to evil mode. There are some text objects and some
;; functions to move around nodes.

;;; Code:

(require 'evil)
(require 'rx)
(require 'treesit)

(defvar evil-ts-condition (rx (or "if" "for" "try" "with" "while") "_statement")
  "Regex used to move to next or last condition.")

(defvar evil-ts-function "function_definition"
  "Regex used to move to next or last class.")

(defvar evil-ts-class "class_definition"
  "Regex used to move to next or last class.")

(defun evil-ts-beginning-of-class ()
  "Move to the start of a class definition."
  (interactive)
  (treesit-beginning-of-thing evil-ts-class))

(defun evil-ts-end-of-class ()
  "Move to the end of a class definition."
  (interactive)
  (treesit-end-of-thing evil-ts-class))

(defun evil-ts-beginning-of-condition ()
  "Move to the start of a condition definition."
  (interactive)
  (treesit-beginning-of-thing evil-ts-condition))

(defun evil-ts-end-of-condition ()
  "Move to the end of a condition definition."
  (interactive)
  (treesit-end-of-thing evil-ts-condition))

(defun evil-ts-select-obj (obj)
  "Select the region described by OBJ."
  (let* ((node (treesit-thing-at-point obj 'nested))
         (start (if node (treesit-node-start node) nil))
         (end (if node (treesit-node-end node) nil)))
    (when node
      (goto-char end)
      (list start end))))

(defun evil-ts-expand-region ()
  "Expand selection to the closet parent."
  (let* ((point (point))
         (mark (or (mark t) point))
         (start (min point mark))
         (end (max point mark))
         (node (treesit-node-at start))
         (parent (treesit-parent-until node
                                       (lambda (n) (and (> start (treesit-node-start  n))
                                                        (< end (treesit-node-end n))))
                                       nil))
         (pstart (if parent (treesit-node-start parent) nil))
         (pend (if parent (treesit-node-end parent) nil)))
    (when parent
      (goto-char pstart)
      (list pstart pend))))

(evil-define-text-object evil-ts-text-obj-cond (count &optional beg end type)
  (evil-ts-select-obj evil-ts-condition))

(evil-define-text-object evil-ts-text-obj-fun (count &optional beg end type)
  (evil-ts-select-obj evil-ts-function))

(evil-define-text-object evil-ts-text-obj-class (count &optional beg end type)
  (evil-ts-select-obj evil-ts-class))

(evil-define-text-object evil-ts-text-obj-expand-region (count &optional beg end type)
  (evil-ts-expand-region))

(provide 'evil-ts)
;;; evil-ts.el ends here
