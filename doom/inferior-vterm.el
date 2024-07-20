;;; inferior-vterm.el --- Send things to a vterm -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <the_sf@sfd>
;; Maintainer:  <the_sf@sfd>
;; Created: July 14, 2024
;; Modified: July 14, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/the_sf/inferior-vterm
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Send things to a vterm
;;
;;; Code:


(provide 'inferior-vterm)

(require 'vterm)
(require 'cl-lib)
(require 'helm)


(defun inferior-vterm-send-text (buffer-name text)
  (with-current-buffer buffer-name
    (vterm-send-string text)))

(defvar inferior-vterm-target-name nil)

(defun inferior-vterm-select-vterm-buffer--format (buffer)
  (let* ((bname (buffer-name buffer))
         (bdir (buffer-local-value 'default-directory buffer))
         (left-col-len 32)
         (bname-len (length bname)))
    (while  (< bname-len left-col-len)
      (setq bname (concat bname "\t"))
      (setq bname-len (+ bname-len 4)))
    (concat bname bdir)))

(defun inferior-vterm-select-vterm-buffer ()
  (interactive)
  (let* ((vterm-buffers (cl-remove-if-not (lambda (buffer)

                                      (with-current-buffer buffer (derived-mode-p 'vterm-mode)))
                                    (buffer-list)))
         (selection-options (mapcar (lambda (b)
                                      (let* ((entry-name (select-vterm-buffer--format b)))
                                        (cons entry-name b)))
                                    vterm-buffers)))
    (helm :sources
          (helm-build-sync-source "Buffer2"
            :candidates selection-options
            :action (lambda(c) c))
            :prompt "Select a vterm buffer: ")))


(defun inferior-vterm-assign-inferior()
  (interactive)
  (let* ((cur-buf-name (buffer-name (current-buffer)))
         (target-buf-name (buffer-name (inferior-vterm-select-vterm-buffer))))
  (message (concat "Getting called from inside of buffer: " cur-buf-name " pointing to target=" target-buf-name))
  (setq-local inferior-vterm-target-name target-buf-name)))


(defun inferior-vterm-send-region()
  (interactive)
  (if (eq inferior-vterm-target-name nil)
      (inferior-vterm-assign-inferior))
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (inferior-vterm-send-text
     inferior-vterm-target-name
  (buffer-substring
     (car bounds) (cdr bounds)))))


(define-minor-mode inferior-vterm-mode
  "Send regions of shell buffers to inferior vterms"
  :lighter "ivterm"
  :keymap (let* ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c SPC vm") 'inferior-vterm-assign-inferior)
            (define-key map (kbd "C-c C-y C-w") 'inferior-vterm-send-region)
            map)
  (make-local-variable 'inferior-vterm-target-name))

(add-hook 'sh-mode-hook 'inferior-vterm-mode)

(provide 'inferior-vterm-mode)

;;; inferior-vterm.el ends here
