;;; meta-git.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024
;;
;; Author:  <the_sf@sfd>
;; Maintainer:  <the_sf@sfd>
;; Created: January 20, 2024
;; Modified: January 20, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/the_sf/meta-git
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(provide 'meta-git)

(defun meta-git-plist-delete (plist prop)
  "Delete from PLIST the property PROP and return the modified plist."
  (let ((result nil))
    (while plist
      (let ((key (car plist))
            (val (cadr plist)))
        (unless (eq key prop)
          (push val result)
          (push key result)))
      (setq plist (cddr plist)))
    (nreverse result)))

(defun meta-git-in-directory(directory &rest body)
  (let* ((old-directory (cadr (string-split (pwd)))))
    (unwind-protect
        (progn
          (cd directory)
          (eval (cons 'progn body)))
      (cd old-directory))))


(defun meta-git-project-has-uncommitted-changes(directory)
  (meta-git-in-directory directory
                         (quote
                          (let* ((result (string-trim (shell-command-to-string "git status -uno --short | wc -l") nil "\n")))
                           (if (eq (cl-parse-integer result) 0) 0 1)))))

(defvar meta-git-dictionary (mapcar (lambda (d) (cons `(:path ,d) nil)) projectile-known-projects))

(defun meta-git-get-plist(path)
  (car (cl-find path meta-git-dictionary
                :test (lambda(s b)
                        (string= (plist-get (car b) :path) s)))))

(defun meta-git-update-dictionary()
  (mapcar (lambda (d) (plist-put! (meta-git-get-plist d) :has-changes (meta-git-project-has-uncommitted-changes d))) projectile-known-projects))

(defun meta-git-pad-to-multiple(maxn div)
  (+ maxn (- div (mod maxn div))))


(defvar meta-git-tab-width 4)

(defun meta-git-pad-path(path maxnum)
  (let* ((extra-spacing (/ (+ (- meta-git-tab-width 1)
                              (- maxnum (length path)))
                           meta-git-tab-width)))
    (concat path (make-string extra-spacing ?\t))))


(defun meta-git-render-table()
  (with-current-buffer (get-buffer-create "*meta-git*")
    (let* ((max-path-length (meta-git-pad-to-multiple (+ meta-git-tab-width (apply #'max (mapcar (lambda(d) (length (plist-get (car d) :path))) meta-git-dictionary))) meta-git-tab-width)))
      (erase-buffer)
      (mapc (lambda(d)
              (insert (format-message "%s%s\n" (meta-git-pad-path (plist-get (car d) :path) max-path-length)  (if (eq (plist-get (car d) :has-changes) 0) "✅" "❌"))))
            meta-git-dictionary)
      ;(insert (format))
      )))


(defun meta-git-update()
  (meta-git-update-dictionary)
  (meta-git-render-table))

(run-with-timer 1 30 #'meta-git-update)


;;; meta-git.el ends here
