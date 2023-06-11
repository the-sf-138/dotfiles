(after! undo-tree
        (global-undo-tree-mode)
        (define-key evil-normal-state-map "\C-r" nil)
        (evil-define-key 'normal 'global "\C-r" 'undo-tree-redo)
        (evil-define-key 'normal 'global "\M-r" 'undo-tree-visualize))

(after! evil
        (setq evil-emacs-state-modes  nil
              evil-insert-state-modes nil
              evil-normal-state-modes nil)
        (defvar colemak-mode (load-file "~/.emacs.d/colemak-mode.el"))
        (after! general
                (evil-define-key 'normal 'global "s" (general-key-dispatch 'evil-delete
                                                       "q" (general-simulate-key ('evil-delete "u\""))
                                                       "e" (general-simulate-key ('evil-delete "u("))
                                                       "v" (general-simulate-key ('evil-delete "u["))
                                                       "n" (general-simulate-key ('evil-delete "u<"))
                                                       "s" 'evil-delete-whole-line))
                (evil-define-key 'normal 'global "c" (general-key-dispatch 'evil-change
                                                       "q" (general-simulate-key ('evil-change "u\""))
                                                       "e" (general-simulate-key ('evil-change "u("))
                                                       "v" (general-simulate-key ('evil-change "u["))
                                                       "n" (general-simulate-key ('evil-change "u<"))
                                                       "s" 'cd-to-buffer-dir
                                                       "c" 'evil-change-whole-line))
                (evil-define-key 'normal 'global "j" (general-key-dispatch 'evil-yank
                                                       "q" (general-simulate-key ('evil-yank "u\""))
                                                       "e" (general-simulate-key ('evil-yank "u("))
                                                       "v" (general-simulate-key ('evil-yank "u["))
                                                       "n" (general-simulate-key ('evil-yank "u<"))
                                                       "j" 'evil-yank-whole-line))))



(after! avy
        (setq avy-keys '(?r ?s ?t ?n ?e ?i))
        (evil-define-key '(normal) 'global (kbd "SPC n") 'avy-goto-word-1)
        (evil-define-key '(normal) 'global (kbd "SPC e") 'avy-goto-char-1)
        (evil-define-key '(normal) 'global (kbd "SPC t") 'avy-goto-char-2))
(use-package ansi-color
  :ensure t)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
(add-hook 'after-make-frame-functions (lambda (frame)
                                        (set-frame-font "Hack 14" t (list frame))))

(evil-define-key 'motion 'global (kbd "C-+") 'text-scale-increase)
(evil-define-key 'motion 'global (kbd "C--") 'text-scale-decrease)
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")


(after! vterm
        (after! multi-vterm
                (add-hook 'vterm-mode-hook (lambda()
                                             (message "Setting up vterm mode")
                                             (evil-collection-vterm-setup)
                                             (evil-define-key '(insert normal) 'local (kbd "C-c C-n") 'evil-collection-vterm-toggle-send-escape)
                                             (evil-define-key '(insert normal) 'local (kbd "C-c C-c") 'vterm-send-C-c))
                          (evil-define-key '(insert normal) 'local (kbd "C-v") 'vterm-yank)))

        (defun tshell()
          (interactive)
          (setq new-shell-name (read-from-minibuffer "shell buffer name: " nil nil nil nil "*shell*"))
          (multi-vterm)
          (rename-buffer new-shell-name))

        (after! evil
                (evil-define-key 'normal 'global (kbd "SPC s") 'tshell)
                ))

(after! org-bullets
 (add-hook 'org-mode-hook (lambda ()
                            (linum-relative-mode)
                            (rainbow-delimiters-mode)
                            (evil-define-key 'normal 'global (kbd "SPC p") 'org-preview-latex-fragment)
                            (defface org-block-begin-line
                              '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
                              "face used for begin")
                            (defface org-block-background
                              '((t (:background "#FFFFEA")))
                              "face used for background")
                            (defface org-block-end-line
                              '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
                              "face used for end")
                            )))


(after! org
        (setq org-pretty-entities t)
        (setq org-return-follows-link t)
        (setq org-src-fontify-natively t)
        (setq org-src-preserve-indentation t)
        (setq org-todo-keywords
              '((sequence "TODO" "WRITEUP" "|" "DONE")))

        (plist-put org-format-latex-options :scale 2)

        (org-babel-do-load-languages
         'org-babel-load-languages '((R . t) (python . t)
                                     ))
        (setq org-confirm-babel-evaluate nil)
        (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))
(after! switch-window
        (evil-define-key 'normal 'global (kbd "C-x o") 'switch-window)
        (setq-default switch-window-shortcut-style 'qwerty)
        (setq-default switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" "w" "e" "i" "o"))
        (setq-default switch-window-minibuffer-shortcut ?z))
(after! projectile
        (setq projectile-per-project-compilation-buffer t)
        (evil-define-key '(normal) 'global (kbd "SPC ag") 'projectile-ag))
    (setq-default gdb-display-io-nopopup t)
(defun toggle-header-filename(filename)
  (if (equal "C" (file-name-extension filename))
      (concat (file-name-sans-extension filename) ".H")
    (concat (file-name-sans-extension filename) ".C")))
(defun is-cpp-ext(filename)
  (let ((ext (file-name-extension filename)))
    (or (equal ext "H") (equal ext "C"))))
(defun toggle-header()
  (interactive)
  (let ((curr-file (buffer-file-name (current-buffer))))
    (if (is-cpp-ext curr-file)
        (find-file (toggle-header-filename curr-file)))))
(evil-define-key '(normal) 'c++-mode-map (kbd "SPC hh") 'toggle-header)

(defun init-c++-mode()
  (linum-relative-mode)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (setq-local company-backends '(company-capf company-yasnippet company-dabbrev))
  (setq c-basic-offset tab-width))
(add-hook 'c++-mode-hook 'init-c++-mode)

(add-to-list 'auto-mode-alist '("\\.inc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.I$" . c++-mode))


(after! clang-format
        (evil-define-key 'normal 'global (kbd "SPC ff") 'clang-format-buffer))

(defun clang-format-region-at-point()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (clang-format-region (car bounds) (cdr bounds))))
(evil-define-key 'normal 'global (kbd "SPC fr") nil)
(evil-define-key 'normal 'c++-mode-map (kbd "SPC fr") 'clang-format-region-at-point)
(defun init-nxml-mode()
  (modify-syntax-entry ?_ "w" nxml-mode-syntax-table)
  (modify-syntax-entry ?' "'" nxml-mode-syntax-table))
(add-hook 'nxml-mode-hook 'init-nxml-mode)
(after! evil
  (after! helm-swoop
    (evil-define-key 'motion 'global (kbd "/") 'helm-swoop-without-pre-input)
    (evil-define-key 'motion 'global (kbd "?") 'helm-swoop-from-isearch)
    (evil-define-key 'motion 'global (kbd "SPC") nil)
    (evil-define-key 'motion 'global (kbd "SPC /") 'evil-search-forward)))

(after! python-mode
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  (setq python-shell-interpreter "/home/the_sf/.local/bin/ipython3"
        python-shell-interpreter-args "-i --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (add-hook 'inferior-python-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'python-mode-hook (lambda()
                                (modify-syntax-entry ?_ "w" python-mode-syntax-table))))
(after! company
        (setq company-idle-delay 0.1
              company-minimum-prefix-length 2
              company-show-numbers t)

        (after! company-prescient
                (setq history-length 1000
                      prescient-history-length 1000))
        )
(after! magit 
        (evil-collection-magit-setup)
        (evil-define-key 'normal 'magit-mode-map "n" 'evil-next-visual-line)
        (evil-define-key 'normal 'magit-mode-map "e" 'evil-previous-visual-line)
        (evil-define-key 'normal 'magit-mode-map "i" 'evil-forward-char)
        (evil-define-key 'normal 'magit-mode-map "dd" 'evil-goto-first-line)
        (evil-define-key 'normal 'magit-mode-map "D" 'evil-goto-line))
  (defun elisp-init-stuff()
    (interactive)
    (linum-relative-mode)
    (rainbow-delimiters-mode))
  (add-hook 'elisp-mode-hook 'elisp-init-stuff)
(defun cf-compile()
  (let* ((fname (buffer-name (current-buffer)))
         (problem (file-name-sans-extension fname))
         (command (concat "cf --problem " problem)))
    (compile command)))
(after! rustic
        (defun rust-init-stuff()
          (modify-syntax-entry ?_ "w" rustic-mode-syntax-table)
          (linum-relative-mode 1)

          (evil-define-key 'normal 'rustic-mode-map (kbd "SPC fr") 'rustic-format-region)
          (rainbow-delimiters-mode 1))

        (add-hook 'rustic-mode-hook 'rust-init-stuff))
(setq is-pyim-activated nil)

(defun toggle-pyim()
  (interactive)
  (cond ((eq is-pyim-activated nil) (progn (pyim-activate) (setq is-pyim-activated t)))
        (t (progn (pyim-deactivate) (setq is-pyim-activated nil)))))

(evil-define-key '(normal motion insert) 'global (kbd "C-c x") 'toggle-pyim)
(defun edit-config-file()
  (interactive)
  (find-file "/home/the_sf/src/dotfiles/config.org"))
(evil-define-key 'normal 'global (kbd "C-c SPC c") 'edit-config-file)

(defun reload-emacs-config()
  (interactive)
  (load-file "/home/the_sf/src/dotfiles/init.el"))

(defun init-expected-buffers()
  (if (eq (get-buffer "main") nil)
      (progn 
        (multi-vterm)
        (rename-buffer "main"))))
(init-expected-buffers)
(after! helpful
        (global-set-key (kbd "C-x c a") #'helpful-symbol))
(after! elfeed
  (setq elfeed-feeds
                '("https://lukesmith.xyz/index.xml"
                  "https://notstatschat.rbind.io/index.xml"
                  "https://statisticaloddsandends.wordpress.com/feed/"
                  "https://www.fharrell.com/index.xml"
                  "https://errorstatistics.com/feed/"
                  "https://www.countbayesie.com/blog?format=rss"
                  "https://www.allendowney.com/blog/feed/"
                  "https://almostsuremath.com/feed/"
                  )))
(after! gptel
        (setq gptel-api-key (with-temp-buffer
                              (insert-file-contents  "/home/the_sf/keys/open-ai/emacs-key-2")
                              (buffer-string)))
        (setq gptel-default-mode 'org-mode))


(after! linum-relative
        (setq linum-relative-backend 'display-line-numbers-mode))
