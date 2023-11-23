
(straight-use-package 'undo-tree)
(straight-use-package 'posframe)
(straight-use-package 'evil-owl)

(use-package! undo-tree
  :config (global-undo-tree-mode)
  (define-key evil-normal-state-map "\C-r" nil)
  (evil-define-key 'normal 'global "\C-r" 'undo-tree-redo)
  (evil-define-key 'normal 'global "\M-r" 'undo-tree-visualize))

(use-package! evil
  :config (setq evil-emacs-state-modes  nil
                evil-insert-state-modes nil
                evil-normal-state-modes nil)
  (defvar colemak-mode (load-file "~/.emacs.d/colemak-mode.el"))
  (evil-define-key 'normal 'global (kbd "K") 'evil-ex-search-previous)
  (evil-define-key 'normal 'global (kbd ":") 'evil-paste-before)
  (evil-define-key 'normal 'global (kbd ".") 'evil-repeat)
  (evil-define-key 'normal 'global (kbd ",") 'evil-repeat-find-char-reverse)
  (map! :map evil-motion-state-map "g" #'evil-find-char-to)
  (map! :map evil-motion-state-map "C-v" #'evil-visual-block)
                                        ; disable evil-embrace stuff
  (setq! evil-want-Y-yank-to-eol nil))

(defun saving-special-register(f)
  `(lambda() (interactive)
     (evil-set-register ?t (evil-get-register ?\"))
     (call-interactively ,f)
     (evil-set-register ?\" (evil-get-register ?t))))

(use-package! general
  :config
  (evil-define-key 'normal 'global "s" (general-key-dispatch 'evil-delete
                                         "q" (saving-special-register '(general-simulate-key ('evil-delete "u\"")))
                                         "e" (saving-special-register '(general-simulate-key ('evil-delete "u(")))
                                         "v" (saving-special-register '(general-simulate-key ('evil-delete "u[")))
                                         "n" (saving-special-register '(general-simulate-key ('evil-delete "u<")))
                                         "s" 'evil-delete-whole-line))
  (evil-define-key 'normal 'global "c" (general-key-dispatch 'evil-change
                                         "q" (saving-special-register '(general-simulate-key ('evil-change "u\"")))
                                         "e" (saving-special-register '(general-simulate-key ('evil-change "u(")))
                                         "v" (saving-special-register '(general-simulate-key ('evil-change "u[")))
                                         "n" (saving-special-register '(general-simulate-key ('evil-change "u<")))
                                         "s" 'cd-to-buffer-dir
                                         "c" 'evil-change-whole-line))
  (evil-define-key 'normal 'global "j" (general-key-dispatch 'evil-yank
                                         "q" (saving-special-register '(general-simulate-key ('evil-yank "u\"")))
                                         "e" (saving-special-register '(general-simulate-key ('evil-yank "u(")))
                                         "v" (saving-special-register '(general-simulate-key ('evil-yank "u[")))
                                         "n" (saving-special-register '(general-simulate-key ('evil-yank "u<")))
                                         "j" 'evil-yank-whole-line)))

(map! :map evil-motion-state-map "/" #'helm-swoop-without-pre-input)
(map! :map evil-motion-state-map "?" #'helm-swoop-from-isearch)

(use-package! ansi-color)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(add-to-list 'default-frame-alist '(alpha 95 95))
(add-hook 'after-make-frame-functions (lambda (frame)
                                        (set-frame-font "Hack 14" t (list frame))))

(evil-define-key 'motion 'global (kbd "C-+") 'text-scale-increase)
(evil-define-key 'motion 'global (kbd "C--") 'text-scale-decrease)
(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")



(use-package! vterm
  :config (use-package! multi-vterm
            :hook (vterm-mode-hook . (lambda()
                                       (message "Setting up vterm mode")
                                       (evil-collection-vterm-setup)
                                       (evil-define-key '(insert normal) vterm-mode-map (kbd "C-c C-n") 'evil-collection-vterm-toggle-send-escape)
                                       (evil-define-key '(insert normal) vterm-mode-map (kbd "C-c C-c") 'vterm-send-C-c)
                                       (evil-define-key '(insert normal) vterm-mode-map (kbd "C-<escape>") 'vterm-copy-mode)
                                       (evil-define-key '(insert normal) vterm-mode-map (kbd "C-v") 'vterm-yank)
                                       (evil-define-key '(insert normal) vterm-mode-map (kbd "M-:") 'eval-expression)
                                       (evil-define-key '(insert normal) vterm-mode-map (kbd "C-c C-t") 'start-ab-popup)
                                       (evil-define-key '(insert normal) vterm-mode-map (kbd "C-x C-f") 'find-file))
                                   ))
  (after! multi-vterm
    (defun tshell()
      (interactive)
      (setq new-shell-name (read-from-minibuffer "shell buffer name: " nil nil nil nil "*shell*"))
      (multi-vterm)
      (rename-buffer new-shell-name))
    (define-key! doom-leader-map "s" #'tshell))
  )

(use-package! org-bullets
  :hook (org-mode-hook . (lambda ()
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
  (after! evil
    (evil-define-key 'normal 'global (kbd "C-x o") 'switch-window)
    (setq-default switch-window-shortcut-style 'qwerty)
    (setq-default switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" "w" "e" "i" "o"))
    (setq-default switch-window-minibuffer-shortcut ?z)))
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
(after! evil
  (define-key! doom-leader-map hh #'toggle-header))

(defun init-c++-mode()
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (setq-local company-backends '(company-capf company-yasnippet company-dabbrev))
  (setq c-basic-offset tab-width))
(add-hook 'c++-mode-hook 'init-c++-mode)

(add-to-list 'auto-mode-alist '("\\.inc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.I$" . c++-mode))


(defun clang-format-region-at-point()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (clang-format-region (car bounds) (cdr bounds))))

(use-package! clang-format
  :config (add-hook 'c++-mode-hook
                    (lambda()
                      (message "setting doom leader map hooks")
                      (define-key! doom-leader-map "f f" nil)
                      (define-key! doom-leader-map "f f" #'clang-format-buffer)
                      (define-key! doom-leader-map "f r" nil)
                      (define-key! doom-leader-map "f r" #'clang-format-region-at-point))))

(defun init-nxml-mode()
  (modify-syntax-entry ?_ "w" nxml-mode-syntax-table)
  (modify-syntax-entry ?' "'" nxml-mode-syntax-table))
(add-hook 'nxml-mode-hook 'init-nxml-mode)

(after! evil
  (after! avy
    (define-key! doom-leader-map "n" #'avy-goto-word-1)
    (define-key! doom-leader-map "e" #'avy-goto-char-1)
    (define-key! doom-leader-map "t" #'avy-goto-char-2)
    (map! :map evil-motion-state-map "/" #'helm-swoop-without-pre-input)
    (map! :map evil-motion-state-map "?" #'helm-swoop-from-isearch)
    (map! :map helm-swoop--basic-map "C-n" #'helm-next-line)
    (map! :map helm-swoop--basic-map "C-p" #'helm-previous-line)))

(after! python-mode
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)
  (setq python-shell-interpreter "/home/the_sf/.local/bin/ipython3"
        python-shell-interpreter-args "-i --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (add-hook 'inferior-python-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'python-mode-hook (lambda()
                                (rainbow-delimiters-mode-enable)
                                (modify-syntax-entry ?_ "w" python-mode-syntax-table))))
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-show-numbers t)

  (after! company-prescient
    (setq history-length 1000
          prescient-history-length 1000)))
(after! magit
  (evil-collection-magit-setup)
  (evil-define-key 'normal 'magit-mode-map "n" 'evil-next-visual-line)
  (evil-define-key 'normal 'magit-mode-map "e" 'evil-previous-visual-line)
  (evil-define-key 'normal 'magit-mode-map "i" 'evil-forward-char)
  (evil-define-key 'normal 'magit-mode-map "dd" 'evil-goto-first-line)
  (evil-define-key 'normal 'magit-mode-map "D" 'evil-goto-line))
(defun elisp-init-stuff()
  (interactive)
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

    (evil-define-key 'normal rustic-mode-map (kbd "SPC fr") 'rustic-format-region)
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
  (find-file "/home/the_sf/src/dotfiles/doom-dir/config.el"))
(evil-define-key 'normal 'global (kbd "C-c SPC c") 'edit-config-file)

(defun reload-emacs-config()
  (interactive)
  (load-file "/home/the_sf/src/dotfiles/init.el"))

(after! vterm
  (defun init-expected-buffers()
    (if (eq (get-buffer "main") nil)
        (progn
          (multi-vterm)
          (rename-buffer "main"))))
                                        ;(init-expected-buffers)
  )
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


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium"
      browse-url-generic-args (list "--incognito"))

                                        ; UMM

(setq display-line-numbers-type 'relative)


(after! evil
  (defun python-send-current-paragraph ()
    (interactive)
    (save-excursion
      (mark-paragraph)
      (let ((region (buffer-substring-no-properties (region-beginning) (region-end))))
        (python-shell-send-string region)))
    (deactivate-mark))
  (define-key! doom-leader-map "c s" #'python-send-current-paragraph))

(use-package! avy
  :config (setq avy-keys '(?r ?s ?t ?n ?e ?i))
  (define-key! doom-leader-map "n" #'avy-goto-word-1)
  (define-key! doom-leader-map "e" #'avy-goto-char-1)
  (define-key! doom-leader-map "t" #'avy-goto-char-2))


(use-package! evil-owl
  :config (setq evil-owl-display-method 'posframe
                evil-owl-extra-posframe-args '(:width 50 :height 20)
                evil-owl-max-string-length 40))

(set-frame-font "Hack 14")


                                        ; A helm menu for finding the right shell buffer
(defun select-vterm-buffer--format (buffer)
  (let* ((bname (buffer-name buffer))
         (bdir (buffer-local-value 'default-directory buffer))
         (left-col-len 32)
         (bname-len (length bname)))
    (while  (< bname-len left-col-len)
      (setq bname (concat bname "\t"))
      (setq bname-len (+ bname-len 4)))
    (concat bname bdir)))

(defun select-vterm-buffer ()
  (interactive)
  (let* ((vterm-buffers (seq-filter (lambda (buffer)

                                      (with-current-buffer buffer (derived-mode-p 'vterm-mode)))
                                    (buffer-list)))
         (selection-options (mapcar (lambda (b)
                                      (let* ((entry-name (select-vterm-buffer--format b)))
                                        (cons entry-name b)))
                                    vterm-buffers)))
    (helm :sources
          (helm-build-sync-source "Buffer"
                                  :candidates selection-options
                                  :action (lambda (candidate)
                                            (switch-to-buffer candidate)))
          :prompt "Select a vterm buffer: ")))

                                        ; A popup window for querying chatgpt
(defvar ab-popup-name "*ab-popup-buffer*")
(defun setup-ab-buffer()
  (with-current-buffer (get-buffer-create ab-popup-name)
    (org-mode)
    (gptel-mode)
    (insert "*** ")
    (evil-insert 1)))

(posframe-hide ab-popup-name)


(defun create-ab-popup-frame()
  (when (posframe-workable-p)
    (posframe-show ab-popup-name
                   :position (point)
                   :width 80
                   :height 30
                   :border-width 5
                   :border-color "#A7A6AA"
                   :cursor t
                   :cursor-color "#A7A6AA"
                   :accept-focus t
                   :refresh 1
                   :timeout 60)))
(defun select-ab-popup-frame()
  (let* ((misc-posframe-frame (with-current-buffer ab-popup-name posframe--frame)))
    (select-frame-set-input-focus misc-posframe-frame)
    (evil-define-key '(insert normal motion) (current-local-map) (kbd "C-c SPC k") (lambda() (interactive) (posframe-hide ab-popup-name)))))


(defun start-ab-popup()
  (interactive)
  (setup-ab-buffer)
  (create-ab-popup-frame)
  (select-ab-popup-frame))

(evil-define-key 'normal 'emacs-lisp-mode-map (kbd "C-c C-t") 'start-ab-popup)



(define-key! doom-leader-map "c" nil)
(define-key! doom-leader-map "c SPC" nil)
(define-key! doom-leader-map "c SPC c" 'recompile)
