(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-enable-at-startup t)
(package-initialize)

(setq package-check-signature nil)



(require 'bind-key)
(require 'use-package)
(require 'diminish)

;; Packages used
(use-package evil
  :ensure t
  :config (evil-mode))

;(use-package evil-leader
;  :ensure t
;  :config (global-evil-leader-mode)
;  (evil-leader/set-key "SPC"))

(require 'general)          ; better remapping (used in delimiter stuff)
(require 'hydra)            ; creating cool temporary mappings
(require 'ace-jump-mode)    ; better vesion of ez motion
(require 'ansi-color)


; tmux-like functionality
(use-package eyebrowse
  :config (eyebrowse-mode t)
  (evil-define-key 'motion 'global (kbd "M-k") 'eyebrowse-next-window-config)
  (evil-define-key 'motion 'global (kbd "M-m") 'eyebrowse-prev-window-config)
  (evil-define-key 'motion 'global (kbd "SPC") nil)
  (evil-define-key 'motion 'global (kbd "SPC ec") 'eyebrowse-create-window-config)
  (evil-define-key 'motion 'global (kbd "SPC er") 'eyebrowse-rename-window-config)
  (evil-define-key 'motion 'global (kbd "SPC eo") 'eyebrowse-switch-to-window-config))

(global-linum-mode t)

;; attempt to make shell mode less bad
(use-package bash-completion
  :config (bash-completion-setup))

; cool bar with mode name and stuff
(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1))

(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode)
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map)))

; compilation stuff
(setq compilation-scroll-output t)
; colorize the compilation output

(defun colorize-compilation-buffer()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

; Make the shell look pretty
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)

(defun set-up-comint-mode()
  (message " === setting up comint mode")
  (linum-mode -1)
  (whitespace-mode -1))
(add-hook 'comint-mode-hook #'set-up-comint-mode)

;; next-error will skip warning
(setq compilation-skip-threshold 1)

;; Evil Mode ish
(evil-mode t)

;; Show cool line numbers
(setq global-linum-mode t)

(defun write-struct-def ()
  (interactive)
  (insert "struct  {\n};")
  (evil-line 0)
  (evil-move-end-of-line)
  (backward-char 2)
  (evil-insert-state))

;; cpp bindings
(evil-define-key 'insert 'global "\C-k" "#include <")
(evil-define-key 'insert 'global "\C-u" "#pragma once")
(evil-define-key 'insert 'global "\C-c" 'write-struct-def)
(evil-define-key 'insert 'global "\C-t" "template <")
(evil-define-key 'insert 'global "\C-y" "typename ")
(evil-define-key 'insert 'global "\C-d" "auto&&")
; in cpp we want the underscore to be part of words

(defun show-all-window-configs()
  (interactive)
  (print (mapcar 'window-buffer (window-list))))

;-----------------------------------------------------------
;                    LUA
;-----------------------------------------------------------
(use-package lua-mode
  :ensure t)

;-----------------------------------------------------------
;                   HYDRA
;-----------------------------------------------------------

;; Hydra for moving around and navigate a window / set of panes
;; ends with a jump to word
(defhydra hydra-move (:post (progn (call-interactively 'ace-jump-mode)))
  "move"
  ("f" (progn (message "jumping...")) "jump hydra" :exit t)
  ("i" (progn (evil-scroll-right 0)))
  ("e" (progn (evil-scroll-up 0)))
  ("n" (progn (evil-scroll-down 0)))
  ("h" (progn (evil-scroll-left 0)))
  ("k" (progn (eyebrowse-next-window-config nil)))
  ("m" (progn (eyebrowse-prev-window-config 1)))
  ("s" (progn (show-all-window-configs)))
  ("I" (progn (windmove-right)))
  ("E" (progn (windmove-up)))
  ("N" (progn (windmove-down)))
  ("H" (progn (windmove-left))))
(evil-define-key nil 'global "\C-s" 'hydra-move/body)

(defhydra hydra-compilation ()
  "navigate compilation mode"
  ("c" (progn (message "exitting...")) "compilation hydra" :exit t)
  ("j" (progn (compilation-next-error 1)))
  ("f" (progn (compile-goto-error)))
  ("r" (progn (recompile)))
  ("gg" evil-goto-first-line)
  ("G" evil-goto-line)
  ("d" (progn (compilation-previous-error 1))))
(evil-define-key 'motion 'global "\C-u" 'hydra-compilation/body)

(defun swl()
  (interactive)
  (shrink-window 10))

(defun ewl()
  (interactive)
  (enlarge-window 10))

(defhydra hydra-layout()
  "generate a buffer layout"
  ("c" (progn (message "exiting hydra-layout")) "layout" :exit t)
  ("f" (progn (evil-window-split)))
  ("d" (progn (evil-window-vsplit)))
  ("l" (progn (shrink-window-horizontally 10)))
  ("h" (progn (enlarge-window-horizontally 10)))
  ("k" (progn (shrink-window 10)))
  ("j" (progn (enlarge-window 10)))
  ("L" (progn (windmove-right)))
  ("K" (progn (windmove-up)))
  ("J" (progn (windmove-down)))
  ("H" (progn (windmove-left))))
(evil-define-key '(normal motion) 'global (kbd "C-x l") 'hydra-layout/body)

(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs"))

(defun clean-evil-escape()
  (interactive)
  (message "escaping clean")
  (keyboard-quit)
  (evil-escape)
  (keyboard-quit))
(evil-define-key '(normal motion insert visual) 'global (kbd "ESC") nil)


(define-key evil-normal-state-map "\C-r" nil)
(evil-define-key 'normal 'global "\C-r" 'undo-tree-redo)
(evil-define-key 'normal 'global "\M-r" 'undo-tree-visualize)

; jumping around
(evil-define-key '(normal) 'global (kbd "SPC SPC") 'ace-jump-mode)

(define-key evil-normal-state-map "\C-h" nil)
(evil-define-key 'motion 'global "\C-b" 'helm-apropos)

(setq shell-file-name "zsh")
(setq shell-command-switch "-ic")

;; Highlight whitespace and lines > 160 chars
(setq-default whitespace-line-column 160)

; indentation shit
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)
(setq-default tab-width 4)

(defvar colemake-mode (load-file "/home/the_sf/.emacs.d/colemak-mode.el"))

(load-theme 'gruvbox t)
(menu-bar-mode 1)

; So that the shell doesnt break tring to put stuff in less
(setenv "PAGER" "cat")

;; bullshit for tabs
;; TODO: figure out how to not have this binding in term-mode
(setq indent-tabs-mode nil)
(evil-define-key 'insert 'global (kbd "TAB") 'tab-to-tab-stop)

; using the system clipboard
(evil-define-key 'normal 'global "\C-c" 'clipboard-yank)
(evil-define-key 'normal 'global "\C-v" nil)


(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (lambda ()
                                   (org-bullets-mode 1))))

(defun print-major-mode()
  (interactive)
  (print major-mode))

(evil-define-key '(normal insert) 'global (kbd "C-SPC") 'completion-help-at-)
(evil-define-key '(normal insert) 'global "\C-p" nil)

(evil-define-key '(normal motion) 'global "\M-x" 'helm-M-x)


;; Get rid of the annoying compliation mode map
(add-hook 'compilation-mode-hook (lambda() (setq compilation-mode-map evil-normal-state-map)))

; start all modes in normal
(setq evil-emacs-state-modes  nil)
(setq evil-insert-state-modes nil)
(setq evil-normal-state-modes nil)

(defun cd-to-buffer-dir()
  (interactive)
  (cd default-directory))

;; setting up stuff for working with delimiters
(evil-define-key 'normal 'global "d" (general-key-dispatch 'evil-delete
                                                                                "q" (general-simulate-key ('evil-delete "i\""))
                                                                                "k" (general-simulate-key ('evil-delete "i("))
                                                                                "v" (general-simulate-key ('evil-delete "i["))
                                                                                "j" (general-simulate-key ('evil-delete "i<"))
                                                                                "c"  'evil-delete-whole-line))

(evil-define-key 'normal 'global "c" (general-key-dispatch 'evil-change
                                                                                "q" (general-simulate-key ('evil-change "i\""))
                                                                                "k" (general-simulate-key ('evil-change "i("))
                                                                                "v" (general-simulate-key ('evil-change "i["))
                                                                                "j" (general-simulate-key ('evil-change "i<"))
                                                                                "r" (general-simulate-key ('evil-change "i'"))
                                                                                "d" 'cd-to-buffer-dir
                                                                                "c"  'evil-change-whole-line))

(evil-define-key 'normal 'global "y" (general-key-dispatch 'evil-yank
                                                                                "q" (general-simulate-key ('evil-yank "i\""))
                                                                                "k" (general-simulate-key ('evil-yank "i("))
                                                                                "v" (general-simulate-key ('evil-yank "i["))
                                                                                "j" (general-simulate-key ('evil-yank "i<"))
                                                                                "y"  'evil-yank-whole-line))

(evil-define-key 'normal 'global "\C-t" 'run-eq-align)

;; A bunch of shitty functions to find code and configs faster
;; TODO make these a less turd macro or something
(defmacro define-project (project-name project-dir)
  `(defun ,project-name()
        (interactive)
  (setq saved-dir default-directory)
  (cd ,project-dir)
  (call-interactively 'find-file)))

(desktop-save-mode 1)

;; default to window'd stuff
(evil-define-key 'normal 'global (kbd "C-;") nil)
(evil-define-key 'normal 'global (kbd "C-;") 'evil-command-window)
(evil-define-key 'normal 'global (kbd "C-/") 'evil-command-window-search-forward)

;; message the last time this file was modified
(defun lt()
  (interactive)
  (message (format-time-string "%D %H:%M:%S" (visited-file-modtime))))


(defun lcd()
  (interactive)
  (cd (file-name-directory (buffer-file-name))))


;------------------------------------
;                ESS
;------------------------------------
;(use-package ess
;             :init (require 'ess-site)
;             :config
;             (setq inferior-R-program-name "R_PATH_HERE")
;             (setq ess-eval-visibly-p nil)
;             :ensure ess)

;  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))
(defun ess-init-stuff()
  (modify-syntax-entry ?_ "w" ess-r-mode-syntax-table))
(add-hook 'ess-r-mode-hook 'ess-init-stuff)


;------------------------------------
;                SWITCH-WINDOW
;------------------------------------
(use-package switch-window
  :ensure t
  :bind (
         ("C-x o" . switch-window))
  :config
  (setq-default switch-window-shortcut-style 'qwerty)
  (setq-default switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" "w" "e" "i" "o"))
  (setq-default switch-window-minibuffer-shortcut ?z))

; special stuff fot when running in a terminal
(if (not window-system)
    (set-terminal-coding-system 'utf-8))

(setq-default gdb-display-io-nopopup t) ; prevent annoying io buffer

;; so that underscores are considered part of the word
(defun cpp-init-stuff()
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (setq-local company-backends '(company-lsp company-yasnippet company-dabbrev))
  (setq c-basic-offset tab-width))
(add-hook 'c++-mode-hook 'cpp-init-stuff)

(defun nxml-init-stuff()
  (modify-syntax-entry ?_ "w" nxml-mode-syntax-table)
  (modify-syntax-entry ?' "'" nxml-mode-syntax-table))
(add-hook 'nxml-mode-hook 'nxml-init-stuff)

;--------------------------
;           HELM
;--------------------------
(use-package helm
  :ensure t
  :config (helm-mode))

;(setq helm-locate-command locate-db-command)
;(evil-define-key '(normal motion) 'global (kbd "C-x d") 'helm-locate)
;(evil-define-key '(normal motion) 'global "\C-f" 'helm-find-files)

(define-key evil-normal-state-map (kbd "C-x C-f") 'helm-find-files)
(define-key evil-normal-state-map (kbd "SPC df") 'helm-find-files)
(define-key evil-normal-state-map (kbd "SPC k") 'helm-resume)



;--------------------------
;           ELPY
;--------------------------
(use-package elpy
  :ensure t
  :config (elpy-enable)
  (setenv "IPY_TEST_SIMPLE_PROMPT" nil)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (add-hook 'inferior-python-mode-hook 'ansi-color-for-comint-mode-on))

;--------------------------------------------
;                   CPP
;--------------------------------------------
(add-to-list 'auto-mode-alist '("\\.inc$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.I$" . c++-mode))

;--------------------------------------------
;                   SHELL
;--------------------------------------------
(defun tshell()
  (interactive)
  (setq new-shell-name (read-from-minibuffer "shell buffer name: " nil nil nil nil "*shell*"))
  (shell)
  (rename-buffer new-shell-name))
(evil-define-key 'normal 'global (kbd "SPC tm") 'tshell)



;--------------------------------------------
;                   ORG-MODE
;--------------------------------------------
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(eval-after-load 'org '(require 'org-pdfview))
(add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda(file link) (org-pdfview-open link))))
(setq org-return-follows-link t)

(setq org-mobile-directory "~/orgmobile")

      ;(setq company-backends '(company-lsp company-elisp company-dabbrev))

(org-babel-do-load-languages
'org-babel-load-languages '((R . t) (python . t)))

(setq org-src-fontify-natively t)
(setq org-pretty-entities t)

(evil-define-key 'normal 'global (kbd "SPC c") 'org-cycle)

;--------------------------------------------
;                   YASNIPPET
;--------------------------------------------
;(setq tags-table-listi '())
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package company
  :ensure t
  :config (global-company-mode)
  (setq company-idle-delay 0.0))

(use-package lsp-mode
  :ensure t)
(require 'lsp-clients)

(use-package helm-swoop
  :ensure t
  :config
  (evil-define-key 'motion 'global (kbd "/") 'helm-swoop-without-pre-input)
  (evil-define-key 'motion 'global (kbd "?") 'helm-swoop-from-isearch)
  (evil-define-key 'motion 'global (kbd "SPC") nil)
  (evil-define-key 'motion 'global (kbd "SPC /") 'evil-search-forward))

;; TODO get special shit so colemak mode works
(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-override-mode)
  (turn-off-evil-snipe-mode)
  (evil-snipe-mode -1)
  (setq evil-snipe-scope 'whole-visible)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))


(use-package magit
  :ensure t)
(use-package evil-magit
  :ensure t)


; Stuff for transparency
; xcompgr -C &
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

(use-package clang-format
  :ensure t)
(evil-define-key 'normal 'global (kbd "SPC ff") 'clang-format-buffer)

(defun clang-format-region-at-point()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'paragraph)))
    (clang-format-region (car bounds) (cdr bounds))))
(evil-define-key 'normal 'global (kbd "SPC fr") 'clang-format-region-at-point)

;--------------------------------------------
;                   CTAG
;--------------------------------------------
;(setq tags-table-list '())

;------------------------------------------
;                   CFParser
;------------------------------------------
(add-to-list 'load-path "/home/the_sf/src/cfparser")
(require 'cf-mode)


(evil-define-key 'normal 'global (kbd "SPC ag") 'helm-do-grep-ag)
(defun git-diff-to-buffer()
  (interactive)
  (shell-command "git diff"))
(evil-define-key 'normal 'global (kbd "SPC dg") 'git-diff-to-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(evil-snipe-mode t)
 '(package-selected-packages
   (quote
    (company-lsp lsp-mode clang-format leetcode lua-mode evil-magit magit evil-colemak-basics evil-colemak-minimal irony spacemacs-theme evil-snipe try yasnippet-snippets org-pdfview pdf-view-restore pdf-tools org-bullets evil-surround ess switch-window xterm-color use-package telephone-line soothe-theme modalka hydra helm haskell-mode gruvbox-theme general eyebrowse evil-visual-mark-mode evil-easymotion elpy doom disable-mouse diminish darktooth-theme color-theme bash-completion auto-complete ace-window ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
