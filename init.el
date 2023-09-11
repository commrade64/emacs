;;________________________________________________________________
;;;    General settings
;;________________________________________________________________

;;; Packages
(require 'package)

;; initialize sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; package manager
(require 'use-package)
(setq use-package-always-ensure t)
;;;

;;; (better) general defaults
;;;; General But Better Defaults
(setq-default
  ad-redefinition-action 'accept     ; Silence warnings for redefinition.
  confirm-kill-emacs 'yes-or-no-p    ; Confirm before exiting Emacs.
  cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows.
  speedbar t                         ; Quick file access with bar.
  default-directory "~/"
  custom-safe-themes t
  load-prefer-newer t               ; don't use the compiled code if its the older package.
  ;delete-by-moving-to-trash t       ; move deleted files to trash.
  make-backup-files t               ; create backup file on save
  kept-new-versions 1               ; but only 1
  kept-old-versions 1               ; ...
  auto-save-default t               ; auto-save every buffer that visits a file.
  auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30).
  auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300).
  compilation-always-kill t         ; kill compilation process before starting another.
  compilation-ask-about-save nil    ; save all buffers on `compile'.
  compilation-scroll-output t
  tab-width 4
  indent-tabs-mode nil              ; set indentation with spaces instead of tabs with 4 spaces.
  indent-line-function 'insert-tab
  require-final-newline t
  x-select-enable-clipboard t       ; Makes killing/yanking interact with the clipboard.
  save-interprogram-paste-before-kill t ; Save clipboard strings into kill ring before replacing them.
  apropos-do-all t                  ; Shows all options when running apropos.
  mouse-yank-at-point t             ; Mouse yank commands yank at point instead of at click.
  message-log-max 1000
  ;fill-column 80
  make-pointer-invisible t          ; hide cursor when writing.
  column-number-mode t              ; show (line,column) in mode-line.
  cua-selection-mode t              ; delete regions.
  enable-recursive-minibuffers t    ; allow commands to be run on minibuffers.
  dired-kill-when-opening-new-dired-buffer t   ; delete dired buffer when opening another directory
  backward-delete-char-untabify-method 'hungry ; Alternatives is: 'all (remove all consecutive whitespace characters, even newlines).
)

(setq
  debug-on-error init-file-debug     ; Reduce debug output, well, unless we've asked for it.
  jka-compr-verbose init-file-debug
  read-process-output-max (* 64 1024); 64kb
  ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
  idle-update-delay 1.0              ; default is 0.5.
  scroll-step 1                      ; scroll with less jump.
  scroll-preserve-screen-position t
  scroll-margin 3
  scroll-conservatively 101
  scroll-up-aggressively 0.01
  scroll-down-aggressively 0.01
  lazy-lock-defer-on-scrolling t     ; set this to make scolloing faster.
  auto-window-vscroll nil            ; Lighten vertical scroll.
  fast-but-imprecise-scrolling nil
  mouse-wheel-scroll-amount '(1 ((shift) . 1))
  mouse-wheel-progressive-speed nil
  hscroll-step 1                     ; Horizontal Scroll.
  hscroll-margin 1
  help-window-select t               ; select help window when opened
  redisplay-skip-fontification-on-input t
  tab-always-indent 'complete        ; smart tab behavior - indent or complete.
  visible-bell t                     ; Flash the screen on error, don't beep.
  view-read-only t					; Toggle ON or OFF with M-x view-mode (or use e to exit view-mode).
  use-dialog-box nil                 ; Don't pop up UI dialogs when prompting.
  echo-keystrokes 0.1                ; Show Keystrokes in Progress Instantly.
  delete-auto-save-files t           ; deletes buffer's auto save file when it is saved or killed with no changes in it.
  kill-whole-line t 			        ; kills the entire line plus the newline
  save-place-forget-unreadable-files nil
  blink-matching-paren t             ; Blinking parenthesis.
  next-line-add-newlines nil         ; don't automatically add new line, when scroll down at the bottom of a buffer.
  require-final-newline t            ; require final new line.
  mouse-sel-retain-highlight t       ; keep mouse high-lighted.
  highlight-nonselected-windows nil
  transient-mark-mode t              ; highlight the stuff you are marking.
  ffap-machine-p-known 'reject       ; Don't ping things that look like domain names.
  pgtk-wait-for-event-timeout 0.001
  display-line-numbers-type 'relative
  speedbar-show-unknown-files t ; browse source tree with Speedbar file browser
)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;;________________________________________________________________
;;;;    Key-bindings
;;________________________________________________________________

(use-package general
  :config
  (general-evil-setup t)

   (general-create-definer custom/leader-keys
			   :keymaps '(normal insert visual emacs)
			   :prefix "SPC"
			   :global-prefix "C-SPC")
   (custom/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(defun custom/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; EVIL (VI Emulation Layer), yay!
(use-package evil
  :init
  (setq
   evil-auto-indent t
   evil-undo-system 'undo-tree
   evil-want-integration t
   evil-want-keybinding nil
   evil-want-C-u-scroll t
   evil-want-C-i-jump nil)
  ;; :hook (evil-mode . custom/leader-keys)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop) ;; insert tabs naturally
  ;;(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; use visual-line motions even outside of the visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;________________________________________________________________
;;;;    Fonts
;;________________________________________________________________

;; set default font

(add-hook 'after-make-frame-functions (lambda (f) (set-face-attribute 'default f :font (font-spec :family "monospace" :size 14.0 :weight 'regular))))

(add-hook 'after-make-frame-functions (lambda (f) (set-face-attribute 'org-document-title nil :font "monospace" :weight 'bold :height 1.2)))


;;________________________________________________________________
;;;;   UI
;;________________________________________________________________

;; disable welcome message and other ui stuff
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; colorscheme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;; mode-line
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;;________________________________________________________________
;;;    Additional packages and configurations
;;________________________________________________________________

;; ivy - a generic completion mechanism
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-inital-inputs-alist nil)) ;; don't start searches with 

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; hydra (continious use of a key)
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
	  "scale text"
	  ("j" text-scale-increase "in")
	  ("k" text-scale-decrease "out")
	  ("f" nil "finished" :exit t))

(custom/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects/code")
    (setq projectile-project-search-path '("~/projects/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-visualizer-diff nil)
  (undo-tree-history-directory-alist '(("." . "~/.local/state/emacs/undodir")))
  (undo-tree-visualizer-timestamps t))

;;; org mode
(use-package org
  :config
  (org-indent-mode 1)
  (visual-line-mode 1)
  (setq org-ellipsis "↴") ; 󰁆,↴,󱞣,󱞤,󰘌,▼,▶,󰘀
  (setq org-startup-indented t))

;;; increase the size of various heading
(dolist (face '((org-level-1 . 1.1)
                (org-level-2 . 1.075)
                (org-level-3 . 1.05)
                (org-level-4 . 1.025)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font "monospace" :weight 'medium :height (cdr face)))

;;; org-bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "✸" "◈" "◇")))

(defun custom/org-mode-visual-fill ()
  (setq visual-fill-column-width 75
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;;; visual-fill-column
(use-package visual-fill-column
  :hook (org-mode . custom/org-mode-visual-fill))


;;; IDK what this is, but Emacs generated it automagically
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-bullets catppuccin-theme undo-tree evil-magit magit counsel-projectile projectile hydra evil-collection evil general doom-themes helpful counsel which-key rainbow-delimiters ivy-rich doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
