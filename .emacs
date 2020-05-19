(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (helm-projectile helm projectile-ripgrep projectile go-mode company-lsp company lsp-ui lsp-mode phi-search-mc phi-search multiple-cursors))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; It is really annoying that 'C->' doesn't move the screen to the next match.
;; This section fixes that and uses a mixture of phi-search, phi-search-mc and multiple-cursors to get a vscode-like multiple-cursors experience.
(require 'multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; If isearch is active, grab the search phrase and active phi-search mode with it and perform a single forward search using phi-search.
;; Once phi-search mode is active, we need to call phi-search-mc/mark-next instead of
;; phi-search-isearch-mc/mark-next as isearch mode is no longer active then.
(defun mc-isearch/mark-next-like-this-and-cycle-forward ()
  (interactive)
  (message "isearch forward!")
  (phi-search-from-isearch-mc/mark-next 1)
  (phi-search-recenter))

;; If isearch is active, grab the search phrase and active phi-search mode with it and perform a single backwards search using phi-search.
;; Once phi-search mode is active, we need to call phi-search-mc/mark-previous instead of
;; phi-search-isearch-mc/mark-previous as isearch mode is no longer active then.
(defun mc-isearch/mark-previous-like-this-and-cycle-backward ()
  (interactive)
  (phi-search-from-isearch-mc/mark-previous 1)
  (mc/cycle-backward))

;; Add a cursor at the next occurence of the search filter.
;; This assumes that phi-search mode is already active and the search filter is populated.
(defun mc/mark-next-like-this-and-cycle-forward ()
  (interactive)
  (phi-search-mc/mark-next 1)
  (phi-search-recenter))

;; Add a cursor at the previous occurence of the search filter.
;; This assumes that phi-search mode is already active and the search filter is populated.
(defun mc/mark-previous-like-this-and-cycle-backward ()
  (interactive)
  (phi-search-mc/mark-previous 1)
  (mc/cycle-backward))

(require 'phi-search)

;; Setup key mapping for phi-search mode to call our recentering wrapper funcs instead
;; of the regular mc/mark-* funcs.
(defun gpaul-mc/setup-keys ()
  (let ((map phi-search-default-map))
    (define-key map [remap mc/mark-next-like-this]     'mc/mark-next-like-this-and-cycle-forward)
    (define-key map [remap mc/mark-previous-like-this] 'mc/mark-next-like-this-and-cycle-backward)
;    (define-key map [remap mc/mark-all-like-this]      'phi-search-mc/mark-all)
    ))
(gpaul-mc/setup-keys)

;; Setup key mapping for isearch mode to call our recentering wrapper funcs that switch to phi-search mode instead
;; of the regular mc/mark-* funcs.
(defun gpaul-mc-isearch/setup-keys ()
  (let ((map isearch-mode-map))
    (define-key map [remap phi-search]                 'phi-search-from-isearch)
    (define-key map [remap mc/mark-next-like-this]     'mc-isearch/mark-next-like-this-and-cycle-forward)
    (define-key map [remap mc/mark-previous-like-this] 'mc-isearch/mark-next-like-this-and-cycle-backward)
    ))
(add-hook 'isearch-mode-hook 'gpaul-mc-isearch/setup-keys)

(global-set-key (kbd "C-x C-k") 'kill-region)

(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

(toggle-fullscreen)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )



(setq scroll-step 1) ;; keyboard scroll one line at a time

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; list colum in gutter
(column-number-mode t)

;;disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; don't ask: https://emacs.stackexchange.com/questions/15117/how-to-use-o-to-open-from-dired-ibuffer-into-another-frame
(setq split-height-threshold nil)
(setq split-width-threshold nil)


(require 'lsp-mode)
(add-hook 'go-mode-hook 'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(require 'lsp-ui)
(require 'company)
(require 'company-lsp)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(global-set-key (kbd "C-'") 'company-complete)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable t
      lsp-enable-snippet nil
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)


(require 'projectile)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(helm-mode 1)

;; (setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(helm-projectile-on)

;; (projectile-mode +1)

;; Must enable helm before ido
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(ido-mode 1)

;; make font bigger
(set-face-attribute 'default nil :height 120)
