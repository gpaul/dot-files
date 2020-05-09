(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (company-lsp lsp-mode dired-sidebar latex-preview-pane flymake-shellcheck company org-jira hlint-refactor lsp-ui lsp-haskell cargo lsp-rust flycheck-rust hindent hasky-stack flycheck-haskell exwm magit sublimity deadgrep rg projectile-ripgrep racer rust-mode toml-mode persp-projectile perspective markdown-mode fiplr protobuf-mode python-mode haskell-mode flymake-haskell-multi pungi jedi project-explorer flymake-go flycheck go-snippets go-projectile go-guru go-gopath go-errcheck go-dlv projectile ido-yes-or-no multiple-cursors go-autocomplete go-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 80)))))

;; the original isearch-style multi-cursors shortcuts
(require 'multiple-cursors)

;; It is really annoying that 'C->' doesn't move the screen to the next match.
;; This section fixes that.
(defun mc/mark-next-like-this-and-cycle-forward ()
  (interactive)
  (mc/mark-next-like-this 1)
  (mc/cycle-forward))

(defun mc/mark-previous-like-this-and-cycle-backward ()
  (interactive)
  (mc/mark-previous-like-this 1)
  (mc/cycle-backward))

(progn
 (add-to-list 'mc/cmds-to-run-once 'mc/mark-next-like-this-and-cycle-forward)
 (add-to-list 'mc/cmds-to-run-once 'mc/mark-previous-like-this-and-cycle-backward)
 (mc/save-lists)
 )

(global-set-key (kbd "C->") 'mc/mark-next-like-this-and-cycle-forward)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this-and-cycle-backward)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;; uncomment for phi-search integration
;; then overridden by phi-search
;(global-set-key (kbd "C-s") 'phi-search)
;(global-set-key (kbd "C-r") 'phi-search-backward)
;(phi-search-mc/setup-keys)
;(add-hook 'isearch-mode-hook 'phi-search-from-isearch-mc/setup-keys)

;(setq phi-search-limit           10000
;      phi-search-case-sensitive  'guess)


;(set-face-attribute 'phi-search-selection-face nil
;                    :background "orange")
;(set-face-attribute 'phi-search-match-face nil
;                    :background "red")
;;;;; uncomment for phi-search integration

(global-set-key (kbd "C-x C-k") 'kill-region)

(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

(toggle-fullscreen)

(ido-mode t)

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

(require 'go-projectile)

;; somehow useful for running godoc ... ?
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(require 'company)                                   ; load company mode
(require 'company-go)                                ; load company mode go backend

;(add-hook 'go-mode-hook #'lsp)

(defun my-go-mode-hook ()
  (add-to-list 'exec-path "/home/gpaul/gopath.tools/bin")
  (add-to-list 'exec-path "/home/gustav/gopath.tools/bin")
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .2)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;(require 'go-autocomplete)
;(require 'auto-complete-config)
;(ac-config-default)

;;Project Explorer
(require 'project-explorer)
(global-set-key (kbd "M-e") 'project-explorer-toggle)

;;Project Explorer omit regex

(setq pe/omit-regex (concat pe/omit-regex "\\|__pycache__"))

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(setq-default fill-column 79); wrap paragraphs at 79 characters

;; fuzzy project search using fiplr
(global-set-key (kbd "C-x f") 'fiplr-find-file)

;; enable projectile mode
(projectile-mode)

;; enable persp-projectile
(persp-mode)
(define-key projectile-mode-map (kbd "C-c p p") 'projectile-persp-switch-project)
(define-key projectile-mode-map (kbd "C-c p f") 'projectile-find-file)

;; rust

(require 'rust-mode)

;; Here we'll add the function that was dynamically generated by the
;; call to lsp-define-stdio-client to the major-mode hook of the
;; language we want to run it under.
;;
;; This function will turn lsp-mode on and call the command given to
;; start the LSP server.
(add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)

(with-eval-after-load 'lsp-mode
  (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (require 'lsp-rust))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;;; rust autocomplete
(add-hook 'racer-mode-hook #'company-mode)

(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

;;disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; don't ask: https://emacs.stackexchange.com/questions/15117/how-to-use-o-to-open-from-dired-ibuffer-into-another-frame
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; ripgrep bindings
(rg-enable-default-bindings)

(require 'sublimity-scroll)

;; haskell

(setq lsp-haskell-process-path-hie "hie-wrapper")
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)
(setq hindent-reformat-buffer-on-save t)
(add-hook 'haskell-mode-hook 'hlint-refactor-mode) ;; apply suggestions

(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp-haskell-enable)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
(add-hook 'haskell-mode-hook 'flycheck-mode)

;; set font size
;;(set-face-attribute 'default (selected-frame) :height 70)
;;(set-face-attribute 'default nil :height 90)
;;(set-face-attribute 'default nil :height 140)

; (desktop-save-mode 1)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 80)))))

(set-face-attribute 'default nil :height 80)
