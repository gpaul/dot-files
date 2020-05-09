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
 '(package-selected-packages (quote (phi-search-mc phi-search multiple-cursors))))
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
