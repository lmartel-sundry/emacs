;;; PACKAGES
(message "CHECKING FOR PACKAGES...")

(setq package-list '(
                     solarized-theme
                     help+ help-fns+ help-mode+
                     flyspell flycheck ; find-file-in-project
                     smex ido-vertical-mode ; ido-ubiquitous
                     paredit ; smartparens
                     auctex reftex
                     robe company enh-ruby-mode
                     haskell-mode
                     markdown-mode
                     ; magit
                     ; evil helm
                     ; 4clojure clojure-mode
                    ))

; repositories
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                        ))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; CUSTOMIZATION
(message "LOADING CUSTOM SETTINGS...")
(load-theme 'solarized-dark t)

; Die tabs die
(setq-default indent-tabs-mode nil)

; Trim trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Do backups more often, but don't put them in the same damn folder
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))
(setq vc-make-backup-files t)
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 9   ;; Number of newest versions to keep.
      kept-old-versions 1   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)


; Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

; Helm M-x
; (require 'helm)
; (global-set-key (kbd "M-x") 'helm-M-x)

; Smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
; Old M-x
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

; Recentf
(require 'recentf)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(recentf-mode t)
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

; Smartparens default config
; (require 'smartparens-config)

; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; Flyspell
(setq ispell-dictionary "english"); Default dictionary. To change do M-x ispell-change-dictionary RET.

; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; LaTeX config
; AUCTex
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t)  ; Enable parse on save.
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)   ; PDF mode (rather than DVI-mode)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)


;; Ruby config
; Enh-ruby-mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . enh-ruby-mode))

; Robe and hooks
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))
(eval-after-load 'company
  '(push 'company-robe company-backends))


;; Haskell config
; haskell-mode
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)

;; Markdown config
; markdown-mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
