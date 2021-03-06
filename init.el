;;; PACKAGES
(message "CHECKING FOR PACKAGES...")

(setq package-list '(
                     exec-path-from-shell
                     solarized-theme
                     help+ help-fns+ help-mode+
                     org
                     iedit
                     flyspell flycheck ; find-file-in-project
                     smex ido-vertical-mode ; ido-ubiquitous
                     smartparens ; paredit
                     auctex reftex
                     robe company enh-ruby-mode rvm
                     haskell-mode flycheck-haskell company-ghc
                     markdown-mode
                     ; magit
                     ; evil helm
                     4clojure clojure-mode
                     typescript-mode tss
                    ))

; repositories
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
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


; Load $PATH properly even if launched from OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; CUSTOMIZATION
(message "LOADING CUSTOM SETTINGS...")

;; Visual stuff
(load-theme 'solarized-dark t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))


;; Global shortcuts, inspired by Yegge's Effective Emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\M-s" 'isearch-forward-regexp)
(global-set-key "\M-r" 'isearch-backward-regexp)


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
(require 'smartparens-config)

; Clojure-mode
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

; Flyspell
(setq ispell-dictionary "english"); Default dictionary. To change do M-x ispell-change-dictionary RET.

; Company mode
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-ghc))

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
; Use rvm, not system ruby
(require 'rvm)
(rvm-use-default)

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


;; Typescript config
(require 'tss)
(setq tss-popup-help-key "C-:")
(setq tss-jump-to-definition-key "C->")
(setq tss-implement-definition-key "C-c i")
(tss-config-default)
(add-hook 'typescript-mode-hook 'tss-setup-current-buffer)

;; Haskell config
; haskell-mode
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
; (setq haskell-process-type 'stack-ghci)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


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
 '(company-backends
   (quote
    (capf company-robe company-capf company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-cmake company-capf
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-oddmuse company-files company-dabbrev)))
 '(company-idle-delay 0)
 '(flycheck-idle-change-delay 0)
 '(haskell-check-command "hlint"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
