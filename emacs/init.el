(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package sudo-edit
  :ensure t)

(setq-default
 mac-command-modifier 'meta

 backup-directory-alist '(("." . "~/.emacs.d.bak"))
 backup-by-copying t
 version-control t
 delete-old-versions t)

(add-to-list 'default-frame-alist '(font . "SF Mono-14"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode '(0 . 0))

(use-package base16-theme
  :ensure t
  :config (load-theme 'base16-default-dark t))

(use-package ivy
  :ensure t
  :config (ivy-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(setq-default
 insert-directory-program "gls"
 dired-use-ls-dired t
 dired-listing-switches "-aBhl  --group-directories-first"

 c-basic-offset 4
 cperl-indent-level 4
 tab-width 4
 indent-tabs-mode t
 column-number-mode t

 whitespace-style
 '(face
   tabs tab-mark
   spaces space-mark
   ;; newline newline-mark
   space-before-tab
   space-after-tab
   indentation
   trailing
   ;; lines
   empty
   missing-newline-at-eof))

(add-hook 'text-mode-hook
		  (lambda ()
			(evil-local-mode)))

(add-hook 'prog-mode-hook
		  (lambda ()
			(let ((display-line-numbers-type 'relative))
			  (display-line-numbers-mode -1))
			(electric-pair-local-mode 1)
			(global-hl-line-mode)
			(whitespace-mode)))

(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook (lambda ()
							 (setq cperl-indent-parens-as-block t)))

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-c s") 'shell-command)
(global-set-key (kbd "C-c i")
				(lambda () (interactive)
				  (eldoc-minibuffer-message "Indenting buffer...")
				  (indent-region 0 (point-max))
				  (eldoc-minibuffer-message "Indenting buffer...done")))
(global-set-key (kbd "C-c m") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c l")
				(lambda () (interactive)
				  (load-file "~/.emacs.d/init.el")))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package company
  :ensure t
  :config
  (setq
   company-backends '((company-capf
					   company-dabbrev-code
					   company-keywords
					   company-files))
   company-idle-delay 0.0
   company-minimum-prefix-length 2)
  :hook ((prog-mode . company-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t nil)))
 '(cperl-hash-face ((t (:inherit default))))
 '(underline ((t nil))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f700bc979515153bef7a52ca46a62c0aa519950cc06d539df4f3d38828944a2c" "1ad12cda71588cc82e74f1cabeed99705c6a60d23ee1bb355c293ba9c000d4ac" "00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729" "36c5acdaf85dda0dad1dd3ad643aacd478fb967960ee1f83981d160c52b3c8ac" "3d9938bbef24ecee9f2632cb25339bf2312d062b398f0dfb99b918f8f11e11b1" "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2" "a3a71b922fb6cbf9283884ac8a9109935e04550bcc5d2a05414a58c52a8ffc47" default))
 '(package-selected-packages
   '(base16-theme go-mode highlight-numbers rainbow-delimiters ivy vterm sudo-edit rainbow-mode nginx-mode multiple-cursors markdown-mode magit gdscript-mode ef-themes company)))
