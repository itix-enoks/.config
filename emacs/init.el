(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq-default
 backup-directory-alist '(("." . "~/.emacs.d.bak"))
 backup-by-copying t
 version-control t
 delete-old-versions t)

(add-to-list 'default-frame-alist '(font . "JuliaMono-12"))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(fringe-mode '(10 . 10))

(icomplete-mode 1)

(load-theme 'masked t)
(use-package nord-theme
  :ensure t
  :disabled
  :config (load-theme 'nord t))

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

(setq-default
 dired-listing-switches "-aBhl  --group-directories-first"

 mac-command-modifier 'meta

 frame-resize-pixelwise t
 window-resize-pixelwise t

 c-basic-offset 4
 cperl-indent-level 4
 tab-width 4
 indent-tabs-mode nil
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

(add-hook 'prog-mode-hook
          (lambda ()
            (let ((display-line-numbers-type 'relative))
              (display-line-numbers-mode -1))
            (electric-pair-local-mode -1)
            (global-hl-line-mode -1)
            (whitespace-mode)))

(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook (lambda ()
                             (setq cperl-indent-parens-as-block t)))

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-c s") 'shell-command)
(global-set-key (kbd "C-c i")
                (lambda () (interactive)
                  (eldoc-minibuffer-message "Formatting buffer...")
                  (whitespace-cleanup)
                  (untabify 0 (point-max))
                  (indent-region 0 (point-max))
                  (eldoc-minibuffer-message "Formatting buffer...done")))
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c l")
                (lambda () (interactive)
                  (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-<return>")
                (lambda () (interactive)
                  (let ((old-column (current-column)))
                    (move-end-of-line nil) (newline) (insert-char ?\s old-column))))

(use-package drag-stuff
  :ensure t
  :config
  (global-set-key (kbd "M-n") 'drag-stuff-down)
  (global-set-key (kbd "M-p") 'drag-stuff-up)
  (drag-stuff-mode 1))

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
 '(underline ((t nil)))
 '(variable-pitch ((t (:foundry "outline" :family "Roboto Mono")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("664111db1521fe3351061dc87aea95fa98b3f244f4b830fbc048d39c3a8bc125"
     "8ef15701c0f2453d232d60bfa0b81074f832b884c2e149ec2ae4c1a89c374b86"
     "13bd95b605d4415176da8feb6e58e077017f3d41489d2cb1aaae4db1584727ed"
     "acbcabc1a02827b86775a0abc440223fb58e9d98568ab6f70dfe262718762de9"
     "87113683fa3d4b2622bf6c976f4729cb4ef4a06af94bede814bed1b219c75bfa"
     "b00cc2256e10038afabfb9a6052eb8c14d6c642ebacce00531645cb0589d0f81"
     "e223120256455daba01b6c68510b48fac813acab05c314510e47aea377b23634"
     default))
 '(package-selected-packages
   '(base16-theme color-theme-modern company drag-stuff go-mode
                  highlight-numbers multiple-cursors nord-theme
                  powershell rainbow-mode rust-mode typescript-mode)))
