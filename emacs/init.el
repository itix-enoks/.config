(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq-default
 backup-directory-alist '(("." . "~/.emacs.d.bak"))
 backup-by-copying t
 version-control t
 delete-old-versions t)

(let ((font-name (cond ((string= system-name "DESKTOP-X3N") "Hack-10")
                       ((string= system-name "Muhammeds-Air.home") "Menlo-12"))))
  (set-frame-font font-name))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(fringe-mode '(0 . 0))

(icomplete-mode 1)

(load-theme 'masked t)

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
            (electric-pair-local-mode 1)
            (global-hl-line-mode 1)
            (whitespace-mode 1)
            (visual-line-mode 1)))

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
   '("20ec17410ca506cfda88f9101834cd5a9f75a7c02a97489f8aedc93d75d34b8c"
     "8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f"
     "e5db25f43c0078e14f813981f4edf1770713b98c2151a968f465f8847322b60c"
     default))
 '(package-selected-packages
   '(base16-theme company drag-stuff ef-themes gdscript-mode go-mode
                  goto-chg highlight-numbers ivy magit markdown-mode
                  multiple-cursors nginx-mode rainbow-delimiters
                  rainbow-mode rust-mode sudo-edit typescript-mode
                  vterm web-mode)))
