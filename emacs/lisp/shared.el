;; Put this in your init.el file:
;; (setq load-path (cons "~/.emacs.d/lisp/" load-path))
;; (load-library "shared.el")
;; If you're on Windows, also specify your home directory BEFORE loading
;; the library:
;; (setq default-directory "C:/Users/enoks/")
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq disabled-command-function nil
      inhibit-splash-screen t)

(setq
 mac-command-modifier 'meta

 frame-resize-pixelwise t
 window-resize-pixelwise t

 c-basic-offset 2
 cperl-indent-level 2
 tab-width 2
 indent-tabs-mode nil
 column-number-mode t

 whitespace-style
 '(face
   ;;tabs tab-mark
   ;;spaces space-mark
   ;;newline newline-mark
   space-before-tab
   space-after-tab
   indentation
   trailing
   ;;lines
   empty
   missing-newline-at-eof))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode '(0 . 0))
(icomplete-mode 1)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c b") 'eval-buffer)
(global-set-key (kbd "C-c s") 'shell-command)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c I")
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
(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c y")
                (lambda () (interactive)
                  (let ((previous-column (current-column)))
                    (beginning-of-line)
                    (push-mark)
                    (end-of-line)
                    (kill-ring-save nil nil t)
                    (newline)
                    (yank)
                    (move-to-column previous-column))))

(add-hook 'prog-mode-hook
          (lambda ()
            (let ((display-line-numbers-type 'normal))
              (display-line-numbers-mode 1))
            (electric-pair-local-mode -1)
            (global-hl-line-mode -1)
            (whitespace-mode 1)))

(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook (lambda ()
                             (setq cperl-indent-parens-as-block t)))

(use-package highlight-numbers
  :disabled t
  :ensure t
  :hook ((prog-mode . highlight-numbers-mode)))

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
   company-minimum-prefix-length 3)
  :hook ((prog-mode . company-mode)))

(use-package magit
  :ensure t)

(use-package rust-mode
  :ensure t)
