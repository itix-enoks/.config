;;(setq default-directory "C:/Users/enoks/")
(setq load-path (cons "~/.emacs.d/lisp/" load-path))
(load-library "shared.el")

(set-frame-font "SF Mono")
(load-theme 'leuven t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(rust-mode magit company multiple-cursors drag-stuff)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
