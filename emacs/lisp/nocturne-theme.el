;;; nocturne-theme.el --- Nocturne color theme for GNU Emacs.

;; Copyright (C) 2024 M. Enes Kaya

;; Author: M. Enes Kaya
;; E-mail: enoks@tutanota.com
;; URL: https://github.com/enoks1/nocturne-theme
;; Version: 0.1

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; TODO: magit, isearch, icomplete

(deftheme nocturne ()
          "Nocturne theme for GNU Emacs")

;; colors with `+c' are lighter; and with `-c' darker
(let ((nocturne-bg+2        "gray19")
      (nocturne-bg+1        "gray7")
      (nocturne-bg          "gray0")

      (nocturne-hi          "cyan")

      (nocturne-fg          "gray99")
      (nocturne-fg-1        "gray66")
      (nocturne-fg-2        "gray33")

      ;; disable bold/italic change them to 'normal'
      (bold               'bold)
      (italic             'italic))

  (custom-theme-set-faces
   'nocturne

   ;; compilation
   `(compilation-info ((t (:foreground "green"))))
   `(compilation-warning ((t (:foreground "orange"))))
   `(compilation-error ((t (:foreground "red"))))
   `(compilation-mode-line-fail ((t (:foreground "red" :weight ,bold))))
   `(compilation-mode-line-exit ((t (:foreground "green" :weight ,bold))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,nocturne-fg :underline t))))
   `(font-lock-comment-face ((t (:foreground ,nocturne-fg-2))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,nocturne-fg-2))))
   `(font-lock-constant-face ((t (:foreground ,nocturne-fg-1))))
   `(font-lock-doc-face ((t (:foreground ,nocturne-fg-1))))
   `(font-lock-function-name-face ((t (:foreground ,nocturne-fg :weight ,bold :underline t))))
   `(font-lock-keyword-face ((t (:foreground ,nocturne-fg :weight ,bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,nocturne-fg :underline t))))
   `(font-lock-string-face ((t (:foreground ,nocturne-fg :underline t))))
   `(font-lock-type-face ((t (:foreground ,nocturne-fg :background ,nocturne-bg+2))))
   `(font-lock-variable-name-face ((t (:foreground ,nocturne-fg :slant ,italic))))
    `(font-lock-warning-face ((t (:foreground ,nocturne-fg))))
   `(font-lock-negation-char-face ((t (:foreground ,nocturne-fg :weight ,bold))))

   ;; general
   `(cursor ((t (:background ,nocturne-fg))))
   `(default ((t (:foreground ,nocturne-fg :background ,nocturne-bg))))
   `(fringe ((t (:foreground ,nocturne-fg :background ,nocturne-bg+1))))
   `(minibuffer-prompt ((t (:foreground ,nocturne-fg-1))))
   `(region ((t (:background ,nocturne-bg+2))))
   `(link ((t (:foreground ,nocturne-fg :underline t))))
   `(link-visited ((t (:foreground ,nocturne-fg-1 :underline t))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,nocturne-hi))))

   ;; line-numbers
   `(line-number ((t (:inherit default :foreground ,nocturne-bg+2))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,nocturne-bg+2 :weight ,bold))))

   ;; mode-line
   `(mode-line-active ((t (:foreground ,nocturne-fg :background ,nocturne-bg :slant ,italic))))
   `(mode-line-inactive ((t (:foreground ,nocturne-fg-1 :background ,nocturne-bg :slant ,italic))))
   `(mode-line-buffer-id ((t (:slant ,italic :weight ,bold))))

   ;; whitespace
   `(whitespace-space ((t (:foreground ,nocturne-bg+1 :background ,nocturne-bg))))
   `(whitespace-tab ((t (:foreground ,nocturne-bg+1 :background ,nocturne-bg))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nocturne)

;;; nocturne-theme.el ends here.
