;;; masked-theme.el --- Masked color theme for GNU Emacs.

;; Copyright (C) 2024 M. Enes Kaya

;; Author: M. Enes Kaya
;; E-mail: enoks@tutanota.com
;; URL: https://github.com/enoks1/masked-theme
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

(deftheme masked
  "Masked theme for GNU Emacs.")

(defgroup masked nil
  "Masked theme and it's settings."
  :group 'masked)

(defcustom masked-theme-enable-italic t
  "Enable or disable all italic faces."
  :type 'boolean)

(defcustom masked-theme-enable-bold t
  "Enable or disable all bold faces."
  :type 'boolean)

;; colors with `+c' are lighter; and with `-c' darker
(let ((masked-bg+3        "#4a8a7a")
      (masked-bg+2        "#3a6d60")
      (masked-bg+1        "#2a5046")
      (masked-bg          "#00382c")
      (masked-bg-1        "#0f1f1a")

      (masked-red         "#c77575")
      (masked-green       "#87f09d")
      (masked-yellow      "#987d3e")
      (masked-cyan        "#d1b687")
      (masked-magenta     "#897399")

      (masked-fg          "#c7b176")
      (masked-fg-1        "#969696")
      (masked-fg-2        "#696969")

      (masked-green-hi    "#11ff11")

      (masked-black       "#181818")
      (masked-white       "#dddddd")

      (italic             (if masked-theme-enable-italic 'italic 'normal))
      (bold               (if masked-theme-enable-bold 'bold 'normal)))

  (custom-theme-set-faces
   'masked

   ;; ansi-term / vterm
   `(term-color-black ((t (:foreground ,masked-black :background ,masked-black))))
   `(term-color-red ((t (:foreground ,masked-red :background ,masked-red))))
   `(term-color-green ((t (:foreground ,masked-green :background ,masked-green))))
   `(term-color-blue ((t (:foreground ,masked-cyan :background ,masked-cyan))))
   `(term-color-yellow ((t (:foreground ,masked-yellow :background ,masked-yellow))))
   `(term-color-magenta ((t (:foreground ,masked-magenta :background ,masked-magenta))))
   `(term-color-cyan ((t (:foreground ,masked-cyan :background ,masked-cyan))))
   `(term-color-white ((t (:foreground ,masked-fg :background ,masked-fg))))

   ;; compilation
   `(compilation-info ((t (:foreground ,masked-green))))
   `(compilation-warning ((t (:foreground ,masked-yellow))))
   `(compilation-error ((t (:foreground ,masked-red))))
   `(compilation-mode-line-fail ((t (:foreground ,masked-red :weight ,bold))))
   `(compilation-mode-line-exit ((t (:foreground ,masked-green :weight ,bold))))

   ;; dired
   `(dired-directory ((t (:foreground ,masked-cyan :weight ,bold))))
   `(dired-ignored ((t (:foreground ,masked-cyan))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,masked-green))))
   `(font-lock-comment-face ((t (:foreground ,masked-green-hi))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:foreground ,masked-fg))))
   `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-function-name-face ((t (:foreground ,masked-fg))))
   `(font-lock-keyword-face ((t (:foreground ,masked-white))))
   `(font-lock-preprocessor-face ((t (:foreground ,masked-red))))
   `(font-lock-string-face ((t (:foreground ,masked-yellow))))
   `(font-lock-type-face ((t (:foreground ,masked-green))))
   `(font-lock-variable-name-face ((t (:foreground ,masked-fg))))
   `(font-lock-warning-face ((t (:foreground ,masked-red))))
   `(font-lock-negation-char-face ((t (:foreground ,masked-green))))

   ;; general
   `(cursor ((t (:background ,masked-green))))
   `(default ((t (:foreground ,masked-fg :background ,masked-bg))))
   `(fringe ((t (:foreground ,masked-fg :background ,masked-bg-1))))
   `(minibuffer-prompt ((t (:foreground ,masked-white))))
   `(region ((t (:foreground ,masked-white :background ,masked-magenta))))
   `(link ((t (:foreground ,masked-white :underline t))))
   `(link-visited ((t (:foreground ,masked-white :underline t))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,masked-cyan))))

   ;; line-numbers
   `(line-number ((t (:inherit default :foreground ,masked-bg+3 :background ,masked-bg))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,masked-bg+3 :background ,masked-bg :weight ,bold))))

   ;; mode-line
   `(mode-line-active ((t (:foreground ,masked-bg :background ,masked-yellow))))
   `(mode-line-inactive ((t (:foreground ,masked-bg :background ,masked-fg-1))))
   `(mode-line-buffer-id ((t (:weight ,bold))))

   ;; org
   `(org-date ((t (:foreground ,masked-cyan :background ,masked-bg))))
   `(org-hide ((t (:foreground ,masked-fg-1 :background ,masked-bg))))
   `(org-todo ((t (:foreground ,masked-red :background ,masked-bg))))
   `(org-done ((t (:foreground ,masked-green :background ,masked-bg))))
   `(org-headline-done ((t (:inherit org-done))))
   `(org-level-1 ((t (:foreground ,masked-red :background ,masked-bg))))
   `(org-level-2 ((t (:foreground ,masked-magenta :background ,masked-bg))))
   `(org-level-3 ((t (:foreground ,masked-cyan :background ,masked-bg))))
   `(org-level-4 ((t (:foreground ,masked-cyan :background ,masked-bg))))
   `(org-level-5 ((t (:foreground ,masked-green :background ,masked-bg))))
   `(org-level-6 ((t (:foreground ,masked-yellow :background ,masked-bg))))
   `(org-level-7 ((t (:foreground ,masked-bg+3 :background ,masked-bg))))

   ;; rust
   `(rust-unsafe ((t (:foreground ,masked-fg))))
   `(rust-question-mark ((t (:foreground ,masked-fg))))
   `(rust-ampersand-face ((t (:foreground ,masked-fg))))
   `(rust-string-interpolation ((t (:foreground ,masked-fg))))
   `(rust-builtin-formatting-macro ((t (:foreground ,masked-fg))))

   ;; whitespace
   `(whitespace-space ((t (:foreground ,masked-bg+1 :background ,masked-bg ))))
   `(whitespace-tab ((t (:foreground ,masked-bg+1 :background ,masked-bg ))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'masked)

;;; masked-theme.el ends here.
