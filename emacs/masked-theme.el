;;; masked-theme.el --- Masked color theme for GNU Emacs.

;; Copyright (C) 2025 M. Enes Kaya

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

;; TODO: md

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
(let ((masked-fg           "honeydew")

      (masked-bg           "#0f1f1a")
      (masked-bg+1         "#2a5046")
      (masked-bg+2         "#3a6d60")
      (masked-bg+3         "#4a8a7a")

      (masked-emerald      "#00cd8c")
      (masked-green        "pale green")
      (masked-cyan         "light blue")
      (masked-red          "tomato")
      (masked-blue         "#0059d1")
      (masked-yellow       "pale goldenrod")

      (italic             (if masked-theme-enable-italic 'italic 'normal))
      (bold               (if masked-theme-enable-bold 'bold 'normal)))

  (custom-theme-set-faces
   'masked

   ;; compilation
   `(compilation-info ((t (:foreground ,masked-green))))
   `(compilation-warning ((t (:foreground ,masked-yellow))))
   `(compilation-error ((t (:foreground ,masked-red))))
   `(compilation-mode-line-fail ((t (:foreground ,masked-red :weight ,bold))))
   `(compilation-mode-line-exit ((t (:foreground ,masked-green :weight ,bold))))

   ;; dired
   `(dired-directory ((t (:foreground ,masked-green))))
   `(dired-ignored ((t (:foreground ,masked-green))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,masked-emerald))))
   `(font-lock-comment-face ((t (:foreground ,masked-bg+3))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:foreground ,masked-yellow))))
   `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-function-name-face ((t (:foreground ,masked-green))))
   `(font-lock-keyword-face ((t (:foreground ,masked-emerald :weight ,bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,masked-red))))
   `(font-lock-string-face ((t (:foreground ,masked-cyan))))
   `(font-lock-type-face ((t (:foreground ,masked-green))))
   `(font-lock-variable-name-face ((t (:foreground ,masked-yellow))))
   `(font-lock-warning-face ((t (:foreground ,masked-red))))
   `(font-lock-negation-char-face ((t (:foreground ,masked-fg))))

   ;; general
   `(cursor ((t (:background ,masked-green))))
   `(default ((t (:foreground ,masked-fg :background ,masked-bg))))
   `(fringe ((t (:foreground ,masked-fg :background ,masked-bg+2))))
   `(minibuffer-prompt ((t (:foreground ,masked-fg :weight ,bold))))
   `(region ((t (:background ,masked-blue))))
   `(link ((t (:foreground ,masked-fg :weight ,bold :underline t))))
   `(link-visited ((t (:foreground ,masked-fg :weight ,bold :underline t))))
   `(show-paren-match ((t (:background ,masked-blue))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,masked-green))))

   ;; highlight-line
   `(hl-line ((t (:background ,masked-bg+1))))

   ;; line-numbers
   `(line-number ((t (:inherit default :foreground ,masked-bg+3 :background ,masked-bg))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,masked-bg+3 :background ,masked-bg :weight ,bold))))

   ;; mode-line
   `(mode-line-active ((t (:foreground ,masked-bg :background ,masked-emerald))))
   `(mode-line-inactive ((t (:foreground ,masked-bg :background ,masked-bg+2))))
   `(mode-line-buffer-id ((t (:weight ,bold))))

   ;; rust
   `(rust-unsafe ((t (:foreground ,masked-red))))
   `(rust-question-mark ((t (:inherit font-lock-keyword-face))))
   `(rust-ampersand-face ((t (:foreground ,masked-emerald))))
   `(rust-string-interpolation ((t (:inherit font-lock-string-face :slant ,italic))))
   `(rust-builtin-formatting-macro ((t (:foreground ,masked-emerald))))

   ;; vhdl
   `(vhdl-font-lock-function-face ((t (:foreground ,masked-yellow))))

   ;; whitespace
   `(whitespace-space ((t (:foreground ,masked-bg+1 :background ,masked-bg ))))
   `(whitespace-tab ((t (:foreground ,masked-bg+1 :background ,masked-bg ))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'masked)

;;; masked-theme.el ends here.
