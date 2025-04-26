;;; emerald-theme.el --- Emerald color theme for GNU Emacs.

;; Copyright (C) 2025 M. Enes Kaya

;; Author: M. Enes Kaya
;; E-mail: enoks@tutanota.com
;; URL: https://github.com/enoks1/emerald-theme
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

(deftheme emerald
  "Emerald theme for GNU Emacs.")

(defgroup emerald nil
  "Emerald theme and it's settings."
  :group 'emerald)

(defcustom emerald-theme-enable-italic t
  "Enable or disable all italic faces."
  :type 'boolean)

(defcustom emerald-theme-enable-bold t
  "Enable or disable all bold faces."
  :type 'boolean)

;; colors with `+c' are lighter; and with `-c' darker
(let ((emerald-fg-1         "dark khaki")
      (emerald-fg           "khaki")
      (emerald-fg+1         "pale goldenrod")

      (emerald-bg           "#0f1f1a")
      (emerald-bg+1         "#2a5046")
      (emerald-bg+2         "#3a6d60")
      (emerald-bg+3         "#4a8a7a")

      (emerald-emerald      "#00cd8c")
      (emerald-green        "pale green")
      (emerald-cyan         "#20c7b5")
      (emerald-red          "#ff534d")

      (italic             (if emerald-theme-enable-italic 'italic 'normal))
      (bold               (if emerald-theme-enable-bold 'bold 'normal)))

  (custom-theme-set-faces
   'emerald

   ;; compilation
   `(compilation-info ((t (:foreground ,emerald-green))))
   `(compilation-warning ((t (:foreground "orange red"))))
   `(compilation-error ((t (:foreground ,emerald-red))))
   `(compilation-mode-line-fail ((t (:foreground ,emerald-red :weight ,bold))))
   `(compilation-mode-line-exit ((t (:foreground ,emerald-green))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,emerald-emerald))))
   `(font-lock-comment-face ((t (:foreground "lime green"))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-constant-face ((t (:foreground ,emerald-green))))
   `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-function-name-face ((t (:foreground ,emerald-green))))
   `(font-lock-keyword-face ((t (:foreground "white"))))
   `(font-lock-preprocessor-face ((t (:foreground ,emerald-fg+1))))
   `(font-lock-string-face ((t (:foreground ,emerald-cyan))))
   `(font-lock-type-face ((t (:foreground ,emerald-green))))
   `(font-lock-variable-name-face ((t (:foreground ,emerald-green))))
   `(font-lock-warning-face ((t (:foreground ,emerald-red :weight ,bold))))
   `(font-lock-negation-char-face ((t (:foreground ,emerald-fg))))

   ;; general
   `(cursor ((t (:background ,emerald-green))))
   `(default ((t (:foreground ,emerald-fg :background ,emerald-bg))))
   `(fringe ((t (:foreground ,emerald-fg :background ,emerald-bg+2))))
   `(minibuffer-prompt ((t (:foreground ,emerald-fg :weight ,bold))))

   ;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,emerald-green))))

   ;; highlight-line
   `(hl-line ((t (:background ,emerald-bg+1))))

   ;; line-numbers
   `(line-number ((t (:inherit default :foreground ,emerald-bg+3 :background ,emerald-bg))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,emerald-bg+3 :background ,emerald-bg :weight ,bold))))

   ;; mode-line
   `(mode-line-active ((t (:foreground ,emerald-bg :background ,emerald-fg-1))))
   `(mode-line-inactive ((t (:foreground ,emerald-bg :background ,emerald-bg+2))))
   `(mode-line-buffer-id ((t (:weight ,bold))))

   ;; rust
   `(rust-unsafe ((t (:foreground ,emerald-red))))
   `(rust-question-mark ((t (:inherit font-lock-keyword-face))))
   `(rust-ampersand-face ((t (:inherit font-lock-keyword-face))))
   `(rust-builtin-formatting-macro ((t (:inherit font-lock-keyword-face))))
   `(rust-string-interpolation ((t (:inherit font-lock-string-face :slant ,italic))))

   ;; vhdl
   `(vhdl-font-lock-function-face ((t (:foreground ,emerald-green))))

   ;; whitespace
   `(whitespace-space ((t (:foreground ,emerald-bg+1 :background ,emerald-bg ))))
   `(whitespace-tab ((t (:foreground ,emerald-bg+1 :background ,emerald-bg ))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'emerald)

;;; emerald-theme.el ends here.
