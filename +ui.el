;;; private/my/+ui.el -*- lexical-binding: t; -*-

;; (setq doom-one-brighter-comments t
;;       doom-city-lights-brighter-comments t
;;       doom-one-light-brighter-comments t)

;; (setq doom-theme 'doom-one-light)
(load-theme 'doom-one-light t)

(setq evil-emacs-state-cursor `(box ,(doom-color 'violet)))

(when (display-graphic-p)
  (setq user-font
        (cond
         ((find-font (font-spec :name "CartographCF Nerd Font")) "CartographCF Nerd Font")
         ((find-font (font-spec :name "OperatorMono Nerd Font")) "OperatorMono Nerd Font")
         ((find-font (font-spec :name "Droid Sans Mono")) "Droid Sans Mono")
         ((find-font (font-spec :name "Droid Sans Fallback")) "Droid Sans Fallback")))

  ;; calculate the font size based on display-pixel-height
  (setq resolution-factor (eval (/ (x-display-pixel-height) 1080.0)))
  (setq doom-font (font-spec :family user-font :size (eval (round (* 13 resolution-factor))))
        doom-big-font (font-spec :family user-font :size (eval (round (* 18 resolution-factor))))
        doom-variable-pitch-font (font-spec :family user-font :size (eval (round (* 13 resolution-factor))))
        doom-modeline-height (eval (round (* 14 resolution-factor))))
  (setq doom-font-increment 1)

  ;; set initl screen size
  (setq initial-frame-alist
        '((width . 110)
          (height . 65))))

(add-hook! 'doom-first-buffer-hook
  (defun +my/change-cjk-font ()
    "change the cjk font and its size to align the org/markdown tables when have
cjk characters. Font should be twice the width of asci chars so that org tables align.
This will break if run in terminal mode, so use conditional to only run for GUI."
    (when (display-graphic-p)
      (setq user-cjk-font
            (cond
             ((find-font (font-spec :name "Hiragino Sans GB")) "Hiragino Sans GB") ; for macos
             ((find-font (font-spec :name "Noto Sans CJK SC")) "Noto Sans CJK SC") ; for linux
             ))
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family user-cjk-font
                                             :size (eval (round (* 15 resolution-factor)))))))))

;; Update window divider in terminal
;; https://www.reddit.com/r/emacs/comments/3u0d0u/how_do_i_make_the_vertical_window_divider_more/
(unless (display-graphic-p)
  (defun my-change-window-divider ()
    (ignore-errors
      (let ((display-table (or buffer-display-table standard-display-table)))
        (set-display-table-slot display-table 5 ?│)
        ;; (set-window-display-table (selected-window) display-table)
        )))
  (add-hook 'window-configuration-change-hook #'my-change-window-divider))

(after! doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-major-mode-icon t
        ;; My mac vsplit screen won't fit
        doom-modeline-window-width-limit (- fill-column 10)))

(setq +workspaces-on-switch-project-behavior t)

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

;; disable line-numbers by default
(setq display-line-numbers-type nil)

(defface breakpoint-enabled '((t)) "Breakpoint face.")

;; Faces need to postpone renderring
;; custom-set-faces! doesn't work properly when you switch doom themes
(custom-theme-set-faces! doom-theme
  `(hl-line :background ,(doom-color 'bg-alt)) ; sometimes ranger doesn't show hl-line color
  `(doom-modeline-debug-visual :background ,(doom-blend 'red 'base0 0.3))
  `(mode-line :background ,(doom-blend 'dark-blue 'base0  0.2))
  `(mode-line-inactive :background ,(doom-color 'bg-alt))
  `(vertical-border :background ,(doom-color 'black) :foreground ,(doom-color 'bg-alt))
  '(font-lock-doc-face :italic t)
  '(font-lock-comment-face :italic t)
  '(font-lock-builtin-face :italic t)
  '(font-lock-type-face :italic t)
  `(show-paren-match :background ,(doom-blend 'teal 'base0 0.6) :foreground ,(doom-color 'base1))
  '(tide-hl-identifier-face :inherit 'lsp-face-highlight-read)
  `(breakpoint-enabled :background ,(doom-color 'red) :foreground ,(doom-lighten (doom-color 'red) 0.5))
  `(dap-ui-pending-breakpoint-face :background ,(doom-color 'red) :foreground "white")
  `(dap-ui-verified-breakpoint-face :background ,(doom-blend 'red 'base0 0.2))
  `(lsp-ui-peek-highlight :foreground ,(doom-color 'blue))
  `(ivy-posframe-border :background ,(doom-color 'blue))
  `(magit-diff-file-heading :background ,(doom-blend 'blue 'base0 0.2))
  `(magit-diff-file-heading-highlight :background ,(doom-blend 'blue 'base0 0.5))
  '(markdown-header-face-1 :inherit 'org-level-1)
  '(markdown-header-face-2 :inherit 'org-level-2)
  '(markdown-header-face-3 :inherit 'org-level-3)
  `(web-mode-jsx-depth-1-face :background ,(doom-blend 'teal 'base0 0.1))
  `(web-mode-jsx-depth-2-face :background ,(doom-blend 'teal 'base0 0.2))
  `(web-mode-jsx-depth-3-face :background ,(doom-blend 'teal 'base0 0.3))
  `(web-mode-jsx-depth-4-face :background ,(doom-blend 'teal 'base0 0.4))
  `(web-mode-jsx-depth-5-face :background ,(doom-blend 'teal 'base0 0.5))
  `(flyspell-incorrect :underline ,(doom-color 'red))
  `(flyspell-duplicate :underline ,(doom-color 'orange))
  `(flymake-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-error :underline (:style wave :color ,(doom-color 'red)))
  `(flycheck-info :underline (:style wave :color ,(doom-color 'green)))
  `(ein:cell-input-area :background ,(doom-blend 'red 'base0 0.15))
  `(ein:cell-input-prompt :background ,(doom-color 'red) :foreground ,(doom-color 'base0) :bold t)
  `(font-lock-comment-face :foreground ,(doom-color 'blue))
  `(font-lock-doc-face :foreground ,(doom-color 'blue)))

(custom-theme-set-faces! 'doom-city-lights
  ;; ediff
  `(ediff-current-diff-A :foreground ,(doom-color 'red)   :background ,(doom-blend 'red 'base0 0.2))
  `(ediff-current-diff-B :foreground ,(doom-color 'green) :background ,(doom-blend 'green 'base0 0.2))
  `(ediff-current-diff-C :foreground ,(doom-color 'blue)  :background ,(doom-blend 'blue 'base0 0.2))
  `(ediff-current-diff-Ancestor :foreground ,(doom-color 'teal)  :background ,(doom-blend 'teal 'base0 0.2))
  )

;; for terminal
(unless (display-graphic-p)
  (custom-set-faces!
    `(mode-line-inactive :background ,(doom-darken (doom-color 'bg-alt) 0.05) :foreground ,(doom-color 'fg))))

;; (when IS-MAC
;;   ;; enable ligatures support
;;   ;; details here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;   (ignore-errors
;;     (mac-auto-operator-composition-mode)))


(after! ibuffer
  (setq-hook! 'ibuffer-hook ibuffer-formats
              '((mark modified read-only locked " "
                      (name 50 18 :left :elide)
                      " "
                      (size 9 -1 :right)
                      " "
                      (mode 16 16 :left :elide)
                      " " filename-and-process)
                (mark " "
                      (name 16 -1)
                      " " filename))))

(use-package! all-the-icons-ibuffer
  :after ibuffer
  :init (all-the-icons-ibuffer-mode 1)
  )

(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
                                     ("PID"      7 t)
                                     ("Status"   7 t)
                                     ("Buffer"  15 t)
                                     ("TTY"     12 t)
                                     ("Command"  0 t)]))

(after! centered-window
  (setq cwm-centered-window-width 160))
