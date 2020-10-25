;;; private/my/+ui.el -*- lexical-binding: t; -*-

(load-theme 'doom-one-light t)

(when (display-graphic-p)
  (setq user-font
        (cond
         ((find-font (font-spec :name  "CartographCF Nerd Font")) "CartographCF Nerd Font")
         ((find-font (font-spec :name  "OperatorMono Nerd Font")) "OperatorMono Nerd Font")
         ((find-font (font-spec :name  "Droid Sans Mono")) "Droid Sans Mono")
         ((find-font (font-spec :name  "Droid Sans Fallback")) "Droid Sans Fallback")))
  (cond (IS-MAC
         (setq doom-font (font-spec :family user-font :size 15)
               doom-big-font (font-spec :family user-font :size 21)
               doom-modeline-height 24))
        (IS-LINUX
         (setq resolution-factor (eval (/ (x-display-pixel-height) 1080.0)))
         (setq doom-font (font-spec :family user-font :size (eval (round (* 13 resolution-factor))))
               doom-big-font (font-spec :family user-font :size (eval (round (* 18 resolution-factor))))
               doom-modeline-height (eval (round (* 14 resolution-factor))))))

  ;; set initl screen size
  (setq initial-frame-alist
        '((width . 110)
          (height . 65))))

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
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-icon t
        ;; My mac vsplit screen won't fit
        doom-modeline-window-width-limit (- fill-column 10)))

(setq +workspaces-on-switch-project-behavior t)

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

;; disable line-numbers by default
(setq display-line-numbers-type nil)

(defface breakpoint-enabled '((t)) "Breakpoint face.")

;; my custom faces
(custom-set-faces!
  '(variable-pitch :family nil)
  '(font-lock-doc-face :italic t)
  '(font-lock-comment-face :italic t)
  '(font-lock-builtin-face :italic t)
  '(font-lock-type-face :italic t)
  `(show-paren-match :background ,(doom-lighten (doom-color 'teal) 0.4) :foreground ,(doom-color 'base1))
  '(tide-hl-identifier-face :inherit 'lsp-face-highlight-read)
  `(breakpoint-enabled :background ,(doom-color 'red) :foreground "white")
  `(lsp-ui-peek-highlight :foreground "white")
  `(ivy-posframe-border :background ,(doom-color 'blue))
  `(magit-diff-file-heading :background ,(doom-lighten (doom-color 'blue) 0.5))
  `(magit-diff-file-heading-highlight :background ,(doom-lighten (doom-color 'blue) 0.2))
  '(markdown-header-face-1 :inherit 'org-level-1)
  '(markdown-header-face-2 :inherit 'org-level-2)
  '(markdown-header-face-3 :inherit 'org-level-3)
  `(web-mode-jsx-depth-1-face :background ,(doom-lighten (doom-color 'teal) 0.9))
  `(web-mode-jsx-depth-2-face :background ,(doom-lighten (doom-color 'teal) 0.8))
  `(web-mode-jsx-depth-3-face :background ,(doom-lighten (doom-color 'teal) 0.7))
  `(web-mode-jsx-depth-4-face :background ,(doom-lighten (doom-color 'teal) 0.6))
  `(web-mode-jsx-depth-5-face :background ,(doom-lighten (doom-color 'teal) 0.5))
  `(flyspell-incorrect :underline ,(doom-color 'red))
  `(flyspell-duplicate :underline ,(doom-color 'orange))
  '(flymake-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-warning :underline (:style wave :color ,(doom-color 'yellow)))
  `(flycheck-error :underline (:style wave :color ,(doom-color 'red)))
  `(flycheck-info :underline (:style wave :color ,(doom-color 'green)))
  `(rime-default-face :background ,(doom-lighten (doom-color 'red) 0.85))
  `(doom-modeline-debug-visual :background ,(doom-lighten (doom-color 'red) 0.85))
  `(ein:cell-input-area :background ,(doom-lighten (doom-color 'red) 0.85))
  `(ein:cell-input-prompt :background ,(doom-color 'red) :foreground ,(doom-color 'base0) :bold t))

(custom-theme-set-faces! 'doom-city-lights
  `(hl-line :background ,(doom-color 'base0))
  `(magit-diff-file-heading-highlight :foreground ,(doom-color 'base0))
  `(magit-diff-file-heading :foreground ,(doom-color 'base4))
  ;; ediff
  `(ediff-current-diff-A :foreground ,(doom-color 'red)   :background ,(doom-darken (doom-color 'red) 0.8))
  `(ediff-current-diff-B :foreground ,(doom-color 'green) :background ,(doom-darken (doom-color 'green) 0.8))
  `(ediff-current-diff-C :foreground ,(doom-color 'blue)  :background ,(doom-darken (doom-color 'blue) 0.8))
  `(ediff-current-diff-Ancestor :foreground ,(doom-color 'teal)  :background ,(doom-darken (doom-color 'teal) 0.8))
  `(markdown-code-face :background ,(doom-color 'base2)))

;; for terminal
(unless (display-graphic-p)
  (custom-theme-set-faces! 'doom-one-light
    `(vertical-border :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'blue))
    `(mode-line :background ,(doom-lighten (doom-color 'blue) 0.8) :foreground ,(doom-color 'fg))
    `(mode-line-inactive :background ,(doom-color 'base2) :foreground ,(doom-color 'fg))
    `(font-lock-comment-face :foreground ,(doom-color 'base6))
    `(font-lock-doc-face :foreground ,(doom-color 'base6))))

;; (when IS-MAC
;;   ;; enable ligatures support
;;   ;; details here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;   (ignore-errors
;;     (mac-auto-operator-composition-mode)))

(after! ibuffer
  ;; set ibuffer name column width
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 50 50 :left :nil) " "
                (size-h 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process))))

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


;; doom modeline rime segment
(after! (:and doom-modeline rime)
  (set-face-attribute 'rime-indicator-face nil
                      :foreground 'unspecified
                      :inherit 'doom-modeline-buffer-major-mode)
  (set-face-attribute 'rime-indicator-dim-face nil
                      :foreground 'unspecified
                      :inherit 'doom-modeline-buffer-minor-mode)

  (doom-modeline-def-segment input-method
    "Define the current input method properties."
    (propertize (cond (current-input-method
                       (concat (doom-modeline-spc)
                               current-input-method-title
                               (doom-modeline-spc)))
                      ((and (bound-and-true-p evil-local-mode)
                            (bound-and-true-p evil-input-method))
                       (concat
                        (doom-modeline-spc)
                        (nth 3 (assoc default-input-method input-method-alist))
                        (doom-modeline-spc)))
                      (t ""))
                'face (if (doom-modeline--active)
                          (or (get-text-property 0 'face (rime-lighter))
                              'doom-modeline-buffer-major-mode)
                        'mode-line-inactive)
                'help-echo (concat
                            "Current input method: "
                            current-input-method
                            "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
                'mouse-face 'mode-line-highlight
                'local-map mode-line-input-method-map))
  )
