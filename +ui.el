;;; private/my/+ui.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Operator Mono Lig" :size 16))
(setq doom-big-font (font-spec :family "Operator Mono Lig" :size 22))
(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

(setq +doom-modeline-height 28)
(setq +doom-modeline-buffer-file-name-style 'relative-to-project)
(setq doom-theme 'doom-one-light)

(setq +workspaces-on-switch-project-behavior t)

;; set initl screen size
(setq initial-frame-alist
      '((width . 110)
        (height . 65)))

;; disable line-numbers by default
(setq display-line-numbers-type nil)

;; (global-visual-line-mode)

(defun +my/set-faces ()
  (custom-set-faces
   `(show-paren-match ((t (:background ,(doom-color 'teal) :foreground ,(doom-color 'base1)))))
   '(tide-hl-identifier-face ((t (:inherit lsp-face-highlight-read))))
   '(lsp-intellij-face-code-lens-run ((t (:background "honeydew"))))
   ))
(add-hook! 'doom-load-theme-hook #'+my/set-faces)

;; enable natural title bar for emacs-plus
(if IS-EMACS-PLUS
    (progn
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . light))))

;; enable ligatures support
;; details here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
(progn
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 ;; (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

;; set the wrap line symbol
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b11110000
   #b00010000
   #b00010000
   #b00010000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00001000
   #b00001000
   #b00001000
   #b00001111
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000])
(setq visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
