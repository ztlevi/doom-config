;;; private/my/+ui.el -*- lexical-binding: t; -*-

(cond (IS-MAC
       (setq doom-font (font-spec :family "Operator Mono Lig" :size 16)
             doom-big-font (font-spec :family "Operator Mono Lig" :size 22)
             +modeline-height 32))
      (IS-LINUX
       (setq doom-font (font-spec :family "Ubuntu Mono" :size 36)
             doom-big-font (font-spec :family "Ubuntu Mono" :size 40)
             +modeline-height 62)))

;; override modeline icon func
(defun +doom-ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (when icon
            (concat
             (all-the-icons-material icon :face face :height 1 :v-adjust (or voffset -0.2))
             (if text +modeline--vspc)))
          (if text (propertize text 'face face))))

(setq +doom-modeline-buffer-file-name-style 'relative-to-project)

;; set different theme for tty
(if (display-graphic-p)
    (setq doom-theme 'doom-one-light)
  (setq doom-theme 'doom-one))

(setq +workspaces-on-switch-project-behavior t)

(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

;; set initl screen size
(setq initial-frame-alist
      '((width . 110)
        (height . 65)))

;; disable line-numbers by default
(setq display-line-numbers-type nil)

;; my custom faces
(defun +my/set-faces ()
  (custom-set-faces
   `(show-paren-match ((t (:background ,(doom-color 'teal) :foreground ,(doom-color 'base1)))))
   '(tide-hl-identifier-face ((t (:inherit lsp-face-highlight-read))))
   '(lsp-intellij-face-code-lens-run ((t (:background "honeydew"))))
   '(markdown-header-face-1 ((t (:inherit org-level-1))))
   '(markdown-header-face-2 ((t (:inherit org-level-2))))
   '(markdown-header-face-3 ((t (:inherit org-level-3))))
   ))
(add-hook! 'doom-load-theme-hook #'+my/set-faces)

(when IS-MAC
  ;; enable natural title bar for emacs-plus
  (when IS-EMACS-PLUS
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . light)))

  ;; enable ligatures support
  ;; details here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  (mac-auto-operator-composition-mode))

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

(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
			                         ("PID"      7 t)
			                         ("Status"   7 t)
			                         ("Buffer"  15 t)
			                         ("TTY"     12 t)
			                         ("Command"  0 t)]))
