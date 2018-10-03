;;; private/my/+ui.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Operator Mono Lig" :size 16))
(setq doom-big-font (font-spec :family "Operator Mono Lig" :size 18))
(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

(setq +doom-modeline-height 28)
(setq doom-theme 'doom-one-light)

;; set initl screen size
(setq initial-frame-alist
      '((width . 110)
        (height . 65)))

(defun update-custom-faces ()
  (custom-set-faces
   '(hl-line ((t (:background "#dcdcdc"))))))
(add-hook 'ranger-mode-load-hook 'update-custom-faces)
