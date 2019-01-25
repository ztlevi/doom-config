;;; private/my/config.el -*- lexical-binding: t; -*-

(load! "+os")
(load! "+misc")
(load! "+text")
(load! "+prog")
(load! "+ui")
(load! "+bindings")

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

(setq-default fill-column 100
              delete-trailing-lines t)

;; Delete the selection when pasting
(delete-selection-mode 1)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; check minified-file
(add-to-list 'magic-mode-alist (cons #'+my/check-minified-file 'fundamental-mode))

(set-popup-rules! '(("^\\*helpful" :size 0.35)
                    ("^\\*Ibuffer\\*$" :size 0.35)
                    ("^\\*info.*" :size 80 :side right)
                    ("^\\*Man.*" :size 80 :side right)
                    ("^\\*Customize" :actions display-buffer)
                    ("^\\*edit-indirect" :size 0.6)
                    ("^\\*YASnippet Tables\\*$" :size 0.35)
                    ("^\\*grep\\*$" :size 0.35)
                    ("^\\*Anaconda\\*$" :size 0.35)
                    ("^\\*helm kill ring\\*$" :size 0.35)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35)
                    ))

;; Load system profile for different machines
(let ((profile "~/.config/doom/profile.el"))
  (when (file-exists-p profile)
    (load-file profile)))
