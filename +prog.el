;;; ~/.doom.d/+prog.el -*- lexical-binding: t; -*-

;; ///////////////////////// COMPANY /////////////////////////
(after! company
  (setq company-minimum-prefix-length 2
        company-quickhelp-delay nil
        company-show-numbers t
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))

(def-package! company-lsp
  :after company
  :init
  ;; Language servers have better idea filtering and sorting,
  ;; don't filter results on the client side.
  (setq company-transformers nil
        company-lsp-cache-candidates nil))

(set-lookup-handlers! 'emacs-lisp-mode :documentation #'helpful-at-point)

;; ///////////////////////// FLYCHECK /////////////////////////
(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(javascript-tide
                  jsx-tide javascript-jshint
                  typescript-tide json-jsonlist
                  c/c++-clang c/c++-cppcheck
                  ))

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck"))

(defun disable-flycheck-mode ()
  (flycheck-mode -1))
;; (add-hook! '(emacs-lisp-mode-hook) 'disable-flycheck-mode)

;; camcel case spellcheck
(def-package! wucuo
  :hook
  (js2-mode . wucuo-start))

;; ///////////////////////// PYTHON /////////////////////////
(after! python
  (setq python-indent-offset 2
        python-shell-interpreter "python3"
        pippel-python-command "python3"
        importmagic-python-interpreter "python3"
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8")
  ;; if you use pyton2, then you could comment the following 2 lines
  ;; (setq python-shell-interpreter "python2"
  ;;       python-shell-interpreter-args "-i")
  )

(def-package! py-isort
  :init
  (setq python-sort-imports-on-save t)
  (defun spacemacs//python-sort-imports ()
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))
  (add-hook 'before-save-hook 'spacemacs//python-sort-imports))

(def-package! importmagic
  :commands importmagic-fix-symbol-at-point)

(def-package! lsp-python
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable)
  :config
  (set-company-backend! 'python-mode 'company-lsp)
  (set-lookup-handlers! 'python-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))

;; ///////////////////////// JS /////////////////////////
(def-package! import-js
  :init
  (add-hook! 'js2-mode-hook 'run-import-js))
(advice-add '+javascript|cleanup-tide-processes :after 'kill-import-js)

;; /////////////////////////// WEB ////////////////////////
(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

;; ///////////////////////// LISP /////////////////////////
(def-package! lispy
  :hook ((common-lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :config
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode))

(def-package! lispyville
  :when (featurep! :feature evil)
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     (escape insert)
     (slurp/barf-lispy)
     additional-movement)))

;; ///////////////////////// LSP /////////////////////////
(defun toggle-lsp-ui-doc ()
  (interactive)
  (if lsp-ui-doc-mode
      (progn
        (lsp-ui-doc-mode -1)
        (lsp-ui-doc--hide-frame))
    (lsp-ui-doc-mode 1))
  (setq-local eldoc-documentation-function 'ignore))

(defun my-lsp-mode-hook ()
  ;; disable lsp-highlight-symbol
  ;; (setq lsp-highlight-symbol-at-point nil)

  ;; toggle off lsp-ui-doc by default
  (toggle-lsp-ui-doc))
(add-hook 'lsp-mode-hook #'my-lsp-mode-hook)

(def-package! lsp-mode
  :config
  ;; disable lsp eldoc
  (setq lsp-enable-eldoc nil)

  ;; avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
  (setq lsp-message-project-root-warning t)
  )

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; temporary fix for flycheck
  (setq lsp-ui-flycheck-enable nil)

  ;; set lsp-ui-doc position
  (setq lsp-ui-doc-position 'at-point)

  (setq
   lsp-ui-doc-include-signature t
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-doc-header nil
   ;; lsp-ui-doc-background (doom-color 'base4)
   lsp-ui-doc-border (doom-color 'fg)

   lsp-ui-peek-force-fontify nil
   lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))

  (after! lsp-ui-mode
    (custom-set-faces
     ;; '(ccls-sem-global-variable-face ((t (:underline t :weight extra-bold))))
     '(lsp-face-highlight-read ((t (:background "#f1aaa3"))))
     '(lsp-face-highlight-write ((t (:background "#a6dade"))))
     ;; '(lsp-ui-sideline-current-symbol ((t (:foreground "grey38" :box nil))))
     ;; '(lsp-ui-sideline-symbol ((t (:foreground "grey30" :box nil))))
     ))
  )

;; /////////////////////////// RUST /////////////////////////////
(def-package! rust-mode
  :mode "\\.rs$")

(def-package! lsp-rust
  :init (add-hook 'rust-mode-hook #'lsp-rust-enable))

;; //////////////////////// Debug & Run ////////////////////////////
(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))
