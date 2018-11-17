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
(def-package! wucuo
  :init
  (add-hook! (js2-mode rjsx-mode go-mode c-mode c++-mode) #'wucuo-start))

(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(
                  javascript-jshint handlebars
                  json-jsonlist json-python-json
                  c/c++-clang c/c++-cppcheck c/c++-gcc
                  ))

  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (after! tide
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
    (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
    (flycheck-add-next-checker 'typescript-tslint 'typescript-tide 'append)
    (flycheck-add-next-checker 'javascript-eslint 'tsx-tide 'append))

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck"))

(defun disable-flycheck-mode ()
  (flycheck-mode -1))
;; (add-hook! (emacs-lisp-mode) 'disable-flycheck-mode)

;; ///////////////////////// PYTHON /////////////////////////
(after! python
  (setq python-indent-offset 4
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

(def-package! pipenv
  :init
  (setq pipenv-with-projectile t))

(after! conda
  (setq conda-anaconda-home (expand-file-name "~/.conda"))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(conda-env-activate conda-env-deactivate))
    (advice-add func :after (λ! (flycheck-mode -1) (flycheck-mode))))

  (conda-env-autoactivate-mode t))

;; //////////////////// JS, TS, WEB //////////////////////
(def-package! import-js
  :init
  (add-hook! 'js2-mode-hook 'run-import-js))
(advice-add '+javascript|cleanup-tide-processes :after 'kill-import-js)

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

;; //////////////////////// GO ///////////////////////////
(add-hook! 'go-mode-hook (setq indent-tabs-mode nil))

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
    (lsp-ui-doc-mode 1)))

(defun my-lsp-mode-hook ()
  ;; disable lsp-highlight-symbol
  ;; (setq lsp-highlight-symbol-at-point nil)

  ;; toggle off lsp-ui-doc by default
  (toggle-lsp-ui-doc))
(add-hook 'lsp-mode-hook #'my-lsp-mode-hook)

(def-package! lsp-mode
  :config
  ;; avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
  (setq lsp-message-project-root-warning t)
  )

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-border "black")

  ;; temporary fix for flycheck
  (setq lsp-ui-flycheck-enable nil)

  ;; set lsp-ui-doc position
  (setq lsp-ui-doc-position 'at-point)

  (setq lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t)

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
