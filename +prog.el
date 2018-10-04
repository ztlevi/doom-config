;;; ~/.doom.d/+prog.el -*- lexical-binding: t; -*-

;; ///////////////////////// COMPANY /////////////////////////
(after! company
  (setq company-minimum-prefix-length 2
        company-quickhelp-delay nil
        company-show-numbers t
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        ))

(def-package! company-lsp
  :after company
  :init
  (setq company-transformers nil company-lsp-cache-candidates nil)
  )

(set-lookup-handlers! 'emacs-lisp-mode :documentation #'helpful-at-point)

(map!
 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-v"        #'company-next-page
     "M-v"        #'company-previous-page
     "C-i"        #'company-complete-selection)))

;; ///////////////////////// FLYCHECK /////////////////////////
(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(javascript-tide
                  jsx-tide javascript-jshint
                  typescript-tide json-jsonlist
                  c/c++-clang c/c++-cppcheck c/c++-gcc
                  ))

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck"))

(add-hook! '(emacs-lisp-mode-hook text-mode-hook) (λ! disable-flycheck-mode))

(map!
 (:leader
   :n "el" #'flycheck-list-errors
   :n "ev" #'flycheck-verify-setup))

;; ///////////////////////// PYTHON /////////////////////////
(after! python
  (setq python-indent-offset 4
        python-sort-imports-on-save t
        python-shell-interpreter "python3"
        pippel-python-command "python3"
        importmagic-python-interpreter "python3"
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8")
  ;; if you use pyton2, then you could comment the following 2 lines
  ;; (setq python-shell-interpreter "python2"
  ;;       python-shell-interpreter-args "-i")

  (defun spacemacs//python-sort-imports ()
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))
  (add-hook 'before-save-hook 'spacemacs//python-sort-imports)
  )

(def-package! py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

(def-package! importmagic
  :commands importmagic-fix-symbol-at-point)

(def-package! lsp-python
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable))

(map!
 (:after python-mode
   (:map python-mode-map
     (:leader
       :n "m=" 'py-autopep8-buffer
       :n "mi" 'importmagic-fix-symbol-at-point
       ))))

;; ///////////////////////// JS /////////////////////////
(def-package! import-js
  :init
  (run-import-js))

(map!
 (:leader
   (:after js2-mode
     (:map js2-mode-map
       :n "mi" 'import-js-import
       :n "mf" 'import-js-fix))
   (:after rjsx-mode
     (:map rjsx-mode-map
       :n "mi" 'import-js-import
       :n "mf" 'import-js-fix)))
 (:after tide
   :map tide-references-mode-map
   "C-k" 'tide-find-previous-reference
   "p" 'tide-find-previous-reference
   "C-j" 'tide-find-next-reference
   "n" 'tide-find-next-reference
   "C-l" 'tide-goto-reference
   ))

;; ///////////////////////// LISP /////////////////////////
(def-package! lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :config
  (setq lispy-outline "^;; \\(?:;[^#]\\|\\*+\\)"
        lispy-outline-header ";; "
        lispy-ignore-whitespace t)
  (map! :map lispy-mode-map
        :i "C-c (" #'lispy-wrap-round
        :i "_" #'special-lispy-different
        "d" nil
        :i [remap delete-backward-char] #'lispy-delete-backward))

;; Also use lispyville in prog-mode for [ ] < >
(def-package! lispyville
  :after (evil)
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     (escape insert)
     (slurp/barf-lispy)
     additional-movement))
  (map! :map emacs-lisp-mode-map
        :n "gh" #'helpful-at-point
        :n "C-<left>" #'lispy-forward-barf-sexp
        :n "C-<right>" #'lispy-forward-slurp-sexp
        :n "C-M-<left>" #'lispy-backward-slurp-sexp
        :n "C-M-<right>" #'lispy-backward-barf-sexp
        :n "<tab>" #'lispyville-prettify
        :localleader
        :n "e" (λ! (save-excursion (forward-sexp) (eval-last-sexp nil)))
        )
  )

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

  (advice-add #'lsp-ui-doc--eldoc :override #'+my/lsp-ui-doc--eldoc)
  (after! lsp-ui-mode
    (custom-set-faces
     ;; '(ccls-sem-global-variable-face ((t (:underline t :weight extra-bold))))
     '(lsp-face-highlight-read ((t (:background "#f1aaa3"))))
     '(lsp-face-highlight-write ((t (:background "#a6dade"))))
     ;; '(lsp-ui-sideline-current-symbol ((t (:foreground "grey38" :box nil))))
     ;; '(lsp-ui-sideline-symbol ((t (:foreground "grey30" :box nil))))
     ))

  )

(map!
 (:after lsp-mode
   :n "M-j" #'toggle-lsp-ui-doc)
 (:after lsp-ui-peek
   :map lsp-ui-peek-mode-map
   "h" #'lsp-ui-peek--select-prev-file
   "j" #'lsp-ui-peek--select-next
   "k" #'lsp-ui-peek--select-prev
   "l" #'lsp-ui-peek--select-next-file)
 )
