;;; ~/.doom.d/+prog.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPANY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        company-lsp-cache-candidates nil)
  (set-company-backend! 'lsp-mode 'company-lsp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cspell-base-program (executable-find "cspell"))
(defvar cspell-config-file-path (concat "'" (expand-file-name  "~/Dotfiles/cspell.json") "'"))
(defvar cspell-args (string-join `("--config" ,cspell-config-file-path) " "))
(defun cspell-check-buffer ()
  (interactive)
  (if cspell-base-program
      (let* ((file-name (concat "'" (buffer-file-name) "'"))
             (command (string-join `(,cspell-base-program ,cspell-args ,file-name) " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g csepll`")
    ))

(defun cspell-check-directory ()
  (interactive)
  (if cspell-base-program
      (let* ((files "'**/*.{js,jsx,ts,tsx,c,cc,cpp,h,hh,hpp,go,json}'")
             (command (string-join `(,cspell-base-program ,cspell-args ,files) " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g csepll`")))


;; (def-package! wucuo
;;   :defer t
;;   :init
;;   (add-hook! (js2-mode rjsx-mode go-mode c-mode c++-mode) #'wucuo-start))


(after! flycheck
  (setq-default flycheck-disabled-checkers
                '(
                  javascript-jshint handlebars
                  json-jsonlist json-python-json
                  c/c++-clang c/c++-cppcheck c/c++-gcc
                  ))

  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (after! tide
    (flycheck-add-next-checker 'javascript-eslint '(t . javascript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . jsx-tide) 'append)
    (flycheck-add-next-checker 'typescript-tslint '(t .  typescript-tide) 'append)
    (flycheck-add-next-checker 'javascript-eslint '(t . tsx-tide) 'append))

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck"))

(defun disable-flycheck-mode ()
  (flycheck-mode -1))
;; (add-hook! (emacs-lisp-mode) 'disable-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(after! python
  (add-hook! python-mode #'lsp)
  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        pippel-python-command "python3"
        importmagic-python-interpreter "python3"
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8")

  ;; Resolve pylint cannot find relative PYTHONPATH issue
  (add-hook! python-mode (setenv "PYTHONPATH" (doom-project-root)))

  ;; if you use pyton2, then you could comment the following 2 lines
  ;; (setq python-shell-interpreter "python2"
  ;;       python-shell-interpreter-args "-i")
  )


(def-package! py-isort
  :defer t
  :init
  (setq python-sort-imports-on-save t)
  (defun spacemacs//python-sort-imports ()
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))
  (add-hook! 'python-mode-hook
    (add-hook 'before-save-hook #'spacemacs//python-sort-imports nil t)))


(def-package! importmagic
  :defer t
  :hook (python-mode . importmagic-mode)
  :commands (importmagic-fix-imports importmagic-fix-symbol-at-point))


(after! pipenv
  (setq pipenv-with-projectile t)
  ;; Override pipenv--clean-response to trim color codes
  (defun pipenv--clean-response (response)
    "Clean up RESPONSE from shell command."
    (replace-regexp-in-string "\n\\[0m$" "" (s-chomp response)))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(pipenv-activate pipenv-deactivate))
    (advice-add func :after #'reset-flycheck)))


(after! conda
  (setq conda-env-home-directory (expand-file-name "~/.conda"))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(conda-env-activate conda-env-deactivate))
    (advice-add func :after #'reset-flycheck))

  (setq conda-message-on-environment-switch nil)
  (conda-env-autoactivate-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS, WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! indium
  :defer t)


(def-package! import-js
  :defer t
  :init
  (add-hook! (js2-mode rjsx-mode)
    (add-hook 'after-save-hook #'import-js-fix nil t)))
(advice-add '+javascript|cleanup-tide-processes :after 'kill-import-js)


(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! go-mode
  (add-hook! go-mode (setq indent-tabs-mode nil))
  (add-hook! go-mode #'lsp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;; (toggle-lsp-ui-doc)
  )

(def-package! lsp-mode
  :config
  (require 'lsp-clients)
  ;; avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
  (setq lsp-auto-guess-root t)
  )


(def-package! lsp-ui
  :init
  (add-hook 'lsp-ui-mode-hook #'my-lsp-mode-hook)
  :config
  (setq lsp-ui-doc-border "black")

  ;; set lsp-ui-doc position
  ;; (setq lsp-ui-doc-position 'at-point)

  (setq lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t)

  ;; (set-lookup-handlers! 'lsp-ui-mode
  ;;   :definition #'lsp-ui-peek-find-definitions
  ;;   :references #'lsp-ui-peek-find-references)

  (after! lsp-ui-mode
    (custom-set-faces
     ;; '(ccls-sem-global-variable-face ((t (:underline t :weight extra-bold))))
     '(lsp-face-highlight-read ((t (:background "#f1aaa3"))))
     '(lsp-face-highlight-write ((t (:background "#a6dade"))))
     ;; '(lsp-ui-sideline-current-symbol ((t (:foreground "grey38" :box nil))))
     ;; '(lsp-ui-sideline-symbol ((t (:foreground "grey30" :box nil))))
     ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG & RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))
