;;; private/my-python/config.el -*- lexical-binding: t; -*-

(map! :after python
      :localleader
      :map python-ts-mode-map
      :desc "Insert breakpoint" "b" #'+python/toggle-breakpoint
      :desc "Insert default breakpoint" "B" #'+python/toggle-default-breakpoint
      :desc "Toggle debugpy lines" "d" #'+python/toggle-debugpy-lines
      (:prefix "t"
       :desc "Copy python breakpoint" "b" #'+python/copy-pdb-breakpoint-of-current-line
       :desc "Copy python cmd" "p" #'+python/copy-python-cmd
       :desc "Copy pytest cmd" "y" #'+python/copy-pytest-cmd
       :desc "Copy unittest cmd" "u" #'+python/copy-unittest-cmd)
      (:prefix ("i" . "Import")
       :desc "Remove unused impoorts" "r" #'+python/autoflake-remove-imports
       :desc "Isort buffer"    "s" #'python-isort-autosave-mode
       :desc "Insert copied import" "p" #'+python/insert-temp-import
       :desc "Copy module import " "i" #'+python/yank-module-import)
      (:prefix ("v" . "ENV")
       "c" #'conda-env-activate
       "C" #'conda-env-deactivate
       :desc "Activate .venv" "p" (λ! (pyvenv-activate (concat (doom-project-root) ".venv")))
       ;; deactivate not work well. Just try activate again.
       "P" #'pyvenv-deactivate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! python
  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8")

  ;; if you use pyton2, then you could comment the following 2 lines
  ;; (setq python-shell-interpreter "python2"
  ;;       python-shell-interpreter-args "-i")
  )
(add-hook! 'python-ts-mode-hook #'+python/annotate-pdb)

(add-hook! 'python-ts-mode-hook (setq-local format-all-formatters '(("Python" ruff))))

(after! lsp-pyls
  ;; disable live-mode for mypy
  (lsp-register-custom-settings `(("pyls.plugins.pyls_mypy.enabled" t)))
  (lsp-register-custom-settings `(("pyls.plugins.pyls_mypy.live_mode" t)))

  ;; ignore some linting info
  (setq lsp-pyls-plugins-pycodestyle-ignore  [ "E501" ]
        lsp-pyls-plugins-pylint-args [ "--errors-only" ]))

(after! lsp-pyright
  (setq lsp-pyright-python-executable-cmd "python3"))

(use-package! py-isort
  :defer t
  :init
  (defvar my-enable-isort-before-save t)
  (defun my-python-isrot-before-save ()
    (when my-enable-isort-before-save
      (py-isort-before-save)))
  (define-minor-mode python-isort-autosave-mode
    "Isort autosave mode."
    :lighter " Isort"
    :global nil
    (when (not (derived-mode-p 'python-ts-mode))
      (error "Isort only works with Python buffers"))
    (if python-isort-autosave-mode
        (add-hook! 'before-save-hook :local #'my-python-isrot-before-save)
      (remove-hook! 'before-save-hook :local #'my-python-isrot-before-save)))

  ;; (add-hook! 'python-ts-mode-hook #'python-isort-autosave-mode)
  )

(after! pipenv
  (setq pipenv-with-projectile t)
  ;; Override pipenv--clean-response to trim color codes
  (defun pipenv--clean-response (response)
    "Clean up RESPONSE from shell command."
    (replace-regexp-in-string "\n\\[0m$" "" (s-chomp response)))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(pipenv-activate pipenv-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))


(after! conda
  (setq conda-anaconda-home (cond ((file-directory-p "~/.miniconda") "~/.miniconda")
                                  ((file-directory-p "~/.anaconda") "~/.anaconda")))
  (when (file-exists-p! "~/.conda")
    (setq conda-env-home-directory (expand-file-name "~/.conda")))
  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(conda-env-activate conda-env-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))

(after! pyvenv
  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(pyvenv-activate pyvenv-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))

(after! poetry
  (remove-hook 'python-ts-mode-hook #'poetry-tracking-mode)
  (dolist (func '(poetry-venv-workon poetry-venv-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))

;; For pytest-mode
(set-evil-initial-state! '(comint-mode) 'normal)
(set-evil-initial-state! '(shell-mode) 'normal)
