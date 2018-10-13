;;; private/lsp-intellij/config.el -*- lexical-binding: t; -*-

(def-package! lsp-intellij
  :commands lsp-intellij-enable
  :hook
  (java-mode . lsp-intellij-enable)
  (kotlin-mode . lsp-intellij-enable)
  :config
  (add-hook! (java-mode)
    (setq-local company-lsp-enable-snippet t)
    (setq-local company-lsp-cache-candidates t))

  (set-company-backend! '(java-mode kotlin-mode) 'company-lsp)
  (set-lookup-handlers! 'java-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  )
