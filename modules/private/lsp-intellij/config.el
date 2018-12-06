;;; private/lsp-intellij/config.el -*- lexical-binding: t; -*-

(def-package! lsp-intellij
  :defer t
  :commands lsp-intellij-enable
  :hook
  (java-mode . lsp-intellij-enable)
  (kotlin-mode . lsp-intellij-enable)
  :init
  (add-hook! (java-mode kotlin-mode)
    (setq-local company-lsp-enable-snippet t)
    (setq-local company-lsp-cache-candidates t))
  :config
  (set-company-backend! '(java-mode kotlin-mode) 'company-lsp)
  (set-lookup-handlers! '(java-mode kotlin-mode)
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))

(map!
 :map* (java-mode-map kotlin-mode-map)
 :i "." #'spacemacs/java-lsp-completing-dot
 :i ":" #'spacemacs/java-lsp-completing-double-colon
 :localleader
 :desc "Run at point"             :n "," #'lsp-intellij-run-at-point
 :desc "Open configuration"       :n "c" #'lsp-intellij-open-run-configurations
 (:desc "goto" :prefix "g"
   :desc "Find definitions"       :n "d" #'lsp-ui-peek-find-definitions
   :desc "Goto type definition"   :n "t" #'lsp-goto-type-definition
   :desc "Goto implementation"    :n "i" #'lsp-goto-implementation
   :desc "Find references"        :n "D" #'lsp-ui-peek-find-references)
 :desc "Find Apropos"             :n "h" #'xref-find-apropos
 (:desc "project" :prefix "p"
   :desc "Build project"          :n "b" #'lsp-intellij-build-project
   :desc "Run project"            :n "r" #'lsp-intellij-run-project
   :desc "Open project structure" :n "s" #'lsp-intellij-open-project-structure)
 :desc "Format buffer"            :n "f" #'lsp-format-buffer
 :desc "Toggle IntelliJ frame"    :n "v" #'lsp-intellij-toggle-frame-visibility
 )
