;;; private/lsp-intellij/config.el -*- lexical-binding: t; -*-

(use-package! lsp-intellij
  :commands lsp-intellij-enable
  :hook
  (java-mode . lsp-intellij-enable)
  (kotlin-mode . lsp-intellij-enable)
  :init
  (add-hook! (java-mode kotlin-mode)
    (setq-local company-lsp-enable-snippet t)
    (setq-local company-lsp-cache-candidates t)))

(map!
 :map* (java-mode-map kotlin-mode-map)
 :i "." #'spacemacs/java-lsp-completing-dot
 :i ":" #'spacemacs/java-lsp-completing-double-colon
 :localleader
 :desc "Run at point"             "," #'lsp-intellij-run-at-point
 :desc "Open configuration"       "c" #'lsp-intellij-open-run-configurations
 (:prefix ("g" . "goto")
   :desc "Find definitions"       "d" #'lsp-ui-peek-find-definitions
   :desc "Goto type definition"   "t" #'lsp-goto-type-definition
   :desc "Goto implementation"    "i" #'lsp-goto-implementation
   :desc "Find references"        "D" #'lsp-ui-peek-find-references)
 :desc "Find Apropos"             "h" #'xref-find-apropos
 (:prefix ("p" . "project")
   :desc "Build project"          "b" #'lsp-intellij-build-project
   :desc "Run project"            "r" #'lsp-intellij-run-project
   :desc "Open project structure" "s" #'lsp-intellij-open-project-structure)
 :desc "Format buffer"            "f" #'lsp-format-buffer
 :desc "Toggle IntelliJ frame"    "v" #'lsp-intellij-toggle-frame-visibility
 )
