;;; ~/.doom.d/+prog.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! format-all
  :hook (emacs-lisp-mode . format-all-mode)
  :defer t)


(use-package! breadcrumb
  :defer t
  :when (modulep! :tools lsp +eglot)
  :hook
  (prog-mode . breadcrumb-local-mode)
  (text-mode . breadcrumb-local-mode)
  )


(use-package! which-func
  :defer t
  :commands which-function)


(after! company
  ;; (setq company-idle-delay 0.2)
  (setq company-format-margin-function #'company-detect-icons-margin))


(use-package! graphql-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.graphqls\\'" . graphql-mode)))


(use-package! protobuf-mode
  :defer t)


(use-package! gn-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.gni?\\'" . gn-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! bazel-mode
  :defer t)

(add-to-list 'auto-mode-alist '("\\.inl\\'" . +cc-c-c++-objc-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode))

(defun +cc/copy-lldb-breakpoint-of-current-line ()
  "Copy a pdb like breakpoint on the current line."
  (interactive)
  (kill-new
   (concat "b " (file-name-nondirectory (buffer-file-name))
           " : " (number-to-string (line-number-at-pos)))))

(after! lsp-clangd
  (setq lsp-clients-clangd-args '("-j=3"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2)
  (after! dap-mode
    (require 'dap-codelldb)))

(after! eglot
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS, WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! '(web-mode-hook html-mode-hook) (setq-local format-all-formatters '(("HTML" prettier))))
(add-hook! 'typescript-mode-hook (setq-local format-all-formatters '(("TypeScript" prettier))))
(add-hook! 'rjsx-mode-hook (setq-local format-all-formatters '(("JavaScript" prettier))))

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

(setq lsp-clients-typescript-init-opts
      '(:importModuleSpecifierPreference "relative"))

(after! eglot
  ;; https://www.reddit.com/r/emacs/comments/11bqzvk/emacs29_and_eglot_inlay_hints/
  ;; https://github.com/microsoft/TypeScript/blob/main/src/server/protocol.ts#L3410-L3539
  (set-eglot-client! '(typescript-mode js-mode js-ts-mode tsx-ts-mode typescript-ts-mode)
                     '("typescript-language-server" "--stdio" :initializationOptions
                       (:preferences (:importModuleSpecifierPreference "relative"
                                      :allowRenameOfImportPath t
                                      ))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (set-formatter! 'google-java-format "google-java-format -" :modes '(java-mode))

;; jdtls mirror in China
;; (setq lsp-java-jdt-download-url "http://mirrors.ustc.edu.cn/eclipse/jdtls/milestones/1.1.2/jdt-language-server-1.1.2-202105191944.tar.gz")

(after! lsp-java
  ;; eclipse.jdt.ls needs java 11
  (dolist (java_path '("/usr/lib/jvm/java-11-amazon-corretto.x86_64"
                       "/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home"))
    (if (file-directory-p java_path)
        (setq lsp-java-configuration-runtimes `[(:name "JavaSE-11"
                                                 :path ,java_path
                                                 :default t)]
              lsp-java-java-path (concat java_path "/bin/java")
              dap-java-java-command (concat java_path "/bin/java")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (after! go-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP & DAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use format-all by default
(setq +format-with-lsp nil)

(setq +lsp-prompt-to-install-server 'quiet)

(after! lsp-mode
  (setq lsp-log-io nil
        lsp-file-watch-threshold 4000
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-headerline-breadcrumb-segments '(file symbols)
        lsp-imenu-index-symbol-kinds '(File Module Namespace Package Class Method Enum Interface
                                       Function Variable Constant Struct Event Operator TypeParameter)
        )
  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]third-party\\'"
                 "[/\\\\]buildtools\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]build\\'"
                 ))
    (push dir lsp-file-watch-ignored-directories))
  )

(after! lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-lens-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100))

(use-package lsp-docker
  :when (not (modulep! :tools lsp +eglot))
  :defer t
  :commands lsp-docker-init-clients
  :config
  (defvar lsp-docker-client-packages
    '(lsp-css lsp-clients lsp-bash lsp-go lsp-pyls lsp-html lsp-typescript
      lsp-terraform lsp-cpp))

  (defvar lsp-docker-client-configs
    (list
     (list :server-id 'bash-ls :docker-server-id 'bashls-docker :server-command "bash-language-server start")
     (list :server-id 'clangd :docker-server-id 'clangd-docker :server-command "ccls")
     (list :server-id 'css-ls :docker-server-id 'cssls-docker :server-command "css-languageserver --stdio")
     (list :server-id 'dockerfile-ls :docker-server-id 'dockerfilels-docker :server-command "docker-langserver --stdio")
     (list :server-id 'gopls :docker-server-id 'gopls-docker :server-command "gopls")
     (list :server-id 'html-ls :docker-server-id 'htmls-docker :server-command "html-languageserver --stdio")
     (list :server-id 'pyls :docker-server-id 'pyls-docker :server-command "pyls")
     (list :server-id 'ts-ls :docker-server-id 'tsls-docker :server-command "typescript-language-server --stdio")))

  ;; (lsp-docker-init-clients
  ;;  :path-mappings `((,(file-truename "~/av") . "/code"))
  ;;  ;; :docker-image-id "my-lsp-docker-container:1.0"
  ;;  :client-packages '(lsp-pyls)
  ;;  :client-configs lsp-docker-client-configs)
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


(after! realgud (advice-remove #'realgud:terminate #'+debugger--cleanup-after-realgud-a))


(when (modulep! :tools debugger)
  (defun +my/dap-start ()
    (interactive)
    (dap-mode 1)
    (call-interactively #'dap-debug))

  (defun +my/dap-delete-output-and-stderr-buffers ()
    (doom/kill-matching-buffers " stderr*" (buffer-list))
    (doom/kill-matching-buffers " out*" (buffer-list)))

  ;; (add-hook! dap-mode-hook ((tooltip-mode 1)))

  (after! dap-mode
    ;; (setq dap-auto-configure-features '(sessions locals expressions controls tooltip))
    (setq lsp-enable-dap-auto-configure nil)

    ;; use M-u to exit dap-hydra
    (after! dap-hydra
      (defhydra+ dap-hydra () ("M-u" nil)))

    ;; Toggle dap-hydra whenever breakpoint is triggered
    ;; (add-hook 'dap-stopped-hook
    ;;           (lambda (arg) (call-interactively #'dap-hydra)))
    )

  (map! :leader
        (:prefix ("d" . "debug")
         :desc "Start debugger" "d" #'+my/dap-start
         :desc "Start last debugger" "D" #'dap-debug-last
         :desc "Remove DAP outpput buffers" "K" #'+my/dap-delete-output-and-stderr-buffers
         (:prefix ("b" . "breakpoint")
                  "b" #'dap-breakpoint-toggle
                  "c" #'dap-breakpoint-condition)
         "B" #'dap-ui-breakpoints
         "h" #'dap-hydra
         "r" #'dap-debug-restart
         "l" #'dap-ui-locals
         "e" #'dap-ui-expressions
         "a" #'dap-ui-expressions-add
         "R" #'dap-ui-expressions-remove
         "f" #'dap-switch-stack-frame
         "q" #'dap-disconnect
         "s" #'dap-ui-sessions
         "k" #'dap-delete-session
         "K" #'dap-delete-all-sessions
         "S" #'realgud-short-key-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode sxhkd-mode
  '(?#)
  '("alt" "Escape" "super" "bspc" "ctrl" "space" "shift") nil
  '("sxhkdrc") nil
  "Simple mode for sxhkdrc files.")
