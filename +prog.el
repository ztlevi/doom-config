;;; ~/.doom.d/+prog.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq +format-on-save-enabled-modes '(emacs-lisp-mode nix-mode))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! bazel-mode
  :defer t
  :commands bazel-mode
  :init
  (add-to-list 'auto-mode-alist '("BUILD\\(\\.bazel\\)?\\'" . bazel-mode))
  (add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))
  :config
  ;; disable format-all becuase it doesn't sort BUILD list variables
  (setq bazel-mode-buildifier-before-save t)
  (appendq! +format-on-save-enabled-modes '(bazel-mode)))

(add-to-list 'auto-mode-alist '("\\.inl\\'" . +cc-c-c++-objc-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode))

(setq-default c-basic-offset 2)

(after! cc-mode
  (c-add-style
   "my-cc" '("user"
             (c-basic-offset . 2)
             (c-offsets-alist
              . ((innamespace . 0)
                 (access-label . -)
                 (case-label . 0)
                 (member-init-intro . +)
                 (topmost-intro . 0)
                 (arglist-cont-nonempty . +)))))
  (setq c-default-style "my-cc"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS, WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(set-formatter! 'google-java-format "google-java-format -" :modes '(java-mode))

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
              dap-java-java-command (concat java_path "/bin/java"))))

  (setq-hook! 'java-mode-hook c-basic-offset 4))


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
        lsp-file-watch-threshold 3000
        lsp-headerline-breadcrumb-enable t)
  (dolist (dir '("[/\\\\]\\.ccls-cache$"
                 "[/\\\\]\\.mypy_cache$"
                 "[/\\\\]\\.pytest_cache$"
                 "[/\\\\]\\.cache$"
                 "[/\\\\]\\.clwb$"
                 "[/\\\\]_build$"
                 "[/\\\\]__pycache__$"
                 "[/\\\\]bazel-bin$"
                 "[/\\\\]bazel-code$"
                 "[/\\\\]bazel-genfiles$"
                 "[/\\\\]bazel-out$"
                 "[/\\\\]bazel-testlogs$"
                 "[/\\\\]third_party$"
                 "[/\\\\]third-party$"
                 ))
    (push dir lsp-file-watch-ignored-directories))
  )

(after! lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100))

(use-package lsp-docker
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


(defun +my/dap-start ()
  (interactive)
  (dap-mode 1)
  (call-interactively #'dap-debug))

;; (add-hook! dap-mode-hook ((tooltip-mode 1)))

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-segments '(file symbols)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode sxhkd-mode
  '(?#)
  '("alt" "Escape" "super" "bspc" "ctrl" "space" "shift") nil
  '("sxhkdrc") nil
  "Simple mode for sxhkdrc files.")
