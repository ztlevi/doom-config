;;; private/my-cc/config.el -*- lexical-binding: t; -*-

(after! cc-mode
  ;; https://github.com/radare/radare2
  (c-add-style
   "radare2"
   '((c-basic-offset . 4)
     (indent-tabs-mode . t)
     (c-auto-align-backslashes . nil)
     (c-offsets-alist
      (arglist-intro . ++)
      (arglist-cont . ++)
      (arglist-cont-nonempty . ++)
      (statement-cont . ++)
      )))
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
  (setq c-default-style "my-cc")

  (add-hook 'c-mode-common-hook
            (lambda ()
              (modify-syntax-entry ?_ "w")))

  (add-to-list 'auto-mode-alist '("\\.inc\\'" . +cc-c-c++-objc-mode))

  (map!
   :map (c-mode-map c++-mode-map)
   (:localleader
     :n "a" #'ccls/references-address
     :n "c" #'ccls-call-hierarchy
     :n "f" #'ccls/references-not-call
     :n "lp" #'ccls-preprocess-file
     :n "lf" #'ccls-reload
     :n "m" #'ccls/references-macro
     :n "r" #'ccls/references-read
     :n "w" #'ccls/references-write
     :desc "breakpoint"
     :n "db" (lambda ()
               (interactive)
               (evil-open-above 1)
               (insert "volatile static int z=0;while(!z)asm(\"pause\");")
               (evil-normal-state))))
  )


(def-package! ccls
  :when (executable-find "ccls")
  :defer t
  :init (add-hook! (c-mode c++-mode cuda-mode objc-mode) #'+ccls//enable)
  :config
  (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode)

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

  (evil-set-initial-state 'ccls-tree-mode 'emacs)
  (set-company-backend! '(c-mode c++-mode objc-mode) 'company-lsp)
  (set-lookup-handlers! '(c-mode c++-mode objc-mode)
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  )


(def-package! flycheck-google-cpplint)


(after! flycheck
  (setq flycheck-c/c++-googlelint-executable "cpplint")
  (flycheck-add-next-checker 'c/c++-gcc '(warning . c/c++-googlelint))

  (setq flycheck-c/c++-gcc-executable "gcc-7"
        flycheck-gcc-include-path '("/usr/local/inclue"))

  (add-hook! c++-mode-hook
    (setq flycheck-gcc-language-standard "c++11"
          flycheck-clang-language-standard "c++11"))
  )
