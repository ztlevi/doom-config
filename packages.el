;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

;; disabled packages
(packages! (exec-path-from-shell :disable t)
           (solaire-mode :disable t)
           (magithub :disable t))

;; misc
(packages! avy
           evil-nerd-commenter
           edit-indirect
           atomic-chrome
           link-hint
           symbol-overlay
           tldr
           (blog-admin :recipe (:fetcher github :repo "codefalling/blog-admin"))
           youdao-dictionary
           wucuo
           org-wild-notifier
           forge
           )

;; programming
(packages! lsp-mode lsp-ui company-lsp
           import-js indium
           importmagic py-isort
           rust-mode
           )
