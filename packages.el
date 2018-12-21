;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

;; disabled packages
(packages! (exec-path-from-shell :disable t)
           (solaire-mode :disable t))

;; misc
(packages! avy
           evil-nerd-commenter
           edit-indirect
           atomic-chrome
           link-hint
           symbol-overlay
           tldr
           try
           (blog-admin :recipe (:fetcher github :repo "codefalling/blog-admin"))
           youdao-dictionary
           wucuo
           )

;; programming
(packages! lsp-mode lsp-ui company-lsp
           wucuo import-js
           importmagic py-isort
           lsp-rust rust-mode
           )
