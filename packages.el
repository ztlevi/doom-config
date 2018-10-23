;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

;; disabled packages
(packages! (anaconda-mode :disable t)
           (exec-path-from-shell :disable t)
           (solaire-mode :disable t))

;; misc
(packages! avy
           evil-nerd-commenter
           edit-indirect
           atomic-chrome
           all-the-icons-dired
           link-hint
           symbol-overlay
           tldr
           try
           (blog-admin :recipe (:fetcher github :repo "codefalling/blog-admin"))
           youdao-dictionary
           )

;; programming
(packages! lispyville
           lsp-mode lsp-ui company-lsp
           wucuo import-js
           lsp-python importmagic py-isort
           lsp-rust rust-mode
           )

(package! ghub+ :recipe (:fetcher github :repo "vermiculus/ghub-plus" :commit "f36334b"))
