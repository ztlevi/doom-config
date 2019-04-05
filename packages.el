;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

;; disabled packages
(packages! (exec-path-from-shell :disable t)
           (solaire-mode :disable t)
           (cquery :disable t)
           (anaconda-mode :disable t)
           (company-anaconda :disable t))

;; misc
(packages! avy
           edit-indirect
           atomic-chrome
           link-hint
           symbol-overlay
           tldr
           (blog-admin :recipe (:fetcher github :repo "codefalling/blog-admin"))
           youdao-dictionary
           wucuo
           org-wild-notifier
           (vterm-toggle :recipe (:fetcher github :repo "jixiuf/vterm-toggle"))
           )

;; programming
(packages! lsp-mode lsp-ui company-lsp
           import-js indium
           importmagic py-isort
           rust-mode
           (flycheck-google-cpplint :recipe (:fetcher github :repo "flycheck/flycheck-google-cpplint"))
           )
