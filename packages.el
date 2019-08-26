;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

;; disabled packages
(packages! (solaire-mode :disable t)
           (anaconda-mode :disable t)
           (company-anaconda :disable t)
           (diredfl :disable t)
           (dired-k :disable t)
           (pyimport :disable t))

;; misc
(packages! avy
           helm
           dired-narrow
           diff-hl
           edit-indirect
           atomic-chrome
           link-hint
           symbol-overlay
           tldr
           (blog-admin :recipe (:fetcher github :repo "codefalling/blog-admin"))
           youdao-dictionary
           wucuo
           grip-mode
           org-wild-notifier
           (vterm-toggle :recipe (:fetcher github :repo "jixiuf/vterm-toggle"))
           )

;; programming
(packages! lsp-mode lsp-ui company-lsp
           import-js indium
           lsp-python-ms importmagic py-isort
           (flycheck-google-cpplint :recipe (:fetcher github :repo "flycheck/flycheck-google-cpplint"))
           )
