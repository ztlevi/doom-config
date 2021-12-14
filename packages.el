;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

;; disabled packages
(disable-packages! solaire-mode
                   realgud
                   realgud-trepan-ni
                   anaconda-mode
                   company-anaconda
                   lsp-python-ms
                   pyimport)

;; text
(package! adoc-mode)
(package! tldr)
(package! edit-indirect)
(package! blog-admin :recipe (:host github :repo "codefalling/blog-admin"))
(package! youdao-dictionary)
(package! link-hint)
(package! symbol-overlay)
(package! pomm :recipe (:host github :repo "SqrtMinusOne/pomm.el"))

;; misc
(package! keycast)
(package! evil-string-inflection)
(package! all-the-icons-ibuffer)
(package! dired-narrow)
(package! atomic-chrome)
(package! git-link)
(package! citre)
(package! imenu-list)
(package! tmux-pane)
(package! lsp-docker)
(package! rime :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c")))
(package! highlight-indent-guides)
(package! go-translate)
;; https://github.com/tecosaur/screenshot/issues/11
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot") :pin "f8204e82dc0c1158c401735d36a143e6f6d24cf5")
(package! code-review)

;; programming
(package! bazel-mode :recipe (:host github :repo "bazelbuild/emacs-bazel-mode"))
(package! graphql-mode)
(package! protobuf-mode)
(package! gn-mode)
