;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

;; disabled packages
(disable-packages! solaire-mode
                   anaconda-mode
                   company-anaconda
                   lsp-python-ms
                   pyimport)

;; text
;; (package! org-wild-notifier)
(package! adoc-mode)
(package! tldr)
(package! edit-indirect)
(package! blog-admin :recipe (:host github :repo "codefalling/blog-admin"))
(package! youdao-dictionary)
(package! link-hint)
(package! symbol-overlay)

;; misc
(package! all-the-icons-ibuffer)
(package! dired-narrow)
(package! atomic-chrome)
(package! git-link)
(package! counsel-etags)
(package! imenu-list)
(package! tmux-pane)
(package! lsp-docker)
(package! rime :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c")))
(package! highlight-indent-guides)

;; programming
(package! bazel-mode :recipe (:host github :repo "bazelbuild/emacs-bazel-mode"))
(package! import-js)
