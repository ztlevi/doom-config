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
;; (package! org-wild-notifier)
(package! adoc-mode)
(package! tldr)
(package! edit-indirect)
(package! blog-admin :recipe (:host github :repo "codefalling/blog-admin"))
(package! youdao-dictionary)
(package! link-hint)
(package! symbol-overlay)

;; misc
(package! keycast)
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
(package! go-translate)

;; programming
(unpin! company)
(package! company :pin "f3aacd77d0135c09227400fef45c54b717d33f2e")
(package! bazel-mode :recipe (:host github :repo "bazelbuild/emacs-bazel-mode"))
(package! import-js)
