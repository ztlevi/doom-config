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
(package! helm)
(package! dired-narrow)
(package! atomic-chrome)
(package! git-link)
(package! wucuo)
(package! vterm-toggle :recipe (:host github :repo "jixiuf/vterm-toggle"))
(package! counsel-etags)
(package! imenu-list)
(package! tmux-pane)
(package! rime :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c")))
(package! color-rg :recipe (:host github :repo "manateelazycat/color-rg"))
(package! snails :recipe (:host github :repo "manateelazycat/snails"))
(package! fuz :recipe (:host github :repo "rustify-emacs/fuz.el"))

;; programming
(package! bazel-mode :recipe (:host github :repo "bazelbuild/emacs-bazel-mode"))
(package! import-js)
;; (package! importmagic)
(package! py-isort)
(package! flycheck-mypy)
(package! flycheck-google-cpplint :recipe (:host github :repo "flycheck/flycheck-google-cpplint"))
