;; -*- no-byte-compile: t; -*-
;;; private/my-cc/packages.el

(when (executable-find "ccls")
  (package! ccls :recipe (:fetcher github :repo "MaskRay/emacs-ccls" :commit "07ad553950e69f862f7c74c9b1f02c00ab450d22")))
(package! flycheck-google-cpplint :recipe (:fetcher github :repo "flycheck/flycheck-google-cpplint"))
