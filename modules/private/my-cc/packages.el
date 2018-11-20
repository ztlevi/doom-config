;; -*- no-byte-compile: t; -*-
;;; private/my-cc/packages.el

(when (executable-find "ccls")
  (package! ccls))
(package! flycheck-google-cpplint :recipe (:fetcher github :repo "flycheck/flycheck-google-cpplint"))
