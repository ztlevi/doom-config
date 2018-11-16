;; -*- no-byte-compile: t; -*-
;;; private/my-cc/packages.el

(package! ccls)
(package! flycheck-google-cpplint :recipe (:fetcher github :repo "flycheck/flycheck-google-cpplint"))
