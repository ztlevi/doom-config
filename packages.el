;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

;; disabled packages
(disable-packages! solaire-mode
                   anaconda-mode
                   company-anaconda
                   dired-k
                   pyimport)

;; misc
(package! avy)
(package! helm)
(package! dired-narrow) 
(package! diff-hl) 
(package! edit-indirect) 
(package! atomic-chrome) 
(package! link-hint) 
(package! symbol-overlay) 
(package! tldr) 
(package! (blog-admin :recipe (:fetcher github :repo "codefalling/blog-admin"))) 
(package! youdao-dictionary) 
(package! wucuo) 
(package! grip-mode) 
(package! org-wild-notifier) 
(package! (vterm-toggle :recipe (:fetcher github :repo "jixiuf/vterm-toggle"))) 

;; programming
(package! import-js) 
(package! indium) 
(package! lsp-python-ms) 
(package! importmagic) 
(package! py-isort) 
(package! (flycheck-google-cpplint :recipe (:fetcher github :repo "flycheck/flycheck-google-cpplint"))) 
