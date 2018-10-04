;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el
(disable-packages! cmake-mode company-irony company-irony-c-headers flycheck-irony
                   irony irony-eldoc ivy-rtags rtags anaconda-mode conda)

(package! avy)
(package! atomic-chrome)
(package! all-the-icons-dired)

(package! eshell-autojump)
(package! evil-nerd-commenter)
(package! link-hint)
(package! htmlize)
(package! smart-forward)
(package! symbol-overlay)
(package! tldr)
(package! try)

;; programming
(package! py-autopep8)
(package! importmagic)
(package! prettier-js)
(package! import-js)
(package! rust-mode)
(package! lsp-rust)
(package! lispyville)
(package! lsp-mode)
(package! lsp-ui)
(package! lsp-python)
(package! company-lsp)
(package! function-args)
(package! lpy :recipe (:fetcher github :repo "abo-abo/lpy" :files ("*")))

(disable-packages! company-prescient exec-path-from-shell solaire-mode)
