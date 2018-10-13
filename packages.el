;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el
(disable-packages! cmake-mode company-irony company-irony-c-headers flycheck-irony
                   irony irony-eldoc ivy-rtags rtags anaconda-mode conda
                   company-prescient exec-path-from-shell solaire-mode)

;; misc
(package! avy)
(package! atomic-chrome)
(package! all-the-icons-dired)
(package! link-hint)
(package! symbol-overlay)
(package! tldr)
(package! try)

;; programming
(package! lispyville)
(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)
(package! wucuo)
(package! import-js)
(package! lsp-python)
(package! importmagic)
(package! lsp-rust)
(package! rust-mode)
