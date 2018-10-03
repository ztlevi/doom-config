;;; private/my/config.el -*- lexical-binding: t; -*-

(load! "+funcs")
(load! "+bindings")
(load! "+org")
(load! "+ui")
(load! "+misc")
(load! "+prog")

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

(def-package! avy
  :commands (avy-goto-char-timer)
  :init
  (setq avy-timeout-seconds 0.2)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p))
  )

(after! eshell
  (defun eshell/l (&rest args) (eshell/ls "-l" args))
  (defun eshell/e (file) (find-file file))
  (defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))
  (defun eshell/ft (&optional arg) (treemacs arg))

  (defun eshell/up (&optional pattern)
    (let ((p (locate-dominating-file
              (f-parent default-directory)
              (lambda (p)
                (if pattern
                    (string-match-p pattern (f-base p))
                  t)))
             ))
      (eshell/pushd p)))
  )

(def-package! eshell-autojump)

(def-package! evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines)
  )

(after! evil-snipe
  (setq evil-snipe-scope 'buffer)
  )

(def-package! link-hint
  :commands link-hint-open-link link-hint-open-all-links)

(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))

(after! realgud
  (setq realgud-safe-mode nil)
  (evil-collection-define-key 'normal 'realgud:shortkey-mode-map
    "d" #'realgud:cmd-newer-frame
    "D" #'realgud:cmd-delete
    "u" #'realgud:cmd-older-frame
    )
  )

(defun +advice/xref-set-jump (&rest args)
  (lsp-ui-peek--with-evil-jumps (evil-set-jump)))
(advice-add '+lookup/definition :before #'+advice/xref-set-jump)
(advice-add '+lookup/references :before #'+advice/xref-set-jump)


(defvar +my/xref-blacklist nil
  "List of paths that should not enable xref-find-* or dumb-jump-go")

;;; Override
;; This function is transitively called by xref-find-{definitions,references,apropos}
(after! xref
  ;; This is required to make `xref-find-references' not give a prompt.
  ;; `xref-find-references' asks the identifier (which has no text property)
  ;; and then passes it to `lsp-mode', which requires the text property at
  ;; point to locate the references.
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))

  (defun xref--show-xrefs (xrefs display-action &optional always-show-list)
    ;; PATCH
    (lsp-ui-peek--with-evil-jumps (evil-set-jump))

    ;; PATCH Jump to the first candidate
    (if (not (cdr xrefs))
        (xref--pop-to-location (car xrefs) display-action)
      (funcall xref-show-xrefs-function xrefs
               `((window . ,(selected-window))))
      ))
  )

(after! ivy-xref
  ;; (defun ivy-xref-show-xrefs (xrefs alist)
  ;;   (minibuffer-with-setup-hook #'hydra-ivy/body
  ;;      (minibuffer-with-setup-hook #'ivy-toggle-calling
  ;;        (ivy-read "xref: " (ivy-xref-make-collection xrefs)
  ;;                  :require-match t
  ;;                  :action #'(lambda (candidate)
  ;;                              (xref--show-location (cdr candidate) 'quit))))))
  ;; (push '(xref-find-references) ivy-display-functions-alist)
  (push '(ivy-xref-show-xrefs . nil) ivy-sort-functions-alist)
  )

(def-package! rust-mode
  :mode "\\.rs$"
  :config
  (map! :map rust-mode-map
        :leader
        :n "=" #'rust-format-buffer
        )
  )

(def-package! smart-forward)

(def-package! symbol-overlay
  :commands (symbol-overlay-put))

(def-package! lsp-rust
  :init (add-hook 'rust-mode-hook #'lsp-rust-enable)
  :config
  )

(def-package! tldr
  :commands (tldr)
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (set-popup-rule! "^\\*tldr\\*" :side 'right :select t :quit t)
  )

(after! nav-flash
  ;; (defun nav-flash-show (&optional pos end-pos face delay)
  ;; ...
  ;; (let ((inhibit-point-motion-hooks t))
  ;; (goto-char pos)
  ;; (beginning-of-visual-line) ; work around args-out-of-range error when the target file is not opened
  (defun +advice/nav-flash-show (orig-fn &rest args)
    (ignore-errors (apply orig-fn args)))
  (advice-add 'nav-flash-show :around #'+advice/nav-flash-show))

(set-popup-rules! '(("^\\*helpful" :size 0.4)
                    ("^\\*info.*" :size 80 :side right)
                    ("^\\*Man.*" :size 80 :side right)
                    ))

(let ((profile "~/.config/doom/profile.el"))
  (when (file-exists-p profile)
    (load-file profile)))
