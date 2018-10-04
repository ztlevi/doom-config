;;; private/my/+bindings.el -*- lexical-binding: t; -*-

(define-inline +my/prefix-M-x (prefix)
  (inline-quote
   (lambda () (interactive)
     (setq unread-command-events (string-to-list ,prefix))
     (call-interactively #'execute-extended-command))))

(define-inline +my/simulate-key (key)
  (inline-quote
   (lambda () (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key))))))

(define-key! evil-ex-completion-map
  "C-k" #'kill-line
  "C-d" #'delete-forward-char)

(map!
 (:leader
   :nmv "SPC" #'counsel-M-x

   :nm "fo" #'zt/open-file-or-directory-in-external-app
   :nm "br" #'revert-buffer-no-confirm
   :nm "bf" #'+macos/reveal-in-finder
   :nm "bF" #'+macos/reveal-project-in-finder
   :nm "bT" #'+macos/reveal-project-in-iterm
   :nm "bT" #'+macos/reveal-in-iterm
   :nm "bc" #'+macos/reveal-project-in-vscode
   :nm "bC" #'+macos/reveal-in-vscode
   :nm "bM" #'+macos/reveal-in-typora
   :nm "bm" #'view-echo-area-messages
   :nmv "bk" #'kill-this-buffer

   :nmv "oI" #'ibuffer
   )

 "C-M-\\" #'indent-region-or-buffer
 "C-h h" nil
 "C-h C-k" #'find-function-on-key
 "C-h C-f" #'find-function-at-point
 "C-h C-v" #'find-variable-at-point
 :nmvi "C-`" #'+popup/toggle
 :nmv "C-e" #'doom/forward-to-last-non-comment-or-eol
 :nmv "C-a" #'doom/backward-to-bol-or-indent
 :nmv "M-s" #'evil-write-all
 :nmv "M-e" #'+ivy/switch-workspace-buffer
 :nmv "C-s" #'swiper
 :nmv "M-p" #'counsel-git
 :nmv "M-/" #'evilnc-comment-or-uncomment-lines
 :i   "C-k" #'kill-line
 :i   "C-d" #'delete-forward-char
 :v "<backspace>" (kbd "\"_d")
 :v "<del>" (kbd "\"_d")
 :v "C-r" #'zt/evil-quick-replace

 (:map prog-mode-map
   ;; Override default :n < > ( )
   ;; :nm "<" #'lispyville-previous-opening
   ;; :nm ">" #'lispyville-next-closing

   ;; :n "C-h" #'lispyville-backward-up-list
   ;; :n "C-j" #'lispyville-forward-sexp
   ;; :n "C-k" #'lispyville-backward-sexp
   ;; :n "C-l" #'lispyville-up-list

   ;; :n "H"  #'lsp-ui-peek-jump-backward
   ;; :n "L"  #'lsp-ui-peek-jump-forward
   :m "C-S-h"  #'+my/xref-jump-backward-file
   :m "C-S-l"  #'+my/xref-jump-forward-file
   )

 :n "M-u" (+my/simulate-key "[")
 :n "M-i" (+my/simulate-key "]")
 :m "M-h"  #'smart-up
 :m "M-l"  #'smart-down
 :n "M-."  #'+lookup/definition
 ;; :n "M-j"  #'+my/find-definitions

 :n "C-c a" #'org-agenda
 :n "C-,"  #'+my/find-references
 ;; all symbols
 :n ";"    (λ! (if lsp-mode
                   (progn (+my/avy-document-symbol t)
                          (+my/find-definitions))
                 (avy-goto-word-0 nil)))
 ;; outline
 :n "z;"   (λ! (+my/avy-document-symbol nil) (+my/find-definitions))

 :n "ga"   #'lsp-ui-find-workspace-symbol
 :n "gc"   #'evilnc-comment-or-uncomment-lines
 :n "gf"   #'+my/ffap
 :n "go"   (λ! (message "%S" (text-properties-at (point))))

 :n "[ M-u" #'symbol-overlay-switch-backward
 :n "] M-i" #'symbol-overlay-switch-forward

 (:leader
   :n "M-u" (+my/simulate-key "SPC [")
   :n "M-i" (+my/simulate-key "SPC ]")
   (:desc "app" :prefix "a"
     :desc "genhdr" :n "g"
     (λ! (shell-command-on-region (point-min) (point-max) "genhdr" t t))
     :desc "genhdr windows" :n "G"
     (λ! (shell-command-on-region (point-min) (point-max) "genhdr windows" t t))
     )
   (:prefix "f"
     :n "p" #'treemacs-projectile
     :n "C-p" #'+default/find-in-config
     :n "C-S-p" #'+default/browse-config
     :n "t" #'treemacs
     )
   (:prefix "g"
     :n "*" (+my/prefix-M-x "magit-")
     )
   (:prefix "h"
     :n "C" #'helpful-command
     )
   :desc "lispyville" :n "L" (+my/prefix-M-x "lispyville ")
   (:prefix "o"
     :n "c" #'counsel-imenu-comments
     :n "d" #'+debugger:start
     :n "o" #'symbol-overlay-put
     :n "q" #'symbol-overlay-remove-all
     )
   (:prefix "p"
     :n "e" #'projectile-run-eshell
     :n "f" #'counsel-projectile-find-file
     :n "*" (+my/prefix-M-x "projectile-")
     )

   (:desc "search" :prefix "s"
     :n "b" #'swiper-all
     :desc "Directory"              :nv "d" #'+ivy/project-search-from-cwd
     :desc "Project"                :nv "s" #'+ivy/project-search
     :desc "Symbols"                :nv "i" #'imenu
     :desc "Symbols across buffers" :nv "I" #'imenu-anywhere
     :desc "Online providers"       :nv "o" #'+lookup/online-select
     )

   (:desc "toggle" :prefix "t"
     :n "d" #'toggle-debug-on-error
     :n "D" #'+my/realtime-elisp-doc
     :n "v" #'visual-line-mode
     )
   )

 (:prefix "C-x"
   :n "e"  #'pp-eval-last-sexp
   :n "u" #'link-hint-open-link
   )

 (:after elisp-mode
   (:map emacs-lisp-mode-map
     :n "M-j" #'lispy-splice))
 (:after evil-collection-info
   :map Info-mode-map
   "/" #'Info-search
   "?" #'Info-search-backward
   )

 (:after realgud
   (:map realgud-track-mode-map
     :in ";" #'realgud-window-src-undisturb-cmd)
   (:map realgud:shortkey-mode-map
     :n "e" (λ! (realgud:cmd-run-command (thing-at-point 'symbol) "eval"))
     :n "t" #'realgud:cmd-tbreak
     :n "U" #'realgud:cmd-until
     :n "1" (λ! (+my/realgud-eval-nth-name-forward 1))
     :n "2" (λ! (+my/realgud-eval-nth-name-forward 2))
     :n "3" (λ! (+my/realgud-eval-nth-name-forward 3))
     :n "4" (λ! (+my/realgud-eval-nth-name-forward 4))
     :n "5" (λ! (+my/realgud-eval-nth-name-forward 5))
     :n "6" (λ! (+my/realgud-eval-nth-name-forward 6))
     :n "7" (λ! (+my/realgud-eval-nth-name-forward 7))
     :n "8" (λ! (+my/realgud-eval-nth-name-forward 8))
     :n "9" (λ! (+my/realgud-eval-nth-name-forward 9))
     :n "M-1" (λ! (+my/realgud-eval-nth-name-backward 1))
     :n "M-2" (λ! (+my/realgud-eval-nth-name-backward 2))
     :n "M-3" (λ! (+my/realgud-eval-nth-name-backward 3))
     :n "M-4" (λ! (+my/realgud-eval-nth-name-backward 4))
     :n "M-5" (λ! (+my/realgud-eval-nth-name-backward 5))
     :n "M-6" (λ! (+my/realgud-eval-nth-name-backward 6))
     :n "M-7" (λ! (+my/realgud-eval-nth-name-backward 7))
     :n "M-8" (λ! (+my/realgud-eval-nth-name-backward 8))
     :n "M-9" (λ! (+my/realgud-eval-nth-name-backward 9))
     ))
 )
