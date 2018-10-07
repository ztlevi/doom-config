;;; private/my/+bindings.el -*- lexical-binding: t; -*-

(define-key! 'global
  [remap projectile-find-file] #'counsel-git)

(map!
 "C-M-\\" #'indent-region-or-buffer
 "C-h h" nil
 "C-h C-k" #'find-function-on-key
 "C-h C-f" #'find-function-at-point
 "C-h C-v" #'find-variable-at-point
 "<f8>" #'describe-mode
 :v "M-v" #'evil-visual-paste
 :v "p" #'evil-visual-paste
 :nmvi "C-`" #'+popup/toggle
 :nmvi "M-w" #'+workspace/close-window-or-workspace
 :nmvi "M-m" #'kmacro-call-macro
 :nmv "C-e" #'doom/forward-to-last-non-comment-or-eol
 :nmv "C-a" #'doom/backward-to-bol-or-indent
 :nmv "M-s" #'evil-write-all
 :nmv "M-e" #'+ivy/switch-workspace-buffer
 :nmv "C-s" #'swiper
 :nmv "M-p" #'counsel-git
 :nmv "M-/" #'evilnc-comment-or-uncomment-lines
 :i   "C-n" #'next-line
 :i   "C-p" #'previous-line
 :i   "C-k" #'kill-line
 :i   "C-d" #'delete-forward-char
 :v "<backspace>" (kbd "\"_d")
 :v "<del>" (kbd "\"_d")
 :v "C-r" #'+my/evil-quick-replace
 :mv "C-;" #'flyspell-correct-previous-word-generic

 :n "M-u" (+my/simulate-key "[")
 :n "M-i" (+my/simulate-key "]")
 :m "M-h"  #'smart-up
 :m "M-l"  #'smart-down
 :n "M-."  #'+lookup/definition
 ;; :n "M-j"  #'+my/find-definitions

 :n "C-c a" #'org-agenda
 :n "C-,"  #'+my/find-references

 ;; outline
 :n "z;"   (λ! (+my/avy-document-symbol nil) (+my/find-definitions))

 :n "ga"   #'lsp-ui-find-workspace-symbol
 :n "gf"   #'+my/ffap
 :n "go"   (λ! (message "%S" (text-properties-at (point))))

 :n "[ M-u" #'symbol-overlay-switch-backward
 :n "] M-i" #'symbol-overlay-switch-forward

 (:prefix "C-x"
   :n "e"  #'pp-eval-last-sexp
   :n "u" #'link-hint-open-link)

 (:leader
   :nmv "SPC" #'counsel-M-x

   :n "M-u" (+my/simulate-key "SPC [")
   :n "M-i" (+my/simulate-key "SPC ]")
   :desc "lispyville" :n "L" (+my/prefix-M-x "lispyville ")
   (:desc "app" :prefix "a"
     :n "s" #'prodigy
     :desc "genhdr" :n "g"
     (λ! (shell-command-on-region (point-min) (point-max) "genhdr" t t))
     :desc "genhdr windows" :n "G"
     (λ! (shell-command-on-region (point-min) (point-max) "genhdr windows" t t)))
   (:prefix "b"
     :desc "Last buffer" :nmv "l" #'evil-switch-to-windows-last-buffer
     :nmv "b" #'ivy-switch-buffer
     :nm "r" #'revert-buffer-no-confirm
     :nm "f" #'+macos/reveal-in-finder
     :nm "F" #'+macos/reveal-project-in-finder
     :nm "T" #'+macos/reveal-project-in-iterm
     :nm "T" #'+macos/reveal-in-iterm
     :nm "c" #'+macos/reveal-project-in-vscode
     :nm "C" #'+macos/reveal-in-vscode
     :nm "M" #'+macos/reveal-in-typora
     :nm "m" #'view-echo-area-messages
     :nm "U" #'+my/untabify-buffer
     :nmv "k" #'kill-current-buffer)
   (:prefix "f"
     :nm "o" #'+macos/open-in-default-program
     :n "p" #'treemacs-projectile
     :n "C-p" #'+default/find-in-config
     :n "C-S-p" #'+default/browse-config
     :n "t" #'treemacs)
   (:prefix "g"
     :n "*" (+my/prefix-M-x "magit-"))
   (:prefix "r"
     :n "r" #'rjsx-mode
     :n "i" #'ivy-resume)
   (:prefix "h"
     :n "C" #'helpful-command)
   (:desc "error" :prefix "e"
     :n "n" #'flycheck-next-error
     :n "p" #'flycheck-previous-error
     :n "l" #'flycheck-list-errors
     :n "v" #'flycheck-verify-setup)
   (:prefix "o"
     :nmv "I" #'ibuffer
     :nv "x" #'link-hint-open-all-links
     :nv "c" #'counsel-imenu-comments
     :n "d" #'+debugger:start
     :n "o" #'symbol-overlay-put
     :n "q" #'symbol-overlay-remove-all)
   (:prefix "p"
     :n "e" #'projectile-run-eshell
     :n "*" (+my/prefix-M-x "projectile-"))
   (:desc "toggle" :prefix "t"
     :n "d" #'toggle-debug-on-error
     :n "D" #'+my/realtime-elisp-doc
     :n "v" #'visual-line-mode))

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
 (:after lispy
   (:map lispy-mode-map
     :i "C-c (" #'lispy-wrap-round
     :i "_" #'special-lispy-different
     :i [remap delete-backward-char] #'lispy-delete-backward
     :n "M-j" #'lispy-splice))
 (:after elisp-mode
   :map emacs-lisp-mode-map
   :n "gh" #'helpful-at-point
   :n "C-<left>" #'lispy-forward-barf-sexp
   :n "C-<right>" #'lispy-forward-slurp-sexp
   :n "C-M-<left>" #'lispy-backward-slurp-sexp
   :n "C-M-<right>" #'lispy-backward-barf-sexp
   :n "<tab>" #'lispyville-prettify
   :localleader
   :n "e" (λ! (save-excursion (forward-sexp) (eval-last-sexp nil))))
 (:after lsp-ui
   :map lsp-ui-mode-map
   :n "M-j" #'toggle-lsp-ui-doc)
 (:after lsp-ui-peek
   :map lsp-ui-peek-mode-map
   "h" #'lsp-ui-peek--select-prev-file
   "j" #'lsp-ui-peek--select-next
   "k" #'lsp-ui-peek--select-prev
   "l" #'lsp-ui-peek--select-next-file)
 (:after python
   (:map python-mode-map
     :localleader
     :n "=" 'py-autopep8-buffer
     :n "i" 'importmagic-fix-symbol-at-point))
 (:after js2-mode
   (:map js2-mode-map
     :localleader
     :n "i" 'import-js-import
     :n "f" 'import-js-fix))
 (:after rjsx-mode
   (:map rjsx-mode-map
     :localleader
     :n "i" 'import-js-import
     :n "f" 'import-js-fix))
 (:after tide
   :map tide-references-mode-map
   "C-k" 'tide-find-previous-reference
   "p" 'tide-find-previous-reference
   "C-j" 'tide-find-next-reference
   "n" 'tide-find-next-reference
   "C-l" 'tide-goto-reference)
 (:after rust-mode
   (:map rust-mode-map
     :localleader
     :n "=" #'rust-format-buffer))
 (:after markdown-mode
   (:map markdown-mode-map
     :localleader
     :desc "Edit" :n "x" (+my/simulate-key "C-c C-s")
     (:desc "Insert" :prefix "i")
     :desc "Table" :prefix "t"
     :n "r" #'markdown-table-insert-row
     :n "c" #'markdown-table-insert-row))
 (:after ivy
   :map ivy-minibuffer-map
   "<tab>" #'ivy-partial-or-done
   "C-<return>" #'ivy-immediate-done
   "C-j" #'ivy-call-and-recenter
   "C-;" #'ivy-avy
   "C-b" #'backward-word
   "C-f" #'forward-word
   "C-k" #'ivy-kill-line
   "C-v" #'ivy-scroll-up-command
   "M-v" #'ivy-scroll-down-command)
 (:after magit-blame
   (:map magit-blame-mode-map
     :n "o" #'magit-blame--git-link-commit))
 (:after git-rebase
   (:map git-rebase-mode-map
     "M-j" #'git-rebase-move-line-down
     "M-k" #'git-rebase-move-line-up
     "SPC" nil))
 (:after evil-vars
   :map evil-ex-completion-map
   "C-k" #'kill-line
   "C-d" #'delete-forward-char)
 (:after evil-collection-info
   :map Info-mode-map
   "/" #'Info-search
   "?" #'Info-search-backward)
 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-v"        #'company-next-page
     "M-v"        #'company-previous-page
     "<tab>"      nil
     "C-i"        #'company-complete-selection))
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
