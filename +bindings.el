;;; private/my/+bindings.el -*- lexical-binding: t; -*-

(map!
 "C-M-\\" #'indent-region-or-buffer
 "C-h h" nil
 "C-h C-k" #'find-function-on-key
 "C-h C-f" #'find-function-at-point
 "C-h C-v" #'find-variable-at-point
 "<f8>" #'describe-mode

 :nmvi "C-`" #'+popup/toggle
 :nmvi "M-w" #'+workspace/close-window-or-workspace
 :nmvi "C-e" #'doom/forward-to-last-non-comment-or-eol
 :nmvi "C-a" #'doom/backward-to-bol-or-indent
 :nmvi "M-a" #'mark-whole-buffer
 :nmvi "M-c" #'evil-yank
 :nmvi "M-q" (if (daemonp) #'delete-frame #'evil-quit-all)
 :nmvi "M-s" #'evil-write-all
 :nmvi "M-f" #'swiper
 :nmvi "C-s" #'swiper
 :nmvi "M-e" #'+ivy/switch-workspace-buffer
 :nmvi "M-p" #'counsel-git
 :nmvi "C-;" #'flyspell-correct-previous-word-generic
 :nmvi "M-m" #'kmacro-call-macro
 :nmvi "M-;" #'+my/insert-semicolon-at-the-end-of-this-line
 :nmvi "C-M-;" #'+my/delete-semicolon-at-the-end-of-this-line
 :nmvi "M-/" #'evilnc-comment-or-uncomment-lines
 :nv   "M-." #'+lookup/definition

 :i "C-n" #'next-line
 :i "C-p" #'previous-line
 :i "C-k" #'kill-line
 :i "C-d" #'delete-forward-char

 :v "p"     #'evil-visual-paste
 :v "M-v"   #'evil-visual-paste
 :v "C-r"   #'+my/evil-quick-replace
 :v "<del>" (kbd "\"_d")
 :v "<backspace>" (kbd "\"_d")

 (:prefix "C-x"
   :n "e"  #'pp-eval-last-sexp)
 (:prefix "C-c"
   :ni "/" #'company-files
   :desc "Text properties at point" :nmv "f" (λ! (message "%S" (text-properties-at (point)))))

 (:leader
   :desc "counsel-M-x" :nmv "SPC" #'counsel-M-x
   :desc "window"      :nmv "w"  evil-window-map

   :desc "lispyville" :n "L" (+my/prefix-M-x "lispyville ")
   (:desc "app" :prefix "a"
     :n "s" #'prodigy
     :n "b" #'blog-admin-start
     :n "l" #'list-processes
     :nmv "x" #'align-regexp)
   (:desc "buffer" :prefix "b"
     :desc "Last buffer" :nmv "l" #'evil-switch-to-windows-last-buffer
     :nmv  "b" #'ivy-switch-buffer
     :nm   "r" #'revert-buffer-no-confirm
     :nm   "m" #'view-echo-area-messages
     :nm   "U" #'+my/untabify-buffer
     :nmv  "k" #'kill-current-buffer)
   (:desc "workspace" :prefix [tab]
     :desc "Switch workspace" :n [tab] #'+workspace/switch-to
     :desc "Display tab bar"  :n "."   #'+workspace/display)
   (:desc "file" :prefix "f"
     :n "f" #'counsel-find-file)
   (:desc "git" :prefix "g"
     :desc "M-x magit-*" :n "*" (+my/prefix-M-x "magit-"))
   (:desc "help" :prefix "h"
     :n "C" #'helpful-command)
   (:desc "error" :prefix "e"
     :n "n" #'flycheck-next-error
     :n "p" #'flycheck-previous-error
     :n "l" #'flycheck-list-errors
     :n "v" #'flycheck-verify-setup)
   (:desc "open" :prefix "o"
     :desc "Ibuffer"            :nmv "I" #'ibuffer
     :desc "Open link"          :n   "x" #'link-hint-open-link
     :desc "Ansi-Term"          :n   "s" #'+term/open
     :desc "Ansi-Term in popup" :n   "S" #'+term/open-popup-in-project
     :desc "Youdao dictionary"  :n   "y" #'youdao-dictionary-search-at-point-tooltip
     :desc "Youdao play voice"  :n   "Y" #'youdao-dictionary-play-voice-at-point
     :desc "Imenu comments"     :n   "c" #'counsel-imenu-comments
     :desc "Debugger start"     :n   "d" #'+debugger:start
     (:when IS-MAC
       :desc "Reveal in default program"  :nm "f" #'+macos/open-in-default-program
       :desc "Reveal in Finder"           :nm "o" #'+macos/reveal-in-finder
       :desc "Reveal project in Finder"   :nm "O" #'+macos/reveal-project-in-finder
       :desc "Reveal in Terminal"         :nm "t" #'+macos/reveal-in-terminal
       :desc "Reveal project in Terminal" :nm "T" #'+macos/reveal-project-in-terminal
       :desc "Reveal in VSCode"           :nm "c" #'+macos/reveal-in-vscode
       :desc "Reveal project in VSCode"   :nm "C" #'+macos/reveal-project-in-vscode)
     (:when IS-LINUX
       :desc "Reveal in default program"  :nm "f" #'+shell/open-in-default-program
       :desc "Reveal in Finder"           :nm "o" #'+shell/reveal-in-finder
       :desc "Reveal project in Finder"   :nm "O" #'+shell/reveal-project-in-finder
       :desc "Reveal in Terminal"         :nm "t" #'+shell/reveal-in-terminal
       :desc "Reveal project in Terminal" :nm "T" #'+shell/reveal-project-in-terminal
       :desc "Reveal in VSCode"           :nm "c" #'+shell/reveal-in-vscode
       :desc "Reveal project in VSCode"   :nm "C" #'+shell/reveal-project-in-vscode))
   (:desc "insert" :prefix "i"
     :n "o" #'symbol-overlay-put
     :n "q" #'symbol-overlay-remove-all)
   (:desc "project" :prefix "p"
     :n "e" #'projectile-run-eshell
     :n "*" (+my/prefix-M-x "projectile-"))
   (:desc "toggle" :prefix "t"
     :n "r" #'rjsx-mode
     :n "d" #'toggle-debug-on-error
     :n "D" #'+my/realtime-elisp-doc
     :n "L" #'toggle-truncate-lines
     :n "v" #'visual-line-mode)
   (:desc "jump" :prefix "j"
     :n "j" #'avy-goto-char-timer
     :n "l" #'avy-goto-line
     :n "b" #'avy-pop-mark)
   (:desc "search" :prefix "/"
     :desc "Project"   :nmv "/" #'+ivy/project-search
     :desc "Comments"  :nmv "c" #'counsel-imenu-comments
     :desc "Directory" :nmv "d" #'+ivy/project-search-from-cwd)
   )

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
 (:map +popup-mode-map
   :n "q" #'quit-window)
 (:after lispy
   (:map lispy-mode-map
     :n "M-j" #'lispy-splice
     :i "C-c (" #'lispy-wrap-round
     :i "_" #'special-lispy-different
     :i [remap kill-line] #'lispy-kill
     :i [remap delete-backward-char] #'lispy-delete-backward))
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
     :desc "Reveal in Typora" :n "o" #'+macos/reveal-in-typora
     :desc "Edit" :n "x" (+my/simulate-key "C-c C-s")
     (:desc "Insert" :prefix "i"
       :n "r" #'markdown-table-insert-row
       :n "c" #'markdown-table-insert-column)))
 (:after ivy
   :map ivy-occur-grep-mode-map
   "SPC" nil
   :map ivy-minibuffer-map
   "<tab>" #'ivy-partial-or-done
   "C-<return>" #'ivy-immediate-done
   "C-j" #'ivy-call-and-recenter
   "C-;" #'ivy-avy
   "C-k" #'ivy-kill-line
   "C-v" #'ivy-scroll-up-command
   "A-v" #'ivy-scroll-down-command)
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
     "C-v"   #'company-next-page
     "A-v"   #'company-previous-page
     "<tab>" nil
     "C-i"   #'company-complete-selection))
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
