;;; private/my/+bindings.el -*- lexical-binding: t; -*-

(when IS-MAC (setq mac-command-modifier 'meta
                   mac-option-modifier  'alt))

;; Distinguish C-i from TAB
(when (display-graphic-p)
  (define-key input-decode-map "\C-i" [C-i])
  (map! "<C-i>" #'better-jumper-jump-forward))

(map!
 ;; overrides other minor mode keymaps (just for non-evil)
 (:map override ;; general-override-mode-map
  "M-q"   (if (daemonp) #'delete-frame #'save-buffers-kill-terminal)
  "M-p"   #'+ivy/projectile-find-file
  "M-y"   #'helm-show-kill-ring
  "C-h m" #'describe-mode
  "C-]"   #'yas-expand
  "M-;"   #'+my/insert-semicolon-at-the-end-of-this-line
  "C-M-;" #'+my/delete-semicolon-at-the-end-of-this-line)
 "M-`"   #'other-frame
 "C-M-o" #'other-frame
 ;; fix OS window/frame navigation/manipulation keys
 "M-w" #'delete-window
 "M-W" #'delete-frame
 "M-n" #'+default/new-buffer
 "M-N" #'make-frame
 "C-M-f" #'toggle-frame-fullscreen
 "M-t" #'transpose-words
 :gn "C-t" nil
 ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
 ;; it imposes some other functionality and overhead we don't need)
 "M-z" #'undo
 "M-Z" #'redo
 "M-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
 "M-v" #'yank-with-delete-region
 "M-s" #'evil-write-all
 ;; Buffer-local font scaling
 "M-0" (λ! (text-scale-set 0))
 "M-=" #'text-scale-increase
 "M--" #'text-scale-decrease
 ;; Conventional text-editing keys & motions
 "M-a" #'mark-whole-buffer
 :gni [M-RET]    #'+default/newline-below
 :gni [M-S-RET]  #'+default/newline-above
 :gi  [M-backspace] #'backward-kill-word
 :gi  [M-left]      #'backward-word
 :gi  [M-right]     #'forward-word
 ;; Swiper
 "M-f" #'swiper
 "C-s" #'swiper
 ;; Help
 "C-h h"   nil
 "C-h C-k" #'find-function-on-key
 "C-h C-f" #'find-function-at-point
 "C-h C-v" #'find-variable-at-point
 "<f8>"    #'describe-mode
 ;; Others
 "M-P"    #'+ivy/project-search-specific-files
 "M-e"    #'+ivy/switch-workspace-buffer
 "C-M-\\" #'indent-region-or-buffer
 "M-m"    #'kmacro-call-macro
 "M-/"    #'doom/toggle-comment-region-or-line)

(map!
 ;; Unix text-editing keys & motions
 :gi "C-n" #'next-line
 :gi "C-p" #'previous-line
 :gi "C-b" #'backward-char
 :gi "C-f" #'forward-char
 :gi "C-k" #'kill-line
 :gi "C-d" #'delete-forward-char

 :v "C-r"   #'+my/evil-quick-replace
 :v "DEL" (kbd "\"_d")
 :v "<del>" (kbd "\"_d")
 :v "<backspace>" (kbd "\"_d")
 :nmv "-" (λ! (better-jumper-jump-backward 1))
 :nmv "=" (λ! (better-jumper-jump-forward 1))

 :gnmvi "C-e" #'doom/forward-to-last-non-comment-or-eol
 :gnmvi "C-a" #'doom/backward-to-bol-or-indent
 :gnmvi "M-." #'+lookup/definition
 :nv "ge"  #'counsel-etags-find-tag-at-point
 :nv "gE"  #'counsel-etags-find-tag

 (:prefix "C-x"
  :n "e"  #'pp-eval-last-sexp)
 (:prefix "C-c"
  :desc "Text properties at point" :nmv "f" (λ! (message "%S" (text-properties-at (point))))))

;; leader/localleader is not compatible with :gnvmi
(map! :leader
      :desc "Snails" :nmv "RET" #'snails
      :desc "counsel-M-x" :nmv "SPC" #'counsel-M-x
      :desc "lispyville" :n "L" (+my/prefix-M-x "lispyville ")

      (:prefix-map ("a" . "app")
       "s" #'prodigy
       "b" #'blog-admin-start
       :desc "List process" "p" #'list-processes
       :desc "Kill process" "P" #'counsel-list-processes
       "x" #'align-regexp)
      (:prefix "b"                      ; buffer
       :desc "Switch buffer" "b" #'ivy-switch-buffer
       :desc "Switch workspace buffer" "B" #'+ivy/switch-workspace-buffer
       "h" #'+doom-dashboard/open
       "r" #'revert-buffer-no-confirm
       "R" #'reload-buffer-no-confirm
       "U" #'+my/untabify-buffer)
      (:prefix "c"                      ; code
       :desc "Toggle Comment"         "/" #'doom/toggle-comment-region-or-line
       :desc "Treemacs symbols"       "i" #'lsp-treemacs-symbols
       :desc "Format-all buffer"      "F" #'format-all-buffer
       :desc "LSP organize imports"   "I" #'lsp-organize-imports
       :desc "Treemacs references"    "D" #'lsp-treemacs-references
       :desc "Cspell check buffer"    "c" #'cspell-check-buffer
       :desc "Cspell check directory" "C" #'cspell-check-HEAD)
      (:prefix "TAB"
       :desc "Switch workspace" "TAB" #'+workspace/switch-to)
      (:prefix "f"                      ; file
       :desc "Yank relative filename" "Y" #'+default/yank-relative-buffer-filename
       :desc "Save all" "s" #'evil-write-all
       :desc "Deer"     "j" #'deer)
      (:prefix "g"                      ; git
       "s" nil
       (:after smerge-mode
        :desc "Smerge" "s" smerge-basic-map)
       :desc "Browse file or region" "oo" #'git-link
       :desc "Magit browse commit"   "oc" #'+vc/git-browse-commit
       :desc "Magit wip worktree"    "w"  #'magit-wip-log-worktree
       :desc "M-x magit-*" "*" (+my/prefix-M-x "magit-"))
      (:prefix "h"                      ; help
       "C" #'helpful-command)
      (:prefix-map ("e" . "error")
       :desc "Flymake next error"      "N" #'flymake-goto-next-error
       :desc "Flymake previous error"  "P" #'flymake-goto-prev-error
       :desc "Flymake list errors"     "L" #'flymake-show-diagnostics-buffer
       :desc "Flycheck next error"     "n" #'flycheck-next-error
       :desc "Flycheck previous error" "p" #'flycheck-previous-error
       :desc "Flycheck explain error"  "e" #'flycheck-explain-error-at-point
       :desc "Flycheck list errors"    "l" #'flycheck-list-errors
       :desc "Flycheck verify setup"   "v" #'flycheck-verify-setup)
      (:prefix "o"                      ; open
       :desc "Kill ring"             "k" #'helm-show-kill-ring
       :desc "Imenu list"            "i" #'imenu-list
       :desc "Open link"             "x" #'link-hint-open-link
       :desc "Open link at point"    "X" #'link-hint-open-link-at-point
       :desc "Vterm"                 "s" #'+vterm/toggle
       :desc "Project run Vterm"     "S" #'+vterm/here
       :desc "Toggle eshell popup"   "e" #'+eshell/toggle
       :desc "Project run Eshell"    "E" #'projectile-run-eshell
       :desc "Youdao dictionary"     "y" (if (display-graphic-p) #'youdao-dictionary-search-at-point-tooltip
                                           #'youdao-dictionary-search-at-point)
       :desc "Youdao play voice"     "Y" #'youdao-dictionary-play-voice-at-point
       :desc "Docker open apps"      ";" #'+docker/reveal-in-apps
       (:when IS-MAC
        :desc "Reveal in default program"  "f" #'+macos/open-in-default-program
        :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
        :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
        :desc "Reveal in Terminal"         "t" #'+macos/reveal-in-terminal
        :desc "Reveal project in Terminal" "T" #'+macos/reveal-project-in-terminal
        :desc "Reveal file in Apps"        "," #'+shell/reveal-in-apps
        :desc "Reveal project in Apps"     "." #'+shell/reveal-project-in-apps)
       (:when IS-LINUX
        :desc "Reveal in default program"  "f" #'+shell/open-in-default-program
        :desc "Reveal in Finder"           "o" #'+shell/reveal-in-finder
        :desc "Reveal project in Finder"   "O" #'+shell/reveal-project-in-finder
        :desc "Reveal in Terminal"         "t" #'+shell/reveal-in-terminal
        :desc "Reveal project in Terminal" "T" #'+shell/reveal-project-in-terminal
        :desc "Reveal file in Apps"        "," #'+shell/reveal-in-apps
        :desc "Reveal project in Apps"     "." #'+shell/reveal-project-in-apps))
      (:prefix "i"                      ; insert
       "v" #'add-dir-local-variable
       "o" #'symbol-overlay-put
       "q" #'symbol-overlay-remove-all)
      (:prefix "p"                      ; project
       "*" (+my/prefix-M-x "projectile-")
       :desc "Update projectile list" "u" #'update-projectile-known-projects)
      (:prefix ("d" . "debug")
       :desc "Start dap debugger" "d" #'+my/dap-start
       "b" #'dap-breakpoint-toggle
       "h" #'dap-hydra
       "l" #'dap-ui-locals
       "q" #'dap-disconnect
       "s" #'dap-ui-sessions
       "k" #'dap-delete-session
       "K" #'dap-delete-all-sessions
       "S" #'realgud-short-key-mode)
      (:prefix "t"                      ; toggle
       "c" #'rainbow-mode
       "C" #'centered-window-mode
       "d" #'toggle-debug-on-error
       "D" #'+my/realtime-elisp-doc
       "L" #'toggle-truncate-lines
       "S" #'size-indication-mode
       "I" #'ivy-rich-mode
       "v" #'visual-line-mode)
      (:prefix-map ("j" . "jump")
       "j" #'avy-goto-char-timer
       "l" #'avy-goto-line
       "b" #'avy-pop-mark
       "t" #'yas-describe-tables)
      (:prefix "s"                      ; search
       :desc "Comments"  "c" #'counsel-imenu-comments
       :desc "Jump to bookmark" "m" #'helm-bookmarks
       :desc "Search project"            "p" #'+my/search-project
       :desc "Search project customly"   "P" #'color-rg-customized-search
       :desc "Project (hidden)" "h" #'+ivy/project-search-with-hidden-files))

(map!
 (:map prog-mode-map
  :i "TAB" #'doom/dumb-indent
  :i "<backtab>" #'doom/dumb-dedent)
 (:after ranger
  (:map ranger-normal-mode-map
   "M-1" nil
   "M-2" nil
   "M-3" nil
   "M-4" nil
   "M-5" nil
   "M-6" nil
   "M-7" nil
   "M-8" nil
   "M-9" nil
   "M-0" nil
   "g"   nil
   "q" #'ranger-close-and-kill-inactive-buffers
   "f" #'counsel-find-file
   "F" #'dired-narrow                 ; use `; g` to quit dired-narrow
   "M-g" #'ranger-go
   "yr" #'ranger-copy-relative-path
   "C-TAB" #'ranger-next-tab
   "C-S-TAB" #'ranger-prev-tab
   "U" #'dired-unmark-all-files
   "u" #'dired-unmark
   "(" #'dired-hide-details-mode
   "+" #'dired-create-directory))
 (:after lispy
  (:map lispy-mode-map
   :i "_" #'special-lispy-different
   :i [remap kill-line] #'lispy-kill
   :i [remap delete-backward-char] #'lispy-delete-backward
   :n "M-r"   nil
   :n "M-s"   nil
   :n "M-v"   nil
   :n "M-<left>" #'lispy-forward-barf-sexp
   :n "M-<right>" #'lispy-forward-slurp-sexp
   :n "C-M-<left>" #'lispy-backward-slurp-sexp
   :n "C-M-<right>" #'lispy-backward-barf-sexp))
 (:after lispyville
  (:map lispyville-mode-map
   :n "M-r"   nil
   :n "M-s"   nil
   :n "M-v"   nil
   :n "C-M-r" #'lispy-raise-sexp
   :n "C-M-s" #'lispy-splice
   :n "M-V"   #'lispy-convolute-sexp
   :n "TAB" #'lispyville-prettify))
 (:after elisp-mode
  :map emacs-lisp-mode-map
  :n "gh" #'helpful-at-point)
 (:after lsp-ui
  :map lsp-ui-mode-map
  "C-j" (λ!! #'lsp-ui-doc-mode))
 (:after lsp-ui-peek
  :map lsp-ui-peek-mode-map
  "h" #'lsp-ui-peek--select-prev-file
  "j" #'lsp-ui-peek--select-next
  "k" #'lsp-ui-peek--select-prev
  "l" #'lsp-ui-peek--select-next-file)
 (:after python
  :localleader
  :map python-mode-map
  :desc "Insert breakpoint" "b" #'+python/toggle-breakpoint
  :desc "Copy pdb breakpoint" "B" #'+python/copy-pdb-breakpoint-of-current-line
  (:prefix ("i" . "Import")
   :desc "Remove unused impoorts" "r" #'+python/autoflake-remove-imports
   :desc "Isort buffer"    "s" #'python-isort-autosave-mode
   :desc "Insert copied import" "p" #'+python/insert-temp-import
   :desc "Copy module import " "i" #'+python/yank-module-import)
  (:prefix ("v" . "ENV")
   "c" #'conda-env-activate
   "C" #'conda-env-deactivate
   "w" #'pyvenv-workon
   "v" #'pyvenv-activate
   "V" #'pyvenv-deactivate
   "p" #'pipenv-activate
   "P" #'pipenv-deactivate))
 (:after pyenv-mode
  (:map pyenv-mode-map
   "C-c C-s" nil
   "C-c C-u" nil))
 (:after js2-mode
  (:map js2-mode-map
   :localleader
   :desc "Import js"  "i" #'import-js-import
   :desc "Import all" "f" #'import-js-fix))
 (:after rjsx-mode
  (:map rjsx-mode-map
   :localleader
   :desc "Import js"  "i" #'import-js-import
   :desc "Import all" "f" #'import-js-fix))
 (:after tide
  :map tide-references-mode-map
  "C-k" #'tide-find-previous-reference
  "p"   #'tide-find-previous-reference
  "C-j" #'tide-find-next-reference
  "n"   #'tide-find-next-reference
  "C-l" #'tide-goto-reference)
 (:after org
  (:map org-mode-map
   :localleader
   "z" #'org-add-note
   "L" #'org-toggle-link-display))
 (:after evil-vars
  (:map evil-window-map
   :leader
   (:prefix "w"
    :desc "evil-window-decrease-height" "-" (λ! (evil-window-decrease-height 10))
    :desc "evil-window-increase-height" "+" (λ! (evil-window-increase-height 10))
    :desc "evil-window-decrease-width" "<"  (λ! (evil-window-decrease-width 20))
    :desc "evil-window-increase-width" ">"  (λ! (evil-window-increase-width 20)))))
 (:after iedit
  (:map iedit-mode-occurrence-keymap
   "M-D" nil))
 (:after evil-org
  (:map evil-org-mode-map
   :i "C-d" nil
   :i "C-t" nil
   :i "C-h" nil
   :i "C-k" nil))
 (:after markdown-mode
  (:map evil-markdown-mode-map
   :i "C-d" nil)
  (:map markdown-mode-map
   :desc "Markdown Cycle" :nv [tab] #'markdown-cycle
   :desc "Insert item below" :ni "<C-return>"   (λ! (+org/insert-item-below 1))
   :desc "Insert item above" :ni "<S-C-return>" (λ! (+org/insert-item-above 1))
   (:localleader
    (:when IS-MAC
     :desc "Reveal in Typora" "o" #'+macos/reveal-in-typora)
    (:when IS-LINUX
     :desc "Reveal in Typora" "o" #'+shell/reveal-in-typora)
    :desc "Fix Copy"           "F" #'+my/markdown-copy-fix
    :desc "Insert header line" "-" #'org-table-insert-hline
    :desc "Crete Table from region" "|" #'org-table-create-or-convert-from-region
    :desc "Edit" "x" (+my/simulate-key "C-c C-s")
    (:prefix ("i" . "Insert")
     "r" #'markdown-table-insert-row
     "c" #'markdown-table-insert-column))))
 (:after grep
  :map grep-mode-map
  "SPC" nil)
 (:after wgrep
  :map wgrep-mode-map
  :n "RET" #'ivy-occur-press-and-switch)
 (:after ivy
  :map ivy-occur-grep-mode-map
  "C-d" nil
  "SPC" nil
  "DEL" #'ivy-occur-delete-candidate
  :nv "gr" #'ivy-occur-revert-buffer
  :map ivy-minibuffer-map
  "TAB" #'ivy-partial-or-done
  "<C-return>" #'ivy-immediate-done
  "C-b" nil
  "C-j" #'ivy-call-and-recenter
  "C-k" #'ivy-kill-line
  "C-v" #'ivy-scroll-up-command
  "A-v" #'ivy-scroll-down-command
  "M-v" #'yank)
 (:after minibuffer
  :map minibuffer-local-map
  "C-k" 'kill-line)
 (:after magit-mode
  (:map magit-mode-map
   "M-p" nil
   "M-n" nil
   "M-w" nil))
 (:after magit-diff
  (:map magit-diff-mode-map            ; for magit diff/rev mode
   "C-o" #'magit-diff-visit-file-other-window))
 (:after magit-blame
  (:map magit-blame-mode-map
   :n "o" #'magit-blame--git-link-commit))
 (:after git-rebase
  (:map git-rebase-mode-map
   "SPC" nil))
 (:after evil-vars
  :map evil-ex-completion-map
  "C-b" nil
  "C-k" #'kill-line
  "C-d" #'delete-forward-char)
 (:after evil-collection-info
  :map Info-mode-map
  "/" #'Info-search
  "?" #'Info-search-backward)
 (:after company
  (:map company-active-map
   "TAB"       nil
   [tab]       nil
   [backtab]   nil
   "C-j"   #'company-show-location
   "C-i"   #'company-complete-selection))
 (:after adoc-mode
  (:map adoc-mode-map
   :localleader
   :desc "adoc preview" "p" (λ! (browse-url buffer-file-name))))
 (:after vterm
  (:map vterm-mode-map
   "M-e" nil
   "M-w" #'+workspace/close-window-or-workspace))
 (:after color-rg
  (:map color-rg-mode-map
   "j" nil "k" nil "l" nil "h" nil
   "C-k" #'color-rg-jump-prev-keyword
   "C-j" #'color-rg-jump-next-keyword
   :nv "gr" #'color-rg-rerun)
  (:map color-rg-mode-edit-map
   "C-c C-k" #'color-rg-quit))
 (:after term
  (:map term-raw-map
   :i "M-v" #'term-paste)))
