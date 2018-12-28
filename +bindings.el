;;; private/my/+bindings.el -*- lexical-binding: t; -*-

(define-key! 'global
  [remap swiper] #'swiper
  [remap forward-word] #'forward-char
  [remap backward-word] #'backward-char)

(map!
 "C-M-\\" #'indent-region-or-buffer
 "C-h h" nil
 "C-h C-k" #'find-function-on-key
 "C-h C-f" #'find-function-at-point
 "C-h C-v" #'find-variable-at-point
 "<f8>"    #'describe-mode

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

 :v "C-r"   #'+my/evil-quick-replace
 :v "<del>" (kbd "\"_d")
 :v "<backspace>" (kbd "\"_d")

 (:when (featurep! :feature workspaces)
   :nmvi "M-t" #'+workspace/new
   :nmvi "M-1" (λ! (+workspace/switch-to 0))
   :nmvi "M-2" (λ! (+workspace/switch-to 1))
   :nmvi "M-3" (λ! (+workspace/switch-to 2))
   :nmvi "M-4" (λ! (+workspace/switch-to 3))
   :nmvi "M-5" (λ! (+workspace/switch-to 4))
   :nmvi "M-6" (λ! (+workspace/switch-to 5))
   :nmvi "M-7" (λ! (+workspace/switch-to 6))
   :nmvi "M-8" (λ! (+workspace/switch-to 7))
   :nmvi "M-9" (λ! (+workspace/switch-to 8))
   )

 (:prefix "C-x"
   :n "e"  #'pp-eval-last-sexp)
 (:prefix "C-c"
   :ni "/" #'company-files
   :desc "Text properties at point" :nmv "f" (λ! (message "%S" (text-properties-at (point))))))

(map! :leader
      :desc "counsel-M-x" :nmv "SPC" #'counsel-M-x
      :desc "lispyville" :n "L" (+my/prefix-M-x "lispyville ")

      (:prefix ("a" . "app")
        "s" #'prodigy
        "b" #'blog-admin-start
        "p" #'list-processes
        "x" #'align-regexp)
      (:prefix "b"                      ; buffer
        :desc "Last buffer" "l" #'evil-switch-to-windows-last-buffer
        "b" #'ivy-switch-buffer
        "r" #'revert-buffer-no-confirm
        "m" #'view-echo-area-messages
        "U" #'+my/untabify-buffer
        "k" #'kill-current-buffer)
      (:prefix "c"                      ; code
        :desc "Cspell check buffer"    "c" #'cspell-check-buffer
        :desc "Cspell check directory" "C" #'cspell-check-directory)
      (:prefix [tab]
        :desc "Switch workspace" [tab] #'+workspace/switch-to
        :desc "Display tab bar"  "."   #'+workspace/display)
      (:prefix "f"                      ; file
        :desc "find file" "f" #'counsel-find-file
        :desc "deer"      "j" #'deer)
      (:prefix "g"                      ; git
        :desc "Magit status" "g" #'magit-status
        :desc "Magit browse commit" "O" #'+vc/git-browse-commit
        :desc "M-x magit-*" "*" (+my/prefix-M-x "magit-"))
      (:prefix "h"                      ; help
        "C" #'helpful-command)
      (:prefix ("e" . "error")
        :desc "flycheck-next-error"     "n" #'flycheck-next-error
        :desc "flycheck-previous-error" "p" #'flycheck-previous-error
        :desc "flycheck-list-errors"    "l" #'flycheck-list-errors
        :desc "flycheck-verify-setup"   "v" #'flycheck-verify-setup)
      (:prefix "o"                      ; open
        :desc "Kill ring"             "k" #'helm-show-kill-ring
        :desc "Open link"             "x" #'link-hint-open-link
        :desc "Open link at point"    "X" #'link-hint-open-link-at-point
        :desc "Ansi-Term"             "s" #'+term/open-popup
        :desc "Project run Ansi-Term" "S" #'+term/open-popup-in-project
        :desc "Eshell popup"          "e" #'+eshell/open-popup
        :desc "Project run Eshell"    "E" #'projectile-run-eshell
        :desc "Ibuffer"               "I" #'ibuffer
        :desc "Youdao dictionary"     "y" #'youdao-dictionary-search-at-point-tooltip
        :desc "Youdao play voice"     "Y" #'youdao-dictionary-play-voice-at-point
        :desc "Debugger start"        "d" #'+debugger:start
        (:when IS-MAC
          :desc "Reveal in default program"  "f" #'+macos/open-in-default-program
          :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
          :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
          :desc "Reveal in Terminal"         "t" #'+macos/reveal-in-terminal
          :desc "Reveal project in Terminal" "T" #'+macos/reveal-project-in-terminal
          :desc "Reveal file in Apps"        "," #'+macos/reveal-in-apps
          :desc "Reveal project in Apps"     "." #'+macos/reveal-project-in-apps)
        (:when IS-LINUX
          :desc "Reveal in default program"  "f" #'+linux/open-in-default-program
          :desc "Reveal in Finder"           "o" #'+linux/reveal-in-finder
          :desc "Reveal project in Finder"   "O" #'+linux/reveal-project-in-finder
          :desc "Reveal in Terminal"         "t" #'+linux/reveal-in-terminal
          :desc "Reveal project in Terminal" "T" #'+linux/reveal-project-in-terminal
          :desc "Reveal file in Apps"        "," #'+linux/reveal-in-apps
          :desc "Reveal project in Apps"     "." #'+linux/reveal-project-in-apps))
      (:prefix "i"                      ; insert
        "v" #'add-dir-local-variable
        "o" #'symbol-overlay-put
        "q" #'symbol-overlay-remove-all)
      (:prefix "p"                      ; project
        "*" (+my/prefix-M-x "projectile-"))
      (:prefix "t"                      ; toggle
        "c" #'centered-window-mode
        "r" #'rjsx-mode
        "d" #'toggle-debug-on-error
        "D" #'+my/realtime-elisp-doc
        "L" #'toggle-truncate-lines
        "I" #'ivy-rich-mode
        "v" #'visual-line-mode)
      (:prefix ("j" . "jump")
        "j" #'avy-goto-char-timer
        "l" #'avy-goto-line
        "b" #'avy-pop-mark)
      (:prefix "s"                      ; snippet
        "t" #'yas/describe-tables)
      (:prefix "/"                      ; search
        :desc "Project"   "/" #'+ivy/project-search
        :desc "Project (hidden)" "h" #'+ivy/project-search-with-hidden-files
        :desc "Comments"  "c" #'counsel-imenu-comments
        :desc "Directory" "d" #'+ivy/project-search-from-cwd))

(map!
 (:map +popup-mode-map
   :n "q" #'quit-window)
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
     "g" nil
     "q" #'ranger-close-and-kill-inactive-buffers
     "f" #'counsel-find-file
     "M-g" #'ranger-go
     "C-<tab>" #'ranger-next-tab
     "C-S-<tab>" #'ranger-prev-tab
     "U" #'dired-unmark-all-files
     "u" #'dired-unmark
     "(" #'dired-hide-details-mode
     "+" #'dired-create-directory))
 (:after lispy
   (:map lispy-mode-map
     :i "_" #'special-lispy-different
     :i [remap kill-line] #'lispy-kill
     :i [remap delete-backward-char] #'lispy-delete-backward
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
     :n "<tab>" #'lispyville-prettify))
 (:after elisp-mode
   :map emacs-lisp-mode-map
   :n "gh" #'helpful-at-point
   :localleader
   :desc "Eval last expression" "e" (λ! (save-excursion (forward-sexp) (eval-last-sexp nil))))
 (:after lsp-ui
   :map lsp-ui-mode-map
   "C-j" #'toggle-lsp-ui-doc)
 (:after lsp-ui-peek
   :map lsp-ui-peek-mode-map
   "h" #'lsp-ui-peek--select-prev-file
   "j" #'lsp-ui-peek--select-next
   "k" #'lsp-ui-peek--select-prev
   "l" #'lsp-ui-peek--select-next-file)
 (:after python
   :localleader
   :map python-mode-map
   :desc "Import at point" "i" #'importmagic-fix-symbol-at-point
   :desc "Import all"      "I" #'importmagic-fix-imports
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
     :desc "Import js"  "i" 'import-js-import
     :desc "Import all" "f" 'import-js-fix))
 (:after rjsx-mode
   (:map rjsx-mode-map
     :localleader
     :desc "Import js"  "i" 'import-js-import
     :desc "Import all" "f" 'import-js-fix))
 (:after tide
   :map tide-references-mode-map
   "C-k" 'tide-find-previous-reference
   "p" 'tide-find-previous-reference
   "C-j" 'tide-find-next-reference
   "n" 'tide-find-next-reference
   "C-l" 'tide-goto-reference)
 (:after org
   (:map org-mode-map
     :localleader
     "s" #'org-schedule
     (:prefix ("a" . "Archive")
       "a" #'org-archive-subtree-default
       "s" #'org-archive-subtree)
     "L" #'org-toggle-link-display))
 (:after evil-org
   (:map evil-org-mode-map
     :i "C-d" nil
     :i "C-t" nil
     :i "C-h" nil
     :i "C-k" nil))
 (:after markdown-mode
   (:map markdown-mode-map
     :ni [M-return]   (λ! (+org/insert-item 'below))
     :ni [S-M-return] (λ! (+org/insert-item 'above))
     :localleader
     (:when IS-MAC
       :desc "Reveal in Typora" "o" #'+macos/reveal-in-typora)
     (:when IS-LINUX
       :desc "Reveal in Typora" "o" #'+linux/reveal-in-typora)
     :desc "Edit" "x" (+my/simulate-key "C-c C-s")
     (:prefix ("i" . "Insert")
       "r" #'markdown-table-insert-row
       "c" #'markdown-table-insert-column)))
 (:after grep
   :map grep-mode-map
   "SPC" nil)
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
   "s-v" #'ivy-scroll-down-command)
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
     "s-v"   #'company-previous-page
     "<tab>" nil
     "C-j"   #'company-show-location
     "C-i"   #'company-complete-selection))
 (:after term
   (:map term-raw-map
     :i "M-v" #'term-paste)))

(cond (IS-MAC
       (setq mac-command-modifier 'meta
             mac-option-modifier  'super)))

(define-key!
  ;; Buffer-local font scaling
  "M-+" (λ! (text-scale-set 0))
  "M-=" #'text-scale-increase
  "M--" #'text-scale-decrease
  ;; Fix frame-switching on MacOS
  "M-`" #'other-frame
  ;; Simple window/frame navigation/manipulation
  "M-w" #'delete-window
  "M-W" #'delete-frame
  "M-n" #'+default/new-buffer
  "M-N" #'make-frame
  ;; Textmate-esque bindings
  "M-a" #'mark-whole-buffer
  "M-b" #'+default/compile
  "M-f" #'swiper
  "M-q" (if (daemonp) #'delete-frame #'evil-quit-all)
  ;; Restore OS undo, save, copy, & paste keys (without cua-mode, because
  ;; it imposes some other functionality and overhead we don't need)
  "M-z" #'undo
  "M-s" #'save-buffer
  "M-c" (if (featurep 'evil) 'evil-yank 'copy-region-as-kill)
  "M-v" #'yank
  ;; textmate-esque newline insertion
  [M-return]    #'evil-open-below
  [S-M-return]  #'evil-open-above
  ;; textmate-esque deletion
  [M-backspace] #'doom/backward-kill-to-bol-and-indent)
