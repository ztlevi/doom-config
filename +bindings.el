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

      (:prefix "a"                      ; app
        :n "s" #'prodigy
        :n "b" #'blog-admin-start
        :n "p" #'list-processes
        :nmv "x" #'align-regexp)
      (:prefix "b"                      ; buffer
        :desc "Last buffer" :nmv "l" #'evil-switch-to-windows-last-buffer
        :nmv  "b" #'ivy-switch-buffer
        :nm   "r" #'revert-buffer-no-confirm
        :nm   "m" #'view-echo-area-messages
        :nm   "U" #'+my/untabify-buffer
        :nmv  "k" #'kill-current-buffer)
      (:prefix "c"                      ; code
        :desc "Cspell check buffer" :n "c" #'cspell-check-buffer
        :desc "Cspell check directory" :n "C" #'cspell-check-directory)
      (:prefix [tab]                    ;workspace
        :desc "Switch workspace" :n [tab] #'+workspace/switch-to
        :desc "Display tab bar"  :n "."   #'+workspace/display)
      (:prefix "f"                      ; file
        :desc "find file" :n "f" #'counsel-find-file
        :desc "deer"      :nm "j" #'deer)
      (:prefix "g"                      ; git
        :desc "Magit status" :nm "g" #'magit-status
        :desc "Magit browse commit" :n "O" #'+vc/git-browse-commit
        :desc "M-x magit-*" :n "*" (+my/prefix-M-x "magit-"))
      (:prefix "h"                      ; help
        :n "C" #'helpful-command)
      (:prefix "e"                      ; error
        :desc "flycheck-next-error"     :n "n" #'flycheck-next-error
        :desc "flycheck-previous-error" :n "p" #'flycheck-previous-error
        :desc "flycheck-list-errors"    :n "l" #'flycheck-list-errors
        :desc "flycheck-verify-setup"   :n "v" #'flycheck-verify-setup)
      (:prefix "o"                      ; open
        :desc "Kill ring"             :n    "k" #'helm-show-kill-ring
        :desc "Open link"             :n    "x" #'link-hint-open-link
        :desc "Open link at point"    :n    "X" #'link-hint-open-link-at-point
        :desc "Ansi-Term"             :nm   "s" #'+term/open-popup
        :desc "Project run Ansi-Term" :nm   "S" #'+term/open-popup-in-project
        :desc "Eshell popup"          :nm   "e" #'+eshell/open-popup
        :desc "Project run Eshell"    :nm   "E" #'projectile-run-eshell
        :desc "Ibuffer"               :nm   "I" #'ibuffer
        :desc "Youdao dictionary"     :n    "y" #'youdao-dictionary-search-at-point-tooltip
        :desc "Youdao play voice"     :n    "Y" #'youdao-dictionary-play-voice-at-point
        :desc "Debugger start"        :n    "d" #'+debugger:start
        (:when IS-MAC
          :desc "Reveal in default program"  :nm "f" #'+macos/open-in-default-program
          :desc "Reveal in Finder"           :nm "o" #'+macos/reveal-in-finder
          :desc "Reveal project in Finder"   :nm "O" #'+macos/reveal-project-in-finder
          :desc "Reveal in Terminal"         :nm "t" #'+macos/reveal-in-terminal
          :desc "Reveal project in Terminal" :nm "T" #'+macos/reveal-project-in-terminal
          :desc "Reveal file in Apps"        :nm "," #'+macos/reveal-in-apps
          :desc "Reveal project in Apps"     :nm "." #'+macos/reveal-project-in-apps)
        (:when IS-LINUX
          :desc "Reveal in default program"  :nm "f" #'+linux/open-in-default-program
          :desc "Reveal in Finder"           :nm "o" #'+linux/reveal-in-finder
          :desc "Reveal project in Finder"   :nm "O" #'+linux/reveal-project-in-finder
          :desc "Reveal in Terminal"         :nm "t" #'+linux/reveal-in-terminal
          :desc "Reveal project in Terminal" :nm "T" #'+linux/reveal-project-in-terminal
          :desc "Reveal file in Apps"        :nm "," #'+linux/reveal-in-apps
          :desc "Reveal project in Apps"     :nm "." #'+linux/reveal-project-in-apps))
      (:prefix "i"                      ; insert
        :n "v" #'add-dir-local-variable
        :n "o" #'symbol-overlay-put
        :n "q" #'symbol-overlay-remove-all)
      (:prefix "p"                      ; project
        "*" (+my/prefix-M-x "projectile-"))
      (:prefix "t"                      ; toggle
        :n "c" #'centered-window-mode
        :n "r" #'rjsx-mode
        :n "d" #'toggle-debug-on-error
        :n "D" #'+my/realtime-elisp-doc
        :n "L" #'toggle-truncate-lines
        :n "I" #'ivy-rich-mode
        :n "v" #'visual-line-mode)
      (:prefix "j"                      ; jump
        :n "j" #'avy-goto-char-timer
        :n "l" #'avy-goto-line
        :n "b" #'avy-pop-mark)
      (:prefix "s"                     ; snippet
        :n "t" #'yas/describe-tables)
      (:prefix "/"                      ; search
        :desc "Project"   :nmv "/" #'+ivy/project-search
        :desc "Project (hidden)" :nmv "h" #'+ivy/project-search-with-hidden-files
        :desc "Comments"  :nmv "c" #'counsel-imenu-comments
        :desc "Directory" :nmv "d" #'+ivy/project-search-from-cwd))

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
   :desc "Eval last expression" :n "e" (λ! (save-excursion (forward-sexp) (eval-last-sexp nil))))
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
   :desc "Import at point" :n "i" 'importmagic-fix-symbol-at-point
   :desc "Import all" :n "I" 'importmagic-fix-imports
   (:prefix "v"
     :n "c" #'conda-env-activate
     :n "C" #'conda-env-deactivate
     :n "w" #'pyvenv-workon
     :n "v" #'pyvenv-activate
     :n "V" #'pyvenv-deactivate
     :n "p" #'pipenv-activate
     :n "P" #'pipenv-deactivate))
 (:after pyenv-mode
   (:map pyenv-mode-map
     "C-c C-s" nil
     "C-c C-u" nil))
 (:after js2-mode
   (:map js2-mode-map
     :localleader
     :desc "Import js"  :n "i" 'import-js-import
     :desc "Import all" :n "f" 'import-js-fix))
 (:after rjsx-mode
   (:map rjsx-mode-map
     :localleader
     :desc "Import js"  :n "i" 'import-js-import
     :desc "Import all" :n "f" 'import-js-fix))
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
 (:after org
   (:map org-mode-map
     :localleader
     :n "s" #'org-schedule
     :n "L" #'org-toggle-link-display))
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
       :desc "Reveal in Typora" :n "o" #'+macos/reveal-in-typora)
     (:when IS-LINUX
       :desc "Reveal in Typora" :n "o" #'+linux/reveal-in-typora)
     :desc "Edit" :n "x" (+my/simulate-key "C-c C-s")
     (:desc "Insert" :prefix "i"
       :n "r" #'markdown-table-insert-row
       :n "c" #'markdown-table-insert-column)))
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
     "C-j"   #'company-show-location
     "C-i"   #'company-complete-selection))
 (:after term
   (:map term-raw-map
     :i "M-v" #'term-paste)))

(cond (IS-MAC
       (setq mac-command-modifier 'meta
             mac-option-modifier  'alt)))

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
