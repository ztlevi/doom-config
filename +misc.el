;;; ~/.doom.d/+misc.el -*- lexical-binding: t; -*-

;; Use chrome to browse
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      (cond (IS-MAC "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
            ((executable-find "google-chrome-stable") "google-chrome-stable")
            ((executable-find "/opt/google/chrome/chrome") "/opt/google/chrome/chrome")
            ((executable-find "google-chrome") "google-chrome")))

;; Set personal ispell dictionary file
(when (file-exists-p (expand-file-name "~/.aspell.en.pws"))
  (setq ispell-personal-dictionary (expand-file-name "~/.aspell.en.pws")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INPUT METHOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! rime
  :defer t
  :custom
  (rime-user-data-dir (expand-file-name "~/.config/fcitx/rime"))
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
   '(rime-predicate-evil-mode-p
     rime-predicate-after-alphabet-char-p
     rime-predicate-prog-in-code-p))
  (rime-inline-ascii-trigger 'shift-l)
  :bind
  ;; C-\ to toggle-input-method
  ;; C-` to toggle
  ;; , and . to page up and down
  (:map rime-mode-map
   ;; open rime menu
   ("C-`" . 'rime-send-keybinding))
  (:map rime-active-mode-map
   ("C-j" . 'rime-inline-ascii))
  :config
  (when IS-MAC
    (setq rime-librime-root "~/.emacs.d/librime/dist"))
  ;; Set Nixos env
  (when (and IS-LINUX (executable-find "nix"))
    (setq rime-emacs-module-header-root (concat (shell-command-to-string "nix eval --raw '(let pkgs = import <nixpkgs> {}; in with pkgs; lib.getLib emacs)'") "/include")
          rime-librime-root (shell-command-to-string "nix eval --raw '(let pkgs = import <nixpkgs> {}; in with pkgs; lib.getLib librime)'")
          rime-share-data-dir (concat (shell-command-to-string "nix eval --raw '(let pkgs = import <nixpkgs> {}; in with pkgs; lib.getLib brise)'") "/share/rime-data"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SSH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! ssh-deploy
  (setq ssh-deploy-automatically-detect-remote-changes 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-cross-lines t
      evil-split-window-below t
      evil-vsplit-window-right t
      evil-emacs-state-cursor `(box ,(doom-color 'violet)))


(after! evil
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))


(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer)
  (push 'prodigy-mode evil-snipe-disabled-modes))


(use-package! tmux-pane
  :unless (display-graphic-p)
  :defer t
  :config
  (defvar my-tmux-pane-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-t k")
        (lambda () (interactive) (tmux-pane--windmove "up"  "-U")))
      (define-key map (kbd "C-t j")
        (lambda () (interactive) (tmux-pane--windmove "down"  "-D")))
      (define-key map (kbd "C-t h")
        (lambda () (interactive) (tmux-pane--windmove "left" "-L")))
      (define-key map (kbd "C-t l")
        (lambda () (interactive) (tmux-pane--windmove "right" "-R")))
      (define-key map (kbd "C-t C-k")
        (lambda () (interactive) (tmux-pane--windmove "up"  "-U")))
      (define-key map (kbd "C-t C-j")
        (lambda () (interactive) (tmux-pane--windmove "down"  "-D")))
      (define-key map (kbd "C-t C-h")
        (lambda () (interactive) (tmux-pane--windmove "left" "-L")))
      (define-key map (kbd "C-t C-l")
        (lambda () (interactive) (tmux-pane--windmove "right" "-R")))
      map))

  (define-minor-mode my-tmux-pane-mode
    "Seamlessly navigate between tmux pane and emacs window"
    :init-value nil
    :global t
    :keymap 'my-tmux-pane-mode-map)

  :hook (after-init . my-tmux-pane-mode))


(use-package! imenu-list
  :defer t
  :config
  (set-popup-rules! '(("^\\*Ilist\\*" :side right :size 40 :select t))))


(after! nav-flash
  ;; (defun nav-flash-show (&optional pos end-pos face delay)
  ;; ...
  ;; (let ((inhibit-point-motion-hooks t))
  ;; (goto-char pos)
  ;; (beginning-of-visual-line) ; work around args-out-of-range error when the target file is not opened
  (defun +advice/nav-flash-show (orig-fn &rest args)
    (ignore-errors (apply orig-fn args)))
  (advice-add 'nav-flash-show :around #'+advice/nav-flash-show))

;; Use ) key to toggle it
(after! dired
  ;; Rust version ls
  (when-let (exa (executable-find "exa"))
    (setq insert-directory-program exa)
    (setq dired-listing-switches (string-join (list "-ahl" "--group-directories-first") " ")))
  )

(after! (:and ranger dired)
  (setq ranger-hide-cursor t
        ranger-show-hidden 'format
        ranger-deer-show-details nil)

  (defun ranger-copy-relative-path ()
    "Copy the current file path relative to `default-directory path."
    (interactive)
    (let ((current-prefix-arg 1))
      (call-interactively 'dired-copy-filename-as-kill)))

  (defun ranger-close-and-kill-inactive-buffers ()
    "ranger close current buffer and kill inactive ranger buffers"
    (interactive)
    (ranger-close)
    (ranger-kill-buffers-without-window))
  ;; do not kill buffer if exists in windows
  (defun ranger-disable ()
    "Interactively disable ranger-mode."
    (interactive)
    (ranger-revert)))


(after! dash-docs
  (setq dash-docs-use-workaround-for-emacs-bug nil)
  (setq dash-docs-browser-func 'browse-url-generic))


(use-package! highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (defun +indent-guides-init-faces-h ()
    (when (display-graphic-p)
      (highlight-indent-guides-auto-set-faces))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IVY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! (:and ivy ivy-prescient)
  (setq ivy-prescient-retain-classic-highlighting t))


(after! ivy-posframe
  ;; Lower internal-border-width on MacOS
  (when IS-MAC
    (setq ivy-posframe-border-width 5))

  ;; Use minibuffer to display ivy functions
  (dolist (fn '(+ivy/switch-workspace-buffer
                ivy-switch-buffer))
    (setf (alist-get fn ivy-posframe-display-functions-alist) #'ivy-display-function-fallback)))

(after! ivy-rich
  (plist-put! ivy-rich-display-transformers-list
              'ivy-switch-buffer
              '(:columns
                ((ivy-switch-buffer-transformer (:width 60))
                 (ivy-rich-switch-buffer-size (:width 7))
                 (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                 (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                 (ivy-rich-switch-buffer-project (:width 15 :face success))
                 (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                :predicate
                (lambda (cand) (get-buffer cand)))))


(after! counsel
  ;; counsel-rg-base-command is configurable
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable))


(use-package! counsel-etags
  :defer t
  :init
  (add-hook! 'prog-mode-hook
    (add-hook! 'after-save-hook
               :append :local 'counsel-etags-virtual-update-tags))
  :config
  (setq counsel-etags-update-interval 60))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QUICKRUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! quickrun
  ;; quickrun--language-alist
  (when IS-LINUX
    (quickrun-set-default "c++" "c++/g++")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECTILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! projectile
  (setq compilation-read-command nil)   ; no prompt in projectile-compile-project
  ;; . -> Build
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake %s"
                                    :compile "cmake --build Debug"
                                    :test "ctest")

  ;; set projectile-known-projects after magit
  (after! magit
    (update-projectile-known-projects))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! git-link
  (setq git-link-open-in-browser t)

  (add-to-list 'git-link-remote-alist
               '("git\\.bst\\.ai" git-link-github-http))
  (add-to-list 'git-link-commit-remote-alist
               '("git\\.bst\\.ai" git-link-commit-github-http))
  (add-to-list 'git-link-remote-alist
               '("rnd-github-usa-g\\.huawei\\.com" git-link-github-http))
  (add-to-list 'git-link-commit-remote-alist
               '("rnd-github-usa-g\\.huawei\\.com" git-link-commit-github-http))

  ;; OVERRIDE
  (advice-add #'git-link--select-remote :override #'git-link--read-remote)
  )


(after! magit
  (setq magit-repository-directories '(("~/dev" . 2))
        magit-save-repository-buffers nil
        git-commit-style-convention-checks nil
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  (magit-wip-after-apply-mode t)
  (magit-wip-before-change-mode t))


(after! forge
  (push '("github.argo.ai" "github.argo.ai/api/v3"
          "github.argo.ai" forge-github-repository)
        forge-alist)

  ;; TEMP
  ;; (setq ghub-use-workaround-for-emacs-bug 'force)

  (defvar forge-show-all-issues-and-pullreqs t
    "If nil, only show issues and pullreqs assigned to me.")

  (defun +my/forge-toggle-all-issues-and-pullreqs ()
    (interactive)
    (setq forge-insert-default '(forge-insert-pullreqs forge-insert-issues))
    (setq forge-insert-assigned '(forge-insert-assigned-pullreqs forge-insert-assigned-issues))
    (if forge-show-all-issues-and-pullreqs
        (progn
          (setq forge-show-all-issues-and-pullreqs nil)
          (remove-hook! 'magit-status-sections-hook #'forge-insert-issues nil t)
          (remove-hook! 'magit-status-sections-hook #'forge-insert-pullreqs nil t)
          (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-pullreqs nil t)
          (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-assigned-issues nil t))
      (progn
        (setq forge-show-all-issues-and-pullreqs t)
        (remove-hook! 'magit-status-sections-hook #'forge-insert-assigned-issues nil t)
        (remove-hook! 'magit-status-sections-hook #'forge-insert-assigned-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-pullreqs nil t)
        (magit-add-section-hook 'magit-status-sections-hook 'forge-insert-issues nil t)))

    ;; refresh magit-status buffer
    (magit-refresh))

  ;; Only show issues and pullreqs assigned to me
  (+my/forge-toggle-all-issues-and-pullreqs)
  )


(after! browse-at-remote
  (add-to-list 'browse-at-remote-remote-type-domains '("github.argo.ai" . "github"))
  (add-to-list 'browse-at-remote-remote-type-domains '("git.bst.ai" . "gitlab")))


(after! magit-todos
  (setq magit-todos-exclude-globs '("third-party/*" "third_party/*")))


;; magit-todos uses hl-todo-keywords
(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("HACK"  . ,(face-foreground 'warning))
          ("TEMP"  . ,(face-foreground 'warning))
          ("DONE"  . ,(face-foreground 'success))
          ("NOTE"  . ,(face-foreground 'success))
          ("DONT"  . ,(face-foreground 'error))
          ("DEBUG"  . ,(face-foreground 'error))
          ("FAIL"  . ,(face-foreground 'error))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("XXXX"  . ,(face-foreground 'error)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATOMIC CHROME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! atomic-chrome
  :defer 3
  :preface
  (defun +my/atomic-chrome-server-running-p ()
    (cond ((executable-find "lsof")
           (zerop (call-process "lsof" nil nil nil "-i" ":64292")))
          ((executable-find "netstat")  ; Windows
           (zerop (call-process-shell-command "netstat -aon | grep 64292")))))
  :hook
  (atomic-chrome-edit-mode . +my/atomic-chrome-mode-setup)
  (atomic-chrome-edit-done . +my/window-focus-google-chrome)
  :config
  (progn
    (setq atomic-chrome-buffer-open-style 'full) ;; or frame, split
    (setq atomic-chrome-url-major-mode-alist
          '(("github\\.com"        . gfm-mode)
            ("emacs-china\\.org"   . gfm-mode)
            ("stackexchange\\.com" . gfm-mode)
            ("stackoverflow\\.com" . gfm-mode)
            ("discordapp\\.com"    . gfm-mode)
            ("coderpad\\.io"       . c++-mode)
            ;; jupyter notebook
            ("localhost\\:8888"    . python-mode)
            ("lintcode\\.com"      . python-mode)
            ("leetcode\\.com"      . python-mode)))

    (defun +my/atomic-chrome-mode-setup ()
      (setq header-line-format
            (substitute-command-keys
             "Edit Chrome text area.  Finish \
`\\[atomic-chrome-close-current-buffer]'.")))

    (if (+my/atomic-chrome-server-running-p)
        (message "Can't start atomic-chrome server, because port 64292 is already used")
      (atomic-chrome-start-server))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRODIGY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! prodigy
  (set-evil-initial-state!
    '(prodigy-mode)
    'normal)

  (prodigy-define-tag
    :name 'jekyll
    :env '(("LANG" "en_US.UTF-8")
           ("LC_ALL" "en_US.UTF-8")))
  ;; define service
  (prodigy-define-service
    :name "ML Gitbook Publish"
    :command "npm"
    :args '("run" "docs:publish")
    :cwd "~/dev/Machine_Learning_Questions"
    :tags '(npm gitbook)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "ML Gitbook Start"
    :command "npm"
    :args '("start")
    :cwd "~/dev/Machine_Learning_Questions"
    :tags '(npm gitbook)
    :init (lambda () (browse-url "http://localhost:4000"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Blog Server"
    :command "hexo"
    :args '("server" "-p" "4000")
    :cwd blog-admin-backend-path
    :tags '(hexo server)
    :init (lambda () (browse-url "http://localhost:4000"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Blog Deploy"
    :command "hexo"
    :args '("deploy" "--generate")
    :cwd blog-admin-backend-path
    :tags '(hexo deploy)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (defun refresh-chrome-current-tab (beg end length-before)
    (call-interactively '+my/browser-refresh--chrome-applescript))
  ;; add watch for prodigy-view-mode buffer change event
  (add-hook 'prodigy-view-mode-hook
            #'(lambda() (set (make-local-variable 'after-change-functions) #'refresh-chrome-current-tab))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-formatter! 'shfmt "shfmt -i=2")

(after! eshell
  ;; eshell-mode imenu index
  (add-hook! 'eshell-mode-hook (setq-local imenu-generic-expression '(("Prompt" " λ \\(.*\\)" 1))))

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


(after! term
  ;; term-mode imenu index
  (add-hook! 'term-mode-hook (setq-local imenu-generic-expression '(("Prompt" "➜\\(.*\\)" 1)))))
