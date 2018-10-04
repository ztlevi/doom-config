;;; ~/.doom.d/+misc.el -*- lexical-binding: t; -*-

;; ////////////////////////// EVIL //////////////////////////
(setq evil-cross-lines t)

;; ////////////////////// YASNIPPETS ///////////////////////
(map!
 (:leader
   (:desc "Yasnippt" :prefix "y"
     :desc "Reload snippets"        :n "r" #'yas/reload-all
     :desc "Describe tables"        :n "d" #'yas/describe-tables
     :desc "New snippet"            :n  "n" #'yas-new-snippet
     :desc "Insert snippet"         :nv "i" #'yas-insert-snippet
     :desc "Find snippet for mode"  :n  "s" #'yas-visit-snippet-file
     :desc "Find snippet"           :n  "S" #'+default/find-in-snippets
     :)))

;; ///////////////////////// IVY ////////////////////////////
(after! ivy
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((counse-rg . ivy--regex-plus)
                                (counsel-grep . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (t . ivy--regex-ignore-order)))

  (push '(+ivy/switch-workspace-buffer) ivy-display-functions-alist)
  (push '(ivy-switch-buffer) ivy-display-functions-alist)
  )

(map!
 (:leader
   (:prefix "b"
     :desc "Last buffer" :n "l" #'evil-switch-to-windows-last-buffer
     :n "b" #'ivy-switch-buffer)
   (:prefix "r"
     :n "i" #'ivy-resume))
 (:after ivy
   :map ivy-minibuffer-map
   "C-j" #'ivy-call-and-recenter
   "C-;" #'ivy-avy
   "C-b" #'backward-char
   "C-f" #'forward-char
   "C-k" #'ivy-kill-line
   "C-v" #'ivy-scroll-up-command
   "M-v" #'ivy-scroll-down-command)
 )

(after! counsel
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable
        counsel-fzf-cmd "fzf -f %s"
        counsel-git-cmd "rg --files"
        ;; Add smart-casing and compressed archive searching (-zS) to default
        ;; command arguments:
        counsel-grep-base-command
        "rg -zS -M 120 --no-heading --line-number --color never '%s' %s"
        counsel-rg-base-command
        "rg -zS -M 120 --no-heading --line-number --color never %s ."))

;; ///////////////////////// PROJECTILE ///////////////////
(after! projectile
  (setq projectile-require-project-root t)
  (setq compilation-read-command nil)  ; no prompt in projectile-compile-project
  ;; . -> Build
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake %s"
                                    :compile "cmake --build Debug"
                                    :test "ctest")
  )

(after! counsel-projectile
  (ivy-add-actions
   'counsel-projectile-switch-project
   `(("b" counsel-projectile-switch-project-action-switch-to-buffer
      "jump to a project buffer")
     ("s" counsel-projectile-switch-project-action-save-all-buffers
      "save all project buffers")
     ("k" counsel-projectile-switch-project-action-kill-buffers
      "kill all project buffers")
     ("c" counsel-projectile-switch-project-action-compile
      "run project compilation command")
     ("e" counsel-projectile-switch-project-action-edit-dir-locals
      "edit project dir-locals")
     ("v" counsel-projectile-switch-project-action-vc
      "open project in vc-dir / magit / monky")
     ("xe" counsel-projectile-switch-project-action-run-eshell
      "invoke eshell from project root")
     ("xt" counsel-projectile-switch-project-action-run-term
      "invoke term from project root")
     ("_" counsel-projectile-switch-project-action-org-capture
      "org-capture into project"))))

;; //////////////////////// PRETTIER /////////////////////

(def-package! prettier-js
  :commands prettier-js-mode
  :init
  (add-hook! '(js2-mode-hook
               typescript-mode-hook
               ;; typescript-tsx-mode-hook
               rjsx-mode-hook
               json-mode-hook
               css-mode-hook
               web-mode-hook
               markdown-mode-hook
               gfm-mode-hook)
    #'prettier-js-mode)
  )

;; ///////////////////////// Git /////////////////////////
(after! git-link
  (add-to-list 'git-link-remote-alist
               '("isl-122-ubuntu" git-link-gitlab))
  (add-to-list 'git-link-commit-remote-alist
               '("isl-122-ubuntu" git-link-commit-gitlab))
  )

(setq magit-repository-directories '(("~/Develop/Github" . 2)))

(defun magit-blame--git-link-commit ()
  "Git link commit go to current line's magit blame's hash"
  (interactive)
  (cl-letf (((symbol-function 'word-at-point)
             (symbol-function 'magit-blame-copy-hash)))
    (let ((git-link-open-in-browser t))
      (git-link-commit (git-link--read-remote)))))

(map!
 (:after magit-blame
   (:map magit-blame-mode-map
     :n "o" #'magit-blame--git-link-commit))
 (:after git-rebase
   (:map git-rebase-mode-map
     "M-j" #'git-rebase-move-line-down
     "M-k" #'git-rebase-move-line-up
     "SPC" nil))
 )

;; //////////////////// ATOMIC CHROME /////////////////////
(def-package! atomic-chrome
  :defer 3
  :preface
  (defun ztlevi-atomic-chrome-server-running-p ()
    (cond ((executable-find "lsof")
           (zerop (call-process "lsof" nil nil nil "-i" ":64292")))
          ((executable-find "netstat") ; Windows
           (zerop (call-process-shell-command "netstat -aon | grep 64292")))))
  :hook
  (atomic-chrome-edit-mode . ztlevi-atomic-chrome-mode-setup)
  (atomic-chrome-edit-done . (lambda () (shell-command "open -a \"/Applications/Google Chrome.app\"")))
  :config
  (progn
    (setq atomic-chrome-buffer-open-style 'full) ;; or frame, split
    (setq atomic-chrome-url-major-mode-alist
          '(("github\\.com"        . gfm-mode)
            ("emacs-china\\.org"   . gfm-mode)
            ("stackexchange\\.com" . gfm-mode)
            ("stackoverflow\\.com" . gfm-mode)
            ;; jupyter notebook
            ("localhost\\:8888"    . python-mode)
            ("lintcode\\.com"      . python-mode)
            ("leetcode\\.com"      . python-mode)))

    (defun ztlevi-atomic-chrome-mode-setup ()
      (setq header-line-format
            (substitute-command-keys
             "Edit Chrome text area.  Finish \
`\\[atomic-chrome-close-current-buffer]'.")))

    (if (ztlevi-atomic-chrome-server-running-p)
        (message "Can't start atomic-chrome server, because port 64292 is already used")
      (atomic-chrome-start-server))))

;; //////////////////////// PRODIGY ////////////////////
(after! prodigy
  (set-evil-initial-state!
    '(prodigy-mode)
    'normal)
  (push 'prodigy-mode evil-snipe-disabled-modes)

  (prodigy-define-tag
    :name 'jekyll
    :env '(("LANG" "en_US.UTF-8")
           ("LC_ALL" "en_US.UTF-8")))
  ;; define service
  (prodigy-define-service
    :name "Leetcode Solution Website"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6005")
    :cwd "~/Developer/Github/leetcode"
    :tags '(leetcode)
    ;; if don't want to browse instantly, delete the following line
    :init (lambda () (browse-url "http://localhost:6005"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Blog Server"
    :command "hexo"
    :args '("server" "-p" "4000")
    :cwd blog-admin-dir
    :tags '(hexo server)
    :init (lambda () (browse-url "http://localhost:4000"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Blog Deploy"
    :command "hexo"
    :args '("deploy" "--generate")
    :cwd blog-admin-dir
    :tags '(hexo deploy)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hackathon backend"
    :env '(("REDISCLOUD_URL" "redis://rediscloud:MeQVSBSNp82uhej2QW42vQxV2TEcd5xq@redis-14678.c44.us-east-1-2.ec2.cloud.redislabs.com:14678"))
    :command "npm"
    :args '("run" "start")
    :cwd "~/Developer/Github/cryptocurrency_exchange_app/backend"
    :tags '(express)
    :init (lambda () (switch-to-buffer "*prodigy-hackathon-backend*"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (defun refresh-chrome-current-tab (beg end length-before)
    (call-interactively 'ztlevi/browser-refresh--chrome-applescript))
  ;; add watch for prodigy-view-mode buffer change event
  (add-hook 'prodigy-view-mode-hook
            #'(lambda() (set (make-local-variable 'after-change-functions) #'refresh-chrome-current-tab))))

(map!
 (:leader
   :n "as" #'prodigy))

;; (def-package! smartparens
;;   :config
;;   (setq sp-autoinsert-pair nil
;;         sp-autodelete-pair nil
;;         sp-escape-quotes-after-insert nil)
;;   (setq-default sp-autoskip-closing-pair nil)
;;   )
