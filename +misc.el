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

;; (def-package! smartparens
;;   :config
;;   (setq sp-autoinsert-pair nil
;;         sp-autodelete-pair nil
;;         sp-escape-quotes-after-insert nil)
;;   (setq-default sp-autoskip-closing-pair nil)
;;   )
