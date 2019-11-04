;;; private/my/autoload/misc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun yank-with-delete-region ()
  (interactive)
  (when (evil-visual-state-p)
    (delete-region (region-beginning) (region-end)))
  (yank))

;;;###autoload
(defun doom/toggle-comment-region-or-line ()
  "Comments or uncomments the whole region or if no region is
selected, then the current line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;;###autoload
(define-inline +my/prefix-M-x (prefix)
  (inline-quote
   (lambda () (interactive)
     (setq unread-command-events (string-to-list ,prefix))
     (call-interactively #'execute-extended-command))))

;;;###autoload
(define-inline +my/simulate-key (key)
  (inline-quote
   (lambda () (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key))))))

;;;###autoload
(defmacro make--shell (name ip &rest arglist)
  `(defun ,(intern (format "my-shell-%s" name)) ,arglist
     (interactive)
     (find-file ,(format "/sshx:%s:" ip))
     (vterm-toggle-cd)))

;;;###autoload
(defmacro make--ssh (name ip &rest arglist)
  `(defun ,(intern (format "my-ssh-%s" name)) ,arglist
     (interactive)
     (find-file ,(format "/sshx:%s:" ip))))

;;;###autoload
(defun +ivy/project-search-with-hidden-files ()
  (interactive)
  (let ((counsel-rg-base-command "rg -zS --no-heading --line-number --color never --hidden %s . "))
    (+ivy/project-search)))

;;;###autoload
(defvar +my/repo-root-list '("~" "~/Dropbox" "~/go/src" "~/.cache")
  "personal repo root to scan git projects")

;;;###autoload
(defun update-projectile-known-projects ()
  (interactive)
  (require 'magit)
  (let (magit-repos
        magit-abs-repos
        (home (expand-file-name "~")))
    ;; append magit repos at root with depth 1
    (dolist (root +my/repo-root-list)
      (setq magit-abs-repos (append magit-abs-repos (magit-list-repos-1 root 1))))
    (setq magit-abs-repos (append magit-abs-repos (magit-list-repos)))

    ;; convert abs path to relative path (HOME)
    (dolist (repo magit-abs-repos)
      (string-match home repo)
      (push (replace-match "~" nil nil repo 0) magit-repos))
    (setq projectile-known-projects magit-repos)
    (if (file-directory-p "~/av/detection/python/private/")
        (push "~/av/detection/python/private/" projectile-known-projects))))

;; PATCH counsel-esh-history
;;;###autoload
(defun +my/ivy-eshell-history ()
  (interactive)
  (require 'em-hist)
  (let* ((start-pos (save-excursion (eshell-bol) (point)))
         (end-pos (point))
         (input (buffer-substring-no-properties start-pos end-pos))
         (command (ivy-read "Command: "
                            (delete-dups
                             (when (> (ring-size eshell-history-ring) 0)
                               (ring-elements eshell-history-ring)))
                            :initial-input input)))
    (setf (buffer-substring start-pos end-pos) command)
    (end-of-line)))

;;;###autoload
(defun +vc/git-browse-commit (arg)
  "Open the website for the current version controlled file. Fallback to
repository root."
  (interactive "P")
  (require 'git-link)
  (let ((git-link-open-in-browser (not arg)))
    (git-link-commit (git-link--select-remote))))

;;;###autoload
(defun git-link-github-http (hostname dirname filename branch commit start end)
  (format "http://%s/%s/blob/%s/%s"
          hostname
          dirname
          (or branch commit)
          (concat filename
                  (when start
                    (concat "#"
                            (if end
                                (format "L%s-%s" start end)
                              (format "L%s" start)))))))

;;;###autoload
(defun git-link-commit-github-http (hostname dirname commit)
  (format "http://%s/%s/commit/%s"
          hostname
          dirname
          commit))

;;;###autoload
(defun magit-blame--git-link-commit (arg)
  "Git link commit go to current line's magit blame's hash"
  (interactive "P")
  (require 'git-link)
  (cl-letf (((symbol-function 'word-at-point)
             (symbol-function 'magit-blame-copy-hash)))
    (let ((git-link-open-in-browser (not arg)))
      (git-link-commit (git-link--read-remote)))))

;;;###autoload
(defun +my/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (replace-regexp-in-string "/" "\\/" (regexp-quote (buffer-substring-no-properties beg end)) t t)))
      (setq command-string (format "1,$s /%s/%s/g" selection selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

;;;###autoload
(defun +my/markdown-copy-fix ()
  (interactive)
  (let ((case-fold-search nil))
    (dolist (pair '(("<pre>" . "```python")
                    ("<\/pre>" . "```")
                    ("\\*" . "*")
                    ("\\#" . "#")))
      (goto-char (point-min))
      ;; if you need regexp, use search-forward-regexp
      (while (search-forward (car pair) nil t)
        (replace-match (cdr pair))))))


;;;###autoload
(defun iterm-open-new-tab (dir &optional args)
  (do-applescript
   (format
    "
    tell application \"/Applications/iTerm.app\"
        activate
        tell current window
            create tab with default profile
            tell the current session
                write text \"cd %s\"
            end tell
        end tell
    end tell
"
    dir)))

;; "http://xuchunyang.me/Opening-iTerm-From-an-Emacs-Buffer/"
;;;###autoload
(defun +my/iterm-shell-command (command &optional prefix)
  "cd to `default-directory' then run COMMAND in iTerm.
With PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (doom-project-root)
                default-directory))
         ;; if COMMAND is empty, just change directory
         (cmd (format "cd %s ;%s" dir command)))
    (do-applescript
     (format
      "
  tell application \"iTerm2\"
       activate
       set _session to current session of current window
       tell _session
            set command to get the clipboard
            write text \"%s\"
       end tell
  end tell
  " cmd))))

;; https://github.com/syohex/emacs-browser-refresh/blob/master/browser-refresh.el
;;;###autoload
(defun +my/browser-refresh--chrome-applescript ()
  (interactive)
  (do-applescript
   (format
    "
  tell application \"Google Chrome\"
    set winref to a reference to (first window whose title does not start with \"Developer Tools - \")
    set winref's index to 1
    reload active tab of winref
  end tell
" )))

;;;###autoload
(defun +my/window-focus-google-chrome ()
  (cond (IS-MAC (shell-command "open -a \"/Applications/Google Chrome.app\""))
        (IS-LINUX (shell-command "wmctrl -a \"Google Chrome\""))))

;;;###autoload
(defun counsel-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (require 'evil-nerd-commenter)
  (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
    (counsel-imenu)))


;; if the first line is too long, enable fundamental by default
;;;###autoload
(defun get-nth-line-length (n)
  "Length of the Nth line."
  (save-excursion
    (goto-char (point-min))
    (if (zerop (forward-line (1- n)))
        (- (line-end-position)
           (line-beginning-position)))))

;;;###autoload
(defun +my/check-minified-file ()
  (and
   (not (member (file-name-extension (buffer-file-name))
                '("org" "md" "markdown" "txt" "rtf")))
   (cl-loop for i from 1 to (min 30 (count-lines (point-min) (point-max)))
            if (> (get-nth-line-length i) 1000)
            return t
            finally return nil)))


;;;###autoload
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))
