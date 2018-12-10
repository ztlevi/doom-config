;;; private/my/autoload/misc.el -*- lexical-binding: t; -*-

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
(defun +shell-open-with (&optional app-name path)
  "Send PATH to APP-NAME on OSX."
  (interactive)
  (let* ((process-connection-type nil)
         (path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (if app-name
                      (format "%s '%s'" (shell-quote-argument app-name) path)
                    (format "'%s'" path))))
    (message "Running: %s" command)
    (start-process "" nil app-name path)))

;;;###autoload
(defmacro +shell!open-with (id &optional app dir args)
  `(defun ,(intern (format "+linux/%s" id)) ()
     (interactive)
     (if ,dir
         (+shell-open-with ,app ,dir)
       (apply 'start-process "" nil ,app ,args))))

;;;###autoload
(defmacro make--shell (name ip &rest arglist)
  `(defun ,(intern (format "my-shell-%s" name)) ,arglist
     (interactive)
     (let ((default-directory ,(format "/ssh:%s:" ip))
           (explicit-shell-file-name "/bin/bash"))
       (shell (generate-new-buffer-name ,(format "*%s*" name))))))

;;;###autoload
(defmacro make--ssh (name ip &rest arglist)
  `(defun ,(intern (format "my-ssh-%s" name)) ,arglist
     (interactive)
     (find-file ,(format "/ssh:%s:" ip))))

;;;###autoload
(defun +ivy/project-search-with-hidden-files ()
  (interactive)
  (let ((counsel-rg-base-command "rg -zS --no-heading --line-number --color never --hidden %s . "))
    (+ivy/project-search)))

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
(defun git-link-gitlab-http (hostname dirname filename branch commit start end)
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
(defun counsel-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (require 'evil-nerd-commenter)
  (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
    (counsel-imenu)))

(defun my/define-key (keymap key def &rest bindings)
  "Define multi keybind with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

(defun my/realgud-eval-nth-name-forward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (< (point) (point-max)))
        (let ((p (point)))
          (if (not (c-forward-name))
              (progn
                (c-forward-token-2)
                (when (= (point) p) (forward-char 1)))
            (setq name (buffer-substring-no-properties p (point)))
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        nil))))

(defun my/realgud-eval-nth-name-backward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (> (point) (point-min)))
        (let ((p (point)))
          (c-backward-token-2)
          (when (= (point) p) (backward-char 1))
          (setq p (point))
          (when (c-forward-name)
            (setq name (buffer-substring-no-properties p (point)))
            (goto-char p)
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        nil))))

(defun my/realgud-eval-region-or-word-at-point ()
  (interactive)
  (when-let*
      ((cmdbuf (realgud-get-cmdbuf))
       (process (get-buffer-process cmdbuf))
       (expr
        (if (evil-visual-state-p)
            (let ((range (evil-visual-range)))
              (buffer-substring-no-properties (evil-range-beginning range)
                                              (evil-range-end range)))
          (word-at-point)
          )))
    (with-current-buffer cmdbuf
	  (setq realgud:process-filter-save (process-filter process))
	  (set-process-filter process 'realgud:eval-process-output))
    (realgud:cmd-eval expr)
    ))

(defun +my//realtime-elisp-doc-function ()
  (-when-let* ((w (selected-window))
               (s (intern-soft (current-word))))
    (describe-symbol s)
    (select-window w)))

;;;###autoload
(defun +my/realtime-elisp-doc ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (advice-function-member-p #'+my//realtime-elisp-doc-function eldoc-documentation-function)
        (remove-function (local 'eldoc-documentation-function) #'+my//realtime-elisp-doc-function)
      (add-function :after-while (local 'eldoc-documentation-function) #'+my//realtime-elisp-doc-function))))

;;;###autoload
(defun +my/realgud-eval-nth-name-forward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (< (point) (point-max)))
        (let ((p (point)))
          (if (not (c-forward-name))
              (progn
                (c-forward-token-2)
                (when (= (point) p) (forward-char 1)))
            (setq name (buffer-substring-no-properties p (point)))
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)))))

;;;###autoload
(defun +my/realgud-eval-nth-name-backward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (> (point) (point-min)))
        (let ((p (point)))
          (c-backward-token-2)
          (when (= (point) p) (backward-char 1))
          (setq p (point))
          (when (c-forward-name)
            (setq name (buffer-substring-no-properties p (point)))
            (goto-char p)
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)))))
