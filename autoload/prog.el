;;; autoload/prog.el -*- lexical-binding: t; -*-

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

;;;###autoload
(defun async-shell-command-no-window (command)
  "Requisite Documentation"
  (interactive)
  (let
      ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command nil nil)))

;;;###autoload
(defadvice async-shell-command-no-window (around auto-confirm compile activate)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
    ad-do-it))

;;;###autoload
(defun display-which-function ()
  (interactive)
  (message (which-function)))

;;;###autoload
(defun +cc/copy-lldb-breakpoint-of-current-line ()
  "Copy a pdb like breakpoint on the current line."
  (interactive)
  (kill-new
   (concat "b " (file-name-nondirectory (buffer-file-name))
           " : " (number-to-string (line-number-at-pos)))))

;;;###autoload
(defun +go/copy-go-test-run-cmd ()
  "Run single test at point."
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (let ((cmd (concat "go test "
                           "./" (file-relative-name (file-name-directory (buffer-file-name)) (doom-project-root))
                           " -test.v " "-run" "='^\\Q" (match-string-no-properties 2) "\\E$'")))
          (message cmd)
          (kill-new cmd)))
    (error "Must be in a _test.go file")))

;;;###autoload
(defun +go/copy-go-test-dlv-cmd ()
  "Copy go test cmd."
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ \t]+\\(\\(\\w\\|\\s_\\)+\\)")
        (let ((cmd (concat "dlv test --init=breakpoints.dlv "
                           "./" (file-relative-name (file-name-directory (buffer-file-name)) (doom-project-root))
                           " -- -test.v -test.run "
                           "\"^" (match-string 1) "$\""
                           )))
          (message cmd)
          (kill-new cmd)))
    (error "Must be in a _test.go file")))

;;;###autoload
(defun +go/copy-go-breakpoint ()
  "Copy go breakpoint."
  (interactive)
  (let ((cmd (concat "b " (file-relative-name (buffer-file-name) (doom-project-root))
                     ":" (number-to-string (line-number-at-pos)))))
    (message cmd)
    (kill-new cmd)))

;;;###autoload
(defun +go/insert-go-breakpoint ()
  "Insert go breakpoint to breakpoints.dlv."
  (interactive)
  (let ((breakpoint-file (concat (doom-project-root) "breakpoints.dlv"))
        (cmd (concat "b " (file-relative-name (buffer-file-name) (doom-project-root))
                     ":" (number-to-string (line-number-at-pos)) "\n")))
    (with-temp-buffer
      (unless (file-exists-p breakpoint-file)
        (insert "continue")
        (write-file breakpoint-file))
      (find-file breakpoint-file)
      (goto-char (point-min))
      (insert cmd)
      (save-buffer)
      (kill-buffer))))

;; modified from `dape-breakpoint-save'
;;;###autoload
(defun +go/write-project-breakpoints ()
  "Write dape breakpoints to breakpoints.dlv file in project root."
  (interactive)
  (when (eq major-mode 'go-mode)
    (let ((project-root (doom-project-root))
          (breakpoint-file (concat (doom-project-root) "breakpoints.dlv"))
          (result ""))
      (cl-loop for breakpoint in dape--breakpoints
               for path = (dape--breakpoint-path breakpoint)
               for line = (dape--breakpoint-line breakpoint)
               for condition = (dape--breakpoint-value breakpoint)
               when (and path (string-prefix-p project-root path))
               do (setq result (concat result "b "
                                       (file-relative-name path project-root)
                                       ":"
                                       (number-to-string line)
                                       (if condition (concat " " condition) "")
                                       "\n")))
      (with-temp-buffer
        (insert result)
        (insert "continue")
        (write-file breakpoint-file)))))

;;;###autoload
(defun +ai/copy-current-line ()
  "Copy the context of current line."
  (interactive)
  (let ((cmd (concat "Given `" (file-relative-name (buffer-file-name) (doom-project-root))
                     "` line " (number-to-string (line-number-at-pos)) " as context.\n")))
    (message cmd)
    (kill-new cmd)))

;;;###autoload
(defun +ai/copy-current-function ()
  "Copy the context of current function."
  (interactive)
  (let ((cmd (concat "Given `" (file-relative-name (buffer-file-name) (doom-project-root))
                     "` line " (number-to-string
                                (save-excursion (beginning-of-defun) (line-number-at-pos))) " as context.\n")))
    (message cmd)
    (kill-new cmd)))
