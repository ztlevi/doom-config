;;; ~/.doom.d/autoload/prog.el -*- lexical-binding: t; -*-

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
(defun +go/current-function-name ()
  "Get the name of the current Go function."
  (interactive)
  (save-excursion
    (re-search-backward "^func[ \t]+\\(\\(\\w\\|\\s_\\)+\\)" nil t)
    (match-string 1)))

;;;###autoload
(defun +go/copy-go-test-cmd ()
  "Copy go test cmd."
  (interactive)
  (let ((cmd (concat "dlv test --init=breakpoints.dlv "
                     "./" (file-relative-name (file-name-directory (buffer-file-name)) (doom-project-root))
                     " -- -test.run "
                     "\"^" (+go/current-function-name) "$\""
                     )))
    (message cmd)
    (kill-new cmd)))

;;;###autoload
(defun +go/copy-go-test-run-cmd ()
  "Copy go test cmd."
  (interactive)
  (let ((cmd (concat "go test "
                     "./" (file-relative-name (file-name-directory (buffer-file-name)) (doom-project-root))
                     " -run "
                     "\"^" (+go/current-function-name) "$\""
                     )))
    (message cmd)
    (kill-new cmd)))

;;;###autoload
(defun +go/copy-go-breakpoint ()
  "Copy go test cmd."
  (interactive)
  (let ((cmd (concat "b " (file-relative-name (buffer-file-name) (doom-project-root))
                     ":" (number-to-string (line-number-at-pos)))))
    (message cmd)
    (kill-new cmd)))
