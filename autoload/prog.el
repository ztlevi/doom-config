;;; ~/.doom.d/autoload/prog.el -*- lexical-binding: t; -*-

;;;###autoload
(defun reset-flycheck (&rest _)
  (flycheck-mode -1)
  (flycheck-mode +1))

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
(defun +python/annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "breakpoint()" 'breakpoint-enabled)
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)" 'breakpoint-enabled)
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()" 'breakpoint-enabled)
  (highlight-lines-matching-regexp "trepan.api.debug()") 'breakpoint-enabled)

;;;###autoload
(defun +python/toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((executable-find "trepan3k") "import trepan.api; trepan.api.debug()")
                     ((executable-find "wdb") "import wdb; wdb.set_trace()")
                     ((executable-find "pudb") "import pudb; pudb.set_trace()")
                     ;; ((executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ;; ((executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
                     ((executable-find "pudb3") "import pudb; pudb.set_trace()")
                     ;; ((executable-find "python3.7") "breakpoint()")
                     ;; ((executable-find "python3.8") "breakpoint()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line))))
  (+python/annotate-pdb))

;;;###autoload
(defun +python/copy-pdb-breakpoint-of-current-line ()
  "Copy a pdb like breakpoint on the current line."
  (interactive)
  (kill-new
   (concat "b " (file-relative-name (buffer-file-name) (doom-project-root))
           ":" (number-to-string (line-number-at-pos)))))

;;;###autoload
(defun +python/autoflake-remove-imports ()
  "Remove unused imports."
  (interactive)
  (shell-command
   (concat "autoflake --in-place --remove-all-unused-imports " (buffer-file-name)))
  (revert-buffer-no-confirm))

;;;###autoload
(defun +python/yank-module-import ()
  "Copy the current module's name to the kill ring."
  (interactive)
  (if-let (import (string-join `("from"
                                 ,(replace-regexp-in-string
                                   "/" "\."
                                   (file-relative-name (or (file-name-sans-extension (buffer-file-name))
                                                           (bound-and-true-p list-buffers-directory))
                                                       (doom-project-root)))
                                 "import" ,(which-function))
                               " "))
      (message (kill-new (abbreviate-file-name import)))
    (error "Couldn't find filename in current buffer")))
