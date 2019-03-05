;;; ~/.doom.d/autoload/os.el -*- lexical-binding: t; -*-

(defconst os--trash-pkg-file
  (expand-file-name (if load-in-progress load-file-name (buffer-file-name)))
  "The absolute path to this file.")

(defconst os--trash-pkg-dir
  (file-name-directory os--trash-pkg-file)
  "The absolute path to the directory of this package.")

(defun os--trash-move-file-to-trash (file-name)
  "Move FILE-NAME to trash.

Try to call the `trash' utility first"
  (let ((file-name (expand-file-name file-name)))
    (with-temp-buffer
      (let ((retcode (call-process "trash" nil t nil file-name)))
        (unless (equal retcode 0)
          (error "Failed to trash %S: %S" file-name (buffer-string)))))))

;;;###autoload
(defun os--trash-setup ()
  "Provide trash support for OS X.

Provide `system-move-file-to-trash' as an alias for
`os--trash-move-file-to-trash'.

Note that you still need to set `delete-by-moving-to-trash' to a
non-nil value to enable trashing for file operations."
  (when (not (fboundp 'system-move-file-to-trash))
    (defalias 'system-move-file-to-trash
      'os--trash-move-file-to-trash)))


;;;###autoload
(defun +shell-open-with (&optional app-name dir args)
  "Open shell application."
  (interactive)
  (let* ((process-connection-type nil)
         (dir (expand-file-name
               (replace-regexp-in-string
                "'" "\\'"
                (or dir (if (derived-mode-p 'dired-mode)
                            (dired-get-file-for-visit)
                          (buffer-file-name)))
                nil t)))
         ;; app specific args
         (args (cond ((and ;; Add "-g" if the dir comes with line number
                       (string= app-name "code") (string-match-p "\\:" dir))
                      "-g")))
         )

    (if args
        (progn
          (setq command (format "%s %s %s" app-name args dir))
          (start-process "" nil app-name args dir))
      (progn
        (setq command (format "%s %s" app-name dir))
        (start-process "" nil app-name dir)))
    (shell-command (concat "wmctrl -a \"" app-name "\" "))
    (message command)))

;;;###autoload
(defmacro +shell!open-with (id &optional app dir args)
  `(defun ,(intern (format "+shell/%s" id)) ()
     (interactive)
     (+shell-open-with ,app ,dir ,args)))
