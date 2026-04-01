;;; private/my-transient/amz-cr.el -*- lexical-binding: t; -*-

(require 'transient)

;; Revert transient-display-buffer-action due to transient is popping another buffer
(setq transient-display-buffer-action
      '(display-buffer-in-side-window
        (side . bottom)
        (dedicated . t)
        (inhibit-same-window . t)))

(defun transient-read-amz-workspace-projects (prompt initial-input history)
  (completing-read-multiple
   prompt
   (directory-files
    (file-name-directory (directory-file-name (doom-project-root)))
    nil "^[^.]")
   nil nil initial-input history))

(transient-define-argument amz-cr:-i ()
  :description "Include packages"
  :class 'transient-option
  :key "-i"
  :argument "--include="
  :reader #'transient-read-amz-workspace-projects
  :multi-value t)

(transient-define-argument amz-cr:-r ()
  :description "Update exsting review."
  :class 'transient-option
  :key "-r"
  :argument "--update-review=")

(defun amz-cr--create-cr ()
  "Amazon CR create CR."
  (interactive)
  (let ((cmd (list "cr")))
    (dolist (args (transient-args 'amz-cr))
      (if (listp args)
          (let ((arg (car args)))
            (dolist (element (cdr args))
              (setq cmd (append cmd (list (concat arg element))))))
        (if (string= args "--yes")
            (unless (member "yes | " cmd)
              (push "yes | " cmd))
          (setq cmd (append cmd (list args))))))
    (let ((default-directory (doom-project-root)))
      (async-shell-command (string-join cmd " ")))))


;; Reference magit-log.el https://github.com/magit/magit/blob/main/lisp/magit-log.el
(transient-define-prefix amz-cr ()
  "Create Amazon CR."
  ["Arguments"
   (amz-cr:-i)
   (amz-cr:-r)
   ("-A" "Include all modified packages" "--all")
   ("-N" "New review" "--new-review")
   ("-y" "Yes" "--yes")
   ]
  [["Amazon CR"
    ("c" "Create CR" amz-cr--create-cr)
    ]]
  )

(provide 'amz-cr)
