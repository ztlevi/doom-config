;;; private/my-transient/amazon-cr.el -*- lexical-binding: t; -*-

(defun transient-read-amazon-workspace-projects (prompt initial-input history)
  (magit-completing-read-multiple
   prompt
   (mapcar (lambda (line)
             (save-excursion
               (list line)))
           (directory-files
            (file-name-directory (directory-file-name (cdr (project-current))))
            nil "^[^.]"))
   nil nil initial-input history))

(transient-define-argument amazon-cr:-i ()
  :description "Include packages"
  :class 'transient-option
  :key "-i"
  :argument "--include="
  :reader #'transient-read-amazon-workspace-projects
  :multi-value t)

(transient-define-argument amazon-cr:-r ()
  :description "Update exsting review."
  :class 'transient-option
  :key "-r"
  :argument "--update-review=")


;;;###autoload
(defun amazon-cr-create-cr ()
  "Amazon CR create CR."
  (interactive)
  (let ((buffer (get-buffer-create "*amazon-cr*"))
        (cmd '("cr")))
    (with-current-buffer buffer
      (erase-buffer)
      ;; args like '(("--include=" "p1" "p2") "--all")
      (dolist (args (transient-args 'amazon-cr))
        (if (listp args)
            (progn
              (setq arg (car args))
              (setq args (cdr args))
              (dolist (element args)
                (appendq! cmd (list (concat arg element)))))
          (appendq! cmd (list args))
          ))
      (insert (string-join cmd " ")) (insert "\n")
      (insert (shell-command-to-string (string-join cmd " "))))
    (display-buffer buffer)))

;; Reference magit-log.el https://github.com/magit/magit/blob/main/lisp/magit-log.el
;;;###autoload (autoload 'amazon-cr "amazon-cr" nil t)
(transient-define-prefix amazon-cr ()
  "Create Amazon CR."
  ["Arguments"
   (amazon-cr:-i)
   (amazon-cr:-r)
   ("-A" "Include all modified packages" (nil "--all"))
   ("-N" "New package" (nil "--new-review"))
   ]
  [["Amazon CR"
    ("c" "Create CR" amazon-cr-create-cr)
    ]]
  )
