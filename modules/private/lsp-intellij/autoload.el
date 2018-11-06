;;; private/lsp-intellij/autoload.el -*- lexical-binding: t; -*-

;; https://github.com/Ruin0x11/lsp-intellij-spacemacs/blob/master/funcs.el
;;;###autoload
(defun spacemacs//java-lsp-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))

;;;###autoload
(defun spacemacs/java-lsp-completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (spacemacs//java-lsp-delete-horizontal-space)
  (insert ".")
  (company-lsp 'interactive))

;;;###autoload
(defun spacemacs/java-lsp-completing-double-colon ()
  "Insert double colon and show company completions."
  (interactive "*")
  (spacemacs//java-lsp-delete-horizontal-space)
  (insert ":")
  (let ((curr (point)))
    (when (s-matches? (buffer-substring (- curr 2) (- curr 1)) ":")
      (company-lsp 'interactive))))
