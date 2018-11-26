;;; ~/.doom.d/autoload/prog.el -*- lexical-binding: t; -*-


;;;###autoload
(defun reset-flycheck (&rest _)
  (flycheck-mode -1)
  (flycheck-mode +1))
