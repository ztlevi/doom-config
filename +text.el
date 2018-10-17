;;;  -*- lexical-binding: t; -*-

(add-hook! 'text-mode-hook (setq-local truncate-lines nil))

;; /////////////////////////// ORG /////////////////////////////////
(setq org-ellipsis "  "
      org-directory "~/Dropbox/Org-Notes"
      org-agenda-files (list org-directory))

;; ///////////////////////////// MARKDOWN /////////////////////////////
(after! markdown-mode
  ;; memo: install grip > ‘pip3 install grip‘
  (defvar +my/markdown-process nil)
  (defun +my/markdown-preview (&rest _)
    "Preview markdown file by using grip."
    (when (process-live-p +my/markdown-process)
      (kill-process +my/markdown-process))
    (setq +my/markdown-process
          (start-process-shell-command "grip markdown-preview"
                                       markdown-output-buffer-name
                                       (format "grip --browser '%s'" (buffer-file-name)))))

  ;; Use advice to use preview at both markdown-mode and gfm-mode.
  (when (executable-find "grip")
    (advice-add 'markdown-preview :override '+my/markdown-preview))
  )

(def-package! blog-admin
    :commands blog-admin-start
    :hook (blog-admin-backend-after-new-post . find-file)
    :init
    ;; do your configuration here
    (setq blog-admin-backend-type 'hexo
          blog-admin-backend-path "~/Developer/Github/hexo_blog"
          blog-admin-backend-new-post-in-drafts t
          blog-admin-backend-new-post-with-same-name-dir nil
          blog-admin-backend-hexo-config-file "_config.yml"))
