;;;  -*- lexical-binding: t; -*-

(defvar blog-admin-dir ""
  "blog-admin files location")

(setq org-agenda-files '("~/Dropbox/Org-Notes")
      blog-admin-dir "~/Developer/Github/hexo_blog")

(setq org-capture-templates
      '(
        ("d" "Diary" entry
         (file+olp+datetree "diary.org")
         "* %?" :kill-buffer t)
        ("n" "Notes" entry
         (file+headline +org-default-notes-file "Inbox")
         "* %u %?\n%i" :prepend t :kill-buffer t)
        ("t" "Todo" entry
         (file+headline +org-default-todo-file "Inbox")
         "* [ ] %?\n%i" :prepend t :kill-buffer t)
        ))

(setq org-ellipsis "  ")

(add-hook! 'text-mode-hook (setq-local truncate-lines nil))
