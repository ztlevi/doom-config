;;; private/ranger/config.el -*- lexical-binding: t; -*-

(load! "+vinegar")

(def-package! ranger
  :commands (ranger deer deer-jump-other-window ranger-override-dired-mode)
  :init
  (setq ranger-override-dired t)
  (progn
    ;; set up image-dired to allow picture resize
    (setq image-dired-dir (concat doom-cache-dir "image-dir"))
    (unless (file-directory-p image-dired-dir)
      (make-directory image-dired-dir)))
  (map!
   (:leader
     :n "fj" #'deer
     :n "ar" #'ranger
     :n "ad" #'deer)
   (:after ranger
     (:map ranger-normal-mode-map
       "g" nil
       "q" #'ranger-close-and-kill-inactive-buffers
       "f" #'counsel-find-file
       "C-<tab>" #'ranger-next-tab
       "C-S-<tab>" #'ranger-prev-tab
       "U" #'dired-unmark-all-files
       "u" #'dired-unmark
       "(" #'dired-hide-details-mode
       "+" #'dired-create-directory)))
  :config
  (defun ranger-close-and-kill-inactive-buffers ()
    "ranger close current buffer and kill inactive ranger buffers"
    (interactive)
    (ranger-close)
    (ranger-kill-buffers-without-window))
  ;; do not kill buffer if exists in windows
  (defun ranger-disable ()
    "Interactively disable ranger-mode."
    (interactive)
    (ranger-revert))

  (setq ranger-omit-regexp "^\.DS_Store$"
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details nil
        ranger-max-preview-size 10))

(def-package! all-the-icons-dired
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

(def-package! font-lock+)

(def-package! dired-x
  :commands (dired-jump
             dired-jump-other-window
             dired-omit-mode))

(def-package! dired
  :init
  (add-hook 'dired-mode-hook 'vinegar/dired-setup)
  (map!
   (:after dired
     (:map dired-mode-map
       "j" 'vinegar/move-down
       "k" 'vinegar/move-up
       "-" 'vinegar/up-directory
       "0" 'dired-back-to-start-of-files
       "=" 'vinegar/dired-diff
       "C-j" 'dired-next-subdir
       "C-k" 'dired-prev-subdir
       "I" 'vinegar/dotfiles-toggle
       "~" '(lambda ()(interactive) (find-alternate-file "~/"))
       "RET" (if vinegar-reuse-dired-buffer
                 'dired-find-alternate-file
               'dired-find-file)
       "f" (if (featurep 'ivy)
               'counsel-find-file
             'helm-find-files)
       "J" 'dired-goto-file
       "C-f" 'find-name-dired
       "H" 'diredp-dired-recent-dirs
       "T" 'dired-tree-down
       "K" 'dired-do-kill-lines
       "r" 'revert-buffer
       "C-r" 'dired-do-redisplay
       "g" nil
       "gg" 'vinegar/back-to-top
       "G" 'vinegar/jump-to-bottom))))

(defun osx/post-init-exec-path-from-shell ()
  ;; Use GNU ls as `gls' from `coreutils' if available.  Add `(setq
  ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning when
  ;; not using GNU ls.  We must look for `gls' after `exec-path-from-shell' was
  ;; initialized to make sure that `gls' is in `exec-path'
  (when IS-MAC
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls
              dired-listing-switches "-aBhl --group-directories-first")))))
(osx/post-init-exec-path-from-shell)
