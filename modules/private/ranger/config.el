;;; private/ranger/config.el -*- lexical-binding: t; -*-

(def-package! dired
  :defer t
  :hook (dired-mode . +my/dired-setup)
  :config
  ;; Use GNU ls as `gls' from `coreutils' if available.  Add `(setq
  ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning when
  ;; not using GNU ls.  We must look for `gls' after `exec-path-from-shell' was
  ;; initialized to make sure that `gls' is in `exec-path'
  (when IS-MAC
    (let ((gls (executable-find "gls")))
      (if gls
          (setq insert-directory-program gls)
        (message "Please install `gls` using `brew instal coreutils`..."))))
  (setq dired-listing-switches "-aBhl --group-directories-first"))


(def-package! ranger
  :defer t
  :init
  (setq ranger-override-dired t)
  ;; set up image-dired to allow picture resize
  (setq image-dired-dir (concat doom-cache-dir "image-dir"))
  (unless (file-directory-p image-dired-dir)
    (make-directory image-dired-dir))
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
        ranger-max-preview-size 10)

  (map!
   (:map ranger-normal-mode-map
     "g" nil
     "q" #'ranger-close-and-kill-inactive-buffers
     "f" #'counsel-find-file
     "C-g" #'ranger-go
     "C-<tab>" #'ranger-next-tab
     "C-S-<tab>" #'ranger-prev-tab
     "U" #'dired-unmark-all-files
     "u" #'dired-unmark
     "(" #'dired-hide-details-mode
     "+" #'dired-create-directory
     (:leader
       :m "fj" #'deer
       :m "oj" #'deer
       :m "oJ" #'ranger)
     )))

(map!
 (:leader
   :n "fj" #'deer
   :n "oj" #'deer
   :n "oJ" #'ranger))


(def-package! all-the-icons-dired
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))


(def-package! font-lock+ :defer t)


(def-package! dired-x :defer t)
