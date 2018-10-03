;;; private/ranger/config.el -*- lexical-binding: t; -*-

(load! "+vinegar")

(map!
 (:leader
   :n "ar" #'ranger
   :n "fj" #'deer
   :n "ad" #'deer
   :n "jD" #'deer-jump-other-window
   :n "jd" #'deer)
 (:after ranger
   (:map ranger-normal-mode-map
     "f" #'counsel-find-file
     "C-<tab>" #'ranger-next-tab
     "C-S-<tab>" #'ranger-prev-tab
     "U" #'dired-unmark-all-files
     "u" #'dired-unmark
     "(" #'dired-hide-details-mode
     "+" #'dired-create-directory))
 )

(def-package! ranger
  :commands (ranger deer deer-jump-other-window ranger-override-dired-mode)
  :init
  (progn
    ;; set up image-dired to allow picture resize
    (setq image-dired-dir (concat doom-cache-dir "image-dir"))
    (unless (file-directory-p image-dired-dir)
      (make-directory image-dired-dir)))
  :config
  (setq ranger-omit-regexp "^\.DS_Store$"
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details nil
        ranger-max-preview-size 10)

  (after! doom-one-light-theme
    (custom-set-faces
     '(hl-line ((t (:background "#dcdcdc"))))))
  )

(def-package! all-the-icons-dired
    :defer t
    :hook (dired-mode . all-the-icons-dired-mode))

(def-package! font-lock+)

(def-package! dired-x
    :commands (dired-jump
               dired-jump-other-window
               dired-omit-mode))

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
