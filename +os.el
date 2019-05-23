;;; ~/.doom.d/+os.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when IS-WINDOWS
  (add-to-list 'exec-path "C:/Users/ztlevi/emax/bin"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add executable: Clion -> Tools -> Create Command Line Launcher
(defvar shell-apps '("pycharm" "studio" "clion" "code" "idea")
  "Applications collection used for `+shell!open-with' method")
(defun ivy--read-apps ()
  (ivy-read "Select Applications:" shell-apps))
(defun get-filename-with-line-number ()
  (concat (concat (buffer-file-name) ":")
          (number-to-string (line-number-at-pos))))

(when IS-MAC
  (when (file-directory-p "/Applications/ForkLift.app")
    (+macos!open-with reveal-in-finder "forklift" default-directory)
    (+macos!open-with reveal-project-in-finder "forklift"
                      (or (doom-project-root) default-directory)))

  (+shell!open-with reveal-in-apps (ivy--read-apps)
                    (buffer-file-name)
                    ;; (get-filename-with-line-number)
                    )
  (+shell!open-with reveal-project-in-apps (ivy--read-apps)
                    (or (doom-project-root) default-directory))

  (+macos!open-with reveal-in-typora "typora" buffer-file-name)

  (defun +macos/reveal-in-terminal ()
    (interactive)
    (iterm-open-new-tab default-directory))
  (defun +macos/reveal-project-in-terminal ()
    (interactive)
    (iterm-open-new-tab (or (doom-project-root) default-directory))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LINUX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when IS-LINUX
  (defvar linux-terminal (cond ((executable-find "tilix") "tilix")
                               ((executable-find "konsole") "konsole")
                               ((executable-find "gnome-terminal") "gnome-terminal")))

  (defun linux-terminal-args (dir)
    (cond ((executable-find "tilix") (list dir))
          ((executable-find "konsole") (list "--workdir" dir))
          ((executable-find "gnome-terminal") (list "--working-directory" dir))))

  (defvar linux-finder (cond ((executable-find "xdg-open") "xdg-open")
                             ((executable-find "gvfs-open") "gvfs-open")))

  (+shell!open-with open-in-default-program linux-finder buffer-file-name)

  (+shell!open-with reveal-in-finder linux-finder default-directory)
  (+shell!open-with reveal-project-in-finder linux-finder
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-apps (ivy--read-apps)
                    (buffer-file-name)
                    ;; (get-filename-with-line-number)
                    )
  (+shell!open-with reveal-project-in-apps (ivy--read-apps)
                    (or (doom-project-root) default-directory))

  (+shell!open-with reveal-in-terminal linux-terminal nil (linux-terminal-args default-directory))
  (+shell!open-with reveal-project-in-terminal linux-terminal nil
                    (linux-terminal-args (or (doom-project-root) default-directory)))

  (+shell!open-with reveal-in-typora "typora" buffer-file-name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete to trash
(setq delete-by-moving-to-trash t)

;; using trash over rm
(when (executable-find "trash")
  (os--trash-setup))
