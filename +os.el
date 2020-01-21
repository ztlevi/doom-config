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
(defvar shell-apps '("code" "pycharm" "clion" "idea")
  "Applications collection used for `+shell--open-with' method")
(defun ivy--read-apps ()
  (ivy-read "Select Applications:" shell-apps))
(defun get-filename-with-line-number ()
  (concat (concat (buffer-file-name) ":")
          (number-to-string (line-number-at-pos))))

(when IS-MAC
  (when (file-directory-p "/Applications/ForkLift.app")
    (+macos--open-with reveal-in-finder "forklift" default-directory)
    (+macos--open-with reveal-project-in-finder "forklift"
                       (or (doom-project-root) default-directory)))

  (+shell--open-with reveal-in-apps (ivy--read-apps)
                     (string-join `("-g '" ,(get-filename-with-line-number) "'")))
  (+shell--open-with reveal-project-in-apps (ivy--read-apps)
                     (or (doom-project-root) default-directory))

  (+macos--open-with reveal-in-typora "typora" buffer-file-name)

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
  (defvar linux-terminal (cond ((executable-find "xst") "xst")
                               ((executable-find "tilix") "tilix")
                               ((executable-find "konsole") "konsole")
                               ((executable-find "gnome-terminal") "gnome-terminal")))

  (defun linux-terminal-args (dir)
    (cond ((executable-find "xst") (concat "zsh -c 'cd " dir ";zsh'"))
          ((executable-find "tilix") (concat "--display=:1 " "--working-directory='" dir "'"))
          ((executable-find "konsole") (concat "--workdir='" dir "'"))
          ((executable-find "gnome-terminal") (concat "--working-directory='" dir "'"))))


  (defvar linux-finder (cond ((executable-find "xdg-open") "xdg-open")
                             ((executable-find "gvfs-open") "gvfs-open")))

  (+shell--open-with open-in-default-program linux-finder buffer-file-name)

  (+shell--open-with reveal-in-finder linux-finder default-directory)
  (+shell--open-with reveal-project-in-finder linux-finder
                     (or (doom-project-root) default-directory))

  (+shell--open-with reveal-in-apps (ivy--read-apps)
                     (string-join `("-g '" ,(get-filename-with-line-number) "'")))
  (+shell--open-with reveal-project-in-apps (ivy--read-apps)
                     (or (doom-project-root) default-directory))

  (+shell--open-with reveal-in-terminal linux-terminal (linux-terminal-args default-directory))
  (+shell--open-with reveal-project-in-terminal linux-terminal
                     (linux-terminal-args (or (doom-project-root) default-directory)))

  (+shell--open-with reveal-in-typora "typora" (concat buffer-file-name " &")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOCKER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-docker-project-filename ()
  (string-join `("/code/" ,(replace-regexp-in-string "detection/python/private" "python_root/detection"
                                                     (file-relative-name (buffer-file-name) "~/av")))))

(+docker--open-with reveal-in-docker-pycharm "/usr/local/pycharm-2019.2.5/bin/pycharm.sh"
                    (get-docker-project-filename) "pycharm" "pycharm")
(+docker--open-with reveal-in-docker-clion "/usr/local/clion/bin/clion.sh"
                    (get-docker-project-filename) "clion" "clion")

(defun +docker/reveal-in-apps ()
  (interactive)
  (ivy-read "Select Docker apps"
            '(+docker/reveal-in-docker-pycharm +docker/reveal-in-docker-clion)
            :action #'counsel-M-x-action))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete to trash
(setq delete-by-moving-to-trash t)

;; using trash over rm
(when (executable-find "trash")
  (os--trash-setup))
