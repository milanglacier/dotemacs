;;; my-init-colorscheme.el -*- lexical-binding: t; -*-

(straight-use-package 'doom-themes)
(straight-use-package 'spacemacs-theme)

(defvar my/day-themes
  '(doom-solarized-light spacemacs-light modus-operandi))
(defvar my/night-themes
  '(spacemacs-dark doom-one doom-nord-aurora))

(when (display-graphic-p)
  (add-to-list 'my/night-themes 'modus-vivendi))

(defun my/pickup-random-color-theme (themes)
  (let* ((theme (nth (random (length themes)) themes)))
    (load-theme theme t)
    (when (string-match-p "doom" (symbol-name theme))
      (doom-themes-org-config))))

(defvar my/day-to-night-int 23)
(defvar my/night-to-day-int 7)

(defun my/calculate-time-to-switch-theme (transit-direction)
  (let ((current-min (string-to-number (format-time-string "%M")))
        (current-hour (string-to-number (format-time-string "%H")))
        mins-to-next-hour
        hours-to-switch-theme
        time-to-switch)
    (setq mins-to-next-hour (- 60 current-min))
    (when (eq transit-direction 'day-to-night)
      (setq hours-to-switch-theme (- my/day-to-night-int (1+ current-hour))))
    (when (eq transit-direction 'night-to-day)
      (if (>= current-hour my/day-to-night-int)
          (setq hours-to-switch-theme (+ my/night-to-day-int
                                         (- 24 (1+ current-hour))))
        (setq hours-to-switch-theme (- my/night-to-day-int (1+ current-hour)))))
    (setq time-to-switch (format "%d hours %d minutes"
                                 hours-to-switch-theme
                                 mins-to-next-hour))
    time-to-switch))

(defun set-theme-dynamically ()
  (if (or (>= (string-to-number (format-time-string "%H"))
              my/day-to-night-int)
          (< (string-to-number (format-time-string "%H"))
             my/night-to-day-int))
      (progn
        (my/pickup-random-color-theme my/night-themes)
        (run-at-time (my/calculate-time-to-switch-theme 'night-to-day) nil #'set-theme-dynamically))
    (progn
      (my/pickup-random-color-theme my/day-themes)
      (run-at-time (my/calculate-time-to-switch-theme 'day-to-night) nil #'set-theme-dynamically))))

(set-theme-dynamically)

(provide 'my-init-colorscheme)
;;; my-init-colorscheme.el ends here
