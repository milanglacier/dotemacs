;;; lib-colorscheme.el -*- lexical-binding: t; -*-

(defvar mg-day-themes
    '(leuven)
    "list of colorschemes to be used in light mode")

(defvar mg-night-themes
    '(tango-dark)
    "list of colorschemes to be used in dark mode")

(defvar mg-selected-theme nil
    "selected theme")

(defun mg--pickup-random-color-theme (themes)
    (let* ((theme (nth (random (length themes)) themes)))
        (disable-theme mg-selected-theme)
        (setq mg-selected-theme theme)
        (load-theme theme t)))

(defvar mg-day-to-night-o-clock 23 "The time to switch from day to night themes")
(defvar mg-night-to-day-o-clock 7 "The time to switch from night to day themes")

(defun mg--calculate-time-to-switch-theme (transit-direction)
    "Calculate the time remaining until the next theme switch, based
on the current time, the direction of the transition (`day-to-night'
or `night-to-day'), and the settings of `mg-day-to-night-o-clock' and
`mg-night-to-day-o-clock'.  Returns a formatted string with the amount
of time remaining until the switch."
    (let ((current-min (string-to-number (format-time-string "%M")))
          (current-hour (string-to-number (format-time-string "%H")))
          mins-to-next-hour
          hours-to-switch-theme
          time-to-switch)
        (setq mins-to-next-hour (- 60 current-min))
        (when (eq transit-direction 'day-to-night)
            (setq hours-to-switch-theme (- mg-day-to-night-o-clock (1+ current-hour))))
        (when (eq transit-direction 'night-to-day)
            (if (>= current-hour mg-day-to-night-o-clock)
                    (setq hours-to-switch-theme (+ mg-night-to-day-o-clock
                                                   (- 24 (1+ current-hour))))
                (setq hours-to-switch-theme (- mg-night-to-day-o-clock (1+ current-hour)))))
        (setq time-to-switch (format "%d hours %d minutes"
                                     hours-to-switch-theme
                                     mins-to-next-hour))
        time-to-switch))

;;;###autoload
(defun mg--theme-set-dynamically ()
    "Select a theme at random from `mg-day-themes' or
`mg-night-themes', depending on the current time of day. The
environment variable `THEME_MODE' can override current time.
The time periods for day and night are specified by
`mg-day-to-night-o-clock' and `mg-night-to-day-o-clock',
respectively."
    (let* ((transition-direction
            (if (or (>= (string-to-number (format-time-string "%H"))
                        mg-day-to-night-o-clock)
                    (< (string-to-number (format-time-string "%H"))
                       mg-night-to-day-o-clock))
                    'night-to-day
                'day-to-night))
           (themes (cond
                    ((equal (getenv "THEME_MODE") "day") mg-day-themes)
                    ((equal (getenv "THEME_MODE") "night") mg-night-themes)
                    ((eq transition-direction 'night-to-day) mg-night-themes)
                    ((eq transition-direction 'day-to-night) mg-day-themes))))
        (mg--pickup-random-color-theme themes)
        (run-at-time (mg--calculate-time-to-switch-theme transition-direction) nil #'mg--theme-set-dynamically)))

(provide 'lib-colorscheme)
;;; lib-colorscheme.el ends here
