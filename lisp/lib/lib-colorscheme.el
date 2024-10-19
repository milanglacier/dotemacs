;;; lib-colorscheme.el -*- lexical-binding: t; -*-

(defvar my$day-themes
    '(leuven)
    "list of colorschemes to be used in light mode")

(defvar my$night-themes
    '(tango-dark)
    "list of colorschemes to be used in dark mode")

(defvar my$selected-theme nil
    "selected theme")

(defun my:pickup-random-color-theme (themes)
    (let* ((theme (nth (random (length themes)) themes)))
        (disable-theme my$selected-theme)
        (setq my$selected-theme theme)
        (load-theme theme t)))

(defvar my$day-to-night-o-clock 23 "The time to switch from day to night themes")
(defvar my$night-to-day-o-clock 7 "The time to switch from night to day themes")

(defun my:calculate-time-to-switch-theme (transit-direction)
    "Calculate the time remaining until the next theme switch, based
on the current time, the direction of the transition (`day-to-night'
or `night-to-day'), and the settings of `my$day-to-night-o-clock' and
`my$night-to-day-o-clock'.  Returns a formatted string with the amount
of time remaining until the switch."
    (let ((current-min (string-to-number (format-time-string "%M")))
          (current-hour (string-to-number (format-time-string "%H")))
          mins-to-next-hour
          hours-to-switch-theme
          time-to-switch)
        (setq mins-to-next-hour (- 60 current-min))
        (when (eq transit-direction 'day-to-night)
            (setq hours-to-switch-theme (- my$day-to-night-o-clock (1+ current-hour))))
        (when (eq transit-direction 'night-to-day)
            (if (>= current-hour my$day-to-night-o-clock)
                    (setq hours-to-switch-theme (+ my$night-to-day-o-clock
                                                   (- 24 (1+ current-hour))))
                (setq hours-to-switch-theme (- my$night-to-day-o-clock (1+ current-hour)))))
        (setq time-to-switch (format "%d hours %d minutes"
                                     hours-to-switch-theme
                                     mins-to-next-hour))
        time-to-switch))

;;;###autoload
(defun my:theme-set-dynamically ()
    "Select a theme at random from `my$day-themes' or
`my$night-themes', depending on the current time of day. The
environment variable `CURRENT_BACKGROUND' can override current time.
The time periods for day and night are specified by
`my$day-to-night-o-clock' and `my$night-to-day-o-clock',
respectively."
    (let* ((transition-direction
            (if (or (>= (string-to-number (format-time-string "%H"))
                        my$day-to-night-o-clock)
                    (< (string-to-number (format-time-string "%H"))
                       my$night-to-day-o-clock))
                    'night-to-day
                'day-to-night))
           (themes (cond
                    ((equal (getenv "CURRENT_BACKGROUND") "day") my$day-themes)
                    ((equal (getenv "CURRENT_BACKGROUND") "night") my$night-themes)
                    ((eq transition-direction 'night-to-day) my$night-themes)
                    ((eq transition-direction 'day-to-night) my$day-themes))))
        (my:pickup-random-color-theme themes)
        (run-at-time (my:calculate-time-to-switch-theme transition-direction) nil #'my:theme-set-dynamically)))

(provide 'lib-colorscheme)
;;; lib-colorscheme.el ends here
