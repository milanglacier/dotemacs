;;; config-colorscheme.el -*- lexical-binding: t; -*-

(straight-use-package 'doom-themes)
(straight-use-package 'ef-themes)

(setq mg-day-themes
      '(doom-solarized-light
        doom-one-light
        ef-tritanopia-light
        ef-cyprus
        ef-light)
      mg-night-themes
      '(doom-one
        doom-nord-aurora
        doom-tokyo-night
        ef-dream
        ef-autumn
        doom-opera))

(when (display-graphic-p)
    (push 'modus-vivendi mg-night-themes)
    (push 'modus-operandi mg-day-themes)
    (push 'ef-dark mg-night-themes))

(mg--theme-set-dynamically)

(use-package doom-themes-ext-org
    :if (string-match-p "doom" (symbol-name mg-selected-theme))
    :after org
    :demand t)

(provide 'config-colorscheme)
;;; config-colorscheme.el ends here
