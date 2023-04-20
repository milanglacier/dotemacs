;;; my-init-colorscheme.el -*- lexical-binding: t; -*-

(straight-use-package 'doom-themes)
(straight-use-package 'spacemacs-theme)
(straight-use-package 'ef-themes)

(setq my$day-themes
      '(doom-solarized-light
        spacemacs-light
        doom-one-light
        ef-tritanopia-light
        modus-operandi
        ef-cyprus
        ef-light)
      my$night-themes
      '(spacemacs-dark
        doom-one
        doom-nord-aurora
        doom-opera
        modus-vivendi
        ef-dark)
      )

(my:theme-set-dynamically)

(use-package doom-themes-ext-org
    :if (string-match-p "doom" (symbol-name my$selected-theme))
    :after org
    :demand t)

;; HACK: The following two themes use the background color code
;; "#000000", which renders as the background color of your terminal
;; color scheme instead of the true black. To create a more visually
;; unified experience, we will use a color that is very close to pure
;; black, so that the background appears similar to the authenticated
;; background in the GUI.
(when (and (not (display-graphic-p))
           (memq my$selected-theme '(modus-vivendi ef-dark)))
    (set-background-color "#050000"))

(provide 'my-init-colorscheme)
;;; my-init-colorscheme.el ends here
