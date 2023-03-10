
# Table of Contents

1.  [Features](#orge54d4dd)
2.  [Notes](#org365536b)
    1.  [Startup speed](#orge45d018)
    2.  [Naming conventions (WIP)](#org5664630)
    3.  [lsp-bridge(experimental)](#org5f19f67)



<a id="orge54d4dd"></a>

# Features

-   **Blazingly fast**.
    
    TTY starts in 0.32s on Mac M1 and 0.73s on a VPS with 1 core CPU and
    1GB RAM. The GUI starts in 0.44s on Mac M1. You can even `export
      EDITOR="emacs -nw"` and feel no perceptible startup difference
    comparing to vim! (See [2.1](#orge45d018) for additional details.)

-   **Robust**
    
    Package versions are locked and under version control, so no
    breaking changes are expected.

-   **All-around**
    
    This configuration works well on both TTY and GUI. Compatability on
    TTY is not compromised, while GUI features, including `xwidget`, are
    also well-configured.

-   **Feature rich**
    
    A blazingly fast startup speed doesn&rsquo;t mean it is a lite and minimal
    configuration.  Instead it is &ldquo;heavy&rdquo; and feature rich, including:
    
    -   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.
    
    -   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.
    
    -   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.
    
    -   In-buffer autocompletion frontend based on `acm` (see [2.3](#org5f19f67)).
    
    -   Code completion and navigation based on `lsp-bridge` (lsp) and `citre` (ctags).
    
    -   Integration with `lsp-bridge` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="org365536b"></a>

# Notes


<a id="orge45d018"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="org5664630"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="org5f19f67"></a>

## lsp-bridge(experimental)

`lsp-bridge` is an all-in-one tool that functions as both an lsp backend
and an autocompletion backend (referred to as `acm`). By utilizing an
external process (python) with advanced multi-threading features for
time-consuming tasks, it provides the smoothest completion experience
in emacs. However, the installation of `lsp-bridge` requires system
dependencies on python and multiple python packages, resulting in
added complexities. Therefore, `lsp-bridge` is considered an opt-in
feature and is enabled in this separate branch.

