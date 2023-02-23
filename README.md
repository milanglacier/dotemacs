
# Table of Contents

1.  [Features](#org33a569c)
2.  [Notes](#org8f92494)
    1.  [Startup speed](#org57544d8)
    2.  [Naming conventions (WIP)](#org7c73a58)
    3.  [Corfu or Company?](#orgabfbe35)



<a id="org33a569c"></a>

# Features

-   **Blazingly fast**.
    
    TTY starts in 0.3s on Mac M1 and 0.8s on a VPS with 1 core CPU and
    1GB RAM. The GUI starts in 0.45s on Mac M1. You can even `export
      EDITOR="emacs -nw"` and feel no perceptible startup difference
    comparing to vim! (See [2.1](#org57544d8) for additional details.)

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
    
    -   Modal editing ecosystem in everywhere powered by `evil` and many other extensions.
    
    -   Leader key and localleader key centered keybinding scheme powered by `general`.
    
    -   In-buffer autocompletion frontend based on `company` (see [2.3](#orgabfbe35)).
    
    -   Code completion and navigation based on `eglot` (lsp) and `citre` (ctags).
    
    -   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="org8f92494"></a>

# Notes


<a id="org57544d8"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="org7c73a58"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="orgabfbe35"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

