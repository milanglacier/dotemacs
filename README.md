
# Table of Contents

1.  [Features](#org4b1d649)
2.  [TODOs](#orgfc6dd83)
    1.  [Update `lisp-indent-function` from Doomemacs.](#org682ec0a)
    2.  [Report `org-capture` bugs when inserting entries into table to upstream.](#org35f0b6c)
    3.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#org66a1109)
    4.  [Update `ltex-ls` when the bug has been fixed.](#org9a5ff59)
    5.  [Lazily load third-party plugins for `evil`.](#orgd7f0ff9)
    6.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#orgd342c44)
    7.  [Implement thread-folding for `mu4e`.](#org9c1056f)
3.  [Notes](#org4f81b65)
    1.  [Startup speed](#orgdedab22)
    2.  [Naming conventions (WIP)](#orgeb70cce)
    3.  [Corfu or Company?](#org1f09348)



<a id="org4b1d649"></a>

# Features

-   **Blazing fast**.
    
    TTY starts in 0.32s on MacBook Air (M1, 2020), 0.73s on a VPS with 1
    core CPU and 1 GB RAM, and 0.30s on WSL with Intel i7-1185G7 and 32
    GB RAM. The GUI starts in 0.44s on Mac M1. You can even `export
      EDITOR="emacs -nw"` and feel no perceptible startup difference
    comparing to vim! (See [3.1](#orgdedab22) for additional details.)

-   **Robust**
    
    Package versions are locked and under version control, so no
    breaking changes are expected.

-   **All-around**
    
    This configuration works well on both TTY and GUI. Compatibility on
    TTY is not compromised, while GUI features, including `xwidget`, are
    also well-configured.

-   **Feature rich**
    
    A blazing fast startup speed doesn&rsquo;t mean it is a lite and minimal
    configuration.  Instead, it is &ldquo;heavy&rdquo; and feature rich, including:
    
    -   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.
    
    -   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.
    
    -   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.
    
    -   In-buffer autocompletion frontend based on `company` (see [3.3](#org1f09348)).
    
    -   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).
    
    -   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="orgfc6dd83"></a>

# TODOs


<a id="org682ec0a"></a>

## Update `lisp-indent-function` from Doomemacs.


<a id="org35f0b6c"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="org66a1109"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="org9a5ff59"></a>

## Update `ltex-ls` when the bug has been fixed.


<a id="orgd7f0ff9"></a>

## Lazily load third-party plugins for `evil`.


<a id="orgd342c44"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="org9c1056f"></a>

## Implement thread-folding for `mu4e`.


<a id="org4f81b65"></a>

# Notes


<a id="orgdedab22"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However, note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="orgeb70cce"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="org1f09348"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

