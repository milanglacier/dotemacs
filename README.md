
# Table of Contents

1.  [Features](#org4906f14)
2.  [TODOs](#org089665e)
    1.  [Update `lisp-indent-function` from doomemacs.](#orge6a6981)
    2.  [Report `org-capture` bugs when inserting entries into table to upstream.](#orga841d88)
    3.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#orga5ff0c1)
    4.  [Update `ltex-ls` when the bug has been fixed.](#org7ad5409)
    5.  [Lazily load third-party plugins for `evil`.](#orgc7005ee)
    6.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#orgff8a373)
    7.  [Implement thread-folding for `mu4e`.](#org0f8d152)
3.  [Notes](#org71d63e1)
    1.  [Startup speed](#orge359a5c)
    2.  [Naming conventions (WIP)](#org1e9597c)
    3.  [Corfu or Company?](#org54e60c6)



<a id="org4906f14"></a>

# Features

-   **Blazingly fast**.
    
    TTY starts in 0.32s on Mac M1 and 0.73s on a VPS with 1 core CPU and
    1GB RAM. The GUI starts in 0.44s on Mac M1. You can even `export
      EDITOR="emacs -nw"` and feel no perceptible startup difference
    comparing to vim! (See [3.1](#orge359a5c) for additional details.)

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
    
    -   In-buffer autocompletion frontend based on `company` (see [3.3](#org54e60c6)).
    
    -   Code completion and navigation based on `eglot` (lsp) and `citre` (ctags).
    
    -   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="org089665e"></a>

# TODOs


<a id="orge6a6981"></a>

## Update `lisp-indent-function` from doomemacs.


<a id="orga841d88"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="orga5ff0c1"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="org7ad5409"></a>

## Update `ltex-ls` when the bug has been fixed.


<a id="orgc7005ee"></a>

## Lazily load third-party plugins for `evil`.


<a id="orgff8a373"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="org0f8d152"></a>

## Implement thread-folding for `mu4e`.


<a id="org71d63e1"></a>

# Notes


<a id="orge359a5c"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="org1e9597c"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="org54e60c6"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

