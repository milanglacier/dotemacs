
# Table of Contents

1.  [Features](#org4cf7d18)
2.  [TODOs](#orgd85159a)
    1.  [Update `ltex-ls` when the bug has been fixed.](#orgf079811)
    2.  [Once evil-collection addresses the compatibility issue caused by the breaking changes in mu, update mu to version 1.10.](#org028bab2)
    3.  [Update `lisp-indent-function` from Doomemacs.](#org6f33072)
    4.  [Report `org-capture` bugs when inserting entries into table to upstream.](#org930d55e)
    5.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#orge75213f)
    6.  [Lazily load third-party plugins for `evil`.](#orgb68b33b)
    7.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#org3dc1f75)
    8.  [Implement thread-folding for `mu4e`.](#orge71d0ae)
3.  [Notes](#org1a6201f)
    1.  [Startup speed](#orgcfe5bf3)
    2.  [Naming conventions (WIP)](#org56096ac)
    3.  [Corfu or Company?](#orgf63a6ac)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="org4cf7d18"></a>

# Features

-   **Blazing fast**.
    
    TTY starts in 0.32s on MacBook Air (M1, 2020), 0.73s on a VPS with 1
    core CPU and 1 GB RAM, and 0.30s on WSL with Intel i7-1185G7 and 32
    GB RAM. The GUI starts in 0.44s on Mac M1. You can even `export
      EDITOR="emacs -nw"` and feel no perceptible startup difference
    comparing to vim! (See [3.1](#orgcfe5bf3) for additional details.)

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
    
    -   In-buffer autocompletion frontend based on `company` (see [3.3](#orgf63a6ac)).
    
    -   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).
    
    -   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="orgd85159a"></a>

# TODOs


<a id="orgf079811"></a>

## Update `ltex-ls` when the bug has been fixed.


<a id="org028bab2"></a>

## Once evil-collection addresses the compatibility issue caused by the breaking changes in mu, update mu to version 1.10.


<a id="org6f33072"></a>

## Update `lisp-indent-function` from Doomemacs.


<a id="org930d55e"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="orge75213f"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="orgb68b33b"></a>

## Lazily load third-party plugins for `evil`.


<a id="org3dc1f75"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="orge71d0ae"></a>

## Implement thread-folding for `mu4e`.


<a id="org1a6201f"></a>

# Notes


<a id="orgcfe5bf3"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However, note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="org56096ac"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="orgf63a6ac"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

