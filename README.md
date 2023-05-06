
# Table of Contents

1.  [Features](#orgd8ad978)
    1.  [Blazing fast.](#orgd7319e8)
    2.  [Robust](#org88ea411)
    3.  [Compatability](#org8725a3e)
    4.  [Feature rich](#org6e67d79)
    5.  [Be wild](#org10a7106)
2.  [Showcase](#org8fd3f6f)
    1.  [Welcome screen](#org7d7789d)
    2.  [Code Navigation](#orgc414324)
    3.  [Data Science](#org8223d16)
    4.  [Orgmode](#orga02e602)
3.  [TODOs](#orge3a52ba)
    1.  [Update `lisp-indent-function` from Doomemacs.](#orgf4f5d3d)
    2.  [Report `org-capture` bugs when inserting entries into table to upstream.](#orga1a0a3f)
    3.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#orgf31f346)
    4.  [Lazily load third-party plugins for `evil`.](#orgc71fab0)
    5.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#org040a5d6)
    6.  [Implement thread-folding for `mu4e`.](#org1338e1c)
4.  [Notes](#orgf0f8cf7)
    1.  [Startup speed](#org5a72c84)
    2.  [Naming conventions (WIP)](#org60cdbe1)
    3.  [Corfu or Company?](#orgf4b51d6)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="orgd8ad978"></a>

# Features


<a id="orgd7319e8"></a>

## Blazing fast.

With TTY starting in 0.32 seconds on a MacBook Air (M1, 2020), 0.73
seconds on a VPS with a 1-core CPU and 1 GB RAM, and 0.22 seconds on
WSL with an Intel i7-1185G7 and 32 GB RAM, TTY is exceptionally
fast. The GUI is just as quick, starting in 0.44 seconds on Mac M1 and
0.31 seconds on WSLg. For more information on startup speed, see
[4.1](#org5a72c84).


<a id="org88ea411"></a>

## Robust

Package versions are locked and under version control, so no breaking
changes are expected.


<a id="org8725a3e"></a>

## Compatability

This configuration works well on both TTY and GUI. Compatibility on
TTY is not compromised, while GUI features, including `xwidget`, are
also well-configured.


<a id="org6e67d79"></a>

## Feature rich

A blazing fast startup speed doesn&rsquo;t mean it is a lite and minimal
configuration.  Instead, it is &ldquo;heavy&rdquo; and feature rich, including:

-   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.

-   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.

-   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.

-   In-buffer autocompletion frontend based on `company` (see [4.3](#orgf4b51d6)).

-   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).

-   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="org10a7106"></a>

## Be wild

Randomly select a theme from a curated list each time you start up and
automatically switches between day and night themes at scheduled time.
Additionally, the displayed verses on the welcome screen is also
randomized with each launch. Have a fresh experience at every time. Be
casual and wild!


<a id="org8fd3f6f"></a>

# Showcase


<a id="org7d7789d"></a>

## Welcome screen

![img](assets/welcome-screen.png)

The welcome screen displays two randomly chosen verses from my
carefully chosen collection.  This serves as a scratch buffer where
you can perform Lisp evaluations. Frequently used commands are also
listed, allowing for convenient execution by simply clicking on the
corresponding button.


<a id="orgc414324"></a>

## Code Navigation

![img](assets/lsp-ctags.png)

Making use of LSP and Ctags, navigating code is a breeze. The file
tree is displayed on the leftmost window through `dired-sidebar`, and
the bottom right window showcases the references of a selected symbol
via `LSP find references` (the Emacs command is
`xref-find-references`). In the central floating window, a preview of
the definition of the chosen symbol is displayed with the aid of
`ctags` (the Emacs command is `citre-peek`).


<a id="org8223d16"></a>

## Data Science

![img](assets/data-science.png)

A typical workflow in data science involves multiple components. The
top right window showcases an embedded xwidget widget that displays
the HTML visualization created via `plotly`. In the bottom left window
rests the R REPL console where you can send your code for
execution. Meanwhile, the bottom right window features a chatgpt REPL
console (via [aichat](https://github.com/sigoden/aichat)). I specify
the aichat mode as `exp-code-e` to prompt chatgpt to provide an
explanation of the code you sent.


<a id="orga02e602"></a>

## Orgmode

![img](assets/reveal-js.png)

Write prose in `orgmode`, and export it into `reveal.js` presentation.
The right window displays the HTML slides using xwidget
webkit. Preview slides in emacs without the need to open GUI browser
anymore.


<a id="orge3a52ba"></a>

# TODOs


<a id="orgf4f5d3d"></a>

## Update `lisp-indent-function` from Doomemacs.


<a id="orga1a0a3f"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="orgf31f346"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="orgc71fab0"></a>

## Lazily load third-party plugins for `evil`.


<a id="org040a5d6"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="org1338e1c"></a>

## Implement thread-folding for `mu4e`.


<a id="orgf0f8cf7"></a>

# Notes


<a id="org5a72c84"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However, note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="org60cdbe1"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="orgf4b51d6"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

