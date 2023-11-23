
# Table of Contents

1.  [`emacs-android` setup](#orgaaee539)
2.  [Termux Setup](#org32c86de)
    1.  [Install `xclip`](#orgdb30100)
    2.  [Build `efm-langserver`](#org58894e0)
3.  [Features](#org58de41a)
    1.  [Blazing fast.](#org9b75457)
    2.  [Robust](#org292ead5)
    3.  [Compatability](#org5a83ea5)
    4.  [Feature rich](#org2b05233)
    5.  [Be wild](#org06a7189)
4.  [Showcase](#org0702324)
    1.  [Welcome screen](#org1fe51ed)
    2.  [Code Navigation](#org325ed5b)
    3.  [Data Science](#org2a02d67)
    4.  [Orgmode](#orgc74b74a)
5.  [TODOs](#org8b3bd7b)
    1.  [Report `org-capture` bugs when inserting entries into table to upstream.](#org33e17ec)
    2.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#org8c64c42)
    3.  [Lazily load third-party plugins for `evil`.](#org565d50e)
    4.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#org2913915)
    5.  [Implement thread-folding for `mu4e`.](#org8a1e2f1)
6.  [Prerequisites](#orgea0866a)
7.  [Discussion](#orga75c0a2)
8.  [Notes](#org66c53b9)
    1.  [Startup speed](#org94ea2e5)
    2.  [Naming conventions (WIP)](#org1ac671f)
    3.  [Corfu or Company?](#orgbb9ed93)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="orgaaee539"></a>

# `emacs-android` setup

Please follow the [Instruction](https://sourceforge.net/projects/android-ports-for-gnu-emacs/files/termux/) to install `termux` and `emacs-android`.

To be specific:

> To install Emacs with Termux support, first remove any existing copy
> of Emacs (this will remove all data within Emacs&rsquo;s home directory, so
> make a backup beforehand!) and Termux, to avoid signature or user ID
> conflicts.  Next, install Termux <span class="underline">first</span>, then install Emacs.


<a id="org32c86de"></a>

# Termux Setup


<a id="orgdb30100"></a>

## Install `xclip`

To make the clipboard work:

    pkg in x11-repo
    pkg in xclip


<a id="org58894e0"></a>

## Build `efm-langserver`

Due to an exsiting issue of golang, you cannot use the official build
of `efm-langserver`. You must manually compile the `efm-langserver` from
source. You do not need to compile this program on termux. Instead you
can cross compile the binary in any platform by `GOOS=android go build`.


<a id="org58de41a"></a>

# Features


<a id="org9b75457"></a>

## Blazing fast.

With TTY starting in 0.32 seconds on a MacBook Air (M1, 2020), 0.73
seconds on a VPS with a 1-core CPU and 1 GB RAM, and 0.22 seconds on
WSL with an Intel i7-1185G7 and 32 GB RAM, TTY is exceptionally
fast. The GUI is just as quick, starting in 0.44 seconds on Mac M1 and
0.31 seconds on WSLg. For more information on startup speed, see
[8.1](#org94ea2e5).


<a id="org292ead5"></a>

## Robust

Package versions are locked and under version control, so no breaking
changes are expected.


<a id="org5a83ea5"></a>

## Compatability

This configuration works well on both TTY and GUI. Compatibility on
TTY is not compromised, while GUI features, including `xwidget`, are
also well-configured.


<a id="org2b05233"></a>

## Feature rich

A blazing fast startup speed doesn&rsquo;t mean it is a lite and minimal
configuration.  Instead, it is &ldquo;heavy&rdquo; and feature rich, including:

-   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.

-   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.

-   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.

-   In-buffer autocompletion frontend based on `company` (see [8.3](#orgbb9ed93)).

-   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).

-   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="org06a7189"></a>

## Be wild

Randomly select a theme from a curated list each time you start up and
automatically switches between day and night themes at scheduled time.
Additionally, the displayed verses on the welcome screen is also
randomized with each launch. Have a fresh experience at every time. Be
casual and wild!


<a id="org0702324"></a>

# Showcase


<a id="org1fe51ed"></a>

## Welcome screen

![img](assets/welcome-screen.png)

The welcome screen displays two randomly chosen verses from my
carefully chosen collection.  This serves as a scratch buffer where
you can perform Lisp evaluations. Frequently used commands are also
listed, allowing for convenient execution by simply clicking on the
corresponding button.


<a id="org325ed5b"></a>

## Code Navigation

![img](assets/lsp-ctags.png)

Making use of LSP and Ctags, navigating code is a breeze. The file
tree is displayed on the leftmost window through `dired-sidebar`, and
the bottom right window showcases the references of a selected symbol
via `LSP find references` (the Emacs command is
`xref-find-references`). In the central floating window, a preview of
the definition of the chosen symbol is displayed with the aid of
`ctags` (the Emacs command is `citre-peek`).


<a id="org2a02d67"></a>

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


<a id="orgc74b74a"></a>

## Orgmode

![img](assets/reveal-js.png)

Write prose in `orgmode`, and export it into `reveal.js` presentation.
The right window displays the HTML slides using xwidget
webkit. Preview slides in emacs without the need to open GUI browser
anymore.


<a id="org8b3bd7b"></a>

# TODOs


<a id="org33e17ec"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="org8c64c42"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="org565d50e"></a>

## Lazily load third-party plugins for `evil`.


<a id="org2913915"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="org8a1e2f1"></a>

## DONE Implement thread-folding for `mu4e`.


<a id="orgea0866a"></a>

# Prerequisites

-   This configuration is designed for Emacs 29 or newer versions.
-   Your Emacs must be built with Treesitter support for this
    configuration to work effectively. If you are using a widely used
    package manager, and said manager has updated Emacs to version 29 or
    later, it&rsquo;s highly probable that Treesitter is already built into
    the Emacs version provided via the package manager. We advise
    verifying the package specifications for exact details if you choose
    to leverage a package manager-built Emacs.
-   A separate installation is required for Treesitter grammar.  You can
    execute the command `M-x my~treesit-install-all-language-grammar` to
    install all the language grammars that are currently in use.


<a id="orga75c0a2"></a>

# Discussion

-   It is recommended to use the mailing list `~northyear/.emacs.d-devel@lists.sr.ht`.
-   Alternatively, you are also welcome to open a Github issue.


<a id="org66c53b9"></a>

# Notes


<a id="org94ea2e5"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However, note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="org1ac671f"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="orgbb9ed93"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

