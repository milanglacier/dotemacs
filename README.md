
# Table of Contents

1.  [Termux Setup](#org8db521c)
    1.  [Install `xclip`](#org1ff235e)
    2.  [Build `efm-langserver`](#org1505450)
    3.  [Setup `emacs-jupyter`](#org7472e87)
        1.  [Build `emacs-zmq`](#orgab9c349)
        2.  [Install `jupyter-console`](#org0b42d33)
2.  [Features](#org9a2d704)
    1.  [Blazing fast.](#org90e388e)
    2.  [Robust](#org8464bfe)
    3.  [Compatability](#org492ff78)
    4.  [Feature rich](#org503a67a)
    5.  [Be wild](#orga2e6a70)
3.  [Showcase](#orgc4333bf)
    1.  [Welcome screen](#orgeefc442)
    2.  [Code Navigation](#org3c75395)
    3.  [Data Science](#org4fc3344)
    4.  [Orgmode](#orge93727f)
4.  [TODOs](#org5d40f03)
    1.  [Report `org-capture` bugs when inserting entries into table to upstream.](#org08b5827)
    2.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#org1067e40)
    3.  [Lazily load third-party plugins for `evil`.](#org6714a05)
    4.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#org05d7b5f)
    5.  [Implement thread-folding for `mu4e`.](#org50a303c)
5.  [Prerequisites](#org341f26c)
6.  [Discussion](#org1bc23c9)
7.  [Notes](#orgcbf6f14)
    1.  [Startup speed](#org4a288c2)
    2.  [Naming conventions (WIP)](#org9027ca0)
    3.  [Corfu or Company?](#orgdddb834)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="org8db521c"></a>

# Termux Setup


<a id="org1ff235e"></a>

## Install `xclip`

To make the clipboard work:

    pkg in x11-repo
    pkg in xclip


<a id="org1505450"></a>

## Build `efm-langserver`

Due to an exsiting issue of golang, you cannot use the official build
of `efm-langserver`. You must manually compile the `efm-langserver` from
source. You do not need to compile this program on termux. Instead you
can cross compile the binary in any platform by `GOOS=android go build`.


<a id="org7472e87"></a>

## Setup `emacs-jupyter`


<a id="orgab9c349"></a>

### Build `emacs-zmq`

To succesfully build emacs-zmq, make sure you have installed the following packages:

    package install autoconf automake libtools pkg-config libzmq


<a id="org0b42d33"></a>

### Install `jupyter-console`

It is recommended to install `jupyter-console==6.6.3` instead of
`jupyter-notebook` or `jupyter-lab`, as the installation process for the
latter options is much more complex. Please refer to the [Discussion](https://www.reddit.com/r/termux/s/XTMDCf6bTF)
for more information.


<a id="org9a2d704"></a>

# Features


<a id="org90e388e"></a>

## Blazing fast.

With TTY starting in 0.32 seconds on a MacBook Air (M1, 2020), 0.73
seconds on a VPS with a 1-core CPU and 1 GB RAM, and 0.22 seconds on
WSL with an Intel i7-1185G7 and 32 GB RAM, TTY is exceptionally
fast. The GUI is just as quick, starting in 0.44 seconds on Mac M1 and
0.31 seconds on WSLg. For more information on startup speed, see
[7.1](#org4a288c2).


<a id="org8464bfe"></a>

## Robust

Package versions are locked and under version control, so no breaking
changes are expected.


<a id="org492ff78"></a>

## Compatability

This configuration works well on both TTY and GUI. Compatibility on
TTY is not compromised, while GUI features, including `xwidget`, are
also well-configured.


<a id="org503a67a"></a>

## Feature rich

A blazing fast startup speed doesn&rsquo;t mean it is a lite and minimal
configuration.  Instead, it is &ldquo;heavy&rdquo; and feature rich, including:

-   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.

-   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.

-   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.

-   In-buffer autocompletion frontend based on `company` (see [7.3](#orgdddb834)).

-   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).

-   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="orga2e6a70"></a>

## Be wild

Randomly select a theme from a curated list each time you start up and
automatically switches between day and night themes at scheduled time.
Additionally, the displayed verses on the welcome screen is also
randomized with each launch. Have a fresh experience at every time. Be
casual and wild!


<a id="orgc4333bf"></a>

# Showcase


<a id="orgeefc442"></a>

## Welcome screen

![img](assets/welcome-screen.png)

The welcome screen displays two randomly chosen verses from my
carefully chosen collection.  This serves as a scratch buffer where
you can perform Lisp evaluations. Frequently used commands are also
listed, allowing for convenient execution by simply clicking on the
corresponding button.


<a id="org3c75395"></a>

## Code Navigation

![img](assets/lsp-ctags.png)

Making use of LSP and Ctags, navigating code is a breeze. The file
tree is displayed on the leftmost window through `dired-sidebar`, and
the bottom right window showcases the references of a selected symbol
via `LSP find references` (the Emacs command is
`xref-find-references`). In the central floating window, a preview of
the definition of the chosen symbol is displayed with the aid of
`ctags` (the Emacs command is `citre-peek`).


<a id="org4fc3344"></a>

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


<a id="orge93727f"></a>

## Orgmode

![img](assets/reveal-js.png)

Write prose in `orgmode`, and export it into `reveal.js` presentation.
The right window displays the HTML slides using xwidget
webkit. Preview slides in emacs without the need to open GUI browser
anymore.


<a id="org5d40f03"></a>

# TODOs


<a id="org08b5827"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="org1067e40"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="org6714a05"></a>

## Lazily load third-party plugins for `evil`.


<a id="org05d7b5f"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="org50a303c"></a>

## DONE Implement thread-folding for `mu4e`.


<a id="org341f26c"></a>

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


<a id="org1bc23c9"></a>

# Discussion

-   It is recommended to use the mailing list `~northyear/.emacs.d-devel@lists.sr.ht`.
-   Alternatively, you are also welcome to open a Github issue.


<a id="orgcbf6f14"></a>

# Notes


<a id="org4a288c2"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However, note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="org9027ca0"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="orgdddb834"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

