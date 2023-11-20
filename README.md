
# Table of Contents

1.  [Termux Setup](#orga234110)
    1.  [Install `xclip`](#org1ac610a)
    2.  [Build `efm-langserver`](#orgf09a5b5)
    3.  [Build `emacs-zmq`](#orgca4e868)
2.  [Features](#org8f0f086)
    1.  [Blazing fast.](#org6e1c082)
    2.  [Robust](#orgbc2a4a1)
    3.  [Compatability](#org1f71fa7)
    4.  [Feature rich](#org0d04998)
    5.  [Be wild](#org0516325)
3.  [Showcase](#orgade50db)
    1.  [Welcome screen](#org58a58b2)
    2.  [Code Navigation](#orga390d15)
    3.  [Data Science](#orgb315ed6)
    4.  [Orgmode](#orge3e5985)
4.  [TODOs](#org3b2ab0a)
    1.  [Report `org-capture` bugs when inserting entries into table to upstream.](#orgcc4e89a)
    2.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#orgd75ae6d)
    3.  [Lazily load third-party plugins for `evil`.](#org71011c1)
    4.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#org68ce2c4)
    5.  [Implement thread-folding for `mu4e`.](#org333fc80)
5.  [Prerequisites](#orga401710)
6.  [Discussion](#org5bc1f32)
7.  [Notes](#org677f6db)
    1.  [Startup speed](#org8765a8a)
    2.  [Naming conventions (WIP)](#org07e7e48)
    3.  [Corfu or Company?](#org4b802a9)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="orga234110"></a>

# Termux Setup


<a id="org1ac610a"></a>

## Install `xclip`

To make the clipboard work:

    pkg in x11-repo
    pkg in xclip


<a id="orgf09a5b5"></a>

## Build `efm-langserver`

Due to an exsiting issue of golang, you cannot use the official build
of `efm-langserver`. You must manually compile the `efm-langserver` from
source. You do not need to compile this program on termux. Instead you
can cross compile the binary in any platform by `GOOS=android go build`.


<a id="orgca4e868"></a>

## Build `emacs-zmq`

To succesfully build emacs-zmq, make sure you have installed the following packages:

    package install autoconf automake libtools pkg-config libzmq


<a id="org8f0f086"></a>

# Features


<a id="org6e1c082"></a>

## Blazing fast.

With TTY starting in 0.32 seconds on a MacBook Air (M1, 2020), 0.73
seconds on a VPS with a 1-core CPU and 1 GB RAM, and 0.22 seconds on
WSL with an Intel i7-1185G7 and 32 GB RAM, TTY is exceptionally
fast. The GUI is just as quick, starting in 0.44 seconds on Mac M1 and
0.31 seconds on WSLg. For more information on startup speed, see
[7.1](#org8765a8a).


<a id="orgbc2a4a1"></a>

## Robust

Package versions are locked and under version control, so no breaking
changes are expected.


<a id="org1f71fa7"></a>

## Compatability

This configuration works well on both TTY and GUI. Compatibility on
TTY is not compromised, while GUI features, including `xwidget`, are
also well-configured.


<a id="org0d04998"></a>

## Feature rich

A blazing fast startup speed doesn&rsquo;t mean it is a lite and minimal
configuration.  Instead, it is &ldquo;heavy&rdquo; and feature rich, including:

-   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.

-   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.

-   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.

-   In-buffer autocompletion frontend based on `company` (see [7.3](#org4b802a9)).

-   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).

-   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="org0516325"></a>

## Be wild

Randomly select a theme from a curated list each time you start up and
automatically switches between day and night themes at scheduled time.
Additionally, the displayed verses on the welcome screen is also
randomized with each launch. Have a fresh experience at every time. Be
casual and wild!


<a id="orgade50db"></a>

# Showcase


<a id="org58a58b2"></a>

## Welcome screen

![img](assets/welcome-screen.png)

The welcome screen displays two randomly chosen verses from my
carefully chosen collection.  This serves as a scratch buffer where
you can perform Lisp evaluations. Frequently used commands are also
listed, allowing for convenient execution by simply clicking on the
corresponding button.


<a id="orga390d15"></a>

## Code Navigation

![img](assets/lsp-ctags.png)

Making use of LSP and Ctags, navigating code is a breeze. The file
tree is displayed on the leftmost window through `dired-sidebar`, and
the bottom right window showcases the references of a selected symbol
via `LSP find references` (the Emacs command is
`xref-find-references`). In the central floating window, a preview of
the definition of the chosen symbol is displayed with the aid of
`ctags` (the Emacs command is `citre-peek`).


<a id="orgb315ed6"></a>

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


<a id="orge3e5985"></a>

## Orgmode

![img](assets/reveal-js.png)

Write prose in `orgmode`, and export it into `reveal.js` presentation.
The right window displays the HTML slides using xwidget
webkit. Preview slides in emacs without the need to open GUI browser
anymore.


<a id="org3b2ab0a"></a>

# TODOs


<a id="orgcc4e89a"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="orgd75ae6d"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="org71011c1"></a>

## Lazily load third-party plugins for `evil`.


<a id="org68ce2c4"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="org333fc80"></a>

## DONE Implement thread-folding for `mu4e`.


<a id="orga401710"></a>

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


<a id="org5bc1f32"></a>

# Discussion

-   It is recommended to use the mailing list `~northyear/.emacs.d-devel@lists.sr.ht`.
-   Alternatively, you are also welcome to open a Github issue.


<a id="org677f6db"></a>

# Notes


<a id="org8765a8a"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However, note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="org07e7e48"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="org4b802a9"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

