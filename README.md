
# Table of Contents

1.  [Features](#orgbe69624)
    1.  [Blazing fast.](#orgd18593a)
    2.  [Robust](#org54caff1)
    3.  [Compatability](#org4cd049c)
    4.  [Feature rich](#org8fc00b1)
    5.  [Be wild](#org3fd66c7)
2.  [Showcase](#orgb108adb)
    1.  [Welcome screen](#org1529b3c)
    2.  [Code Navigation](#orga8dfc44)
    3.  [Data Science](#orge89f19b)
    4.  [Orgmode](#orgcc174c7)
3.  [Email Setup](#orgfdb956b)
4.  [TODOs](#org7d815a4)
    1.  [Report `org-capture` bugs when inserting entries into table to upstream.](#org4b17639)
    2.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#org1a5051b)
    3.  [Lazily load third-party plugins for `evil`.](#org0453706)
    4.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#orge64dc27)
5.  [Prerequisites](#org3c8cce5)
6.  [External Dependencies](#orga7061b8)
7.  [Discussion](#org7681250)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="orgbe69624"></a>

# Features


<a id="orgd18593a"></a>

## Blazing fast.

With TTY starting in 0.30 seconds on a MacBook Air (M1, 2020), 0.73
seconds on a VPS with a 1-core CPU and 1 GB RAM, and 0.21 seconds on
WSL with Intel i7 CPU and 32 GB RAM, TTY is exceptionally fast. The
GUI is just as quick, starting in 0.44 seconds on Mac M1 and 0.30
seconds on WSLg.


<a id="org54caff1"></a>

## Robust

Package versions are locked and under version control, so no breaking
changes are expected.


<a id="org4cd049c"></a>

## Compatability

This configuration works well on both TTY and GUI. Compatibility on
TTY is not compromised, while GUI features, including `xwidget`, are
also well-configured.


<a id="org8fc00b1"></a>

## Feature rich

A blazing fast startup speed doesn&rsquo;t mean it is a lite and minimal
configuration.  Instead, it is &ldquo;heavy&rdquo; and feature rich, including:

-   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.

-   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.

-   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.

-   In-buffer autocompletion frontend based on `company`

-   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).

-   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="org3fd66c7"></a>

## Be wild

Randomly select a theme from a curated list each time you start up and
automatically switches between day and night themes at scheduled time.
Additionally, the displayed verses on the welcome screen is also
randomized with each launch. Have a fresh experience at every time. Be
casual and wild!


<a id="orgb108adb"></a>

# Showcase


<a id="org1529b3c"></a>

## Welcome screen

![img](assets/welcome-screen.png)

The welcome screen displays two randomly chosen verses from my
carefully chosen collection.  This serves as a scratch buffer where
you can perform Lisp evaluations. Frequently used commands are also
listed, allowing for convenient execution by simply typing the hint
key.


<a id="orga8dfc44"></a>

## Code Navigation

![img](assets/lsp-ctags.png)

Making use of LSP and Ctags, navigating code is a breeze. The file
tree is displayed on the leftmost window through `dired-sidebar`, and
the bottom right window showcases the references of a selected symbol
via `LSP find references` (the Emacs command is
`xref-find-references`). In the central floating window, a preview of
the definition of the chosen symbol is displayed with the aid of
`ctags` (the Emacs command is `citre-peek`).


<a id="orge89f19b"></a>

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


<a id="orgcc174c7"></a>

## Orgmode

![img](assets/reveal-js.png)

Write prose in `orgmode`, and export it into `reveal.js` presentation.
The right window displays the HTML slides using xwidget
webkit. Preview slides in emacs without the need to open GUI browser
anymore.


<a id="orgfdb956b"></a>

# Email Setup

I use `notmuch` as my email client. For a comprehensive overview of my
email setup, please refer to the [email.org](./email.md) file
located in the current directory.


<a id="org7d815a4"></a>

# TODOs


<a id="org4b17639"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="org1a5051b"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="org0453706"></a>

## Lazily load third-party plugins for `evil`.


<a id="orge64dc27"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="org3c8cce5"></a>

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
    execute the command `M-x mg-treesit-install-all-language-grammar` to
    install all the language grammars that are currently in use.


<a id="orga7061b8"></a>

# External Dependencies

This configuration requires the following core dependencies:

-   A C compiler for building the treesitter grammar
-   universal-ctags
-   ripgrep

There are also additional language-specific dependencies only required
when working with specific languages. These typically include language
servers, formatters, and linters. For instance, you would need
`basedpyright`, `debugpy`, `black`, and `ipython` for python.


<a id="org7681250"></a>

# Discussion

-   It is recommended to use the mailing list `~northyear/.emacs.d-devel@lists.sr.ht`.
-   Alternatively, you are also welcome to open a Github issue.

