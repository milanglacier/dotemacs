
# Table of Contents

1.  [Features](#org3ade0fe)
    1.  [Blazing fast.](#org5e1a6a0)
    2.  [Robust](#orge13015a)
    3.  [Compatability](#org5af64d1)
    4.  [Feature rich](#orgce41bba)
    5.  [Be wild](#orgac66783)
2.  [Showcase](#org00a2ba0)
    1.  [Welcome screen](#org306daad)
    2.  [Code Navigation](#org5cd20dd)
    3.  [Data Science](#orgd023c03)
    4.  [Orgmode](#org2a36ee2)
3.  [TODOs](#org1f4c8dd)
    1.  [Update `lisp-indent-function` from Doomemacs.](#org8a9b0f5)
    2.  [Report `org-capture` bugs when inserting entries into table to upstream.](#orgb07e954)
    3.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#orge43df95)
    4.  [Lazily load third-party plugins for `evil`.](#orge05fdf8)
    5.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#org52b436d)
    6.  [Implement thread-folding for `mu4e`.](#orgb10a1e3)
4.  [Prerequisites](#org2ac272b)
5.  [Notes](#orgd590802)
    1.  [Startup speed](#orga85c549)
    2.  [Naming conventions (WIP)](#orge04f9ac)
    3.  [Corfu or Company?](#org7d7cced)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="org3ade0fe"></a>

# Features


<a id="org5e1a6a0"></a>

## Blazing fast.

With TTY starting in 0.32 seconds on a MacBook Air (M1, 2020), 0.73
seconds on a VPS with a 1-core CPU and 1 GB RAM, and 0.22 seconds on
WSL with an Intel i7-1185G7 and 32 GB RAM, TTY is exceptionally
fast. The GUI is just as quick, starting in 0.44 seconds on Mac M1 and
0.31 seconds on WSLg. For more information on startup speed, see
[5.1](#orga85c549).


<a id="orge13015a"></a>

## Robust

Package versions are locked and under version control, so no breaking
changes are expected.


<a id="org5af64d1"></a>

## Compatability

This configuration works well on both TTY and GUI. Compatibility on
TTY is not compromised, while GUI features, including `xwidget`, are
also well-configured.


<a id="orgce41bba"></a>

## Feature rich

A blazing fast startup speed doesn&rsquo;t mean it is a lite and minimal
configuration.  Instead, it is &ldquo;heavy&rdquo; and feature rich, including:

-   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.

-   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.

-   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.

-   In-buffer autocompletion frontend based on `company` (see [5.3](#org7d7cced)).

-   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).

-   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="orgac66783"></a>

## Be wild

Randomly select a theme from a curated list each time you start up and
automatically switches between day and night themes at scheduled time.
Additionally, the displayed verses on the welcome screen is also
randomized with each launch. Have a fresh experience at every time. Be
casual and wild!


<a id="org00a2ba0"></a>

# Showcase


<a id="org306daad"></a>

## Welcome screen

![img](assets/welcome-screen.png)

The welcome screen displays two randomly chosen verses from my
carefully chosen collection.  This serves as a scratch buffer where
you can perform Lisp evaluations. Frequently used commands are also
listed, allowing for convenient execution by simply clicking on the
corresponding button.


<a id="org5cd20dd"></a>

## Code Navigation

![img](assets/lsp-ctags.png)

Making use of LSP and Ctags, navigating code is a breeze. The file
tree is displayed on the leftmost window through `dired-sidebar`, and
the bottom right window showcases the references of a selected symbol
via `LSP find references` (the Emacs command is
`xref-find-references`). In the central floating window, a preview of
the definition of the chosen symbol is displayed with the aid of
`ctags` (the Emacs command is `citre-peek`).


<a id="orgd023c03"></a>

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


<a id="org2a36ee2"></a>

## Orgmode

![img](assets/reveal-js.png)

Write prose in `orgmode`, and export it into `reveal.js` presentation.
The right window displays the HTML slides using xwidget
webkit. Preview slides in emacs without the need to open GUI browser
anymore.


<a id="org1f4c8dd"></a>

# TODOs


<a id="org8a9b0f5"></a>

## Update `lisp-indent-function` from Doomemacs.


<a id="orgb07e954"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="orge43df95"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="orge05fdf8"></a>

## Lazily load third-party plugins for `evil`.


<a id="org52b436d"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="orgb10a1e3"></a>

## Implement thread-folding for `mu4e`.


<a id="org2ac272b"></a>

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


<a id="orgd590802"></a>

# Notes


<a id="orga85c549"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However, note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="orge04f9ac"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="org7d7cced"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

