
# Table of Contents

1.  [Features](#org1404fcc)
    1.  [Blazing fast.](#org2c9c970)
    2.  [Robust](#org10357e3)
    3.  [Compatability](#org0bc3cfd)
    4.  [Feature rich](#org0255bd8)
    5.  [Be wild](#org2f86df9)
2.  [Showcase](#org6332f87)
    1.  [Welcome screen](#org6a244be)
    2.  [Code Navigation](#orgde56be2)
    3.  [Data Science](#org5271bf8)
    4.  [Orgmode](#org418212e)
3.  [TODOs](#org362d562)
    1.  [Update `lisp-indent-function` from Doomemacs.](#orgee3bf5e)
    2.  [Report `org-capture` bugs when inserting entries into table to upstream.](#org5ea6efb)
    3.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#orga2d58ff)
    4.  [Lazily load third-party plugins for `evil`.](#org8365810)
    5.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#org520faa7)
    6.  [Implement thread-folding for `mu4e`.](#orgd240dcd)
4.  [Notes](#orge1044db)
    1.  [Startup speed](#orga18478e)
    2.  [Naming conventions (WIP)](#org1c14280)
    3.  [Corfu or Company?](#org06aaf7e)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="org1404fcc"></a>

# Features


<a id="org2c9c970"></a>

## Blazing fast.

TTY starts in 0.32s on MacBook Air (M1, 2020), 0.73s on a VPS with 1
core CPU and 1 GB RAM, and 0.30s on WSL with Intel i7-1185G7 and 32 GB
RAM. The GUI starts in 0.44s on Mac M1. You can even `export
EDITOR="emacs -nw"` and feel no perceptible startup difference
comparing to vim! (See [4.1](#orga18478e) for additional details.)


<a id="org10357e3"></a>

## Robust

Package versions are locked and under version control, so no breaking
changes are expected.


<a id="org0bc3cfd"></a>

## Compatability

This configuration works well on both TTY and GUI. Compatibility on
TTY is not compromised, while GUI features, including `xwidget`, are
also well-configured.


<a id="org0255bd8"></a>

## Feature rich

A blazing fast startup speed doesn&rsquo;t mean it is a lite and minimal
configuration.  Instead, it is &ldquo;heavy&rdquo; and feature rich, including:

-   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.

-   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.

-   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.

-   In-buffer autocompletion frontend based on `company` (see [4.3](#org06aaf7e)).

-   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).

-   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="org2f86df9"></a>

## Be wild

Randomly select a theme from a curated list each time you start up and
automatically switches between day and night themes at scheduled time.
Additionally, the displayed verses on the welcome screen is also
randomized with each launch. Have a fresh experience at every time. Be
casual and wild!


<a id="org6332f87"></a>

# Showcase


<a id="org6a244be"></a>

## Welcome screen

![img](assets/welcome-screen.png)

The welcome screen displays two randomly chosen verses from my
carefully chosen collection.  This serves as a scratch buffer where
you can perform Lisp evaluations. Frequently used commands are also
listed, allowing for convenient execution by simply clicking on the
corresponding button.


<a id="orgde56be2"></a>

## Code Navigation

![img](assets/lsp-ctags.png)

Making use of LSP and Ctags, navigating code is a breeze. The file
tree is displayed on the leftmost window through `dired-sidebar`, and
the bottom right window showcases the references of a selected symbol
via `LSP find references` (the Emacs command is
`xref-find-references`). In the central floating window, a preview of
the definition of the chosen symbol is displayed with the aid of
`ctags` (the Emacs command is `citre-peek`).


<a id="org5271bf8"></a>

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


<a id="org418212e"></a>

## Orgmode

![img](assets/reveal-js.png)

Write prose in `orgmode`, and export it into `reveal.js` presentation.
The right window displays the HTML slides using xwidget
webkit. Preview slides in emacs without the need to open GUI browser
anymore.


<a id="org362d562"></a>

# TODOs


<a id="orgee3bf5e"></a>

## Update `lisp-indent-function` from Doomemacs.


<a id="org5ea6efb"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="orga2d58ff"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="org8365810"></a>

## Lazily load third-party plugins for `evil`.


<a id="org520faa7"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="orgd240dcd"></a>

## Implement thread-folding for `mu4e`.


<a id="orge1044db"></a>

# Notes


<a id="orga18478e"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However, note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="org1c14280"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="org06aaf7e"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

