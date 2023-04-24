
# Table of Contents

1.  [Features](#orgd8599c9)
    1.  [Blazing fast.](#org6f63803)
    2.  [Robust](#org706f77f)
    3.  [Compatability](#org144fac7)
    4.  [Feature rich](#org96337d3)
    5.  [Be wild](#orgc7d394e)
2.  [Showcase](#org28a074a)
    1.  [Welcome screen](#org60a36c2)
    2.  [Code Navigation](#orgd73b5bd)
    3.  [Data Science](#org480c02c)
    4.  [Orgmode](#org6f1a258)
3.  [TODOs](#orgfccc8ab)
    1.  [Remove my workaround hack once evil-collection addresses the compatibility issue caused by the breaking changes in mu 1.10+.](#org5f12229)
    2.  [Update `lisp-indent-function` from Doomemacs.](#org2d1de50)
    3.  [Report `org-capture` bugs when inserting entries into table to upstream.](#orgb99e389)
    4.  [Utilize the contextual information from previous code block when editing source block within markdown/org.](#org4d425a0)
    5.  [Lazily load third-party plugins for `evil`.](#orge25c76a)
    6.  [Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.](#org40bd2a0)
    7.  [Implement thread-folding for `mu4e`.](#org6c002e7)
4.  [Notes](#orgfa4929c)
    1.  [Startup speed](#orgb20703f)
    2.  [Naming conventions (WIP)](#orgda3e05f)
    3.  [Corfu or Company?](#org3a662f3)

If you&rsquo;re currently reading this README file in Markdown format, it
has been generated through `org-export`, from its original org
format. For the best experience, please consider reading the org
format file instead.


<a id="orgd8599c9"></a>

# Features


<a id="org6f63803"></a>

## Blazing fast.

TTY starts in 0.32s on MacBook Air (M1, 2020), 0.73s on a VPS with 1
core CPU and 1 GB RAM, and 0.30s on WSL with Intel i7-1185G7 and 32 GB
RAM. The GUI starts in 0.44s on Mac M1. You can even `export
EDITOR="emacs -nw"` and feel no perceptible startup difference
comparing to vim! (See [4.1](#orgb20703f) for additional details.)


<a id="org706f77f"></a>

## Robust

Package versions are locked and under version control, so no breaking
changes are expected.


<a id="org144fac7"></a>

## Compatability

This configuration works well on both TTY and GUI. Compatibility on
TTY is not compromised, while GUI features, including `xwidget`, are
also well-configured.


<a id="org96337d3"></a>

## Feature rich

A blazing fast startup speed doesn&rsquo;t mean it is a lite and minimal
configuration.  Instead, it is &ldquo;heavy&rdquo; and feature rich, including:

-   A modern minibuffer completion experience powered by `vertico+consult+orderless+embark+marginalia` family bucket.

-   Modal editing ecosystem everywhere, powered by `evil` and many other extensions.

-   A keybinding scheme centered around leader and localleader keys, powered by `general` and `which-key`.

-   In-buffer autocompletion frontend based on `company` (see [4.3](#org3a662f3)).

-   Code completion and navigation based on `eglot` (LSP) and `citre` (Ctags).

-   Integration with `eglot` and `org-babel` or `markdown-mode` that takes literate programming to the next level.


<a id="orgc7d394e"></a>

## Be wild

Randomly select a theme from a curated list each time you start up and
automatically switches between day and night themes at scheduled time.
Additionally, the displayed verses on the welcome screen is also
randomized with each launch. Have a fresh experience at every time. Be
casual and wild!


<a id="org28a074a"></a>

# Showcase


<a id="org60a36c2"></a>

## Welcome screen

![img](assets/welcome.png)

The welcome screen displays two verses randomly selected from my
curated collection. The minibuffer displays the GUI
`emacs-init-time`.  This is a scratch buffer where you can perform
lisp evaluation.


<a id="orgd73b5bd"></a>

## Code Navigation

![img](assets/lsp-ctags.png)

Making use of LSP and Ctags, navigating code is a breeze. The file
tree is displayed on the leftmost window through `dired-sidebar`, and
the bottom right window showcases the references of a selected symbol
via `LSP find references` (the Emacs command is
`xref-find-references`). In the central floating window, a preview of
the definition of the chosen symbol is displayed with the aid of
`ctags` (the Emacs command is `citre-peek`).


<a id="org480c02c"></a>

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


<a id="org6f1a258"></a>

## Orgmode

![img](assets/reveal-js.png)

Write prose in `orgmode`, and export it into `reveal.js` presentation.
The right window displays the HTML slides using xwidget
webkit. Preview slides in emacs without the need to open GUI browser
anymore.


<a id="orgfccc8ab"></a>

# TODOs


<a id="org5f12229"></a>

## Remove my workaround hack once evil-collection addresses the compatibility issue caused by the breaking changes in mu 1.10+.


<a id="org2d1de50"></a>

## Update `lisp-indent-function` from Doomemacs.


<a id="orgb99e389"></a>

## Report `org-capture` bugs when inserting entries into table to upstream.


<a id="org4d425a0"></a>

## Utilize the contextual information from previous code block when editing source block within markdown/org.


<a id="orge25c76a"></a>

## Lazily load third-party plugins for `evil`.


<a id="org40bd2a0"></a>

## Configure `evil-args` to use spaces as argument delimiter for `emacs-lisp-mode`.


<a id="org6c002e7"></a>

## Implement thread-folding for `mu4e`.


<a id="orgfa4929c"></a>

# Notes


<a id="orgb20703f"></a>

## Startup speed

Startup speed is measured using `(emacs-init-time)`.

However, note that this metric may fool you.  If you load some packages
in `emacs-startup-hook` or `after-init-hook`, then `(emacs-init-time)`
cannot properly measure your real startup time. Packages loaded at
`emacs-start-hook` and `after-init-hook` are actually not lazy loaded;
they are loaded during your startup anyway. Using these hooks only
skews `(emacs-init-time)` and does not accurately reflect startup
time. This configuration is honest and truly lazy loads packages.


<a id="orgda3e05f"></a>

## Naming conventions (WIP)

-   A symbol prefixed with `my:` indicates it is a function.

-   A symbol prefixed with `my$` indicates it is a variable.

-   A symbol prefixed with `my%` indicates it is a macro.

-   A symbol prefixed with `my~` indicates it is a mode or an interactive command.
    
    (This also means that the derivative variables defined by a mode are
    also prefixed with `my~`, e.g. `my~foo-mode-hook`).

-   A symbol prefixed with `my*` indicates it is generated via closure or macro.

-   A symbol prefixed with `my&` indicates it is a special symbol like faces.


<a id="org3a662f3"></a>

## Corfu or Company?

`Corfu` is a sleek and minimalistic auto-completion UI that uses only
`completion-at-point-functions` as its backend. The GUI experience with
`corfu` is delightful, providing a refreshing and intuitive
interface. However, to maintain full compatibility with TTY, I
continue to use `company` as the auto-completion frontend until `corfu`&rsquo;s
TTY integration is complete.

