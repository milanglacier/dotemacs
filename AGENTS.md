## Commit Messages & Communication

Write commit messages (and explanations to the user) in plain English for
the user, who has not read the code:

- Use complete sentences, not compressed jargon or dense noun phrases.
  Bad: "removing pairs-order nondeterminism". Good: "results no longer
  depend on the order the placeholders are listed in".
- Do not coin terminology (e.g. "walk-based expander", "placeholder DSL");
  say what the thing does instead.

# Coding Styles

## Prefer Flatter Code

prefer `if-let*`/`when-let*` over nested `if`/`when` -> `let` -> `if`/`when`
chains. Binding a `_` to a boolean expression as a non-binding guard inside
`when-let*` is explicitly fine, e.g. `(when-let* ((_ (looking-at "@@")) (start
(point)) ...) ...)`.
