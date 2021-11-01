> Talking never moves anything in Emacs, never did, never will.
>
> &mdash; <cite>2021 Maintainer of GNU Emacs, who then proceeded to keep talking</cite>

## Frequently Asked Questions

### Why?

This is the oft ideated, never sublimated "forge" repository for
emacs.  As it is hosted on a site using
[non-free](https://www.gnu.org/philosophy/categories.en.html)
software, the work herein is not the official GNU Emacs source, and
does not entreat the FSF to enforce its license.

### How often are you merging commits from GNU?

Roughly every ninety minutes.

### How has the code diverged thus far?

- Tree-sitter font highlighting
- Gnus is rewritten to be non-blocking.
- The module `process.c` is rewritten.

Given time, there will be enough improvements that this section of the
README will migrate to a larger NEWS file.  Or not.

### Isn't this xemacs all over again?

Ah, but if it could reach those heights.  If by some miracle it did,
the choices for the FSF are the same as before: enlist RMS to embark
on a coding frenzy that achieves feature parity, grant myself commit
rights, or continue not noticing me.  If my history of user
acquisition is any indication, the last outcome is most likely.

### What do you mean by "commercial"?

I would have called this the less invidious-sounding "NonGNU Emacs"
after "NonGNU Elpa" which, like the present endeavor, was created to
circumvent FSF strictures thereby giving it the procedural latitude to
compete with MELPA (for attention, obviously, not dollars).  But
describing something by what it is **not** breaks some basic marketing
rule.  Is an abortion rights opponent pro-life or pro-choice?

### Why aren't non-patch bugs being mirrored into Issues?

An Issues board with more than say 100 open issues is useless, and
Debbugs (GNU's equivalent of Issues) currently asserts a few thousand of
them.  Moreover, I am dismissive of bug reports without patches, as
they're generally:

1. Verbose
2. Imprecise
3. Lacking a minimum reproducible example

A bug report accompanied by a patch, on the other hand, is a sure sign
that the reporter made an effort to understand the problem.
