> Talking never moves anything in Emacs, never did, never will.
>
> &mdash; <cite>2021 Maintainer of GNU Emacs, who then proceeded to keep talking.</cite>

## Frequently Anticipated Questions

### Why?

This is the oft ideated, never sublimated "forge" repository for
emacs.  As it is hosted on a site using
[non-free](https://www.gnu.org/philosophy/categories.en.html)
software, the work herein is not the official GNU Emacs source, and
does not entreat the FSF to enforce its license.

### How to build?

```
git clone https://github.com/commercial-emacs/commercial-emacs.git
cd commercial-emacs
./autogen.sh
./configure --prefix=$HOME/.local
make -j4
src/emacs
```

### How often are you merging commits from GNU?

Roughly every hour.

### How has the code diverged thus far?

- Performant long lines without narrowing.
- [Tree-sitter font highlighting](#tree-sitter). <sup id="a1">[1](#f1)</sup>
- Gnus is rewritten to be non-blocking.
- Process management is rewritten.
- Tree-sitter replacement of ersatz PPSS syntactic parser.
- [Moving garbage collector rudiments](#moving-collector).

### <a name="tree-sitter"></a>How can I try tree-sitter highlighting?

1. Install Rust library:
```bash
git clone --depth 1 --branch v0.20.10beta3 \
  https://github.com/commercial-emacs/tree-sitter.git
make -C tree-sitter install
```

2. Upon success the user is instructed to update `PKG_CONFIG_PATH` and
`LD_LIBRARY_PATH` in his shell rc file.

3. Start a new shell and build emacs.
```bash
./autogen.sh
./configure --prefix=$HOME/.local --with-tree-sitter
make test/src/tree-sitter-tests
src/emacs -Q --eval \
  "(custom-set-variables '(font-lock-support-mode 'tree-sitter-lock-mode))" \
  --visit src/xdisp.c
```

### <a name="moving-collector"></a>What is a moving garbage collector?

Moving collectors relocate Lisp values in memory, in contrast to the
GNU Emacs collector, which upon allocating say a cons cell, will let
it remain at its birth address in perpetuity.

GNU Emacs's non-moving collector has been unfairly maligned as
antiquated, generally by undergraduates who just implemented a toy
moving collector for their PL class.  They might be surprised to know
that the Boehm paper on which Emacs's collector is based was published
twenty years *after* Cheney's paper on moving collection.

But one thing moving collectors can do that non-moving ones can't is
*generational* sequestration, that is, keeping the youngest cohort of
Lisp values separated from older ones.  This allows fast, intermediary
cycles which only scan the "nursery" generation (the rationale being
old objects are very likely to still be referenced).  A non-moving
collector must traverse the full set on each cycle since its
allocations are interleaved.  This is why Emacs bros are as eager to
raise collection thresholds as motherboard jockeys are to overclock
their BIOS.

### Isn't this xemacs all over again?

Ah, but if it could reach those heights.  If by some miracle it did,
the choices for the FSF are the same as before: enlist RMS to embark
on a coding frenzy that achieves feature parity, grant myself commit
rights, or continue not noticing me.  If my history of user
acquisition is any indication, the last outcome is most likely.

<b id="f1">[1]</b> By Yuan Fu and oldosfan. [↩](#a1)
