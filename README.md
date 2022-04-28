> Talking never moves anything in Emacs, never did, never will.
>
> &mdash; <cite>2021 Maintainer of GNU Emacs, who then proceeded to keep talking</cite>

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

The `make` logic remains broken in the sense that frequently,
incompatible artifacts from a previous build do not get rebuilt.
Preceding the above with,

```
git clean -dfX
```

rebuilds from scratch.

### How often are you merging commits from GNU?

Roughly every hour.

### How has the code diverged thus far?

- Performant long lines.
- [Tree-sitter font highlighting](#tree-sitter). <sup id="a1">[1](#f1)</sup>
- Gnus is rewritten to be non-blocking.
- Process management is rewritten.
- Tree-sitter replacement of ersatz PPSS syntactic parser.

### <a name="tree-sitter"></a>How can I try tree-sitter highlighting?

Install Rust library:
```bash
git clone --depth 1 --branch 0.6.3alpha4 \
  https://github.com/commercial-emacs/tree-sitter.git
make -C tree-sitter install
pkg-config --exact-version=0.6.3alpha4 tree-sitter || echo not found
```

Then build emacs:
```bash
./autogen.sh
LDFLAGS="-L$HOME/.local/lib" CFLAGS="-g3 -O2 -I$HOME/.local/include/" \
  ./configure --prefix=$HOME/.local --with-tree-sitter
make -j4
ldd src/emacs | grep -q tree-sitter || echo not found
make test/src/tree-sitter-tests
src/emacs -Q --eval \
  "(custom-set-variables '(font-lock-support-mode 'tree-sitter-lock-mode))" \
  --visit src/xdisp.c
```

### Isn't this xemacs all over again?

Ah, but if it could reach those heights.  If by some miracle it did,
the choices for the FSF are the same as before: enlist RMS to embark
on a coding frenzy that achieves feature parity, grant myself commit
rights, or continue not noticing me.  If my history of user
acquisition is any indication, the last outcome is most likely.

<b id="f1">[1]</b> By Yuan Fu and oldosfan. [↩](#a1)
