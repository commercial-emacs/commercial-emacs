# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a fork of GNU Emacs called "Commercial Emacs" with several performance and feature enhancements:
- Performant long lines without narrowing
- Tree-sitter font highlighting and syntactic parsing
- Non-blocking Gnus rewrite
- Rewritten process management
- Moving garbage collector rudiments

## Architecture

- `src/`: Core C code (Emacs Lisp interpreter, redisplay, basic editing functions)
- `lisp/`: Emacs Lisp code (most of the editor functionality)
- `lib/`: Source code for libraries used by Emacs and its utilities
- `lib-src/`: Utility programs (movemail, etags, etc.)
- `test/`: Test suite using ERT (Emacs Lisp Regression Testing)
- `doc/`: Documentation sources (manual, lispref, lispintro)
- `etc/`: Architecture-independent data files (tutorials, images)
- `admin/`: Developer tools and Unicode data files

## Development Commands

### Building
- `./autogen.sh` - Generate configure script (required for fresh checkouts)
- `./configure --prefix=$HOME/.local` - Configure build
- `make -j4` - Build Emacs (use appropriate -j value for your system)
- `make install` - Install Emacs
- `make bootstrap` - Clean rebuild from scratch

### Testing
- `make check` - Run all tests (excludes expensive and unstable tests)
- `make check-maybe` - Run tests for files with unresolved prerequisites
- `make check-expensive` - Run all tests including expensive ones
- `make check-all` - Run all tests including unstable ones
- `make check-<dirname>` - Run tests in specific directory (e.g., `make check-src`)
- `make <filename>` - Run specific test file (e.g., `make lisp/files-tests`)

### Cleaning
- `make clean` - Delete built files but preserve configuration
- `make distclean` - Delete all build and configuration files
- `make bootstrap` - Force clean rebuild
- `git clean -dfX` - Clean all ignored files

### Tree-sitter Support
To build with tree-sitter highlighting:
1. Install tree-sitter library v0.20.10beta3
2. Configure with `--with-tree-sitter` flag
3. Test with `make test/src/tree-sitter-tests`

### Custom Build Script
The repository includes `make.sh` which uses:
- ccache for faster compilation
- Custom CFLAGS: `-g3 -O0` (debug build)
- Configuration: `--with-tree-sitter --enable-checking`
- Parallel build with `make -j16`

## Test Organization

Tests use ERT (Emacs Lisp Regression Testing). Key tags:
- `:expensive-test` - Time-intensive tests
- `:nativecomp` - Native compiler tests
- `:unstable` - Development tests

Test results are stored in `<filename>.log` files.

## Code Style

### Emacs Lisp Comments
- **Inline (end-of-line) comments**: Use one semicolon with whitespace before the semicolon, but NO space after it:
  ```elisp
  (setq foo 'bar) ;Preferred style for inline comments
  ```
- **Separate-line comments**: Use two semicolons with a space (traditional style):
  ```elisp
  ;; This is a separate line comment
