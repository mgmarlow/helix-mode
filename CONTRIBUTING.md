## Developing Helix Mode

Make sure the package compiles without errors (don't forget to clean
up the output afterwards: `make clean-elc`):

```
make compile
```

Run the test suite via:

```
make test
```

## Pull Requests

- Follow the [Emacs Lisp Style
  Guide](https://github.com/bbatsov/emacs-lisp-style-guide/).
  
- Run `M-x checkdoc` to ensure comments and docstrings conform to the
  style guide (see [Tips and
  Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html)
  in the Emacs manual).

- If new dependencies are introduced, use [Package
  Lint](https://github.com/purcell/package-lint) to ensure the Emacs
  package metadata is appropriately updated.
