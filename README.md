# Emacs RSpec Mode
RSpec mode provides some convenience functions for dealing with RSpec.

## Installation

You can install via ELPA, or manually by downloading `rspec-mode` and
adding the following to your init file:

```lisp
(add-to-list 'load-path "/path/to/rspec-mode")
(require 'rspec-mode)
```

Provided you have `yasnippet` installed, you can load the snippets:

```lisp
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))
```

(This isn't done automatically to avoid conflicts with snippets you
may already have set up.)

## Usage

If `rspec-mode` is installed properly, it will be started
automatically when `ruby-mode` is started.

See `rspec-mode.el` for further usage.

## Gotchas

### ZSH and RVM

If you use `ZSH` and `RVM`, you may encounter problems running the
specs. It may be so that an older version of Ruby, than the one you
specified in `.rvmrc`, is used. This is because `ZSH` runs a small
script each time a shell is created, which modifies the `$PATH`. The
problem is that it prepends some default paths, such as `/usr/bin`,
which contains another `ruby` binary.

What you can do to solve this is to use `BASH` for running the
specs. This piece of code does the job:

```lisp
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)
```

## Contributing

Love RSpec and Emacs? Great, help out by contributing. The easiest way
to contribute is to checkout the
[git project](https://github.com/pezra/rspec-mode.git), make a change
and then submit a pull request.

### Note on Patches/Pull Requests

 * Fork the project.
 * Make your feature addition or bug fix.
 * Update the version and changelog in the header of rspec-mode.el to
   reflect the change.
 * Send me a pull request. Bonus points for topic branches.
