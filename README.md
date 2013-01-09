# Emacs Rspec Mode
Rspec mode provides some convenience functions related to dealing with RSpec.

## Installation
You may install via ELPA, or install manually by downloading
`rspec-mode.el` and adding the following to your init file.

    (add-to-list 'load-path "/path/to/rspec-mode")
    (require 'rspec-mode)

If `yasnippet` is installed, it will use rspec-mode's snippets. This is
controlled by the variable `rspec-enable-yasnippet-integration`.

## Usage
If `rspec-mode` is installed properly, it will be started
automatically when `ruby-mode` is started.

See `rspec-mode.el` for further usage.

## Gotchas

### Zsh and RVM
If you use `ZSH` and `RVM`, you may encounter problems running the
specs. It may be so that an older version of Ruby, than the one you
specified in `.rvmrc`, is used. This is because `ZSH` runs a small
script each time a shell is created, which modifies the `$PATH`. The
problem is that it prepends some default paths, such as `/usr/bin`,
which contains another `ruby` binary.

What you can do to solve this is to use `BASH` for running the
specs. This piece of code does the job:

    (defadvice rspec-compile (around rspec-compile-around)
      "Use BASH shell for running the specs because of ZSH issues."
      (let ((shell-file-name "/bin/bash"))
        ad-do-it))
    (ad-activate 'rspec-compile)
