# Emacs RSpec Mode
RSpec mode provides some convenience functions for dealing with RSpec.

## Installation

You can install via ELPA, or manually by downloading `rspec-mode` and
adding the following to your init file:

```emacs
(add-to-list 'load-path "/path/to/rspec-mode")
(require 'rspec-mode)
```

Provided you have `yasnippet` installed, you can load the snippets:

```emacs
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))
```

(This isn't done automatically to avoid conflicts with snippets you
may already have set up.)

## Usage

If `rspec-mode` is installed properly, it will be started
automatically when `ruby-mode` is started.

### RSpec Verifiable mode

These keybindings are available in any Ruby source file:

Keybinding  | Description                                                                   |
------------|-------------------------------------------------------------------------------|
`C-c , v`   | Verify the spec file associated with the current buffer                       |
`C-c , a`   | Run spec for entire project                                                   |
`C-c , t`   | Toggle back and forth between a spec and its target                           |
`C-c , e`   | Toggle back and forth between a method and its examples in the spec file      |
`C-c , 4 t` | Find in the other window the spec or the target file                          |
`C-c , 4 e` | As above, but try to navigate to the example or method corresponding to point |
`C-c , r`   | Re-run the last verification process                                          |
`C-c , y`   | Yank the last verification command to clipboard                               |
`C-c , m`   | Run all specs related to the current buffer                                   |
`C-c , c`   | Run the current spec and all after it                                         |
`C-c , s`   | Verify the example or method defined at point                                 |
`C-c , f`   | Re-run just the failed examples from the last run                             |

### RSpec mode

These keybindings are available in Ruby spec files:

Keybinding | Description                                    |
-----------|------------------------------------------------|
`C-c , s`  | Run the specified example at point             |
`C-c , d`  | Toggle the pendingness of the example at point |

### RSpec Dired mode

These keybindings are available in Dired buffers:

Keybinding | Description                                                    |
-----------|----------------------------------------------------------------|
`C-c , v`  | Run all specs in the current directory                         |
`C-c , s`  | Run marked specs or spec at point (works with directories too) |
`C-c , a`  | Run the 'spec' rake task for the project of the current file   |
`C-c , r`  | Re-run the last RSpec invocation                               |

See `rspec-mode.el` for further usage.

### Before verification hook

Any functions in `rspec-before-verification-hook` will be executed
before the verification - `rspec-verify` and variants.

### After verification hook

Any functions in `rspec-after-verification-hook` will be executed
after the verification - `rspec-verify` and variants. The hook will be
executed whatever the outcome of the verification.

## Gotchas
### Debugging

To use `binding.pry` or `byebug`, install `inf-ruby` and add this to
your init file:

```emacs
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
```

When you've hit the breakpoint, hit `C-x C-q` to enable `inf-ruby`.

### RVM

If you use RVM, you may have to set `rspec-use-rvm` to true to make
`rspec-mode` function properly:

```emacs
(setq rspec-use-rvm t)
```

Or set it using Emacs' customization system.

### ZSH and RVM

If you use `ZSH` and `RVM`, you may encounter problems running the
specs. It may be so that an older version of Ruby, than the one you
specified in `.rvmrc`, is used. This is because `ZSH` runs a small
script each time a shell is created, which modifies the `$PATH`. The
problem is that it prepends some default paths, such as `/usr/bin`,
which contains another `ruby` binary.

What you can do to solve this is to use `BASH` for running the
specs. This piece of code does the job:

```emacs
(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)
```

### Vagrant

You can run specs inside a Vagrant box. You can enable it through the
`rspec-use-vagrant-when-possible` customization option. You can also set the
directory where your project is inside your box through the
`rspec-vagrant-cwd` option. This will run specs through the `vagrant ssh -c 'cd
<cwd>; <rspec command>'`.

### Docker

You can run specs inside a Docker container. This can be enabled through the
`rspec-use-docker-when-possible` option. This enabled, rspec is executed
through `docker-compose run`.
The following customization options are available:

Option                           | Default value         | Description                                  |
---------------------------------|-----------------------|-----------------------                       |
`rspec-use-docker-when-possible` | `nil`                 | Enable docker                                |
`rspec-docker-command`           | `docker-compose run`  | Docker command to run                        |
`rspec-docker-cwd`               | `/app/`               | Path rspec to run in inside of the container |
`rspec-docker-container`         | `rspec-container-name`| Name of the container to run rspec into      |

To define the options for different projects, have a look at [Per-Directory Local Variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html).


### Auto-scrolling

Set `compilation-scroll-output`. For example, `(setq compilation-scroll-output t)`
will turn on auto scrolling.


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
