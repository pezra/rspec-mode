;;; rspec-mode.el --- Enhance ruby-mode for RSpec

;; Copyright (C) 2008-2013 Peter Williams <http://barelyenough.org> and others
;; Author: Peter Williams, et al.
;; URL: http://github.com/pezra/rspec-mode
;; Created: 2011
;; Version: 1.8
;; Keywords: rspec ruby
;; Package-Requires: ((ruby-mode "1.0"))

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Commentary:
;;
;; This minor mode provides some enhancements to ruby-mode in
;; the contexts of RSpec specifications.  Namely, it provides the
;; following capabilities:
;;
;;  * toggle back and forth between a spec and it's target (bound to
;;    `\C-c ,t`)
;;
;;  * verify the spec file associated with the current buffer (bound to `\C-c ,v`)
;;
;;  * verify the spec defined in the current buffer if it is a spec
;;    file (bound to `\C-c ,v`)
;;
;;  * verify the example defined at the point of the current buffer (bound to `\C-c ,s`)
;;
;;  * re-run the last verification process (bound to `\C-c ,r`)
;;
;;  * toggle the pendingness of the example at the point (bound to
;;    `\C-c ,d`)
;;
;;  * disable the example at the point by making it pending
;;
;;  * reenable the disabled example at the point
;;
;;  * run all specs related to the current buffer (`\C-c ,m`)
;;
;;  * run the current spec and all after it (`\C-c ,c`)
;;
;;  * run spec for entire project (bound to `\C-c ,a`)
;;
;; You can choose whether to run specs using 'rake spec' or the 'spec'
;; command. Use the customization interface (customize-group
;; rspec-mode) or override using (setq rspec-use-rake-when-possible TVAL).
;;
;; Options will be loaded from spec.opts or .rspec if it exists and
;; rspec-use-opts-file-when-available is not set to nil, otherwise it
;; will fallback to defaults.
;;
;; You can also launch specs from Dired buffers, to do that, add this:
;;
;;   (add-hook 'dired-mode-hook 'rspec-dired-mode)
;;
;; It has almost the same keybindings, but there's no toggle-spec
;; command, and `rspec-dired-verify-single' runs all marked files, or
;; the file at point.
;;
;; Dependencies:
;;
;; If `rspec-use-rvm` is set to true `rvm.el' is required.
;;
;; The expectations depend on `el-expectations.el'.
;;
;;; Change Log:
;;
;; 1.8 - Support for Capybara's acceptance test DSL (Ales Guzik)
;; 1.7 - Support for Spring (Tomy Kaira)
;;     - New commands: `rspec-verify-matching', `rspec-verify-continue'
;;       (Jean-Louis Giordano)
;;     - Run specs from Dired (Adam Sokolnicki)
;;     - Include Yasnippet snippets collection (Dmitry Gutov)
;; 1.6 - Improved keymaps and compile buffer (Dmitry Gutov)
;; 1.5 - Allow key prefix to be customized (`rspec-key-command-prefix')
;; 1.4 - Allow .rspec/spec.opts files to be ignored (user option
;;       `rspec-use-opts-file-when-available')
;; 1.3 - Bundler support (JD Huntington)
;; 1.2 - Rspec2 compatibility  (Anantha Kumaran)
;; 1.1 - Run verification processes from project root directory (Joe Hirn)
;; 1.0 - Advance to end of compilation buffer even if it not the other window (byplayer)
;; 0.8 - RVM support (Peter Williams)
;; 0.7 - follow RoR conventions for file in lib directory (Tim Harper)
;; 0.6 - support for arbitrary spec and rake commands (David Yeu)
;; 0.5 - minor changes from Tim Harper
;; 0.4 - ansi colorization of compliation buffers (teaforthecat)
;; 0.3 - Dave Nolan implements respect for spec.opts config and
;;       custom option to use 'rake spec' task or 'spec' command
;; 0.2 - Tim Harper implemented support for imenu to generate a basic
;;       tag outline
;; 0.1 - Pezra's version in master

;;; Code:
(require 'ruby-mode)
(require 'ansi-color)
(require 'compile)
(eval-when-compile (require 'cl))

(define-prefix-command 'rspec-mode-verifiable-keymap)
(define-prefix-command 'rspec-mode-keymap)

(define-key rspec-mode-verifiable-keymap (kbd "v") 'rspec-verify)
(define-key rspec-mode-verifiable-keymap (kbd "a") 'rspec-verify-all)
(define-key rspec-mode-verifiable-keymap (kbd "t") 'rspec-toggle-spec-and-target)
(define-key rspec-mode-verifiable-keymap (kbd "r") 'rspec-rerun)
(define-key rspec-mode-verifiable-keymap (kbd "m") 'rspec-verify-matching)
(define-key rspec-mode-verifiable-keymap (kbd "c") 'rspec-verify-continue)

(set-keymap-parent rspec-mode-keymap rspec-mode-verifiable-keymap)

(define-key rspec-mode-keymap (kbd "s") 'rspec-verify-single)
(define-key rspec-mode-keymap (kbd "d") 'rspec-toggle-example-pendingness)

(define-prefix-command 'rspec-dired-mode-keymap)
(define-key rspec-dired-mode-keymap (kbd "v") 'rspec-dired-verify)
(define-key rspec-dired-mode-keymap (kbd "s") 'rspec-dired-verify-single)
(define-key rspec-dired-mode-keymap (kbd "a") 'rspec-verify-all)
(define-key rspec-dired-mode-keymap (kbd "r") 'rspec-rerun)

(defgroup rspec-mode nil
  "RSpec minor mode."
  :group 'languages)

(defcustom rspec-use-rake-when-possible t
  "When non-nil and Rakefile is present, run specs via rake spec task."
  :tag "RSpec runner command"
  :type '(radio (const :tag "Use 'rake spec' task" t)
                (const :tag "Use 'spec' command" nil))
  :group 'rspec-mode)

(define-obsolete-variable-alias 'rspec-use-rake-flag
  'rspec-use-rake-when-possible "1.7")

(defcustom rspec-rake-command "rake"
  "The command for rake"
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-spec-command "rspec"
  "The command for spec"
  :type 'string
  :group 'rspec-mode)

(defcustom rspec-use-rvm nil
  "When t, use RVM. Requires rvm.el."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-bundler-when-possible t
  "When t and Gemfile is present, run specs with 'bundle exec'.
Not used when running specs using Zeus or Spring."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-zeus-when-possible t
  "When t and .zeus.sock is present, run specs with 'zeus'."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-spring-when-possible t
  "When t and tmp/spring/spring.pid is present, run specs with 'spring'."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-use-opts-file-when-available t
  "When t, RSpec should use .rspec/spec.opts."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-key-command-prefix  (kbd "C-c ,")
  "The prefix for all rspec related key commands"
  :type 'string
  :group 'rspec-mode)

;;;###autoload
(define-minor-mode rspec-mode
  "Minor mode for RSpec files"
  :lighter " RSpec" :keymap `((,rspec-key-command-prefix . rspec-mode-keymap))
  (if rspec-mode
      (progn
        (rspec-set-imenu-generic-expression)
        (when (boundp 'yas-extra-modes)
          (make-local-variable 'yas-extra-modes)
          (add-to-list 'yas-extra-modes 'rspec-mode)))
    (setq imenu-create-index-function 'ruby-imenu-create-index)
    (setq imenu-generic-expression nil)
    (when (boundp 'yas-extra-modes)
      (setq yas-extra-modes (delq 'rspec-mode yas-extra-modes)))))

;;;###autoload
(define-minor-mode rspec-verifiable-mode
  "Minor mode for Ruby files that have specs"
  :lighter "" :keymap `((,rspec-key-command-prefix . rspec-mode-verifiable-keymap)))

;;;###autoload
(define-minor-mode rspec-dired-mode
  "Minor mode for Dired buffers with spec files"
  :lighter "" :keymap `((,rspec-key-command-prefix . rspec-dired-mode-keymap)))

(defconst rspec-imenu-generic-expression
  '(("Examples"  "^\\( *\\(it\\|describe\\|context\\|feature\\|scenario\\) +.+\\)"          1))
  "The imenu regex to parse an outline of the rspec file")

(defconst rspec-spec-file-name-re "\\(_\\|-\\)spec\\.rb\\'"
  "The regex to identify spec files")

(defun rspec-set-imenu-generic-expression ()
  (make-local-variable 'imenu-generic-expression)
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  (setq imenu-generic-expression rspec-imenu-generic-expression))

(defvar rspec-snippets-dir
  (let ((current (or load-file-name (buffer-file-name))))
    (expand-file-name "snippets" (file-name-directory current)))
  "The directory containing rspec snippets.")

(defun rspec-install-snippets ()
  "Add `rspec-snippets-dir' to `yas-snippet-dirs' and load snippets from it."
  (require 'yasnippet)
  (setq yas-snippet-dirs (cons rspec-snippets-dir (yas-snippet-dirs)))
  (yas-load-directory rspec-snippets-dir))

(defun rspec-class-from-file-name ()
  "Guess the name of the class the spec is for."
  (let* ((name (file-relative-name (buffer-file-name)
                                   (rspec-spec-directory (buffer-file-name))))
         (rules `((,rspec-spec-file-name-re . "") ("/" . "::") ("_" . "")))
         (class (capitalize name)))
    (dolist (rule rules)
      (setq class (replace-regexp-in-string (car rule) (cdr rule) class t t)))
    class))

(defun rspec-top-level-desc-p ()
  "Return t if point is on the first \"describe\" block opener."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (not (catch 'found
             (while (re-search-backward "\\_<\\(describe\\|feature\\)\\_>" nil t)
               (unless (nth 8 (syntax-ppss))
                 (throw 'found t))))))))

(defun rspec-beginning-of-example ()
  "Moves point to the beginning of the example in which the point current is."
  (interactive)
  (let ((start (point)))
    (goto-char
     (save-excursion
       (end-of-line)
       (unless (and (search-backward-regexp "^[[:space:]]*\\(it\\|scenario\\)[[:space:]]*(?[\"']" nil t)
                    (save-excursion (ruby-end-of-block) (< start (point))))
         (error "Unable to find an example"))
       (point)))))

(defun rspec-example-pending-p ()
  "True if the example under point is pending. Otherwise false"
  (interactive)
  (save-excursion
    (rspec-beginning-of-example)
    (re-search-forward "^[[:space:]]*pending\\([[:space:](]\\|$\\)" (save-excursion (ruby-end-of-block) (point)) t)))

(defun rspec-toggle-example-pendingness ()
  "Disables active examples and enables pending examples."
  (interactive)
  (if (rspec-example-pending-p)
      (rspec-enable-example)
    (rspec-disable-example)))

(defun rspec-disable-example ()
  "Disable the example in which the point is located"
  (interactive)
  (when (not (rspec-example-pending-p))
    (save-excursion
      (rspec-beginning-of-example)
      (end-of-line)
      (insert "\npending")
      (indent-for-tab-command))))

(defun rspec-enable-example ()
  "Enable the example in which the point is located"
  (interactive)
  (when (rspec-example-pending-p)
    (save-excursion
      (rspec-beginning-of-example)
      (search-forward-regexp "^[[:space:]]*pending\\([[:space:](]\\|$\\)" (save-excursion (ruby-end-of-block) (point)))
      (beginning-of-line)
      (delete-region (save-excursion (beginning-of-line) (point))
                     (save-excursion (forward-line 1) (point))))))

(defun rspec-verify ()
  "Runs the specified spec, or the spec file for the current buffer."
  (interactive)
  (rspec-run-single-file (rspec-spec-file-for (buffer-file-name))
                         (rspec-core-options)))

(defun rspec-verify-matching ()
  "Runs the specs related to the current buffer. This is more fuzzy that a simple verify."
  (interactive)
  (rspec-run-multiple-files (rspec-all-related-spec-files (buffer-file-name))
                            (rspec-core-options)))

(defun rspec-verify-continue ()
  "Runs the current spec file and the spec files located after it.
This is most useful in combination with the option `--fail-fast',
in long-running test suites."
  (interactive)
  (let ((current-spec-file (rspec-compress-spec-file
                            (rspec-spec-file-for (buffer-file-name)))))
    (rspec-run-multiple-files
     (loop for file in (rspec-all-spec-files (buffer-file-name))
           when (not (string-lessp file current-spec-file))
           collect file)
     (rspec-core-options))))

(defun rspec-verify-single ()
  "Runs the specified example at the point of the current buffer."
  (interactive)
  (rspec-run-single-file
   (rspec-spec-file-for (buffer-file-name))
   (rspec-core-options)
   (concat "--line "
           (save-restriction
             (widen)
             (number-to-string (line-number-at-pos))))))

(defun rspec-dired-verify ()
  "Runs all specs in the current directory."
  (interactive)
  (rspec-run-single-file (dired-current-directory) (rspec-core-options)))

(defun rspec-dired-verify-single ()
  "Runs marked specs or spec at point (works with directories too).
Doesn't use rake, calls rspec directly."
  (interactive)
  (let (rspec-use-rake-when-possible)
    (rspec-compile (mapconcat 'identity (dired-get-marked-files) " ")
                   (rspec-core-options))))

(defun rspec-verify-all ()
  "Runs the 'spec' rake task for the project of the current file."
  (interactive)
  (rspec-run (rspec-core-options)))

(defun rspec-toggle-spec-and-target ()
  "Switches to the spec for the current buffer if it is a
   non-spec file, or switch to the target of the current buffer
   if the current is a spec"
  (interactive)
  (find-file
   (if (rspec-buffer-is-spec-p)
       (rspec-target-file-for (buffer-file-name))
     (rspec-spec-file-for (buffer-file-name)))))

(defun rspec-spec-directory-has-lib? (a-file-name)
  (file-directory-p (concat (rspec-spec-directory a-file-name) "/lib")))

(defun rspec-spec-file-for (a-file-name)
  "Find spec for the specified file"
  (if (rspec-spec-file-p a-file-name)
      a-file-name
    (let ((replace-regex (if (and (rspec-target-lib-file-p a-file-name) (rspec-spec-directory-has-lib? a-file-name))
                             "^\\.\\./"
                           "^\\.\\./[^/]+/"))
          (relative-file-name (file-relative-name a-file-name (rspec-spec-directory a-file-name))))
      (rspec-specize-file-name (expand-file-name (replace-regexp-in-string replace-regex "" relative-file-name)
                                                 (rspec-spec-directory a-file-name))))))

(defun rspec-spec-lib-file-p (a-spec-file-name)
  (string-match (concat "^" (expand-file-name (regexp-quote (concat (rspec-spec-directory a-spec-file-name) "/lib")))) a-spec-file-name))

(defun rspec-target-lib-file-p (a-file-name)
  (string-match (concat "^" (expand-file-name (regexp-quote (concat (rspec-project-root a-file-name) "/lib")))) a-file-name))

(defun rspec-target-file-for (a-spec-file-name)
  "Find the target for a-spec-file-name"
  (car
   (file-expand-wildcards
        (replace-regexp-in-string
         "/spec/"
         (if (rspec-spec-lib-file-p a-spec-file-name) "/" "/*/")
         (rspec-targetize-file-name a-spec-file-name)))))

(defun rspec-specize-file-name (a-file-name)
  "Returns a-file-name but converted in to a spec file name"
  (concat
   (file-name-directory a-file-name)
   (replace-regexp-in-string "\\(\\.rb\\)?$" "_spec.rb" (file-name-nondirectory a-file-name))))

(defun rspec-targetize-file-name (a-file-name)
  "Returns a-file-name but converted into a non-spec file name"
     (concat (file-name-directory a-file-name)
             (rspec-file-name-with-default-extension
              (replace-regexp-in-string "_spec\\.rb" "" (file-name-nondirectory a-file-name)))))

(defun rspec-file-name-with-default-extension (a-file-name)
  "Adds .rb file extension to a-file-name if it does not already have an extension"
  (if (file-name-extension a-file-name)
      a-file-name ;; file has a extension already so do nothing
    (concat a-file-name ".rb")))

(defun rspec-parent-directory (a-directory)
  "Returns the directory of which a-directory is a child"
  (file-name-directory (directory-file-name a-directory)))

(defun rspec-root-directory-p (a-directory)
  "Returns t if a-directory is the root"
  (equal a-directory (rspec-parent-directory a-directory)))

(defun rspec-spec-directory (a-file)
  "Returns the nearest spec directory that could contain specs for a-file"
  (if (file-directory-p a-file)
      (or
       (car (directory-files a-file t "^spec$"))
       (if (rspec-root-directory-p a-file)
           nil
         (rspec-spec-directory (rspec-parent-directory a-file))))
    (rspec-spec-directory (rspec-parent-directory a-file))))

(defun rspec-all-related-spec-files (a-file)
  (let* ((expected-name (file-name-nondirectory (rspec-spec-file-for a-file)))
         (expected-spec-file (concat "/" expected-name)))
    (loop for file in (rspec-all-spec-files a-file)
          when (string-match-p expected-spec-file file)
          collect file)))

(defun rspec-all-files-under-directory (dir)
  (let ((files (file-expand-wildcards (concat dir "/*") nil)))
    (if (null files)
        files
      (delete-dups
       (append files
               (rspec-all-files-under-directory (concat dir "/*")))))))

(defun rspec-compress-spec-file (a-file)
  (file-relative-name a-file (rspec-project-root)))

(defun rspec-all-spec-files (a-file)
  (mapcar 'rspec-compress-spec-file
          (sort (loop for file in (rspec-all-files-under-directory
                                   (rspec-spec-directory a-file))
                      when (rspec-spec-file-p file)
                      collect file)
                'string-lessp)))

(defun rspec-spec-file-p (a-file-name)
  "Returns true if the specified file is a spec"
  (numberp (string-match rspec-spec-file-name-re a-file-name)))

(defun rspec-core-options (&optional default-options)
  "Returns string of options that instructs spec to use options file if it exists, or sensible defaults otherwise"
  (cond ((and rspec-use-opts-file-when-available
              (file-readable-p (rspec-spec-opts-file)))
         (concat "--options " (rspec-spec-opts-file)))
        (t (or default-options
            (rspec-default-options)))))

(defun rspec-bundle-p ()
  (and rspec-use-bundler-when-possible
       (file-readable-p (concat (rspec-project-root) "Gemfile"))))

(defun rspec-zeus-p ()
  (and rspec-use-zeus-when-possible
       (file-exists-p (concat (rspec-project-root) ".zeus.sock"))))

(defun rspec-rake-p ()
  (and rspec-use-rake-when-possible
       ;; Looks inefficient, but the calculation of the root is quite
       ;; fast. Unless this is used over TRAMP, I suppose.
       (not (rspec-spring-p))
       (file-exists-p (concat (rspec-project-root) "Rakefile"))))

(defun rspec-spring-p ()
  (and rspec-use-spring-when-possible
       (file-exists-p (concat (rspec-project-root) "tmp/spring/spring.pid"))))

(defun rspec2-p ()
  (or (string-match "rspec" rspec-spec-command)
      (file-readable-p (concat (rspec-project-root) ".rspec"))))

(defun rspec-default-options ()
  (if (rspec2-p)
      "--format documentation"
    (concat "--format specdoc " "--reverse")))

(defun rspec-spec-opts-file ()
  "Returns filename of spec opts file"
  (if (rspec2-p)
      (expand-file-name ".rspec" (rspec-project-root))
    (expand-file-name "spec.opts" (rspec-spec-directory (rspec-project-root)))))

(defun rspec-runner ()
  "Returns command line to run rspec"
  (let ((bundle-command (if (rspec-bundle-p) "bundle exec " ""))
        (zeus-command (if (rspec-zeus-p) "zeus " nil))
        (spring-command (if (rspec-spring-p) "spring " nil)))
    (concat (or zeus-command spring-command bundle-command)
            (if (rspec-rake-p)
                (concat rspec-rake-command " spec")
              rspec-spec-command))))

(defun rspec-runner-options (&optional opts)
  "Returns string of options for command line"
  (let ((opts (if (listp opts)
                  opts
                (list opts)))
        (use-rake (rspec-rake-p)))
    (concat (when use-rake "SPEC_OPTS=\'")
            (mapconcat 'identity opts " ")
            (when use-rake "\'"))))

(defun rspec-runner-target (target)
  "Returns target file/directory wrapped in SPEC if using rake"
  (let ((use-rake (rspec-rake-p)))
    (concat (when use-rake "SPEC=\'") target (when use-rake "\'"))))

;;;###autoload
(defun rspec-buffer-is-spec-p ()
  "Returns true if the current buffer is a spec"
  (and (buffer-file-name)
       (rspec-spec-file-p (buffer-file-name))))

(defun rspec-run (&optional opts)
  "Runs spec with the specified options"
  (rspec-compile (rspec-spec-directory (rspec-project-root)) opts))

(defun rspec-run-single-file (spec-file &rest opts)
  "Runs spec on a file with the specified options"
  (rspec-compile (rspec-runner-target spec-file) opts))

(defun rspec-run-multiple-files (spec-files &rest opts)
  "Runs spec on a list of files with the specified options"
  (if (null spec-files)
      (message "No spec files found!")
    (rspec-compile (rspec-runner-target (mapconcat 'identity spec-files " ")) opts)))

(defvar rspec-last-directory nil
  "Directory the last spec process ran in.")

(defvar rspec-last-arguments nil
  "Arguments passed to `rspec-compile' at the last invocation.")

(defun rspec-rerun ()
  "Re-run the last RSpec invocation."
  (interactive)
  (if (not rspec-last-directory)
      (error "No previous verification")
    (let ((default-directory rspec-last-directory))
      (apply #'rspec-compile rspec-last-arguments))))

(defun rspec-compile (a-file-or-dir &optional opts)
  "Runs a compile for the specified file or directory with the specified options."
  (setq rspec-last-directory default-directory
        rspec-last-arguments (list a-file-or-dir opts))

  (if rspec-use-rvm
      (rvm-activate-corresponding-ruby))

  (let ((default-directory (or (rspec-project-root) default-directory))
        (compilation-scroll-output t))
    (compile (mapconcat 'identity `(,(rspec-runner)
                                    ,(rspec-runner-options opts)
                                    ,a-file-or-dir) " ")
             'rspec-compilation-mode)))

(defvar rspec-compilation-mode-font-lock-keywords
  '((compilation--ensure-parse)
    ("^\\(Pending\\|Failures\\):$"
     (0 font-lock-function-name-face))
    ("^[0-9]+ examples?, 0 failures.*$"
     (0 compilation-info-face))
    ("^[0-9]+ examples?, \\([0-9]+ failures?\\)"
     (1 compilation-error-face))))

(define-derived-mode rspec-compilation-mode compilation-mode "RSpec Compilation"
  "Compilation mode for RSpec output."
  (set (make-local-variable 'compilation-error-regexp-alist)
       (cons 'rspec compilation-error-regexp-alist))
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (cons '(rspec "rspec +\\([0-9A-Za-z@_./\:-]+\\.rb\\):\\([0-9]+\\)" 1 2)
             compilation-error-regexp-alist-alist))
  (setq font-lock-defaults '(rspec-compilation-mode-font-lock-keywords t))
  (add-hook 'compilation-filter-hook 'rspec-colorize-compilation-buffer nil t))

(defun rspec-colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(defun rspec-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds a rake file."
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (cond ((rspec-root-directory-p directory)
           (error "Could not determine the project root."))
          ((file-exists-p (expand-file-name "Rakefile" directory)) directory)
          ((file-exists-p (expand-file-name "Gemfile" directory)) directory)
          (t (rspec-project-root (file-name-directory (directory-file-name directory)))))))

;;;###autoload
(defun rspec-enable-appropriate-mode ()
  (if (rspec-buffer-is-spec-p)
      (rspec-mode)
    (rspec-verifiable-mode)))

;; Hook up all Ruby buffers.
;;;###autoload
(dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
  (add-hook hook 'rspec-enable-appropriate-mode))

;; Add verify related spec keybinding to rails minor mode buffers
;;;###autoload
(add-hook 'rails-minor-mode-hook 'rspec-verifiable-mode)

(provide 'rspec-mode)
;;; rspec-mode.el ends here
