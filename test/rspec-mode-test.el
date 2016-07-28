;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'ert)
(require 'rspec-mode)

;;; Test regexp matches in compilation buffer
(defun rspec--test-compilation-match-p (example &optional type)
  "Check if string `example' with type `type' matches any of compilation regexps"
  (let* ((type (or type 'error))
         (types '((error . 2) (warning . 1) (info . 0)))
         (type-num (cdr (assq type types))))
    (cl-some (lambda (n) (and (string-match (nth 1 n) example) (= type-num (nth 5 n)))) rspec-compilation-error-regexp-alist-alist)))

(ert-deftest rspec--test-regexp-backtrace ()
  "matches backtrace"
  (should (rspec--test-compilation-match-p "    # ./app/controllers/posts_controller.rb:7:in `create'")))

(ert-deftest rspec--test-regexp-summary ()
  "matches rspec summary lines"
  (should (rspec--test-compilation-match-p "rspec ./spec/foo/bar_spec.rb:21 # description")))

(ert-deftest rspec--test-regexp-deprecation ()
  "does not match deprecation warnings"
  (should-not (rspec--test-compilation-match-p "/path/to/file.rb:112: warning: duplicated key at line 132 ignored: :foobar")))

(ert-deftest rspec--test-regexp-pending ()
  "matches pending specs as info"
  (let ((example "    # ./spec/controllers/pages_controller_spec.rb:65"))
    (should-not (rspec--test-compilation-match-p example 'error))
    (should-not (rspec--test-compilation-match-p example 'info))
    (should (rspec--test-compilation-match-p example 'warning))))
