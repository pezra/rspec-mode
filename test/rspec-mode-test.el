;; -*- lexical-binding: t; -*-
(require 'rspec-mode)

;;; Test regexp matches in compilation buffer
(defun rspec--test-compilation-match-p (example)
  (some 'identity (mapcar (lambda (n) (string-match (nth 1 n) example)) rspec--compilation-error-regexp-alist-alist)))

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
  "does not match pending specs"
  (should-not (rspec--test-compilation-match-p "    # ./spec/controllers/pages_controller_spec.rb:65")))
