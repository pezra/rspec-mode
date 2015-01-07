;; -*- lexical-binding: t; -*-
(require 'rspec-mode)

;;; Test regexp matches in compilation buffer
(defun rspec--test-re ()
  (nth 1 (assq 'rspec rspec--compilation-error-regexp-alist-alist)))

(ert-deftest rspec--test-regexp-backtrace ()
  "matches backtrace"
  (let ((example "    # ./app/controllers/posts_controller.rb:7:in `create'"))
    (should (string-match (rspec--test-re) example))))

(ert-deftest rspec--test-regexp-summary ()
  "matches rspec summary lines"
  (let
      ((example "rspec ./spec/foo/bar_spec.rb:21 # description"))
    (should (string-match (rspec--test-re) example))))

(ert-deftest rspec--test-regexp-deprecation ()
  "does not match deprecation warnings"
  (let
      ((example "/path/to/file.rb:112: warning: duplicated key at line 132 ignored: :foobar"))
    (should-not (string-match (rspec--test-re) example))))
