(require 'el-expectations)
(require 'rspec-mode)

(expectations
 (desc "File name predicates")
 (expect (not nil)
   (rspec-spec-file-p "/foo/bar/baz_spec.rb"))
 (expect nil 
   (rspec-spec-file-p "/foo/bar/baz.rb")))