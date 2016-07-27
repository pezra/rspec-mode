EMACS ?= emacs

.PHONY: elpa clean test

test:
	${EMACS} -Q --batch -l ert --directory . -l test/rspec-mode-test.el \
	         --eval '(ert-run-tests-batch-and-exit)'

elpa: *.el
	@version=`grep -o "Version: .*" rspec-mode.el | cut -c 10-`; \
	dir=rspec-mode-$$version; \
	mkdir -p "$$dir"; \
	cp -r rspec-mode.el snippets rspec-mode-$$version; \
	echo "(define-package \"rspec-mode\" \"$$version\" \
	\"Enhance ruby-mode for RSpec\")" \
	> "$$dir"/rspec-mode-pkg.el; \
	tar cvf rspec-mode-$$version.tar "$$dir"

clean:
	@rm -rf rspec-mode-*/ rspec-mode-*.tar *.elc
