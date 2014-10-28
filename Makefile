EMACS = emacs
EMACSFLAGS =
CASK = cask
EMACS_VERSION = `$(CASK) exec $(EMACS) --version | head -1`
ALCHEMIST = alchemist.el

OBJECTS = alchemist.elc

NO_COLOR=\033[0m
INFO_COLOR=\033[2;32m
STAT_COLOR=\033[2;33m

.PHONY: info test build compile cask clean clean-elc test-elc

info:
	@ echo "\n$(INFO_COLOR)Installed Emacs info: $(NO_COLOR)\n"
	@ echo "  $(STAT_COLOR)[PATH]$(NO_COLOR)    = `which $(EMACS)`"
	@ echo "  $(STAT_COLOR)[VERSION]$(NO_COLOR) = $(EMACS_VERSION)"

build: cask test-elc test

test:
	@ echo "\n$(INFO_COLOR)Run tests: $(NO_COLOR)\n"
	$(CASK) exec $(EMACS) -batch -Q -l test/test-runner.el

test-elc: compile
	@ echo "\n$(INFO_COLOR)Run tests with *.elc: $(NO_COLOR)\n"
	$(CASK) exec $(EMACS) -batch -Q -l test/test-runner.el
	make clean-elc

cask:
	@ echo "\n$(INFO_COLOR)Install package dependencies: $(NO_COLOR)\n"
	@ echo "$(STAT_COLOR)[cask install]$(NO_COLOR)"
	@ echo `$(CASK) install`
	@ echo "$(STAT_COLOR)[cask update]$(NO_COLOR)"
	@ echo `$(CASK) update`

compile:
	@ echo "\n$(INFO_COLOR)Compile: $(NO_COLOR)\n"
	@ echo "$(STAT_COLOR)[$(CASK) exec $(EMACS) -Q -batch -f batch-byte-compile $(KARMA)]$(NO_COLOR)"
	@ echo `$(CASK) exec $(EMACS) -Q -batch -f batch-byte-compile $(KARMA)`

clean:
	@ echo "\n$(INFO_COLOR)Clean environment: $(NO_COLOR)\n"
	rm -f $(OBJECTS)
	rm -rf .cask

clean-elc:
	@ echo "\n$(INFO_COLOR)Clean $(OBJECTS): $(NO_COLOR)\n"
	rm -f $(OBJECTS)

package:
	@ echo "\n$(INFO_COLOR)Package Alchemist: $(NO_COLOR)\n"
	$(CASK) package
