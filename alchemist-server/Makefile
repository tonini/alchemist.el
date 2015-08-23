ELIXIR = elixir

VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')

NO_COLOR=\033[0m
INFO_COLOR=\033[2;32m
STAT_COLOR=\033[2;33m

all: test

test:	test_server
	${MAKE} test_doc
	${MAKE} test_informant
	${MAKE} test_source

test_server:
	@ echo "\n$(INFO_COLOR)Run server tests: $(NO_COLOR)\n"
	$(ELIXIR) test/server_test.exs

test_doc:
	@ echo "\n$(INFO_COLOR)Run documentation tests: $(NO_COLOR)\n"
	$(ELIXIR) test/documentation_test.exs

test_informant:
	@ echo "\n$(INFO_COLOR)Run informant tests: $(NO_COLOR)\n"
	$(ELIXIR) test/informant_test.exs

test_source:
	@ echo "\n$(INFO_COLOR)Run source tests: $(NO_COLOR)\n"
	$(ELIXIR) test/source_test.exs

.PHONY: test test_server test_doc test_informant test_source
