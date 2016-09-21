ELIXIR = elixir

VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')

NO_COLOR=\033[0m
INFO_COLOR=\033[2;32m
STAT_COLOR=\033[2;33m

all: test

test:
	${MAKE} test_servers
	${MAKE} test_helpers
	${MAKE} test_api

test_servers:
	@ echo "\n$(INFO_COLOR)Run server tests: $(NO_COLOR)\n"
	$(ELIXIR) test/server/socket_test.exs
test_helpers:
	@ echo "\n$(INFO_COLOR)Run helper tests: $(NO_COLOR)\n"
	$(ELIXIR) test/helpers/process_commands_test.exs
	$(ELIXIR) test/helpers/module_info_test.exs
	$(ELIXIR) test/helpers/complete_test.exs

test_api:
	@ echo "\n$(INFO_COLOR)Run api tests: $(NO_COLOR)\n"
	$(ELIXIR) test/api/docl_test.exs
	$(ELIXIR) test/api/comp_test.exs
	$(ELIXIR) test/api/defl_test.exs
	$(ELIXIR) test/api/ping_test.exs

api_completer:
	@ echo "\n$(INFO_COLOR)Run api tests: $(NO_COLOR)\n"
	$(ELIXIR) test/api_test.exs


.PHONY: test test_server test_helpers test_api
