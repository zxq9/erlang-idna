PROJECT=idna
PROJECT_DESCRIPTION=A pure Erlang IDNA implementation
PROJECT_VERSION=1.0.2

include erlang.mk

.PHONY: gen

gen:
	$(CURDIR)/mkdata.sh
