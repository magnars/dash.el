# Makefile for Dash.

# Copyright (C) 2021 Free Software Foundation, Inc.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Variables.

EMACS ?= emacs
BATCH := $(EMACS) -Q -batch -L .
ELS := dash.el dash-functional.el
ELCS := $(addsuffix c,$(ELS))

# Targets.

lisp: $(ELCS)
.PHONY: lisp

docs: README.md dash.texi
.PHONY: docs

# ERT_SELECTOR is a Lisp expression determining which tests to run.
# Its format is described in (info "(ert) Test Selectors").  It
# defaults to selecting all tests.  Note that in batch mode, a nil
# selector is the same as t.
check: ERT_SELECTOR ?= t
check: lisp
	$(BATCH) -l dev/examples-to-tests.el -l dev/examples.el \
	  -eval '(ert-run-tests-batch-and-exit (quote $(ERT_SELECTOR)))'
.PHONY: check

all: lisp docs check
.PHONY: all

clean:
	$(RM) $(ELCS)
.PHONY: clean

# Files.

%.elc: %.el
	$(BATCH) -eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $<

dash-functional.elc: dash.elc

README.md: $(ELS) dev/examples-to-docs.el dev/examples.el readme-template.md
	$(BATCH) $(addprefix -l ,$(filter %.el,$^)) -f create-docs-file

dash.texi: $(ELS) dev/examples-to-info.el dev/examples.el dash-template.texi
	$(BATCH) $(addprefix -l ,$(filter %.el,$^)) -f create-info-file
