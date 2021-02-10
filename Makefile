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
ELS := dash.el dev/dash-defs.el
ELCS := $(addsuffix c,$(ELS))
DOCS := README.md dash.texi
TMPLS := readme-template.md dash-template.texi $(wildcard doc/*.texi)

# Targets.

lisp: $(ELCS)
.PHONY: lisp

docs: $(DOCS)
.PHONY: docs

force-docs: maintainer-clean docs
.PHONY: force-docs

# ERT_SELECTOR is a Lisp expression determining which tests to run.
# Its format is described in (info "(ert) Test Selectors").  It
# defaults to selecting all tests.  Note that in batch mode, a nil
# selector is the same as t.
check: ERT_SELECTOR ?= t
check: RUN := '(ert-run-tests-batch-and-exit (quote $(ERT_SELECTOR)))'
check: lisp
	$(BATCH) -l dev/examples.el -eval $(RUN)
.PHONY: check

all: lisp docs check
.PHONY: all

force-all: maintainer-clean lisp docs check
.PHONY: force-all

clean:
	$(RM) $(ELCS)
.PHONY: clean

maintainer-clean: VER := 26
maintainer-clean: MSG := Doc regeneration requires $(VER)+
maintainer-clean: clean
	$(BATCH) -eval '(if (< emacs-major-version $(VER)) (error "$(MSG)"))'
	$(RM) $(DOCS)
.PHONY: maintainer-clean

# Files.

%.elc: WERROR := '(setq byte-compile-error-on-warn t)'
%.elc: %.el
	$(BATCH) -eval $(WERROR) -f batch-byte-compile $<

$(DOCS) &: dev/examples.el $(ELCS) $(TMPLS)
	$(BATCH) -l $< -f dash-make-docs
