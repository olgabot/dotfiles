# This is part of org-edna
#
#  Copyright (C) 2017 Ian Dunn.
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Load defaults
include defaults.mk

# Load local definitions
include local.mk

EMACS=$(emacs) --batch -L $(org_path)
ALLSRC=org-edna.el
SOURCE=$(ALLSRC)
TARGET=$(patsubst %.el,%.elc,$(SOURCE))

.PHONY: clean check local.mk help

all: $(TARGET)

compile: $(TARGET)

%.elc: %.el
	@$(EMACS) \
	-L "." \
	-f batch-byte-compile $<

autoloads: org-edna-autoloads.el

org-edna-autoloads.el:
	@$(EMACS) \
	--eval "(require 'package)" \
	--eval "(setq inhibit-message t)" \
	--eval "(package-generate-autoloads \"org-edna\" \"$$(pwd)\")"

clean:
	-rm -f *.elc

check: compile
	@$(EMACS) \
	-L "." \
	--load "ert" \
	--load "org-edna-tests.el" \
	-f ert-run-tests-batch-and-exit

local.mk:
	@cp -n defaults.mk local.mk

help:
	$(info )
	$(info make all       - Default)
	$(info make compile   - Compile Emacs Lisp Files)
	$(info make autoloads - Generate Autoloads)
	$(info make clean     - Remove generated .elc files)
	$(info make check     - Run Tests)
	@echo ""
