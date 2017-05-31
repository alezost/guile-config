# Makefile --- GNU Makefile to build Guile code

# Copyright © 2017 Alex Kost <alezost@gmail.com>

# This program is free software; you can redistribute it and/or modify
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# The only purpose of this Makefile is to build .scm files to check them
# for potential errors.

MODULES_DIR = modules
SCRIPTS_DIR = scripts

MODULES =					\
  $(MODULES_DIR)/al/utils.scm			\
  $(MODULES_DIR)/al/records.scm			\
  $(MODULES_DIR)/al/plists.scm			\
  $(MODULES_DIR)/al/processes.scm		\
  $(MODULES_DIR)/al/files.scm			\
  $(MODULES_DIR)/al/messages.scm		\
  $(MODULES_DIR)/al/places.scm			\
  $(MODULES_DIR)/al/display.scm			\
  $(MODULES_DIR)/al/sound.scm			\
  $(MODULES_DIR)/al/lirc.scm			\
  $(MODULES_DIR)/al/links.scm			\
  $(MODULES_DIR)/al/sources.scm			\
  $(MODULES_DIR)/al/configs.scm

SCRIPTS =					\
  $(SCRIPTS_DIR)/gui				\
  $(SCRIPTS_DIR)/profile			\
  $(SCRIPTS_DIR)/runbut				\
  $(SCRIPTS_DIR)/system				\
  $(SCRIPTS_DIR)/toggle-tvtime

# SCM_FILES = $(MODULES) $(SCRIPTS)
GO_FILES = $(MODULES:%.scm=%.go)

GUILEC_ENV =					\
  GUILE_AUTO_COMPILE=0

GUILEC_OPTS =					\
  -Warity-mismatch				\
  -Wformat					\
  -Wunbound-variable

GUILEC_ENV +=								\
  GUILE_WARN_DEPRECATED=detailed					\
  GUILE_LOAD_PATH="$(MODULES_DIR):$$GUILE_LOAD_PATH"			\
  GUILE_LOAD_COMPILED_PATH="$(MODULES_DIR):$$GUILE_LOAD_COMPILED_PATH"

all: $(GO_FILES)

$(GO_FILES): %.go: %.scm
	@$(GUILEC_ENV) guild compile $(GUILEC_OPTS) --output=$@ $<

scripts:
	@$(GUILEC_ENV) guild compile $(GUILEC_OPTS) $(SCRIPTS)

clean:
	$(RM) -f $(GO_FILES)

.PHONY: clean scripts

# Makefile ends here
