# Makefile for Fu Bar

FUBAR_SRC=../fubar
ARGS=-Mobjfpc -fPIC -Fu$(FUBAR_SRC)
INST_DEST=/usr/local/bin
INST_CFG_DEST=/etc/damnit
LOC_CFG_DIR=~/.config/damnit
MAN_DEST=/usr/share/man/man1

# Programs used
PP=/usr/bin/fpc
PPTRUNK=~/fpc/compiler/ppcx64
PPWINE=/usr/bin/wine fpc
WINE=/usr/bin/wine
INST=/usr/bin/install
CP=/bin/cp
MKDIR=/bin/mkdir
RM=/bin/rm
MAKE=/usr/bin/make
GIT=/usr/bin/git

# Git
PULLREMOTE=github
PULLBRANCH=master
PUSHREMOTE=github
PUSHBRANCH=master

# overview
.PHONY: help
help:
	@echo
	@echo Targets
	@echo "   build       Build damnit with compiler '$(PP)'"
	@echo "   install     Install to '$(INST_DEST)'"
	@echo "   uninstall   Remove from '$(INST_DEST)'"
	@echo "   clean       Clean binaries"
	@echo "   run         Run fubar here"
	@echo "You may change the paths and compiler by editing this Makefile ;)"
	@echo
	@exit

# target: fubar
.PHONY: build
build: damnit
damnit:
	@$(MAKE) clean >/dev/null
	$(PP) $(ARGS) damnit.pp

.PHONY: install
install: damnit
	# Uninstall, if required
	@$(MAKE) uninstall >/dev/null
	# Prepare Config Path
	$(MKDIR) $(INST_CFG_DEST)
	# Install Binary
	$(INST) damnit $(INST_DEST)
	# Install Misc
	$(INST) $(FUBAR_SRC)/autoload.dat $(INST_CFG_DEST)/autoload
	$(INST) $(FUBAR_SRC)/help.dat $(INST_CFG_DEST)/help
	$(INST) $(FUBAR_SRC)/libplugs.dat $(INST_CFG_DEST)/libplugs
	
.PHONY: uninstall
uninstall:
	$(RM) -rf $(INST_CFG_DEST)
	$(RM) -f $(INST_DEST)/damnit

.PHONY: clean
clean:
	$(RM) -f damnit *.o *.ppu *.so

.PHONY: erasecfg
erasecfg:
	$(RM) -rf $(LOC_CFG_DIR)

.PHONY: run
run: damnit
	./damnit
