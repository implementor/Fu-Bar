# Makefile for Fu Bar

FUBAR_SRC=../fubar
LAZ_PATH=/usr/lib/lazarus/0.9.30
LCL_PATH=$(LAZ_PATH)/lcl
LCL_PATH_INC=$(LCL_PATH)/include
# Available: gtk2 qt
LCL_INTF=gtk2
LCL_INTF_PATH=$(LCL_PATH)/interfaces/$(LCL_INTF)/
SYNEDIT_PATH=$(LAZ_PATH)/components/synedit
ARGS=-Mobjfpc -fPIC -Fu$(FUBAR_SRC):$(LCL_PATH):$(LCL_INTF_PATH):$(LCL_PATH)/*:$(SYNEDIT_PATH) -Fi$(LCL_PATH_INC)
INST_DEST=/usr/local/bin
INST_CFG_DEST=/etc/ragepub
LOC_CFG_DIR=~/.config/ragepub
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
	@echo "   build       Build ragepub with compiler '$(PP)'"
	@echo "   install     Install to '$(INST_DEST)'"
	@echo "   uninstall   Remove from '$(INST_DEST)'"
	@echo "   clean       Clean binaries"
	@echo "   run         Run fubar here"
	@echo "You may change the paths and compiler by editing this Makefile ;)"
	@echo
	@exit

# target: fubar
.PHONY: build
build: ragepub
ragepub:
	@$(MAKE) clean >/dev/null
	$(PP) $(ARGS) ragepub.lpr

.PHONY: install
install: ragepub
	# Uninstall, if required
	@$(MAKE) uninstall >/dev/null
	# Prepare Config Path
	$(MKDIR) $(INST_CFG_DEST)
	# Install Binary
	$(INST) ragepub $(INST_DEST)
	# Install Misc
	$(INST) $(FUBAR_SRC)/autoload.dat $(INST_CFG_DEST)/autoload
	$(INST) $(FUBAR_SRC)/help.dat $(INST_CFG_DEST)/help
	$(INST) $(FUBAR_SRC)/libplugs.dat $(INST_CFG_DEST)/libplugs
	
.PHONY: uninstall
uninstall:
	$(RM) -rf $(INST_CFG_DEST)
	$(RM) -f $(INST_DEST)/ragepub

.PHONY: clean
clean:
	$(RM) -f ragepub *.o *.ppu *.so

.PHONY: erasecfg
erasecfg:
	$(RM) -rf $(LOC_CFG_DIR)

.PHONY: run
run: ragepub
	./ragepub
