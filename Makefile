# Makefile for Fu Bar

ARGS=-Mobjfpc -fPIC
ARGSTRUNK=-Fu~/fpc/rtl -Mobjfpc -fPIC
ARGSWINE=-Mobjfpc -fPIC
INST_DEST=/usr/local/bin
INST_CFG_DEST=/etc/fubar
INST_CFG_RAGEPUB=/etc/ragepub
LOC_CFG_DIR=~/.config/fubar
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
	@echo "   build       Build fubar with compiler '$(PP)'"
	@echo "   buildtrunk  Build fubar with compiler '$(PPTRUNK)'"
	@echo "   build4win   Build fubar with compiler '$(PPWINE)'"
	@echo "   buildX      Build fubar (a)cross both platforms"
	@echo "   install     Install to '$(INST_DEST)'"
	@echo "   uninstall   Remove from '$(INST_DEST)'"
	@echo "   clean       Clean binaries"
	@echo "   erasecfg    Erase local configuration"
	@echo "   run         Run fubar here"
	@echo "   run4win     Run fubar here (wine)"
	@echo "   pull        Pull '$(PULLREMOTE)/$(PULLBRANCH)'"
	@echo "   push        Push '$(PUSHREMOTE)/$(PUSHBRANCH)'"
	@echo "You may change the paths and compiler by editing this Makefile ;)"
	@echo
	@exit

# target: fubar
.PHONY: build
build: fubar libplugtest.so libtcal.so
fubar: fubar.pas interpreter.pas arguments.pas cmd.pas expressions.pas sqrt.pas buffer.pas explain.pas cradle.pas cmplx.pas vars.pas prayerlist.pas plug.pas
	@$(MAKE) clean >/dev/null
	$(PP) $(ARGS) fubar.pas
libplugtest.so: plugtest.pas
	$(PP) $(ARGS) plugtest.pas
libtcal.so: tcal.pas
	$(PP) $(ARGS) tcal.pas
	
.PHONY: build4win
build4win: fubar.exe
fubar.exe: fubar.pas interpreter.pas arguments.pas cmd.pas expressions.pas sqrt.pas buffer.pas explain.pas cradle.pas cmplx.pas vars.pas prayerlist.pas
	@$(MAKE) clean >/dev/null
	$(PPWINE) $(ARGSWINE) fubar.pas

.PHONY: buildtrunk
buildtrunk: fubar.pas interpreter.pas arguments.pas cmd.pas expressions.pas sqrt.pas buffer.pas explain.pas cradle.pas cmplx.pas vars.pas prayerlist.pas
	@$(MAKE) clean >/dev/null
	$(PPTRUNK) $(ARGSTRUNK) fubar.pas
	
.PHONY: buildX
buildX: fubar fubar.exe

.PHONY: install
install: fubar autoload.dat help.dat fubar.1 libplugs.dat libplugtest.so libtcal.so
	# Uninstall, if required
	@$(MAKE) uninstall >/dev/null
	# Install Binary
	$(INST) fubar $(INST_DEST)
	# Prepare Config Dir
	$(MKDIR) $(INST_CFG_DEST)
	# Install Plugins
	$(INST) libplugtest.so $(INST_CFG_DEST)
	$(INST) libtcal.so $(INST_CFG_DEST)
	# Install Manual
	$(INST) fubar.1 $(MAN_DEST)
	# Install Misc
	$(INST) autoload.dat $(INST_CFG_DEST)/autoload
	$(INST) help.dat $(INST_CFG_DEST)/help
	$(INST) libplugs.dat $(INST_CFG_DEST)/libplugs
	
.PHONY: uninstall
uninstall:
	$(RM) -rf $(INST_CFG_DEST)
	$(RM) -f $(INST_DEST)/fubar
	$(RM) -f $(MAN_DEST)/fubar.1

.PHONY: clean
clean:
	$(RM) -f fubar *.o *.ppu fubar.exe *.so

.PHONY: erasecfg
erasecfg:
	$(RM) -rf $(LOC_CFG_DIR)

.PHONY: run
run: fubar
	./fubar
	
.PHONY: run4win
run4win: fubar.exe
	$(WINE) fubar.exe

.PHONY: pull
pull:
	$(GIT) pull $(PULLREMOTE) $(PULLBRANCH)
	
.PHONY: push
push:
	$(GIT) push $(PUSHREMOTE) $(PUSHBRANCH)

%.pas: pull
%.ppu: %.pas
	$(PP) $(ARGS) $<
%.o: %.pas
	$(PP) $(ARGS) $<
