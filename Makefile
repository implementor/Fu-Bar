# Makefile for Fu Bar

ARGS=-Mobjfpc
ARGSTRUNK=-Fu~/fpc/rtl -Mobjfpc
ARGSWINE=-Mobjfpc
INST_DEST=/usr/local/bin

# Programs used
PP=fpc
PPTRUNK=~/fpc/compiler/ppcx64
PPWINE=wine fpc
WINE=wine
INST=/usr/bin/install
CP=/bin/cp
MKDIR=/bin/mkdir
RM=/bin/rm
MAKE=/usr/bin/make

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
	@echo "You may change the paths and compiler by editing this Makefile ;)"
	@echo
	@exit

# target: fubar
.PHONY: build
build: fubar
fubar: fubar.pas interpreter.pas arguments.pas cmd.pas expressions.pas sqrt.pas buffer.pas explain.pas cradle.pas cmplx.pas vars.pas prayerlist.pas
	@$(MAKE) clean >/dev/null
	$(PP) $(ARGS) fubar.pas
	
.PHONY: build4win
fubar4win: fubar.exe
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
install: fubar autoload.dat help.dat
	@$(MAKE) uninstall >/dev/null
	$(INST) fubar $(INST_DEST)
	$(MKDIR) /etc/fubar
	$(CP) autoload.dat autoload
	$(CP) help.dat help
	$(INST) autoload help /etc/fubar
	
.PHONY: uninstall
uninstall:
	$(RM) -rf /etc/fubar
	$(RM) -f $(INST_DEST)/fubar

.PHONY: clean
clean:
	$(RM) -f fubar *.o *.ppu fubar.exe

.PHONY: erasecfg
erasecfg:
	$(RM) -rf ~/.config/fubar

.PHONY: run
run: fubar
	./fubar
	
.PHONY: run4win
run4win: fubar.exe
	$(WINE) fubar.exe
