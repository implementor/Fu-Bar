# Makefile for Fu Bar

ARGS=-Mobjfpc
ARGSTRUNK=-Fu~/fpc/rtl -Mobjfpc
ARGSWINE=-Mobjfpc
INST_DEST=/usr/local/bin
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
	$(INST) fubar.1 $(MAN_DEST)
	
.PHONY: uninstall
uninstall:
	$(RM) -rf /etc/fubar
	$(RM) -f $(INST_DEST)/fubar
	$(RM) -f $(MAN_DEST)/fubar.1

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
