# Makefile for Fu Bar

PP=fpc
# Customize the next line, if required:
PPTRUNK=~/fpc/compiler/ppcx64
ARGS=-Mobjfpc
ARGSTRUNK=-Fu~/fpc/rtl -Mobjfpc

# overview
help:
	@echo
	@echo Targets
	@echo "   fubar       Build fubar with compiler '$(PP)'"
	@echo "   fubartrunk  Build fubar with compiler '$(PPTRUNK)'"
	@echo
	@exit

# target: fubar
fubar: fubar.pas interpreter.pas arguments.pas cmd.pas expressions.pas sqrt.pas buffer.pas explain.pas cradle.pas cmplx.pas vars.pas prayerlist.pas
	$(PP) $(ARGS) fubar.pas

fubartrunk: fubar.pas interpreter.pas arguments.pas cmd.pas expressions.pas sqrt.pas buffer.pas explain.pas cradle.pas cmplx.pas vars.pas prayerlist.pas
	$(PPTRUNK) $(ARGSTRUNK) fubar.pas
