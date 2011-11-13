# Subdirs
DIRS = fubar ragepub damnit

# Build
.PHONY: build
build:
	for i in $(DIRS); do (cd $$i; make build); done

# Clean
.PHONY: clean
clean:
	for i in $(DIRS); do (cd $$i; make clean); done

# Install
.PHONY: install
install:
	for i in $(DIRS); do (cd $$i; make install); done

# Uninstall
.PHONY: uninstall
uninstall:
	for i in $(DIRS); do (cd $$i; make uninstall); done
