EMACS_INCLUDE := /Applications/Emacs.app/Contents/Resources/include
INSTALL_DIR   := $(HOME)/.emacs.d/core

CC      := clang
CFLAGS  := -fPIC -fobjc-arc -shared
LDFLAGS := -framework AppKit -framework QuartzCore

MODULE  := lolipop-core.dylib
LISP    := lolipop-mode.el

.PHONY: all install clean

all: $(LISP) $(MODULE)

$(LISP): lolipop.el
	cp $< $@

$(MODULE): lolipop.m
	$(CC) $(CFLAGS) \
		-I$(EMACS_INCLUDE) \
		$< \
		-o $@ \
		$(LDFLAGS)

install: all
	install -d $(INSTALL_DIR)
	install -m 644 $(LISP) $(INSTALL_DIR)
	install -m 755 $(MODULE) $(INSTALL_DIR)

clean:
	rm -f $(LISP) $(MODULE)
