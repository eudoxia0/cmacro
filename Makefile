LISP = sbcl
LISPOPTS = --no-sysinit --no-userinit

BUILD = build

NAME = cmacro
BUILDAPP = $(BUILD)/buildapp

QLDIR = $(BUILD)/quicklisp
QLURL = http://beta.quicklisp.org/quicklisp.lisp
QLFILE = $(QLDIR)/quicklisp.lisp
QLSETUP = $(QLDIR)/setup.lisp

LISP_QL = $(LISP) $(LISPOPTS) --load $(QLSETUP)

PREFIX = /usr/local
INSTALL_DIR = $(DESTDIR)$(PREFIX)/bin

default: all

$(QLDIR)/setup.lisp:
	@echo "Install Quicklisp"
	mkdir -p $(QLDIR)
	curl -o $(QLFILE) $(QLURL)
ifdef PROXY
	$(LISP) $(LISPOPTS) --load $(QLFILE) \
	  --eval '(quicklisp-quickstart:install :path "$(QLDIR)" :proxy "$(PROXY)")' \
          --quit
else
	$(LISP) $(LISPOPTS) --load $(QLFILE) \
	  --eval '(quicklisp-quickstart:install :path "$(QLDIR)")' \
          --quit
endif
	rm $(QLFILE)

quicklisp: $(QLDIR)/setup.lisp ;

$(BUILD)/buildapp: quicklisp
	@echo "Install Buildapp"
	$(LISP_QL) --eval '(ql:quickload :buildapp)' \
		   --eval '(buildapp:build-buildapp "$(BUILDAPP)")' \
	 	   --quit

buildapp: $(BUILD)/buildapp ;

$(BUILD)/.reqs:
	@echo "Downloading requirements"
	$(LISP_QL) --eval '(ql:quickload :split-sequence)' \
		   --eval '(ql:quickload :trivial-types)' \
		   --eval '(ql:quickload :anaphora)' \
		   --eval '(ql:quickload :esrap)' \
		   --eval '(ql:quickload :yason)' \
	           --eval '(load "$(NAME).asd")' --quit
	touch $@

libs: $(BUILD)/.reqs ;

cmc: quicklisp libs buildapp
	@echo "Building $(NAME)"
	$(BUILDAPP) --output $@ \
		    --asdf-path . \
		    --asdf-tree $(QLDIR)/dists \
                    --load-system $(NAME) \
		    --entry cmacro:main

all: cmc

test: libs
	$(LISP_QL) --eval '(ql:quickload :fiveam)' \
		   --eval '(ql:quickload :$(NAME))' \
		   --eval '(ql:quickload :$(NAME)-test)' \
		   --quit

clean:
	rm -rf $(BUILD)

install:
	mkdir -p $(INSTALL_DIR)
	install -m 755 cmc $(INSTALL_DIR)/cmc

uninstall:
	rm -f $(INSTALL_DIR)/cmc
	rm -f $(INSTALL_DIR)/cmc-lexer
