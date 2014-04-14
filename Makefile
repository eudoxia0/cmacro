LISP = sbcl
LISPOPTS = --no-sysinit --no-userinit

BUILD = build

NAME = cmacro
BUILDAPP = $(BUILD)/buildapp
BIN = $(BUILD)/cmc
LEXER = grammar/cmc-lexer

LIBS = $(BUILD)/.reqs

QLDIR = $(BUILD)/quicklisp
QLURL = http://beta.quicklisp.org/quicklisp.lisp
QLFILE = $(QLDIR)/quicklisp.lisp
QLSETUP = $(QLDIR)/setup.lisp

LISP_QL = $(LISP) $(LISPOPTS) --load $(QLSETUP)

default: all

$(BUILD):
	mkdir -p $(QLDIR)

$(QLSETUP): $(BUILD)
	@echo "Install Quicklisp"
	curl -o $(QLFILE) $(QLURL)
	$(LISP) $(LISPOPTS) --load $(QLFILE) --eval '(quicklisp-quickstart:install :path "$(QLDIR)")' \
          --quit
	rm $(QLFILE)

$(BUILDAPP): $(QLSETUP)
	@echo "Install Buildapp"
	$(LISP_QL) --eval '(ql:quickload :buildapp)' \
		   --eval '(buildapp:build-buildapp "$(BUILDAPP)")' \
	 	   --quit

$(LIBS):
	@echo "Downloading requirements"
	$(LISP_QL) --eval '(ql:quickload :$(NAME))' --quit
	touch $@

$(BIN): $(BUILDAPP) $(LIBS)
	@echo "Building $(NAME)"
	$(BUILDAPP) --output $@ \
		    --asdf-path . \
		    --asdf-tree $(QLDIR)/dists \
                    --load-system $(NAME) \
		    --entry cmacro:main

all: $(BIN)

clean:
	rm -rf $(BUILD)
