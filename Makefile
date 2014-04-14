LISP = sbcl
LISPOPTS = --no-sysinit --no-userinit

BUILD = build

BUILDAPP = $(BUILD)/buildapp
CMACRO = $(BUILD)/cmc
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

$(QLDIR): $(BUILD)
	@echo "Install Quicklisp"
	curl -o $(QLFILE) $(QLURL)
	$(LISP) $(LISPOPTS) --load $(QLFILE) --eval '(quicklisp-quickstart:install :path "$(QLDIR)")' \
          --quit
	rm $(QLFILE)

$(BUILDAPP): $(QLDIR)
	@echo "Install Buildapp"
	$(LISP_QL) --eval '(ql:quickload :buildapp)' \
		   --eval '(buildapp:build-buildapp "$(BUILDAPP)")' \
	 	   --quit

$(LIBS):
	@echo "Downloading requirements"
	$(LISP_QL) --eval '(ql:quickload :cmacro)'
	touch $@

$(CMACRO): $(BUILDAPP) $(LIBS)
	@echo "Building cmacro"

all: $(CMACRO)

clean:
	rm -rf $(BUILD)
