default: cmc

quicklisp:
	# Install Quicklisp
	curl -O http://beta.quicklisp.org/quicklisp.lisp
	sbcl --no-userinit --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
	sbcl --no-userinit --load quicklisp/setup.lisp --eval "(ql:add-to-init-file)" --quit
	rm quicklisp.lisp

cmc:
	# Build executable
	echo "Derp"

install:
	# Install executable
	echo "Herp"
