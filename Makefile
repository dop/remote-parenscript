.PHONEY: test

test:
	sbcl --noinform --load run-tests.lisp --quit
