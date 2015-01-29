all:
	raco exe zordoz.rkt

test:
	raco test private/zo-find.rkt
	raco test private/zo-shell.rkt
	raco test private/zo-string.rkt
	raco test private/zo-transition.rkt
