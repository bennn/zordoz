all:
	raco exe main.rkt
	mv main zordoz

test:
	raco test src/zo-find.rkt
	raco test src/zo-shell.rkt
	raco test src/zo-string.rkt
	raco test src/zo-transition.rkt
