exe:
	raco exe zordoz.rkt

test:
	echo "-- Testing untyped code"
	raco test private/zo-find.rkt
	raco test private/zo-shell.rkt
	raco test private/zo-string.rkt
	raco test private/zo-transition.rkt
	echo "-- Verifying that typed struct definitions compile"
	raco make typed/typed-zo-structs.rkt
