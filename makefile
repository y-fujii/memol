SRCS = \
	misc.ml rational.ml ast.ml pprint.ml parser.mli parser.ml lexer.ml \
	simplify.ml smf.ml main.ml

PROG = memol


.PHONY: all test clean

all: $(PROG)

test: $(PROG)
	./memol testCase.mml

clean:
	rm -f *.cm? *.o lexer.ml parser.mli parser.ml tags $(PROG)


$(PROG): $(SRCS)
	ocamlc -o $(PROG) $(SRCS)

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly
