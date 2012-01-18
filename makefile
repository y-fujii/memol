SRCS = \
	misc.ml ast.ml parser.mli parser.ml lexer.ml sequence.ml generate.ml \
	smf.ml main.ml
LIBS = nums.cma
PROG = memol


.PHONY: all test clean

all: $(PROG)

test: $(PROG)
	./memol test.mol

clean:
	rm -f *.cm? *.o lexer.ml parser.mli parser.ml tags $(PROG)


$(PROG): $(SRCS)
	ocamlc -o $(PROG) $(LIBS) $(SRCS)

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly
