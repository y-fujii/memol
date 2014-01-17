SRCS = \
	misc.ml ast.ml parser.mli parser.ml lexer.ml smf.ml sequence.ml generate.ml \
	jack.ml jack_c.o main.ml
PROG = memol
LIBS = nums.cmxa unix.cmxa -cclib -ljack
CXX  = clang++ -std=c++11 -pedantic -Wall -Wextra -O3


.PHONY: all test clean

all: $(PROG)

test: $(PROG)
	./memol -j test.mol

clean:
	rm -f *.cm? *.o lexer.ml parser.mli parser.ml tags $(PROG)


$(PROG): $(SRCS)
	ocamlopt -w +a-27 -cc "$(CXX)" -o $(PROG) $(LIBS) $(SRCS)

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

jack_c.o: jack_c.cpp
	$(CXX) -c jack_c.cpp -I$$(ocamlopt -where)
