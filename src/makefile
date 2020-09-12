
OBJ=bintree.cmo common.cmo find_merge.cmo
SRC=grid.cmo task.cmo model.cmo decision.cmo

FLAGS= -I ../../lib -package str,num,yojson,ppx_deriving_yojson,ANSITerminal

all: main test

main: $(SRC) main.cmo
	ocamlfind ocamlc $(FLAGS) -linkpkg -o main $(OBJ) $(SRC) main.cmo

test: $(SRC) test.cmo
	ocamlfind ocamlc $(FLAGS) -linkpkg -o test $(OBJ) $(SRC) test.cmo

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<
