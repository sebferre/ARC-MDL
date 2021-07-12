
OBJ=bintree.cmo common.cmo myseq.cmo mdl.cmo iterator.cmo find_merge.cmo intmap.cmo lSet.cmo cis.cmo intset.cmo intrel2.cmo
SRC=grid.cmo task.cmo model.cmo model2.cmo

FLAGS= -g -I ../../lib -package str,num,zarith,yojson,ppx_deriving_yojson,ANSITerminal

all: test

test: $(SRC) test.cmo
	ocamlfind ocamlc $(FLAGS) -linkpkg -o test $(OBJ) $(SRC) test.cmo

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<
