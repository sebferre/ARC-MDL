
OBJ=bintree.cmo common.cmo myseq.cmo mdl.cmo find_merge.cmo
SRC=grid.cmo task.cmo model2.cmo

FLAGS= -g -I ../../lib -package str,num,zarith,yojson,ppx_deriving_yojson,ANSITerminal

all: test

test: $(SRC) test.cmo
	ocamlfind ocamlc $(FLAGS) -linkpkg -o test $(OBJ) $(SRC) test.cmo

webapp: $(SRC)	
# requires to set profiling off (Common.prof_on := false)
	ocamlfind ocamlc $(FLAGS) -package js_of_ocaml,js_of_ocaml-lwt -linkpkg -o html/script.byte $(OBJ) $(SRC)
	js_of_ocaml +zarith_stubs_js/biginteger.js +zarith_stubs_js/runtime.js html/script.byte

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<
