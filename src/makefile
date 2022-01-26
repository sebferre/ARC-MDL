
OBJ=bintree.cmo common.cmo xprint.cmo myseq.cmo mdl.cmo find_merge.cmo
SRC=grid.cmo task.cmo model2.cmo
SRC_FABLIS=focus.cmo syntax.cmo lis.cmo jsutils.cmo webapp.cmo html.cmo widget_focus.cmo widget_suggestions.cmo widget_table.cmo widget_commandline.cmo

FLAGS= -g -I ../../lib -package str,num,zarith,yojson,ppx_deriving_yojson,ANSITerminal
FLAGS_FABLIS = -I ../../fablis/core -I ../../fablis/core/webapp

all: test

test: $(SRC) test.ml
	ocamlfind ocamlc $(FLAGS) -linkpkg -o test $(OBJ) $(SRC) test.ml

lis: $(SRC) arc_lis.ml
# requires to set profiling off (Common.prof_on := false)
	ocamlfind ocamlc $(FLAGS) $(FLAGS_FABLIS) -package js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx -linkpkg -o html/script.byte $(OBJ) $(SRC) $(SRC_FABLIS) arc_lis.ml
	js_of_ocaml +zarith_stubs_js/biginteger.js +zarith_stubs_js/runtime.js html/script.byte

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<
