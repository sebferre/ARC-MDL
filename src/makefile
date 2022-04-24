
OBJ=bintree.cmo common.cmo xprint.cmo iterator.cmo myseq.cmo mdl.cmo find_merge.cmo intmap.cmo
SRC=grid.cmo task.cmo model2.cmo
SRC_FABLIS=focus.cmo syntax.cmo lis.cmo jsutils.cmo webapp.cmo html.cmo widget_focus.cmo widget_suggestions.cmo widget_table.cmo widget_commandline.cmo

FLAGS= -I ../../lib -package str,num,zarith,yojson,ppx_deriving_yojson,ANSITerminal
FLAGS_FABLIS = -I ../../fablis/core -I ../../fablis/core/webapp
#JSOO_FLAGS = -pretty -no-inline -debug-info # for dev
JSOO_FLAGS = --opt 3 # for prod
# +zarith_stubs_js/biginteger.js +zarith_stubs_js/runtime.js # JSOO options for using module Z

all: test

test: $(SRC:.cmo=.cmx) test.ml
	ocamlfind ocamlopt $(FLAGS) -linkpkg -o test $(OBJ:.cmo=.cmx) $(SRC:.cmo=.cmx) test.ml

lis: $(SRC) arc_lis.ml
# requires to set profiling off (Common.prof_on := false)
	ocamlfind ocamlc $(FLAGS) $(FLAGS_FABLIS) -package js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx -linkpkg -o html/script.byte $(OBJ) $(SRC) $(SRC_FABLIS) arc_lis.ml
	js_of_ocaml $(JSOO_FLAGS) html/script.byte

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<

%.cmx: %.ml
	ocamlfind ocamlopt -c $(FLAGS) $<
