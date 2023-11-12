
OBJ=bintree.cmo common.cmo xprint.cmo iterator.cmo myseq.cmo mymap.cmo mdl.cmo range.cmo find_merge.cmo intmap.cmo madil_common.cmo ndtree.cmo task.cmo data.cmo expr.cmo model.cmo task_model.cmo refining.cmo learning.cmo madil.cmo batch.cmo
OBJ_FABLIS=focus.cmo syntax.cmo lis.cmo jsutils.cmo webapp.cmo html.cmo widget_focus.cmo widget_suggestions.cmo widget_table.cmo widget_commandline.cmo
SRC=arc_common.cmo bitmap.cmo grid.cmo mask_model.cmo grid_patterns.cmo segment.cmo domain_arc.cmo

# add -g to have backtractes in case of uncaught exceptions
FLAGS= -g -I ../../lib -I ../../madil/src -package str,num,zarith,yojson,ppx_deriving_yojson,ANSITerminal
FLAGS_FABLIS = -I ../../fablis/core -I ../../fablis/core/webapp
#JSOO_FLAGS = --pretty --no-inline --debug-info # for dev
JSOO_FLAGS = --opt 3 # for prod
# +zarith_stubs_js/biginteger.js +zarith_stubs_js/runtime.js # JSOO options for using module Z

all: batch

batch: $(SRC:.cmo=.cmx) batch_arc.ml
	ocamlfind ocamlopt $(FLAGS) -linkpkg -o batch $(OBJ:.cmo=.cmx) $(SRC:.cmo=.cmx) batch_arc.ml

batchc: $(SRC) batch_arc.ml
	ocamlfind ocamlc $(FLAGS) -linkpkg -o batch $(OBJ) $(SRC) batch_arc.ml

lis: $(SRC) lis_arc.ml
	ocamlfind ocamlc $(FLAGS) $(FLAGS_FABLIS) -package js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx -linkpkg -o html/script.byte $(OBJ_FABLIS) $(OBJ) arc_lis.cmo $(SRC) lis_arc.ml
	js_of_ocaml $(JSOO_FLAGS) html/script.byte

arcathon: $(SRC:.cmo=.cmx) arcathon.ml
	ocamlfind ocamlopt $(FLAGS) -linkpkg -o arcathon $(OBJ:.cmo=.cmx) $(SRC:.cmo=.cmx) arcathon.ml

docker: arcathon
	docker build -t sferre/arcathon .
	docker save sferre/arcathon > docker_madil.tar

install:
	cp -r html/index.html html/index.css html/script.js /local/ferre/web/ferre/arcmdl

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<

%.cmx: %.ml
	ocamlfind ocamlopt -c $(FLAGS) $<
