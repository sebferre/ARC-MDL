
SRC=task.cmo model.cmo decision.cmo

FLAGS= -package yojson,ppx_deriving_yojson,ANSITerminal

main: $(SRC) main.cmo
	ocamlfind ocamlc $(FLAGS) -linkpkg -o main $(SRC) main.cmo

test: $(SRC) test.cmo
	ocamlfind ocamlc $(FLAGS) -linkpkg -o test $(SRC) test.cmo

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<
