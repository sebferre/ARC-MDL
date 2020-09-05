
SRC=task.cmo model.cmo decision.cmo main.cmo

FLAGS= -package yojson,ppx_deriving_yojson

all: $(SRC)
	ocamlfind ocamlc $(FLAGS) -linkpkg -o main $(SRC)

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<
