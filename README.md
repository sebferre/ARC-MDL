# ARC-MDL: an MDL-based Approach to the ARC Challenge

The [Abstraction and Reasoning Corpus
(ARC)](https://github.com/fchollet/ARC) was recently introduced by
François Chollet as a tool to measure broad intelligence in both
humans and machines. It is very challenging as best approaches solve
about 20% tasks while humans solve about 80% tasks.

Our approach is based on descriptive grid models and the Minimum
Description Length (MDL) principle. The grid models describe the
contents of a grid, and support both parsing grids and generating
grids. The MDL principle is used to guide the search for good models,
i.e. models that compress the grids the most. Our approach does not
only predict the output grids, but it also outputs an intelligible
model and explanations for how the model was incrementally built.

## Contents

This repo contains:
- the source code of a command line tool (see `test.ml`),
- the source code of a user interface for interactive model building (see `arc_lis.ml`),
- a technical report, which presents our progression on ARC to this date,
- results, i.e. learning and prediction logs, and images of the solved tasks.

## Web Application

You can play with our system without any installation, through the
online [user interface](http://www.irisa.fr/LIS/ferre/arcmdl/). You
can load the JSON file of a task, and from there successively apply
model refinements, while visualizing the effects on each example of
the task.

In short, here are the main UI components:
- *top left*: the model under construction, in the form `CONSTRUCT output-model WHERE input-model`, along with a number of description lengths (DL);
- *top right*: a list of suggestions for refining the model, from the most compressive one to the less compressive one. Suggestions in red are not compressive but they may useful for testing. The button `Choose` enables to load a new task, which resets the model to the initial empty model;
- *bottom*: examples, one per row, with contents dispatched into three columns:
  - **Example**: the input and the expected output
  - **Description**: the input-output pair, as seen by the current model. Void for test examples.
  - **Prediction**: the input as seen by the input pattern, and the predicted output according to the output template, to be compared with the expected output in the left column.


## Installation Instructions

The tool is developed in [OCaml](https://ocaml.org). The web
application is compiled to Javascript with the
[js_of_ocaml](https://ocsigen.org/js_of_ocaml/latest/manual/overview)
tool. It is strongly recommended to use the
[opam](https://opam.ocaml.org/) tool to manage OCaml dependencies.

The dependencies are:
- OCaml compilers version 4.11.1+
- opam packages: yojson, zarith, ANSITerminal, ppx_deriving_yojson, js_of_ocaml, js_of_ocaml-lwt, js_of_ocaml-ppx
- personal libraries: [ocaml-lib](https://bitbucket.org/sebferre/ocaml-lib/src/), [fablis](https://github.com/sebferre/fablis)

When all dependencies are installed, the tool can be compiled by
moving in the `src` directory, and executing the command `make` for
the command line tool, and `make lis` for the web application. This
respectively generates a `test` executable, and a `src/html/script.js`
file that contains the code of a web application, along with other
files in the `src/html` directory (HTML, CSS, ...).

## Credits

Author: [Sébastien Ferré](http://people.irisa.fr/Sebastien.Ferre/)

Affiliation: Univ. Rennes 1, team [LACODAM](http://team.inria.fr/lacodam/) at IRISA

Copyright © 2019+ Sébastien Ferré, IRISA, Université de Rennes 1, France

Licence: GPLv3

Citation: *Ferré, Sébastien. ‘First Steps of an Approach to the ARC Challenge based on Descriptive Grid Models and the Minimum Description Length Principle’. *arXiv preprint arXiv:2112.00848, 2021.* [PDF](https://arxiv.org/pdf/2112.00848)
