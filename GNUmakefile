
.PHONY : all
all: build-generator run-generator

.PHONY : build-generator
build-generator:
	cabal build ze-ueber-generator --project-dir ze-ueber-generator

.PHONY : run-generator
run-generator:
	cabal run --project-dir ze-ueber-generator ze-ueber-generator -- --prefix "geht di nix an" --data "sicha ned" --postfix "naaaaaa" --target "nicht wirklich"

.PHONY : clean-generator
clean-generator:
	cabal clean --project-dir ze-ueber-generator
