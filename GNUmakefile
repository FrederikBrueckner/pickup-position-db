
.PHONY : all
all: build-generator run-generator clean-site generate-site

.PHONY : build-generator
build-generator:
	cabal build ze-ueber-generator --project-dir ze-ueber-generator

.PHONY : run-generator
run-generator:
	cabal run --project-dir ze-ueber-generator ze-ueber-generator -- --prefix "./ze-ueber-generator/templates/prefix.md" --data "./data.yaml" --postfix "./ze-ueber-generator/templates/postfix.md" --target "./content/_index.md"

.PHONY : clean-site
clean-site:
	rm -fr public/*
	rm -fr resources/*

.PHONY : generate-site
generate-site:
	hugo

.PHONY : clean-generator
clean-generator:
	cabal clean --project-dir ze-ueber-generator
