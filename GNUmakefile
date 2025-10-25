
.PHONY : all
all: build-generator run-generator clean-site generate-site

.PHONY : build-generator
build-generator:
	cabal build ze-ueber-generator --project-dir ze-ueber-generator

.PHONY : run-generator
run-generator:
	cabal run --project-dir ze-ueber-generator ze-ueber-generator -- --prefix-file "./templates/main_prefix.md" --prefix-file "./templates/table_prefix.md" --data-file "./data.yaml" --postfix-file "./templates/table_postfix.md" --target-file "./content/_index.md" --target-scale 34 --target-scale 32 --target-scale 30

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
