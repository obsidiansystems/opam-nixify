
all:
	jbuilder build @install

.PHONY: fmt
fmt:
	ocamlformat --inplace $$(find src -name "*.ml" -o -name "*.mli")
