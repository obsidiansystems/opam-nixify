EXE=_build/default/src/main.exe
all:
	jbuilder build @install

fmt:
	ocamlformat --inplace $$(find src -name "*.ml" -o -name "*.mli")
