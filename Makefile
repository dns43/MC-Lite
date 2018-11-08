

default:
	@eval `opam config env`
	ocamlbuild -j 0 -use-ocamlfind -pkgs yojson -r Main.native 

clean:
	rm -r _build Main.native
