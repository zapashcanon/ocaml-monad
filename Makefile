all : uninstall stream install

stream :
	ocamlbuild -use-ocamlfind monad.cma tagtree.cma monad.cma tagtree.cma 
	ocamlbuild -use-ocamlfind monad.cmxa tagtree.cmxa monad.cmxa tagtree.cmxa
	ocamlbuild -use-ocamlfind stream.docdir/index.html

clean :
	ocamlbuild -clean

test  :
	ocamlbuild -use-ocamlfind tests.native	

install :
	ocamlfind install monad META _build/*.cma _build/*.cmxa _build/*.mli _build/*.cmi

uninstall :
	ocamlfind remove monad