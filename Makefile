all: makeomatic_wrapper.so makeomatic

makeomatic_wrapper.so: makeomatic_wrapper.c
	gcc -o makeomatic_wrapper.so -shared -ldl makeomatic_wrapper.c

makeomatic.cmi: makeomatic.mli
	ocamlc -c makeomatic.mli

makeomatic.cmo: makeomatic.cmi makeomatic.ml
	ocamlc -c  makeomatic.ml

makeomatic.cmx: makeomatic.cmi makeomatic.ml
	ocamlopt -c makeomatic.ml

make_cduce: makeomatic.cmx cduce.ml
	ocamlopt -o make_cduce unix.cmxa str.cmxa makeomatic.cmx cduce.ml


clean:
	rm -f *.cm* *.so *.o *.a *~
	rm -f makeomatic flags a.out