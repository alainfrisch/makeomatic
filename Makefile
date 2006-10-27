all: makeomatic_wrapper.so makeomatic

makeomatic_wrapper.so: makeomatic_wrapper.c
	gcc -o makeomatic_wrapper.so -shared -ldl makeomatic_wrapper.c

makeomatic: makeomatic.ml
	ocamlc -o makeomatic unix.cma str.cma makeomatic.ml


clean:
	rm -f *.cm* *.so *.o *.a *~
	rm -f makeomatic flags a.out