all: lexer.cmo parser.cmo prolog.cmo solver.cmo print.cmo
	ocamlc -g -o prolog unix.cma solver.cmo lexer.cmo parser.cmo prolog.cmo
	ocamlmktop -o print.top solver.cmo parser.cmo lexer.cmo print.cmo

lexer.cmo: lexer.ml parser.cmi
	ocamlc -g -c lexer.ml

parser.cmo: parser.ml parser.cmi
	ocamlc -g -c parser.ml

prolog.cmo: prolog.ml
	ocamlc -g -c prolog.ml

print.cmo: print.ml
	ocamlc -g -c print.ml

parser.cmi: parser.mli
	ocamlc -g -c parser.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll       # generates lexer.ml

parser.mli: parser.mly solver.cmo
	ocamlyacc parser.mly     # generates parser.ml and parser.mli

parser.ml: parser.mly
	ocamlyacc parser.mly     # generates parser.ml and parser.mli

solver.cmo: solver.ml
	ocamlc -g -c solver.ml

clean:
	rm -f *.cm*
	rm -f *.mli
	rm -f lexer.ml parser.ml

submit:
	rm -rf 2016CS10680*
	mkdir 2016CS10680
	cp parser.mly lexer.mll *.ml *.pl Makefile prolog print.top ./2016CS10680/
	zip -r 2016CS10680 2016CS10680
