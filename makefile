BUILD = ocamlbuild -use-ocamlfind  

all: evaluation expr expr_tests \
	 eval_common eval_distinct eval_utils \
	 env evaluation_tests passert miniml \
	 tests

evaluation: evaluation.ml 
	${BUILD} evaluation.byte

expr: expr.ml 
	${BUILD} expr.byte

miniml: miniml.ml 
	${BUILD} miniml.byte

eval_common: eval_common.ml 
	${BUILD} eval_common.byte

eval_distinct: eval_distinct.ml 
	${BUILD} eval_distinct.byte

eval_utils: eval_utils.ml 
	${BUILD} eval_utils.byte

env: env.ml 
	${BUILD} env.byte

expr_tests: expr_tests.ml 
	${BUILD} expr_tests.byte

evaluation_tests: evaluation_tests.ml
	${BUILD} evaluation_tests.byte

tests: tests.ml 
	${BUILD} tests.byte

passert: passert.ml 
	${BUILD} passert.byte