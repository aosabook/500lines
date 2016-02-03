lexer: compile
	@./dist/build/pow-lexer/pow-lexer

parser: ast_to_lisp compile
	@./dist/build/pow-parser/pow-parser | ./src/tools/ast_to_lisp

compile:
	@cabal configure > /dev/null
	@cabal build > /dev/null

clean:
	cabal clean
	rm -f ./src/tools/sdiff

ast_to_lisp:
	@raco exe ./src/tools/ast_to_lisp.rkt

