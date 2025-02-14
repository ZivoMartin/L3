all: ast_interpreter.out program_lexer.out hand_lexer.out serialiser.out simple_lexer.out calculette.out

ast_interpreter.out: compiler/*/*.ml compiler/*/*.mli
	rm -rf ast_interpreter.out
	dune build
	ln -s _build/default/compiler/bin_ast_interpreter/main.exe ast_interpreter.out

program_lexer.out: compiler/course_language/Lexer.mll
	rm -rf program_lexer.out
	dune build
	ln -s _build/default/compiler/bin_program_lexer/main.exe program_lexer.out

hand_lexer.out: td_3/hand_lexer/*.ml
	rm -rf hand_lexer.out
	dune build
	ln -s _build/default/td_3/hand_lexer/main.exe hand_lexer.out

serialiser.out: td_3/serialiser/*.ml
	rm -rf serialiser.out
	dune build
	ln -s _build/default/td_3/serialiser/main.exe serialiser.out

simple_lexer.out: td_3/simple_lexer/*.ml
	rm -rf simple_lexer.out
	dune build
	ln -s _build/default/td_3/simple_lexer/main.exe simple_lexer.out

calculette.out: td_3/calculette_lexer/*.ml
	rm -rf calculette.out
	dune build
	ln -s _build/default/td_3/calculette_lexer/main.exe calculette.out


.PHONY: clean
clean:
	rm -rf *.out doc.html
	dune clean

.PHONY: doc
doc: doc.html

doc.html:
	rm -f doc.html
	dune build @doc
	ln -s _build/default/_doc/_html/index.html doc.html