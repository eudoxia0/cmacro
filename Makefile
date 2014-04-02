default: parser

parser: lexing.l grammar.y
	bison -d grammar.y
	flex lexing.l
	$(CC) lex.yy.c  grammar.tab.c -lfl -o $@
	rm lex.yy.c grammar.tab.c grammar.tab.h
