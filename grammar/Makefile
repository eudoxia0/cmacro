default: parser

parser: lexing.l
	flex lexing.l
	$(CC) lex.yy.c -lfl -o $@
