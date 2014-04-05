enum yytokentype {
  IDENTIFIER = 258,
  I_CONSTANT = 259,
  F_CONSTANT = 260,
  STRING_LITERAL = 261,
  OPERATOR = 262,
};

const char* identifier_names[] = {"idn","int","flt","str","opr"};

const char* map_tok_type(int type) {
  return identifier_names[type-258];
}
