macro tup {
  case {
    match {
      ($(a))
    }
    template {
      $(@getsym tup1)
    }
    toplevel {
      typedef struct { $(a) first; } $(@gensym tup1);
    }
  }
  case {
    match {
      ($(a), $(b))
    }
    template {
      $(@getsym tup2)
    }
    toplevel {
      typedef struct { $(a) first; $(b) second; } $(@gensym tup2);
    }
  }
  case {
    match {
      ($(a), $(b), $(c))
    }
    template {
      $(@getsym tup3)
    }
    toplevel {
      typedef struct { $(a) first; $(b) second; $(c) third; } $(@gensym tup3);
    }
  }
}

typedef tup(double, double, double) triple;

triple origin() {
  return (triple){0, 0, 0};
}

int main() {
  tup(int,int) pair = {1, 2};
  return pair.second + origin().third;
}
