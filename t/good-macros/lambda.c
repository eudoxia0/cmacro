macro lambda {
  case {
    match {
      $(args) -> $(ret) $(body)
    }
    template {
      $(@getsym lambda 0)
    }
    toplevel {
      $(ret) $(@gensym lambda) $(args) $(body)
    }
  }
}

int main() {
  lambda args -> ret body;
  lambda (int a, int b) -> int { return a + b; };
}
