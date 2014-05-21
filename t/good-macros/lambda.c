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
  lambda (int a, int b) -> int { return a + b; };
}
