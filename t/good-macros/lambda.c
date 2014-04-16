macro lambda {
  case {
    match {
      $(args) -> $(ret) $(body)
    }
    toplevel {
      $(ret) $($lambda) $(args) $(body)
    }
    template {
      $(@lambda/0)
    }
  }
}

int main() {
  lambda args -> ret body;
  lambda (int a, int b) -> int { return a + b; };
}
