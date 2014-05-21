macro a {
  case {
    match {
      $(v1 ident) $(v2 int) $(v3 float) $(v4 string)
    }
    template {
      1
    }
  }
}

a test 1 3.14 "test"
