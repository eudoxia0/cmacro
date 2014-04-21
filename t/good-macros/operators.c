macro f {
  case {
    match {
      $(var)
    }
    template {
      $(@to-string var)
    }
  }
}

f x
