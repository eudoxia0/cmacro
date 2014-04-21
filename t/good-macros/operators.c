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

macro g {
  case {
    match {
      $(var)
    }
    template {
      $(@splice var)
    }
  }
}

f x
g (1, 2, 3)
g 1
