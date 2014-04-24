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

macro h {
  case {
    match {
      $(var)
    }
    template {
      $(var)($(@embed my-var int))
    }
  }
}

f x
g (1, 2, 3)
g 1
h a
