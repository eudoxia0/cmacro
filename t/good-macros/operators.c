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

macro h {
  case {
    match {
      $(var)
    }
    template {
      $(@embed my-var int)
    }
  }
}

f x
h a
