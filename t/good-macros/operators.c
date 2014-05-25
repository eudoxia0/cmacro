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
    match { $(var) }
    template { $(@conc 1 var 3) }
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

macro i {
  case {
    match {
      $(list list)
    }
    template {
      $(@splice list)
    }
  }
}

f x
g 2
h a
i (1,2,3)
