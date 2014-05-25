macro recur {
  case {
    match {
      ($(first))
    }
    template {
      $(first)
    }
  }
  case {
    match {
      ($(first) $(others rest))
    }
    template {
      recur($(first)) (recur ($(others)))
    }
  }
}

recur (1 2 3)
