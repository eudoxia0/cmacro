macro cond {
  case {
    match {
      ($(obj)) {
        $(case) $(body)
      }
    }
    template {
      if($(obj) == $(case)) $(body)
    }
  }
  case {
    match {
      ($(obj)) {
        $(case) $(body)
        $(others rest)
      }
    }
    template {
      if($(obj) == $(case)) $(body)
      cond($(obj)) { $(others) }
    }
  }
}

cond(1) {
  1 { puts("1 = 1!") }
  2 { puts("1 = 2!") }
}
