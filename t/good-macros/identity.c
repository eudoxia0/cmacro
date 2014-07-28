macro id {
  case {
    match {
      ($(var))
    }
    template {
      $(var)
    }
  }
}

/* Input to cmacro */
id(1)
/* Output */
1
