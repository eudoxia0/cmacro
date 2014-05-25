macro recur_doto {
  case {
    match {
      ($(obj) $(fn) $(args list);)
    }
    template {
      $(fn)($(obj), $(@splice args));
    }
  }
  case {
    match {
      ($(obj) $(fn) $(args list); $(others rest))
    }
    template {
      recur_doto($(obj) $(fn) ($(args list));)
      recur_doto($(obj) $(others))
    }
  }
}

macro doto {
  case {
    match {
      ($(obj)) { $(args rest) }
    }
    template {
      recur_doto($(obj) $(args))
    }
  }
}

void cancelAccount() {
  doto(Account) {
    setBalance(0);
    setStatus(DISABLED);
  }
}
