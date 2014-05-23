macro var {
  case {
    match {
      $(name) = $(val);
    }
    template {
      typeof($(val)) $(name) = $(val);
    }
  }
}

int main() {
  var a = 10;
  var b = 3.14;
  return a;
}

/* Output */

int main () {
  typeof(10) a = 10;
  typeof(3.14) b= 3.14;
  return a;
}
