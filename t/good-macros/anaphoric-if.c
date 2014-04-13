macro aif {
  case {
    match {
      $(cond)
    }
    template {
      typeof($(cond)) it = $(cond);
      if(it)
    }
  }
}

int main() {
  aif (5 > 1)
    return it;
}
