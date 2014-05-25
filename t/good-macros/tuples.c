macro tup {
  case {
    match {
      ($(a), $(b))
    }
    template {
      struct { $(a) first; $(b) second; }
    }
  }
}

typedef tup(int, int) pair;

pair divrem(int n, int d) {
  return (pair){n/d, n%d};
}

int main() {
  pair division = divrem(10,7);
  return division.second;
}
