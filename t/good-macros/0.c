macro b {
  case {
    match {
      $(derp) 2 ;
    }
    template {
      $(derp)
    }
  }
}

int main() {
  b "example" 2;
}
