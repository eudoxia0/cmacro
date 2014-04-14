macro forEach {
  case {
    match {
      ($(item), $(collection))
    }
    template {
      for(typeof($(item)) $(item) = $(collection).begin();
          $(item) != $(collection).end();
          ++$(item))
    }
  }
}

int main() {
  forEach(item, array) {
    do_something(item);
  }
}
