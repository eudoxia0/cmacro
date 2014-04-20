macro forEach {
  case {
    match {
      ($(item), $(collection)) $(body)
    }
    template {
      {
        size_t index;
        typeof($(collection)[0]) item;
        for(index = 0, item = nth($(collection), 0);
            index < length($(collection));
            index++)
        $(body)
      }
    }
  }
}

int main() {
  forEach(item, array) {
    do_something(item);
  }
}
