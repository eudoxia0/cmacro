#include <stdlib.h>

macro withAllocation {
  case {
    match {
      ($(ptr-name ident), $(type), $(size))
        $(body block)
    }
    template {
      {
        $(type)* $(ptr-name) = malloc(sizeof($(type)) * $(size));
        if($(ptr-name))
          $(body)
        free($(ptr-name));
      }
    }
  }
  case {
    match {
      ($(ptr-name ident), $(type))
        $(body block)
    }
    template {
      withAllocation($(ptr-name), $(type), 1)
        $(body)
    }
  }
}

int main() {
  withAllocation(ptr, int, 10) {
    size_t i;
    for(i = 0; i < 10; i++) {
      ptr[i] = i*2;
    }
  }
  withAllocation(p, int) {
    *p = 10;
    return *p;
  }
}
