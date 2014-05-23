#include <stdlib.h>
#include <stdio.h>

macro lambda {
  case {
    match {
      $(args) -> $(ret) $(body)
    }
    template {
      $(@getsym lambda 0)
    }
    toplevel {
      $(ret) $(@gensym lambda) $(args) $(body)
    }
  }
}

int main() {
  int array[] = {423, 61, 957, 133, 969, 829, 821, 390, 704, 596};
  
  qsort(array, 10, sizeof(int),
        lambda (const void* a, const void* b) -> int
        { return *(int*)a - *(int*)b; });
  for(size_t i = 0; i < 10; i++){
    printf("%i ", array[i]);
  }
  return 0;
}
