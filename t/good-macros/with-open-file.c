#include <stdio.h>

macro withOpenFile {
  case {
    match {
      ($(stream-name ident), $(pathname string), $(direction string))
        $(body block)
    }
    template {
      {
        FILE* $(stream-name) = fopen($(pathname), $(direction));
        if($(stream-name))
          $(body)
        fclose($(stream-name));
      }
    }
  }
}

int main() {
  withOpenFile(file, "README.md", "r") {
    printf("The first character in the README is: %c\n",
           fgetc(file));
  }
  return 0;
}
