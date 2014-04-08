#define TRUE 0
#define FALSE -1

int side_effecting() {
  puts("I'm a side effect!");
  return 1;
}

macro {
  case {
    $(my-var:ident);
  }
}

int main() 
{
  typeof(side_effecting()) a = side_effecting();
  return a;
}
