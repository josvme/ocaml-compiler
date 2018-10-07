#include<stdio.h>

int recurCount(int c) {
  if (c > 5) {
    return c;
  }
  else {
    return recurCount(c+1);
  }
}

int main(int argc, char const *argv[])
{
  struct A {
    int a;
    char b;
  } s;
  s.a= 5;
  s.b='A'; 
  char *a = "Hello World";
  printf("%s", a);
  printf("%d", recurCount(6));
  printf("%d, %c", s.a, s.b);
  return 0;
}
