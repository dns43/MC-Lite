// sample comment

// variable declaration and assignment
int x = 1;

// increment in place
x++;

int my_function (int x, float y) {

  // z is scoped within {}
  int z = 0;
  int i = 0;

  // while loop
  while (i < 10) {
    z = z + i;
    i++;
  }
  return z;
}

// function call
my_function(1, 2.0);
