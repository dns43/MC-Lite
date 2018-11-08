// sample comment

// variable declaration and assignment
int x = 1;

// increment in place
x++;

// declare 1x3 matrix and assign values
mat m[1, 3];
m = [1.0, 1.0, 9999.9];

// assign incorrect number of values
// (this is currently valid but will trigger a semantic error)
m = [1, 2, 3, 4];

// declar 2x2 matrix and assign values in one step
// note ints will be cast to floats when assigned to matrix
mat m2[2, 2] = [1,2,3,4];
mat m3[2, 2] = [6,7,8,9];
mat m4[2, 2] = m2 * m3;

// element wise multiplication:
m4 = m2 .* m3;


// sample function declaration
// note local variable declarations overide enclosing scope
int my_function (int x, float y) {

  // z is scoped within {}
  int z = x + x;
  return 0;
}

// function call
my_function(1, 2.0);
