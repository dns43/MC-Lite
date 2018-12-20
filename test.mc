// sample comment

// variable declaration and assignment
int x = 1;


// declare 1x3 matrix and assign values
mat [1, 3]m;
m = [1.0, 1.0, 9999.9];

// assign incorrect number of values
// (this is currently valid but will trigger a semantic error)
m = [1, 2, 3, 4];

// declar 2x2 matrix and assign values in one step
// note ints will be cast to floats when assigned to matrix
mat [2, 2]m2 = [1,2,3,4];
mat [2, 2]m3 = [6,7,8,9];
mat [2, 2]m4 = m2 * m3;

// element wise multiplication:
m4 = m2 .* m3;




}
