mat[2,2] x;
mat[3, 2] y = [1, 2, 3 ,4, 5, 6];
mat[2, 2] z = [4,3,2,1];

x = [4,5,6,7];

mat[2, 2] h = x;
h = h+x;

x = h*h;
x = x.~;

float f = x[0, 0];


x[0,1] = x[1, 0];

printi(63110);

// asdf
