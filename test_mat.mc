mat[2,2] x;
mat[3, 2] y = [1, 2, 3 ,4, 5, 6];
mat[2, 2] z = [4,3,2,1];

x = [4,5,6,7];
printi(1);
printmat(x);

mat[2, 2] h = x;
printi(2);
printmat(h);
h = h+x;
printi(3);
printmat(h);
printi(4);
printmat(h+1.0);
printmat(1.0+h);
printi(5);
printmat(h-1.0);

x = h*z;
printi(6);
printmat(y);
mat[2,3] t = y.~;
printi(7);
printmat(t);
mat[2,3] t = y;
printi(8);
printmat(t);

printi(90);
printmat(t*0.2);
printi(91);
printmat(0.2*t);

printi(10);
printf(1.1 * 2.0);


float f = x[0, 0];


x[0,1] = x[1, 0];

printi(63110);
printf(1.1);
printmat(x);

// asdf
