mat[2,1] x = [-0.9, 0.1]; // input
mat[2,4] W1 = [0.0, 0.1, 0.02, 0.15, 0.08, 0.05, 0.12, 0.2];
mat[4,1] W2 = [0.23, 0.015, 0.31, 0.0];
mat[4,1] h;
mat[1,1] y;

//h = (W1.~) * x;
h = (W1.~) * x;
printi(1);
printmat(h);
y = (W2.~) * h;

printi(2);
printmat(y);
