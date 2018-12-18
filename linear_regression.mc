//MC-Lite Sample Program - Linear Regression illustrating OLS regession on 10 data points 

mat X[10, 2] =   [1, 1,
1, 2,
1, 3,
1, 4,
1, 5,
1, 6,
1, 7,
1, 8,
1, 9];
mat Y[10, 1] = [1.1, 1.5, 2.3, 2.4, 2.9, 
3.7, 4.2, 4.4, 5.0, 5.5];
mat b[2, 1] = (X.~ * X).inv * X.~ * y
printMat(b) // print result



