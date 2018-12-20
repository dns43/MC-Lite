//Sample Program MC-Lite: Cramer's rule to solve system of linear equations 
mat[2,2] A = [1, 2, 1, -1];
mat[2,2] B = [5, 2, -1, -1];
mat[2,2] C = [1, 5, 1, -1];

float a = A[0, 0]; //1
float b = A[0, 1];//2
float c = A[1, 0];//1
float d= A[1,1]; //-1

float det_1 = (a*c - b*d);

float a = B[0, 0]; //5
float b = B[0, 1];//2
float c = B[1, 0];//-1
float d= B[1,1]; //-1

float det_2 = (a*c - b*d);

float a = C[0, 0]; //1
float b = C[0, 1];//5
float c = C[1, 0];//1
float d= C[1,1]; //-1

float det_3 = (a*c - b*d);

float x = det_2 / det_1;
float y = det_3 / det_1;

printf(x);
printf(y);
