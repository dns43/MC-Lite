//sample program for MC-Lite: Linear Regression
mat A[3,3] = [2, 1, 1, 
  1, -1, -1, 
  1, 2, 1]; //Coefficient Matrix
mat X[0,3] = [3, 0, 0]; //answer column 
mat Dx[3,3] = [3,  1,  1, 
   0, -1, -1,
   0,  2,  1];
mat Dy[3,3] = [2, 3, 1, 1, 0, -1, 1, 0, 1];
mat Dz[3,3] = [2, 1, 3, 1, -1, 0, 1, 2, 0];
float det_A = det(A); //3
float det_A1 = det(Dx); //3
float det_A2 = det(Dy); //-6
float det_A3 = det(Dz); //9
float x = det_A1/det_A; // 1 
float y = det_A2/det_A; //-2
float z = det_A3/det_A; //3 


