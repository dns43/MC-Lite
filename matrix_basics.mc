//basic matrix manipulation

//matrix initialization 

mat A[2,2]; 

mat B[2,2] = [1, 2, 3, 4];

mat C[2,2] = 1;

mat D[1,1];

//matrix indexing 

float i;
float a;
float b; 

i = B[0,1];
D = B [1:2, 1:2];

C = A .* B 
/*example of element-wise multiplication, matrix C will look like - 
 *  2  8 
 * 18  32 
 */

C = A #* B 
/*example of matrix multiplication, matrix C will look like - 
 *  14   20
 *  30  44
 */
  
