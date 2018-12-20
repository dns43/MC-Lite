//basic matrix manipulation

//matrix initialization 

mat[2,2] A; 

mat[2,2] B = [1, 2, 3, 4];

mat[2,2] C = 1;

mat D[1,1];

//matrix indexing 

float i;
float a;
float b; 

i = B[0,1];

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
  
