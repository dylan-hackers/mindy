module: Matrix
author: Russ Schaaf (rsbe@cs.cmu.edu)
synopsis: Matrix and other linear algebra functions
copyright: See below.

//======================================================================
//
// Copyright (c) 1994, 1995  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================


define class <matrix> (<array>)
  slot dimensions :: <sequence>;
  slot components :: <simple-object-vector>;
end class <matrix>;

define constant matrix-no-default = pair(#f, #f);

define method element (mat :: <matrix>, index :: <integer>,
		       #key default = matrix-no-default) => elem :: <number>;
  if (default == matrix-no-default)
    mat.components[index];
  else
    element(mat.components, index, default: default);
  end if;
end method element;

define method element-setter (new-value, mat :: <matrix>,
			      index :: <integer>) => elem :: <number>;
  mat.components[index] := new-value;
end method element-setter;

define method initialize (mat :: <matrix>,
			  #next next-method,
			  #key dimensions = #[1,1], fill = 0);
  if (dimensions.size == 2)
    mat.dimensions := dimensions;
    mat.components := make(<simple-object-vector>,
			   size: reduce1(\*, dimensions), fill: fill);
    next-method();
  else
    error("Matrices must have two dimensions, not %S.", dimensions);
  end;
  mat;
end method;

define method shallow-copy (old-matrix :: <matrix>) => new-matrix :: <matrix>;
  let new-matrix = make(<matrix>, dimensions: old-matrix.dimensions);
  new-matrix.dimensions := shallow-copy(old-matrix.dimensions);
  new-matrix.components := copy-sequence(old-matrix.components);
  new-matrix;
end method shallow-copy;

define method forward-iteration-protocol(mat :: <matrix>)
 => (initial-state :: <number>, limit :: <number>, next-state :: <function>,
     finished-state? :: <function>, current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
  values( 0, reduce1(\*, mat.dimensions), 
          method (mat, state) state + 1 end,
          method (mat, state, limit) state == limit end,
          method (mat, state) state end,
          method (mat, state) mat.components[state] end,
          method (value, mat, state) mat.components[state] := value end,
          method (mat, state) state end);
end method forward-iteration-protocol;

define method matrix(#rest row-vectors) => mat :: <matrix>;
  let row-size = row-vectors[0].size;
  let mat = make(<matrix>,
		 dimensions: vector(row-vectors.size, row-size));
  if (every?(method (x) x.size == row-size; end, row-vectors))
    mat.components := reduce1(concatenate, row-vectors);
  else
    error("All row vectors must have the same size");
  end if;
  mat;
end method matrix;

// my/ is used so that integer division will work.  If two integers are
// divided, a ratio will be returned.

define method my/ (a :: <general-integer>, b :: <general-integer>)
 => c :: <number>;
  let c = a / as(<ratio>, b);
  if (c.integral?)
    c.numerator;
  else
    c;
  end if;
end method my/;

define method my/ (a :: <number>, b :: <number>) => c :: <number>;
  a / b;
end method my/;

//  This function simply returns the identity matrix of the specified dimension
//  It is a _very_ simple algorithm, it merely fills a matrix with zeros, and
//  then puts ones down the diagonal.
//
define method identity-matrix (#key dimensions = #[1,1])
 => the-identity-matrix :: <matrix>;
  let temp-mat = make(<matrix>, dimensions: dimensions, fill: 0);
  let rows = dimension(temp-mat, 0);
  for(i from 0 below rows)
    temp-mat[i,i] := 1;
  end for;
  temp-mat;
end method identity-matrix;


// Adds two matrices.  This is a binary operator that performs matrix addition
//  on the matrices "matrix1" and "matrix2", storing the result in a new matrix
// "temp-mat", "temp-mat" is then returned on exit.  Matrix addition is very
// simple, all it is is adding terms in corresponding positions
// i.e.          | A[0,0] + B[0,0], A[0,1] + B[0,1],     |
//               | A[1,0] + B[1,0], A[1,1] + B[1,1], ... |
//       A + B = |                 .                     |
//               |                 .                     |
//               |                 .     A[i,j] + B[i,j] |
//
define method \+ (matrix1 :: <matrix>, matrix2 :: <matrix>)
 => summation-matrix :: <matrix>;
  if (matrix1.dimensions ~= matrix2.dimensions)
    error("You can not add matrices of different dimensions.");
  else
    let temp-mat = make(<matrix>, dimensions: dimensions(matrix1));
    map-into(temp-mat, \+, matrix1, matrix2);
  end if;
end method \+ ;


// Martix subtraction is (surprise) just like matrix addition, where the
// element i,j in (A - B) is (A[i,j] - B[i,j])
//
define method \- (matrix1 :: <matrix>, matrix2 :: <matrix>)
 => difference-matrix :: <matrix>;
   if (dimensions(matrix1) ~= dimensions(matrix2))
    error("You can not subtract two matrices of different dimensions.\n");
  else
    let temp-mat = make(<matrix>,dimensions: dimensions(matrix1));
    map-into(temp-mat, \-, matrix1, matrix2);
  end if;
end method \- ; 


// Multiplication of a matrix and a scalar quantity.  This simply multiplies
// each element of the matrix by the scalar quantity.
// i.e.        | n * A[0,0], n * A[0,1],     |
//             | n * A[1,0], n * A[1,1], ... |
//     n * A = |              .              |
//             |              .              |
//             |              .   n * A[i,j] |
//
define method \* (a :: <number>, matrix1 :: <matrix>)
 => product-matrix :: <matrix>;
  let temp-mat = make(<matrix>, dimensions: dimensions(matrix1));
  let rows = dimension(matrix1, 0);
  let columns = dimension(matrix1, 1);
  for (i from 0 below rows)
    for (j from 0 below columns)
      temp-mat[i,j] := a * matrix1[i,j];
    end for;
  end for;
  temp-mat;
end method \*;


// Same as above, only the operators are in a different order.
//
define method \* (matrix1 :: <matrix>, a :: <number>)
 => product-matrix :: <matrix>;
  a * matrix1;
end method \*;

//  Multiplication of two matrices.  There are certain restrictions on what
//  matrices can be multiplied.  To multiply two matrices, the dimensions must
//  be MxN for the first matrix, and NxP for the second.  The result of the
//  multiplication will be an MxP matrix.  (Note, this implies A * B is not
//  necessarily equal to B * A)  The element i,j in A * B is the dot (inner)
//  product of the ith row of A with the jth column of B.  The dot product of
//  a vector is the sum of the products of corresponding elements. That is,
//  if vector V=[a,b,c,d] and vector W=[w,x,y,z] then V*W= aw + bx + cy + dz
//  (Note that this is a scalar quantity).  So a multiplication might look like
//  this:
//       | 2  3 |     | 1  4 |     | (2*1 + 3*7), (2*4 + 3*8) |   | 23 32 |
//       | 6  5 |  *  | 7  8 |  =  | (6*1 + 5*7), (6*4 + 5*8) | = | 41 64 |
//
//  The actual algorithm used was reproduced from Sedgewick's Algorithms, Ch. 
//  36.
//  Basically, the algorithm goes through the temporary matrix, filling out
//  each element in the following way: For the i,jth element of the matrix,
//  calculate the dot product an element at a time, by having a number k  range
//  from 0 to the dimension N of the matrix.  Take A[i,k] and multiply it by
//  B[k,j] to get one of the terms in the dot product.  Continue this for each
//  element in the MxP matrix to get the result.
//
define method \* (matrix1 :: <matrix>, matrix2 :: <matrix>)
 => mult-matrix :: <matrix>;
  let rows1 = dimension(matrix1, 0);
  let columns1 = dimension(matrix1, 1);
  let rows2 = dimension(matrix2, 0);
  let columns2 = dimension(matrix2, 1);
  if (columns1 ~= rows2)
    error("You cannot multiply two matrices of dimensions NxM and PxQ where M != P.");
  else 
    let temp-mat = make(<matrix>, dimensions: list(rows1,columns2), 
                   fill: 0 );
    for (i from 0 below rows1)
      for (j from 0 below columns2)
	for (k from 0 below columns1)
	  temp-mat[i,j] := temp-mat[i,j] + matrix1[i,k] * matrix2[k,j];
	end for;
      end for;
    end for;
    temp-mat;
  end if; 
end method \*; 


// The augment-matrix procedure will take matrices A and B and return a matrix
// | A B |.  For example, A = | 2 3 |  B = | 1 5 |
//                            | 4 5 |      | 2 9 |
// augment-matrix(A,B) = | 2 3 1 5 |
//                       | 4 5 2 9 |
//
define method augment-matrix (matrix1 :: <matrix>, matrix2 :: <matrix>)
 => augmented-matrix :: <matrix>;
  let rows1 = dimension(matrix1, 0);
  let columns1 = dimension(matrix1, 1);
  let rows2 = dimension(matrix2, 0);
  let columns2 = dimension(matrix2, 1);
  if (rows1 ~= rows2)
    error("You can't augment two matrices of dimensions NxM and PxQ where N != P\n");
  else
    let temp-mat = make(<matrix>, 
          dimensions: vector(rows1, columns1 + columns2));
    for (j from 0 below rows1)
      for (i from 0 below columns1)
	temp-mat[j,i] := matrix1[j,i];
      end for;
      for (temp-i from columns1 below (columns2 + columns1),
	   i from 0 below columns2)
	temp-mat[j,columns1 + i] := matrix2[j,i];
      end for;
    end for;
    temp-mat;
  end if;
end method;


// This procedure does gauss-jordan elimation on a matrix of dimension N by
// N + 1.  The first N columns are the coefficents in a set of simultaneous
// equations, and the last column is a solution vector.  So if you had the 
// set of equations
//          2(x1) + 4(x2) - 2(x3) = 2
//          4(x1) + 9(x2) - 3(x3) = 8
//         -2(x1) - 3(x2) + 7(x3) = 10 (where the variables are x1, x2, x3)
// The matrix representing this would be |  2  4 -2  2 |
//                                       |  4  9 -3  8 |
//                                       | -2 -3  7 10 |
// And the matrix that is returned is an N by 1 matrix contaning the solution
// for each variable.  In other words, it returns | x1 |
//                                                | x2 |
//                                                | x3 |
//                                                | .  |
//                                                | .  |
//
define method gauss-jordan (matrix1 :: <matrix>)
 => solution-matrix :: <matrix>;
//  let temp-mat = make(<matrix>, dimensions: matrix1.dimensions);
//  temp-mat.contents := shallow-copy(matrix1.contents);
  let temp-mat = shallow-copy(matrix1);
  let temp = 0;
  let rows = dimension(temp-mat, 0);
  let columns = dimension(temp-mat, 1);
  for (i from 0 below rows)
    let max = i;

    // Finds good row for elimination
    for (j from i + 1 below rows)
      if (abs(temp-mat[j,i]) > abs(temp-mat[max,i]))
	max := j;
      end if; 
    end for;

    // Switch good row with ith row
    for (k from i below columns)
      temp := temp-mat[i,k];
      temp-mat[i,k] := temp-mat[max,k];
      temp-mat[max,k] := temp;
    end for;

    // Use elimination to make the upper triangular matrix
    for (j from i + 1 below rows)
      for (k from columns - 1 to i by -1)
	temp-mat[j,k] := (temp-mat[j,k]
			  - temp-mat[i,k] * my/(temp-mat[j,i], temp-mat[i,i]));
      end for;
    end for;
  end for;
  
  // Finds solution matrix and return it.
  let temp-vect = make(<matrix>, dimensions: vector(rows, 1), fill: 0);
  for (j from rows - 1 to 0 by -1)
    let t  = 0;
    for (k from j + 1 below rows)
      t := t + temp-mat[j,k] * temp-vect[k,0];
    end for;
    temp-vect[j,0] := my/((temp-mat[j,columns - 1] - t ), temp-mat[j,j]);
  end for; 
   temp-vect;
end method gauss-jordan;


// Finds the inverse of a matrix, by using a modified gauss-jordan elimination
// Given any matrix, if there is an inverse, the inverse will be returned, 
// otherwise, an error will be signalled. To determine the existance of an
// inverse, the algorithm finds the upper triangular matrix, and then
// multiplies all of the elements along the main diagonal.  If the result of
// this multiplication is zero, there is no inverse, otherwise, there is
// gaurenteed to be an inverse.
//
define method inverse (matrix1 :: <matrix>)
 => inverted-matrix :: <matrix>;
  let rows = dimension(matrix1, 0);
  let columns = dimension(matrix1, 1);
  
  // make sure that matrix is square
  if (rows ~= columns)
    error("Matrix can not be inverted.\n");
  end if;
  
  let temp-mat = make(<matrix>, dimensions: list(rows, 2 * columns));
  temp-mat := augment-matrix(matrix1,
			     identity-matrix( dimensions:
					       dimensions(matrix1)));
//  columns := 2 * columns;

  // Make the upper triangular matrix
 let max :: <integer> = 0;
  let t :: <number> = 0;
  
  for (i from 0 below rows)

    max := i;
    for (j from i + 1 below rows)
      if (abs(temp-mat[j,i]) > abs(temp-mat[max, i]))
	max := j;
      end if;
    end for;
    
    for (k from i below columns * 2)
      t := temp-mat[i,k];
      temp-mat[i,k] := temp-mat[max, k];
      temp-mat[max,k] := t;
    end for;
    
    for (j from i + 1 below rows)
      for (k from (columns * 2) - 1 to i by -1)
	temp-mat[j,k] := (temp-mat[j,k]
			  - temp-mat[i,k] * my/(temp-mat[j,i], temp-mat[i,i]));
      end for;
    end for;
  end for;

 /* for (i from 0 below rows)
    for (j from i + 1 below rows)
      for (k from columns - 1 to i by -1)
	temp-mat[j,k]
	  := temp-mat[j,k] - temp-mat[i,k] * my/(temp-mat[j,i], temp-mat[i,i]);
      end for;
    end for;
  end for;
*/

  // If any element along the diagonal is zero, signal an error
  if (block (exit)
	for (i from 0 below rows)
	  if (temp-mat[i,i] = 0) exit(#t) end;
	end;
	#f;
      end)
    error("Matrix with determinant of zero can not be inverted.");
  end;

  // Find diagonal matrix on the left (non-zero elements along the diagonal
  // only
  for (i from rows - 1 to 1 by -1)
    for (j from  i - 1  to 0 by - 1)
      for (k from (columns * 2) - 1 to 0 by -1)
	temp-mat[j,k]
	  := temp-mat[j,k] - temp-mat[i,k] * my/(temp-mat[j,i], temp-mat[i,i]);
      end for;
    end for;
  end for;

  // Divide through by the diagonal element to get the identity matrix on the
  // left.
  for (i from 0 below rows)
    for (j from (columns * 2) - 1 to i by -1)
      temp-mat[i,j] := my/(temp-mat[i,j], temp-mat[i,i]);
    end for;
  end for;

  //Now we have two augmented square matrices, and we must return just the
  // matrix on the right
  let inverted-matrix = make(<matrix>, dimensions: matrix1.dimensions);
  for (i from 0 below rows)
    for (j from 0 below columns)
      inverted-matrix[i, j] := temp-mat[i, j + columns];
    end for;
  end for;

  inverted-matrix;
  
end method inverse;

// This just does what the first half of inverse does. It reduces the matrix 
// to the upper triangular form, and then returns the product of all of the 
// elements along the diagonal.  This is the determinant of the matrix.
//
define method det (matrix1 :: <matrix>) 
 => determinant :: <number>;
  let rows = dimension(matrix1, 0);
  let columns = dimension(matrix1, 1);
  if (rows ~= columns)
    error("Matrix must be square to have a determinant.");
  end if;
  
  let temp-mat = make(<matrix>, dimensions: dimensions(matrix1));
  map-into(temp-mat, identity, matrix1);
    
  // Finds the upper triangular
  let max :: <integer> = 0;
  let t :: <number> = 0;
  let pos-or-neg :: <integer> = 1;
  
  for (i from 0 below rows)

    max := i;
    for (j from i + 1 below rows)
      if (abs(temp-mat[j,i]) > abs(temp-mat[max, i]))
	max := j;
      end if;
    end for;

    if (~(i = max))
      pos-or-neg := -pos-or-neg;
    end if;
    
    for (k from i below columns)
      t := temp-mat[i,k];
      temp-mat[i,k] := temp-mat[max, k];
      temp-mat[max,k] := t;
    end for;
    
    for (j from i + 1 below rows)
      for (k from columns - 1 to i by -1)
	temp-mat[j,k] := (temp-mat[j,k]
			  - temp-mat[i,k] * my/(temp-mat[j,i], temp-mat[i,i]));
      end for;
    end for;
  end for;

  // Multiplies along the diagonal to find determinant
  for (i from 0 below rows,
       deter = pos-or-neg then (deter * temp-mat[i,i]))
  finally
    deter
  end for;
  
end method det;


// This function transposes a matrix.  It takes a M by N matrix and turns it 
// into an N by M matrix.  All it does is take the i,jth element in the M by N
// matrix, and turns it into the j,ith element in the N by M matrix.
//
define method transpose (matrix1 :: <matrix>)
 => transposed-matrix :: <matrix>;
  let rows = dimension(matrix1, 0);
  let columns = dimension(matrix1, 1);
  let trans-mat = make(<matrix>, dimensions: list(columns, rows));
  for (i from 0 below rows)
    for (j from 0 below columns)
      trans-mat[j,i] := matrix1[i,j];
    end for;
  end for;
  trans-mat;
end method transpose;








