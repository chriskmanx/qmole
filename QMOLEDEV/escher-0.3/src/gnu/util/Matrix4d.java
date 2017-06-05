package gnu.util;


/** 
 * 4-by-4 matrix of <code>double</code> in mathematics.
 *
 * Modified from <code>vecmath</code> by Kenji Hiranabe.
 */
public class Matrix4d {
  public double [] m = new double [16];


  public double determinant () {
    return determinant (m);
  }


  /** <code>det (A)</code>. */
  public static double determinant (double [] A) {
    return (A [0] * A [5] - A [1] * A [4])
      * (A [10] * A [15] - A [11] * A [14])
      -(A [0] * A [6] - A [2] * A [4]) 
      * (A [9] * A [15] - A [11] * A [13])
      +(A [0] * A [7] - A [3] * A [4]) 
      * (A [9] * A [14] - A [10] * A [13])
      +(A [1] * A [6] - A [2] * A [5]) 
      * (A [8] * A [15] - A [11] * A [12])
      -(A [1] * A [7] - A [3] * A [5]) 
      * (A [8] * A [14] - A [10] * A [12])
      +(A [2] * A [7] - A [3] * A [6]) 
      * (A [8] * A [13] - A [9] * A [12]);
  }
  

  public Matrix4d identity () {
    identity (m);
    return this;
  }


  /** <code>I</code>. */ 
  public static void identity (double [] A) {
    A [0+4*0] = 1; A [0+4*1] = 0; A [0+4*2] = 0; A [0+4*3] = 0;
    A [1+4*0] = 0; A [1+4*1] = 1; A [1+4*2] = 0; A [1+4*3] = 0;
    A [2+4*0] = 0; A [2+4*1] = 0; A [2+4*2] = 1; A [2+4*3] = 0;
    A [3+4*0] = 0; A [3+4*1] = 0; A [3+4*2] = 0; A [3+4*3] = 1;
  }


  public Matrix4d invert () {
    return invert (this);
  }


  public Matrix4d invert (Matrix4d A) {
    double [] B = m;
    if (A == this) B = new double [16];
    m = invert (A.m, B);
    return A;
  }


  /** <code>B = A^(-1).</code> */
  public static double [] invert (double [] A, double [] B) {
    double det = determinant (A);
    if (det == 0) return null;

    B [0] = A [5] * (A [10] * A [15] - A [11] * A [14]) 
    + A [6] * (A [11] * A [13] - A [9] * A [15]) + A [7] * 
    (A [9] * A [14] - A [10] * A [13]);

    B [1] = A [9] * (A [2] * A [15] - A [3] * A [14])
    + A [10] * (A [3] * A [13] - A [1] * A [15])
    + A [11] * (A [1] * A [14] - A [2] * A [13]);

    B [2] = A [13] * (A [2] * A [7] - A [3] * A [6])
    + A [14] * (A [3] * A [5] - A [1] * A [7])
    + A [15] * (A [1] * A [6] - A [2] * A [5]);

    B [3] = A [1] * (A [7] * A [10] - A [6] * A [11])
    + A [2] * (A [5] * A [11] - A [7] * A [9])
    + A [3] * (A [6] * A [9] - A [5] * A [10]);

    B [4] = A [6] * (A [8] * A [15] - A [11] * A [12])
    + A [7] * (A [10] * A [12] - A [8] * A [14])
    + A [4] * (A [11] * A [14] - A [10] * A [15]);

    B [5] = A [10] * (A [0] * A [15] - A [3] * A [12])
    + A [11] * (A [2] * A [12] - A [0] * A [14])
    + A [8] * (A [3] * A [14] - A [2] * A [15]);

    B [6] = A [14] * (A [0] * A [7] - A [3] * A [4])
    + A [15] * (A [2] * A [4] - A [0] * A [6])
    + A [12] * (A [3] * A [6] - A [2] * A [7]);

    B [7] = A [2] * (A [7] * A [8] - A [4] * A [11])
    + A [3] * (A [4] * A [10] - A [6] * A [8])
    + A [0] * (A [6] * A [11] - A [7] * A [10]);

    B [8] = A [7] * (A [8] * A [13] - A [9] * A [12])
    + A [4] * (A [9] * A [15] - A [11] * A [13])
    + A [5] * (A [11] * A [12] - A [8] * A [15]);

    B [9] = A [11] * (A [0] * A [13] - A [1] * A [12])
    + A [8] * (A [1] * A [15] - A [3] * A [13])
    + A [9] * (A [3] * A [12] - A [0] * A [15]);

    B [10] = A [15] * (A [0] * A [5] - A [1] * A [4])
    + A [12] * (A [1] * A [7] - A [3] * A [5])
    + A [13] * (A [3] * A [4] - A [0] * A [7]);

    B [11] = A [3] * (A [5] * A [8] - A [4] * A [9])
    + A [0] * (A [7] * A [9] - A [5] * A [11])
    + A [1] * (A [4] * A [11] - A [7] * A [8]);

    B [12] = A [4] * (A [10] * A [13] - A [9] * A [14])
    + A [5] * (A [8] * A [14] - A [10] * A [12])
    + A [6] * (A [9] * A [12] - A [8] * A [13]);

    B [13] = A [8] * (A [2] * A [13] - A [1] * A [14])
    + A [9] * (A [0] * A [14] - A [2] * A [12])
    + A [10] * (A [1] * A [12] - A [0] * A [13]);

    B [14] = A [12] * (A [2] * A [5] - A [1] * A [6])
    + A [13] * (A [0] * A [6] - A [2] * A [4])
    + A [14] * (A [1] * A [4] - A [0] * A [5]);

    B [15] = A [0] * (A [5] * A [10] - A [6] * A [9])
    + A [1] * (A [6] * A [8] - A [4] * A [10])
    + A [2] * (A [4] * A [9] - A [5] * A [8]);
    
    scalar_multiply (B, 1/det, B);
    return B;
  }

  
  public Matrix4d multiply (Matrix4d A, Matrix4d B) {
    double [] C = m;
    if (A == this || B == this) C = new double [16];
    m = multiply (A.m, B.m, C);
    return this;
  }


  /** <code>C = A * B</code>. */
  public static double [] multiply (double [] A, double [] B, 
    double [] C) {

    C [0] = A [0] * B [0] + A [1] * B [4]
      + A [2] * B [8] + A [3] * B [12];
    C [1] = A [0] * B [1] + A [1] * B [5]
      + A [2] * B [9] + A [3] * B [13];
    C [2] = A [0] * B [2] + A [1] * B [6]
      + A [2] * B [10] + A [3] * B [14];
    C [3] = A [0] * B [3] + A [1] * B [7]
      + A [2] * B [11] + A [3] * B [15];

    C [4] = A [4] * B [0] + A [5] * B [4]
      + A [6] * B [8] + A [7] * B [12];
    C [5] = A [4] * B [1] + A [5] * B [5]
      + A [6] * B [9] + A [7] * B [13];
    C [6] = A [4] * B [2] + A [5] * B [6]
      + A [6] * B [10] + A [7] * B [14];
    C [7] = A [4] * B [3] + A [5] * B [7]
      + A [6] * B [11] + A [7] * B [15];

    C [8] = A [8] * B [0] + A [9] * B [4]
      + A [10] * B [8] + A [11] * B [12];
    C [9] = A [8] * B [1] + A [9] * B [5]
      + A [10] * B [9] + A [11] * B [13];
    C [10] = A [8] * B [2] + A [9] * B [6]
      + A [10] * B [10] + A [11] * B [14];
    C [11] = A [8] * B [3] + A [9] * B [7]
      + A [10] * B [11] + A [11] * B [15];

    C [12] = A [12] * B [0] + A [13] * B [4]
      + A [14] * B [8] + A [15] * B [12];
    C [13] = A [12] * B [1] + A [13] * B [5]
      + A [14] * B [9] + A [15] * B [13];
    C [14] = A [12] * B [2] + A [13] * B [6]
      + A [14] * B [10] + A [15] * B [14];
    C [15] = A [12] * B [3] + A [13] * B [7]
      + A [14] * B [11] + A [15] * B [15];

    return C;
  }


  public String toString () {
    return "#Matrix4d"
      + "\n  " + m [0] + " " + m [1] + " " + m [2] + " " + m [3]
      + "\n  " + m [4] + " " + m [5] + " " + m [6] + " " + m [7]
      + "\n  " + m [8] + " " + m [9] + " " + m [10] + " " + m [11]
      + "\n  " + m [12] + " " + m [13] + " " + m [14] + " " + m [15];
  }


  public Matrix4d scalar_multiply (double d) {
    return scalar_multiply (this, d);
  }
  

  public Matrix4d scalar_multiply (Matrix4d A, double d) {
    scalar_multiply (A.m, d, m);
    return this;
  }


  /** B = d * A. */
  public static double [] scalar_multiply (double [] A, 
    double d, double [] B) {

    for (int i=0; i<16; i++) B [i] = A [i] * d;
    return B;
  }


  public void set (int i, int j, double d) {
    set (m, i, j, d);
  }


  /** <code>A(i, j) = d</code>. */
  public static void set (double [] A, int i, int j, double d) {
    A [i + 4*j] = d;
  }
  


  public Matrix4d set_column (int i, Vector4d A) {
    set_column (m, i, A.v);
    return this;
  }


  /** <code>A(i,j) = B</code>. */
  public static double [] set_column (double [] A, int i, double [] B) {
    set_column (A, i, B [0], B [1], B [2], B [3]);
    return A;
  }


  public Matrix4d set_column (int i, double x, double y, 
    double z, double w) {
      
    set_column (m, i, x, y, z, w);
    return this;
  }
      

  /** <code>A(i,j) = [x, y, z, w]</code>. */
  public static double [] set_column (double [] A, int i, double x, double y, 
    double z, double w) {

    set (A, i, 0, x);
    set (A, i, 1, y);
    set (A, i, 2, z);
    set (A, i, 3, w);
    return A;
  }


  public Matrix4d set_column (Vector4d V0, Vector4d V1, 
    Vector4d V2, Vector4d V3) {

    set_column (m, V0.v, V1.v, V2.v, V3.v);
    return this;
  }


  /** <code>A(0,j) = V0, A(1,j) = V1, A(2,j) = V2, A(3,j) = V3</code>. */
  public static double [] set_column (double [] A, double [] V0, double [] V1,
    double [] V2, double [] V3) {

    set_column (A, 0, V0);
    set_column (A, 1, V1);
    set_column (A, 2, V2);
    set_column (A, 3, V3);
    return A;
  }


  public Matrix4d set_row (int j, Vector4d A) {
    set_row (m, j, A.v);
    return this;
  }

  
  /** <code>A(i,j) = B</code>. */
  public static double [] set_row (double [] A, int j, double [] B) {
    set_row (A, j, B [0], B [1], B [2], B [3]);
    return A;
  }


  public Matrix4d set_row (int j, double x, double y, 
    double z, double w) {
      
    set_row (m, j, x, y, z, w);
    return this;
  }    
      

  /** <code>A(i,j) = [x, y, z, w]</code>. */
  public static double [] set_row (double [] A, int j, double x, double y, 
    double z, double w) {

    set (A, 0, j, x);
    set (A, 1, j, y);
    set (A, 2, j, z);
    set (A, 3, j, w);
    return A;
  }


  public Matrix4d set_row (Vector4d V0, Vector4d V1, 
    Vector4d V2, Vector4d V3) {

    set_row (m, V0.v, V1.v, V2.v, V3.v);
    return this;
  }


  /** <code>A(0,j) = V0, A(1,j) = V1, A(2,j) = V2, A(3,j) = V3</code>. */
  public static double [] set_row (double [] A, double [] V0, double [] V1,
    double [] V2, double [] V3) {

    set_row (A, 0, V0);
    set_row (A, 1, V1);
    set_row (A, 2, V2);
    set_row (A, 3, V3);
    return A;
  }
}
