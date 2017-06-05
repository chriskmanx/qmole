package gnu.util;


/** 
 * 4-by-4 matrix of <code>double</code> in mathematics.
 *
 * Modified from <code>vecmath</code> by Kenji Hiranabe.
 */
public class Matrix4d {
  private double [] matrix = new double [16];

  /** <code>det (A)</code>. */
  private double determinant (double [] A) {
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
  
  public Matrix4d invert () {
    return invert (this);
  }


  private Matrix4d invert (Matrix4d A) {
    double [] B = matrix;
    if (A == this) B = new double [16];
    matrix = invert (A.matrix, B);
    return A;
  }


  /** <code>B = A^(-1).</code> */
  private double [] invert (double [] A, double [] B) {
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
    
    scalarMultiply (B, 1/det, B);
    return B;
  }

  public Matrix4d multiply (Matrix4d A, Matrix4d B) {
      double [] C = matrix;
      if (A == this || B == this) C = new double [16];
      matrix = multiply (A.getMatrix(), B.getMatrix(), C);
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

  /** B = d * A. */
  private double [] scalarMultiply (double [] A, 
    double d, double [] B) {

    for (int i=0; i<16; i++) B [i] = A [i] * d;
    return B;
  }


  /** <code>A(i, j) = d</code>. */
  private static void set (double [] A, int i, int j, double d) {
    A [i + 4*j] = d;
  }

  /** <code>A(i,j) = B</code>. */
  private double [] setColumn (double [] A, int i, double [] B) {
    setColumn (A, i, B [0], B [1], B [2], B [3]);
    return A;
  }

  /** <code>A(i,j) = [x, y, z, w]</code>. */
  private double [] setColumn (double [] A, int i, double x, double y, 
    double z, double w) {

    set (A, i, 0, x);
    set (A, i, 1, y);
    set (A, i, 2, z);
    set (A, i, 3, w);
    return A;
  }


  public Matrix4d setColumn (Vector4d V0, Vector4d V1, 
    Vector4d V2, Vector4d V3) {

    setColumn (matrix, V0.getVector(), V1.getVector(), V2.getVector(), V3.getVector());
    return this;
  }


  /** <code>A(0,j) = V0, A(1,j) = V1, A(2,j) = V2, A(3,j) = V3</code>. */
  private double [] setColumn (double [] A, double [] V0, double [] V1,
    double [] V2, double [] V3) {

    setColumn (A, 0, V0);
    setColumn (A, 1, V1);
    setColumn (A, 2, V2);
    setColumn (A, 3, V3);
    return A;
  }
  
  public Matrix4d setRow (int j, double x, double y, double z, double w) {
      setRow (matrix, j, x, y, z, w);
      return this;
  }    
        

  /** <code>A(i,j) = [x, y, z, w]</code>. */
  private double [] setRow (double [] A, int j, double x, double y, 
    double z, double w) {
      set (A, 0, j, x);
      set (A, 1, j, y);
      set (A, 2, j, z);
      set (A, 3, j, w);
      return A;
  }

  public String toString () {
      return "#Matrix4d"
        + "\n  " + matrix [0] + " " + matrix [1] + " " + matrix [2] + " " + matrix [3]
        + "\n  " + matrix [4] + " " + matrix [5] + " " + matrix [6] + " " + matrix [7]
        + "\n  " + matrix [8] + " " + matrix [9] + " " + matrix [10] + " " + matrix [11]
        + "\n  " + matrix [12] + " " + matrix [13] + " " + matrix [14] + " " + matrix [15];
  }
  
  // Get and Set  
  public double[] getMatrix() {
      return matrix;
  }

  public void setMatrix(double[] matrix) {
      this.matrix = matrix;
  }
}
