package gnu.util;


/** 4-element vector of <code>double</code> in mathematics. */
public class Vector4d {
  public static final Vector4d ZERO = new Vector4d ();


  public double [] v = new double [4];


  public Vector4d () {}


  public Vector4d (double [] v) {
    this.v = v;
  }


  public Vector4d (double x, double y, double z, double w) {
    v [0] = x;
    v [1] = y;
    v [2] = z;
    v [3] = w;
  }


  /** <code>C = A x B</code>. */
  public static double [] cross (double [] A, double [] B, double [] C) {
    C [0] = A [1] * B [2] - A [2] * B [1];
    C [1] = -A [0] * B [2] + A [2] * B [0];
    C [2] = A [0] * B [1] - A [1] * B [0];
    return C;
  }


  public Vector4d cross (Vector4d A, Vector4d B) {
    double [] C = v;
    if (A == this || B == this) C = new double [4];
    v = cross (A.v, B.v, C);
    return this;
  }


  public double length () {
    return length (v);
  }


  public static double length (double [] A) {
    return java.lang.Math.sqrt (length_square (A));
  }


  public double length_square () {
    return length_square (v);
  }


  /** <code>|A| = x*x + y*y + z*z</code>. */
  public static double length_square (double [] A) {
    return A [0] * A [0] + A [1] * A [1] + A [2] * A [2];
  }


  public Vector4d minus (Vector4d A, Vector4d B) {
    minus (A.v, B.v, v);
    return this;
  }


  /** <code>C = A - B</code>. */
  public static double [] minus (double [] A, double [] B, double [] C) {
    C [0] = A [0] - B [0];
    C [1] = A [1] - B [1];
    C [2] = A [2] - B [2];
    C [3] = A [3] - B [3];
    return C;
  }
  

  public Vector4d multiply_left (Matrix4d A) {
    return multiply_left (A, this);
  }


  public Vector4d multiply_left (Matrix4d A, Vector4d V) {
    double [] B = v;
    if (V == this) B = new double [4];
    v = multiply_left (A.m, V.v, B);
    return this;
  }


  /** <code>B = A * V</code>. */
  public static double [] multiply_left (double [] A, double [] V, 
    double [] B) {

    B [0] = A [0] * V [0] + A [1] * V [1]
      + A [2] * V [2] + A [3] * V [3];
    B [1] = A [4] * V [0] + A [5] * V [1]
      + A [6] * V [2] + A [7] * V [3];
    B [2] = A [8] * V [0] + A [9] * V [1]
      + A [10] * V [2] + A [11] * V [3];
    B [3] = A [12] * V [0] + A [13] * V [1]
      + A [14] * V [2] + A [15] * V [3];
    return B;
  }    


  public Vector4d multiply_right (Matrix4d A) {
    return multiply_right (this, A);
  }


  public Vector4d multiply_right (Vector4d V, Matrix4d A) {
    double [] B = v;
    if (V == this) B = new double [4];
    v = multiply_right (V.v, A.m, B);
    return this;
  }


  /** <code>B = V * A</code>. */
  public static double [] multiply_right (double [] V, double [] A, 
    double [] B) {

    B [0] = V [0] * A [0] + V [1] * A [4]
      + V [2] * A [8] + V [3] * A [12];
    B [1] = V [0] * A [1] + V [1] * A [5]
      + V [2] * A [9] + V [3] * A [13];
    B [2] = V [0] * A [2] + V [1] * A [6]
      + V [2] * A [10] + V [3] * A [14];
    B [3] = V [0] * A [3] + V [1] * A [7]
      + V [2] * A [11] + V [3] * A [15];
    return B;
  }    


  public Vector4d negate () {
    return negate (this);
  }


  public Vector4d negate (Vector4d A) {
    negate (A.v, v);
    return this;
  }


  /** <code>B = -A</code>. */
  public static double [] negate (double [] A, double [] B) {
    return scalar_multiply (A, -1, B);
  }


  public Vector4d normalize () {
    return normalize (this);
  }


  public Vector4d normalize (Vector4d A) {
    normalize (A.v, v);
    return this;
  }


  /** <code>B = A / |A|</code>. */
  public static double [] normalize (double [] A, double [] B) {
    return scalar_divide (A, length (A), B);
  }


  /** <code>B = A + (a * I)</code>. */
  public static double [] scalar_addition (double [] A, double d, double [] B) {
    B [0] = A [0] + d;
    B [1] = A [1] + d;
    B [2] = A [2] + d;
    B [3] = A [3] + d;
    return B;
  }

    
  public Vector4d scalar_divide (double d) {
    scalar_divide (v, d, v);
    return this;
  }
    

  public Vector4d scalar_divide (Vector4d A, double d) {
    scalar_divide (A.v, d, v);
    return this;
  }
    

  /** <code>B = (1/d) * A</code>. */
  public static double [] scalar_divide (double [] A, double d, double [] B) {
    return scalar_multiply (A, 1/d, B);
  }
    

  public Vector4d scalar_minus (double d) {
    scalar_minus (v, d, v);
    return this;
  }
    

  public Vector4d scalar_minus (Vector4d A, double d) {
    scalar_minus (A.v, d, v);
    return this;
  }
    

  /** <code>B = A - (a * I)</code>. */
  public static double [] scalar_minus (double [] A, double d, double [] B) {
    return scalar_addition (A, -d, B);
  }


  public Vector4d scalar_multiply (double d) {
    return scalar_multiply (this, d);
  }


  public Vector4d scalar_multiply (Vector4d A, double d) {
    scalar_multiply (A.v, d, v);
    return this;
  }


  /** <code>B = d * A</code>. */
  public static double [] scalar_multiply (double [] A, double d, double [] B) {
    B [0] = A [0] * d;
    B [1] = A [1] * d;
    B [2] = A [2] * d;
    B [3] = A [3] * d;
    return B;
  }
    

  public String toString () {
    return "#Vector4d: " + v [0] + " " + v [1] + " " + v [2] + " " + v [3];
  }
}
