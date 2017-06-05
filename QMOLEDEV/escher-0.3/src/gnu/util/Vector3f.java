package gnu.util;


/** 3-element vector of <code>float</code> in mathematics. */
public class Vector3f {
  public static final Vector3f ZERO = new Vector3f ();


  public float [] v = new float [3];


  public Vector3f () {}


  public Vector3f (float [] v) {
    this.v = v;
  }


  public Vector3f (float x, float y, float z, float w) {
    v [0] = x;
    v [1] = y;
    v [2] = z;
  }


  /** <code>C = A x B</code>. */
  public static float [] cross (float [] A, float [] B, float [] C) {
    C [0] = A [1] * B [2] - A [2] * B [1];
    C [1] = -A [0] * B [2] + A [2] * B [0];
    C [2] = A [0] * B [1] - A [1] * B [0];
    return B;
  }


  public Vector3f cross (Vector3f A, Vector3f B) {
    float [] C = v;
    if (A == this || B == this) C = new float [3];
    v = cross (A.v, B.v, C);
    return this;
  }


  public float length () {
    return length (v);
  }


  public static float length (float [] A) {
    return (float) java.lang.Math.sqrt (length_square (A));
  }


  public float length_square () {
    return length_square (v);
  }


  /** <code>|A| = x*x + y*y + z*z</code>. */
  public static float length_square (float [] A) {
    return A [0] * A [0] + A [1] * A [1] + A [2] * A [2];
  }


  public Vector3f minus (Vector3f A, Vector3f B) {
    minus (A.v, B.v, v);
    return this;
  }


  /** <code>C = A - B</code>. */
  public static float [] minus (float [] A, float [] B, float [] C) {
    C [0] = A [0] - B [0];
    C [1] = A [1] - B [1];
    C [2] = A [2] - B [2];
    return C;
  }
  

  public Vector3f negate () {
    return negate (this);
  }


  public Vector3f negate (Vector3f A) {
    negate (A.v, v);
    return this;
  }


  /** <code>B = -A</code>. */
  public static float [] negate (float [] A, float [] B) {
    return scalar_multiply (A, -1, B);
  }


  public Vector3f normalize () {
    return normalize (this);
  }


  public Vector3f normalize (Vector3f A) {
    normalize (A.v, v);
    return this;
  }


  /** <code>B = A / |A|</code>. */
  public static float [] normalize (float [] A, float [] B) {
    return scalar_divide (A, length (A), B);
  }


  /** <code>B = A + (a * I)</code>. */
  public static float [] scalar_addition (float [] A, float d, float [] B) {
    B [0] = A [0] + d;
    B [1] = A [1] + d;
    B [2] = A [2] + d;
    return B;
  }

    
  public Vector3f scalar_divide (float d) {
    scalar_divide (v, d, v);
    return this;
  }
    

  public Vector3f scalar_divide (Vector3f A, float d) {
    scalar_divide (A.v, d, v);
    return this;
  }
    

  /** <code>B = (1/d) * A</code>. */
  public static float [] scalar_divide (float [] A, float d, float [] B) {
    return scalar_multiply (A, 1/d, B);
  }
    

  public Vector3f scalar_minus (float d) {
    scalar_minus (v, d, v);
    return this;
  }
    

  public Vector3f scalar_minus (Vector3f A, float d) {
    scalar_minus (A.v, d, v);
    return this;
  }
    

  /** <code>B = A - (a * I)</code>. */
  public static float [] scalar_minus (float [] A, float d, float [] B) {
    return scalar_addition (A, -d, B);
  }


  public Vector3f scalar_multiply (float d) {
    return scalar_multiply (this, d);
  }


  public Vector3f scalar_multiply (Vector3f A, float d) {
    scalar_multiply (A.v, d, v);
    return this;
  }


  /** <code>B = d * A</code>. */
  public static float [] scalar_multiply (float [] A, float d, float [] B) {
    B [0] = A [0] * d;
    B [1] = A [1] * d;
    B [2] = A [2] * d;
    return B;
  }
    

  public String toString () {
    return "#Vector3f: " + v [0] + " " + v [1] + " " + v [2];
  }
}
