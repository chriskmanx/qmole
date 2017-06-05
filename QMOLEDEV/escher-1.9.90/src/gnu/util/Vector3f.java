package gnu.util;


/** 3-element vector of <code>float</code> in mathematics. */
public class Vector3f {
  public final Vector3f ZERO = new Vector3f ();
  private float [] vector = new float [3];

  public Vector3f () {}


  public Vector3f (float [] v) {
    this.vector = v;
  }


  public Vector3f (float x, float y, float z, float w) {
    vector [0] = x;
    vector [1] = y;
    vector [2] = z;
  }


  /** <code>C = A x B</code>. */
  private float [] cross (float [] A, float [] B, float [] C) {
    C [0] = A [1] * B [2] - A [2] * B [1];
    C [1] = -A [0] * B [2] + A [2] * B [0];
    C [2] = A [0] * B [1] - A [1] * B [0];
    return B;
  }


  public Vector3f cross (Vector3f A, Vector3f B) {
    float [] C = vector;
    if (A == this || B == this) C = new float [3];
    vector = cross (A.vector, B.vector, C);
    return this;
  }


  private float length (float [] A) {
    return (float) java.lang.Math.sqrt (lengthSquare (A));
  }


  public float lengthSquare () {
    return lengthSquare (vector);
  }


  /** <code>|A| = x*x + y*y + z*z</code>. */
  private float lengthSquare (float [] A) {
    return A [0] * A [0] + A [1] * A [1] + A [2] * A [2];
  }


  public Vector3f minus (Vector3f A, Vector3f B) {
    minus (A.vector, B.vector, vector);
    return this;
  }


  /** <code>C = A - B</code>. */
  private float [] minus (float [] A, float [] B, float [] C) {
    C [0] = A [0] - B [0];
    C [1] = A [1] - B [1];
    C [2] = A [2] - B [2];
    return C;
  }
  
  public Vector3f normalize () {
    return normalize (this);
  }

  private Vector3f normalize (Vector3f A) {
    normalize (A.vector, vector);
    return this;
  }

  /** <code>B = A / |A|</code>. */
  private float [] normalize (float [] A, float [] B) {
    return scalarDivide (A, length (A), B);
  }


  /** <code>B = (1/d) * A</code>. */
  private float [] scalarDivide (float [] A, float d, float [] B) {
    return scalarMultiply (A, 1/d, B);
  }
    

  /** <code>B = d * A</code>. */
  private float [] scalarMultiply (float [] A, float d, float [] B) {
    B [0] = A [0] * d;
    B [1] = A [1] * d;
    B [2] = A [2] * d;
    return B;
  }
    

  public String toString () {
    return "#Vector3f: " + vector [0] + " " + vector [1] + " " + vector [2];
  }
  
  // Sets and Gets
  public void setVector(float[] vector) {
    this.vector = vector;
  }


  public float[] getVector() {
    return vector;
  }
}
