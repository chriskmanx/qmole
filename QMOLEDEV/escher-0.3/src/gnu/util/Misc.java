package gnu.util;

import java.util.StringTokenizer;


/**
 * Provide some handy static methods.
 */
public class Misc {
  public static final String VERSION = "0.2.0";


  //-- copy array

  public static double [] copy (double [] src, 
    int src_offset, int length) {

    double [] dst = new double [length];
    System.arraycopy (src, src_offset, dst, 0, length);
    return dst;
  }

  
  public static double [] copy (double [] src, int src_offset) {
    return copy (src, src_offset, src.length-src_offset);
  }


  public static float [] copy (float [] src, int src_offset, int length) {
    float [] dst = new float [length];
    System.arraycopy (src, src_offset, dst, 0, length);
    return dst;
  }

  
  public static float [] copy (float [] src, int src_offset) {
    return copy (src, src_offset, src.length-src_offset);
  }


  //-- linearize array

  public static float [] linearize (float [] [] src) {
    int s1 = src [0].length;
    int s2 = src.length;   
    float [] dst = new float [s1 * s2];
    
    for (int i=0; i<s2; i++) {
      int offset = i * s1;
      System.arraycopy (src [i], 0, dst, offset, s1);
    }
    return dst;
  }


  public static float [] linearize (float [] [] [] src) {
    int s1 = src [0] [0].length;
    int s2 = src [0].length;
    int s3 = src.length;    
    float [] dst = new float [s1 * s2 * s3];
    
    for (int i=0; i<s3; i++)
      for (int j=0; j<s2; j++) {
        int offset = s1 * (j + s2 * i);
        System.arraycopy (src [i] [j], 0, dst, offset, s1);
      }
    return dst;
  }


  //-- string

  public static boolean empty (String s) {
    return s == null || trim (s).length () == 0;
  }
  

  /**
   * @see #tokenize(String, String)
   */
  public static String [] tokenize (String s) {
    return tokenize (s, " \t\n\r\f");
  }

  
  /** Tokenize a string given a delimiter and from-index. */
  public static String [] tokenize (String s, String delim) {    
    StringTokenizer st = new StringTokenizer (s, delim);

    String [] tokens = new String [st.countTokens ()];
    for (int i=0; i<tokens.length; i++)
      tokens [i] = st.nextToken ();
    
    return tokens;
  }


  public static String to_string (double [] d, String prefix) {
    StringBuffer sb = new StringBuffer ();
    for (int i=0; i<d.length; i++) {
      sb.append (prefix);
      sb.append (i + ": ");
      sb.append (d [i]);
    }
    return sb.toString ();
  }


  public static String to_string (int [] j, String prefix) {
    StringBuffer sb = new StringBuffer ();
    for (int i=0; i<j.length; i++) {
      sb.append (prefix);
      sb.append (i + ": ");
      sb.append (j [i]);
    }
    return sb.toString ();
  }


  /**
   * @see #to_string(Object[], String)
   */
  public static String to_string (Object [] objects) {
    return to_string (objects, "\n");
  }


  public static String to_string (Object [] objects, String prefix) {
    StringBuffer sb = new StringBuffer ();
    for (int i=0; i<objects.length; i++) {
      sb.append (prefix);
      sb.append (i + ": ");
      sb.append (objects [i]);
    }
    return sb.toString ();
  }


  public static String trim (String s) {
    if (s == null || s.length () == 0) return s;

    int from = 0;
    while (from < s.length ())
      if (Character.isWhitespace (s.charAt (from))) from++;
      else break;
    if (from == s.length ()) return "";

    int to = s.length () - 1;
    while (to > 0)
      if (Character.isWhitespace (s.charAt (to))) to--;
      else break;

    return s.substring (from, to+1);
  }


  //-- system functions

  public static void sleep (long millis) {
    try {
      Thread.sleep (millis);
    } catch (InterruptedException e) {} // ignore
  }  
}
 
