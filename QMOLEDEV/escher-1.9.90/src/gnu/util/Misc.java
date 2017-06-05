package gnu.util;

import java.util.StringTokenizer;


/**
 * Provide some handy static methods.
 */
public class Misc {
  public static final String VERSION = "0.2.0";


  //-- linearize array

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

  /** Tokenize a string given a delimiter and from-index. */
  public static String [] tokenize (String s, String delim) {    
    StringTokenizer st = new StringTokenizer (s, delim);

    String [] tokens = new String [st.countTokens ()];
    for (int i=0; i<tokens.length; i++)
      tokens [i] = st.nextToken ();
    
    return tokens;
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

}
 
