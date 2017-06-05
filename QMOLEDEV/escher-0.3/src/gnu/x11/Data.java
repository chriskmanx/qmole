package gnu.x11;


/** 
 * Efficient storage of byte array. Similar to "struct" in C/C++. No
 * parsing.
 */
public class Data {  
  /** Sequential writing. */
  public int index;

  /** Lower bound. */
  public int offset;

  public byte [] data;


  /** Writing. */
  public Data () {}


  /** Writing. */
  public Data (int length) {
    data = new byte [length];
  }


  /** Reading. */
  public Data (byte [] data) {
    this.data = data;
  }


  /** Reading. */
  public Data (byte [] [] data) {
    int s1 = data [0].length;
    int s2 = data.length;

    this.data = new byte [s1 * s2];
    for (int i=0; i<s2; i++) 
      write1 (data [i]);
  }


  /** Reading. */
  public Data (byte [] [] [] data) {
    int s1 = data [0] [0].length;
    int s2 = data [0].length;
    int s3 = data.length;

    this.data = new byte [s1 * s2 * s3];
    for (int i=0; i<s3; i++) 
      for (int j=0; j<s2; j++) 
        write1 (data [i] [j]);
  }


  /** Reading. */
  public Data (float [] data) {
    int n = 4 * data.length;
    this.data = new byte [n];
    write4 (data);
  }


  /** Reading. */
  public Data (float [] [] [] data) {
    int s1 = data [0] [0].length;
    int s2 = data [0].length;
    int s3 = data.length;

    // 4 = number of bytes per float
    this.data = new byte [4 * s1 * 4 * s2 * 4 * s3];
    for (int i=0; i<s3; i++) 
      for (int j=0; j<s2; j++)
        write4 (data [i] [j]);
  }


  /** Reading. */
  public Data (int [] [] [] data) {
    int s1 = data [0] [0].length;
    int s2 = data [0].length;
    int s3 = data.length;

    // 4 = number of bytes per integer
    this.data = new byte [4 * s1 * 4 * s2 * 4 * s3];
    for (int i=0; i<s3; i++) 
      for (int j=0; j<s2; j++)
        write4 (data [i] [j]);
  }


  /** Reading. */
  public Data (Data data) {
    this (data.data);
    this.offset = data.offset;
  }


  /** Reading. */
  public Data (int [] data, int unit_size) {
    if (unit_size == 2) {
      int n = len (2 * data.length);
      this.data = new byte [n];
      write2 (data);

    } else if (unit_size == 4) {
      int n = 4 * data.length;
      this.data = new byte [n];
      write4 (data);
    }
  }


  /** Offset Reading. */
  public Data (Data data, int offset) {
    this (data);
    this.offset += offset;
  }


  //-- padded

  
  /** Padded length. */
  public static int len (int n) {
    return (n + 3) & 0xfffffffc;
  }


  /** Padded length. */
  public static int len (String s) {
    return len (s.length ());
  }


  /** Padded unit. */
  public static int unit (byte [] b) {
    return unit (b.length);
  }


  /** Padded unit. */
  public static int unit (int n) {
    return len (n) / 4;
  }
  

  /** Padded unit. */
  public static int unit (String s) {
    return unit (s.length ());
  }


  //-- read
  
  public int read1 (int j) {
    return data [j+offset] & 0xff;
  }


  public int read2 (int j) {
    int b0 = data [j+offset] & 0xff;
    int b1 = data [j+offset+1] & 0xff;
    return (b0 << 8) | b1;
  }


  public int read4 (int j) {
    int b0 = data [j+offset] & 0xff;
    int b1 = data [j+offset+1] & 0xff;
    int b2 = data [j+offset+2] & 0xff;
    int b3 = data [j+offset+3] & 0xff;
    return b0 << 24 | b1 << 16 | b2 << 8 | b3;
  }

  
  public long read8 (int j) {
    long b0 = data [j+offset] & 0xff;
    long b1 = data [j+offset+1] & 0xff;
    long b2 = data [j+offset+2] & 0xff;
    long b3 = data [j+offset+3] & 0xff;
    long b4 = data [j+offset+4] & 0xff;
    long b5 = data [j+offset+5] & 0xff;
    long b6 = data [j+offset+6] & 0xff;
    long b7 = data [j+offset+7] & 0xff;
    return b0 << 56 | b1 << 48 | b2 << 40 | b3 << 32
      | b4 << 24 | b5 << 16 | b6 << 8 | b7;
  }


  public boolean read_boolean (int j) {
    return read1 (j) == 1;
  }


  public boolean read4_boolean (int j) {
    return read4 (j) == 1;
  }


  public double read_double (int j) {
    return Double.longBitsToDouble (read8 (j));
  }


  public float read_float (int j) {
    return Float.intBitsToFloat (read4 (j));
  }


  public String read_string (int j, int length) {
    return new String (data, j+offset, length);
  }

  //-- write1 - given j

  public void write1 (int j, boolean b) {
    write1 (j, b ? 1 : 0);
  }


  public void write1 (int j, byte [] b) {
    write1 (j, b, 0);
  }


  public void write1 (int j, byte [] b, int offset) {
    write1 (j, b, offset, b.length-offset);
  }


  public void write1 (int j, byte [] b, int offset, int length) {
    System.arraycopy (b, offset, data, j+this.offset, length);
  }


  public void write1 (int j, int b) {
    data [j+offset] = (byte) b;
  }


  public void write1 (int j, Data d) {
    write1 (j, d.data, d.offset, d.n ());
  }


  public void write1 (int j, String s) {
    write1 (j, s.getBytes (), 0);
  }

  
  //-- write1 - implicit index

  public void write1 (boolean b) {
    write1 (index, b);
    index++;
  }


  public void write1 (byte [] b) {
    write1 (index, b);
    index += b.length;
  }


  public void write1 (int b) {
    write1 (index, b);
    index++;
  }


  public void write1 (Data d) {
    write1 (d.data, d.offset, d.n ());
  }


  public void write1 (String s) {
    write1 (index, s);
    index += Data.len (s);
  }

  /**
   * Writes a field of type STRING16 into the data buffer.
   *
   * @param s the string to write
   */
  public void write_string16 (String s) {
    write_string16 (index, s);
    boolean odd = s.length() % 2 == 1;
    // If we have an odd number of characters, we have to add a padding of
    // 2.
    index += s.length() * 2 + (odd ? 2 : 0);
  }

  /**
   * Writes a field of type STRING16 into the data buffer at the specified
   * position.
   *
   * @param j the index where to write the string
   * @param s the string to write
   */
  public void write_string16 (int j, String s) {
    char[] chars = s.toCharArray();
    int len = chars.length;
    for (int i = 0; i < len; i++)
      {
        data[j + i * 2] = (byte) (chars[i] >> 8);
        data[j + i * 2 + 1] = (byte) (chars[i] & 0xFF);
      }
  }

  public void write1 (byte [] b, int offset) {
    write1 (index, b, offset);
    index += b.length - offset;
  }


  public void write1 (byte [] b, int offset, int length) {
    write1 (index, b, offset, length);
    index += length;
  }



  //-- write2 - given j

  public void write2 (int j, int s) {
    data [j+offset] = (byte) ((s >> 8) & 0xff);
    data [j+offset+1] = (byte) (s & 0xff);
  }


  public void write2 (int j, byte [] b, int offset) {
    write2 (j, b, offset, b.length);
  }  


  public void write2 (int j, byte [] b, int offset, int length) {
    for (int i=offset; i<length; i++, j+=2) write2 (j, b [i]);
  }  


  public void write2 (int j, int [] s, int offset) {
    write2 (j, s, offset, s.length);
  }


  public void write2 (int j, int [] s, int offset, int length) {
    for (int i=offset; i<length; i++, j+=2) write2 (j, s [i]);
  }


  public void write2 (int j, String s) {
    write2 (j, s.getBytes (), 0);
  }


  // write2 - implicit index

  public void write2 (byte [] b) { write2 (b, 0); }
  public void write2 (int [] b) { write2 (b, 0); }


  public void write2 (int s) {
    write2 (index, s);
    index += 2;
  }


  public void write2 (byte [] b, int offset) {
    write2 (index, b, offset);
    index += b.length;
  }


  public void write2 (byte [] b, int offset, int length) {
    write2 (index, b, offset, length);
    index += length;
  }


  public void write2 (int [] s, int offset) {
    write2 (index, s, offset);
    index += s.length * 2;
  }
   
      
  public void write2 (int [] s, int offset, int length) {
    write2 (index, s, offset);
    index += length * 2;
  }


  public void write2 (String s) {
    write2 (index, s);
    index += Data.len (s);
  }


  //-- write4 - given j

  public void write4 (int j, boolean b) {
    write4 (j, b ? 1 : 0);
  }


  public void write4 (int j, int i) {
    data [j+offset] = (byte) ((i >> 24) & 0xff);
    data [j+offset+1] = (byte) ((i >> 16) & 0xff);
    data [j+offset+2] = (byte) ((i >> 8) & 0xff);
    data [j+offset+3] = (byte) (i & 0xff);
  }


  public void write4 (int j, float f) {
    write4 (j, Float.floatToIntBits (f));
  }


  public void write4 (int j, float [] f, int offset) {
    write4 (j, f, offset, f.length);
  }


  public void write4 (int j, float [] f, int offset, int length) {
    for (int k=offset; k<length; k++, j+=4) write4 (j, f [k]);
  }


  public void write4 (int j, int [] i, int offset) {
    write4 (j, i, offset, i.length);
  }


  public void write4 (int j, int [] i, int offset, int length) {
    for (int k=offset; k<length; k++, j+=4) write4 (j, i [k]);
  }


  //-- write4 - implicit index

  public void write4 (float [] f) { write4 (f, 0); }
  public void write4 (int [] i) { write4 (i, 0); }


  public void write4 (float f) {
    write4 (index, f);
    index += 4;
  }


  public void write4 (int i) {
    write4 (index, i);
    index += 4;
  }


  public void write4 (float [] f, int offset) {
    write4 (index, f, offset);
    index += f.length * 4;
  }


  public void write4 (float [] f, int offset, int length) {
    write4 (index, f, offset, length);
    index += length * 4;
  }


  public void write4 (int [] i, int offset) {
    write4 (index, i, offset);
    index += i.length * 4;
  }


  public void write4 (int [] i, int offset, int length) {
    write4 (index, i, offset, length);
    index += length * 4;
  }


  //-- write8 - given j

  public void write8 (int j, long l) {
    data [j+offset] = (byte) ((l >> 56) & 0xff);
    data [j+offset+1] = (byte) ((l >> 48) & 0xff);
    data [j+offset+2] = (byte) ((l >> 40) & 0xff);
    data [j+offset+3] = (byte) ((l >> 32) & 0xff);
    data [j+offset+4] = (byte) ((l >> 24) & 0xff);
    data [j+offset+5] = (byte) ((l >> 16) & 0xff);
    data [j+offset+6] = (byte) ((l >> 8) & 0xff);
    data [j+offset+7] = (byte) (l & 0xff);
  }


  public void write8 (int j, double d) {
    write8 (j, Double.doubleToLongBits (d));
  }



  public void write8 (int j, double [] d, int offset) {
    write8 (j, d, offset, d.length);
  }


  public void write8 (int j, double [] d, int offset, int length) {
    for (int k=offset; k<length; k++, j+=8) write8 (j, d [k]);
  }


  //-- write8 - implicit index

  public void write8 (long l) {
    write4 (index, l);
    index += 8;
  }


  public void write8 (double d) {
    write8 (index, d);
    index += 8;
  }


  public void write8 (double [] d, int offset) {
    write8 (index, d, offset);
    index += d.length * 8;
  }


  public void write8 (double [] d, int offset, int length) {
    write8 (index, d, offset, length);
    index += length * 8;
  }


  /**
   * Writes X bytes of zero, where X is the number that pads n to a multiple
   * of 4.
   *
   * @param n the number of bytes that have been written and need padding
   */
  public void pad (int n) {
    int x = n % 4;
    index += x;
  }

  public void write_unused (int i) { index += i; }
  public void write1_unused () { index++; }
  public void write2_unused () { index += 2; }
  public void write3_unused () { index += 3; }
  public void write4_unused () { index += 4; }

  
  //-- misc

  public static String to_string (byte [] buffer, int offset, 
    int length, String prefix) {

    StringBuffer sb = new StringBuffer ();

    for (int i=offset; i<length; i++)
      sb.append ("\n" + prefix + i + ": " + byte_to_string (buffer [i]));

    return sb.toString ();
  }

  
  public static String byte_to_string (int b) {
    int int0 = b & 0xff;
    char char0 = (char) b;
    String hex = Integer.toHexString (int0);
    String bin = Integer.toBinaryString (int0);

    // sprintf
    String int1 = "00" + int0;
    int len = int1.length ();
    int1 = int1.substring (len-3, len);
      
    hex = "00" + hex;
    len = hex.length ();
    hex = hex.substring (len-2, len);

    // special chars (newline or tab)
    if (int0 == 10 || int0 == 9) char0 = '?';

    return int1 + " (" + hex + " " + char0 + " " + bin + ")";
  }


  /** Length of byte data. */
  public int n () {
    return data.length - offset;
  }


  /** Padded length of byte data. */
  public int np () {
    return len (n ());
  }


  /** Padding length to make a unit of four. */
  public int p () {
    return np () - n ();
  }


  public String toString () {
    return "#Data"
      + "\n  index: " + index
      + "\n  offset: " + offset
      + to_string (data, offset, data.length, "");
  }
}
