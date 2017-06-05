package gnu.x11.extension.render;

import java.io.IOException;

import gnu.x11.Data;
import gnu.x11.Display;
import gnu.x11.Drawable;
import gnu.x11.Error;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;


/**
 * X Rendering Extension.
 *
 * <p>This extension is still under active development. Please check its <a
 * href="http://www.xfree86.org/~keithp/render/protocol.html">homepage</a>.
 * The implemented version is XRender 0.0.14.
 */
public class Render extends gnu.x11.extension.Extension 
  implements gnu.x11.extension.ErrorFactory {

  public static final String [] MINOR_OPCODE_STRINGS = {
    "QueryVersion",             // 0
    "QueryPictFormats",         // 1
    "QueryPictIndexValues",     // 2
    "QueryDithers",             // 3
    "CreatePicture",            // 4
    "ChangePicture",            // 5
    "SetPictureClipRectangles", // 6
    "FreePicture",              // 7
    "Composite",                // 8
    "Scale",                    // 9
    "Trapezoids",               // 10
    "Triangles",                // 11
    "TriStrip",                 // 12
    "TriFan",                   // 13
    "ColorTrapezoids",          // 14
    "ColorTriangles",           // 15
    "Transform",                // 16
    "CreateGlyphSet",           // 17
    "ReferenceGlyphSet",        // 18
    "FreeGlyphSet",             // 19
    "AddGlyphs",                // 20
    "AddGlyphsFromPicture",     // 21
    "FreeGlyphs",               // 22
    "CompositeGlyphs8",         // 23
    "CompositeGlyphs16",        // 24
    "CompositeGlyphs32",        // 25
    "FillRectangles"            // 26
  };


  public static final int CLIENT_MAJOR_VERSION = 0;
  public static final int CLIENT_MINOR_VERSION = 1;


  public int server_major_version, server_minor_version;


  private PictFormat [] picture_formats_cache;


  // render opcode 0 - query version
  public Render (Display display) 
    throws gnu.x11.extension.NotFoundException { 

    super (display, "RENDER", MINOR_OPCODE_STRINGS, 5, 0);

    // check version before any other operations
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 0, 3);
      o.write_int32 (CLIENT_MAJOR_VERSION);
      o.write_int32 (CLIENT_MINOR_VERSION);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        server_major_version = i.read_int32 ();
        server_minor_version = i.read_int32 ();
        i.skip (16);
      }
    }

  }


  // render opcode 1 - query picture formats
  public PictFormat [] picture_formats () {
    if (picture_formats_cache == null) {
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 1, 1);
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.read_reply (o);
          i.skip (8);
          int count = i.read_int32 ();
          i.skip (20);
          PictFormat [] pfs = new PictFormat [count];
          for (int idx = 0; idx < count; idx++) {
            pfs [idx] = new PictFormat (i);
          }
          // TODO: Read LISTofPICTSCREEN and LISTofSUBPIXEL.
          try { i.skip (i.available ()); } catch (IOException ex) { }
          picture_formats_cache = pfs;
        }
      }
    }
    return picture_formats_cache;
  }


  // render opcode 2 - query picture index values
  public void picture_index_values () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 2, 2);
      o.send ();
    }
  }


  // render opcode 3 - query dithers
  public void dithers (Drawable drawable) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 3, 2);
      o.send ();
    }
  }

  
  public static final int CLEAR = 0;
  public static final int SRC = 1;
  public static final int DST = 2;
  public static final int OVER = 3;
  public static final int OVER_REVERSE = 4;
  public static final int IN = 5;
  public static final int IN_REVERSE = 6;
  public static final int OUT = 7;
  public static final int OUT_REVERSE = 8;
  public static final int ATOP = 9;
  public static final int ATOP_REVERSE = 10;
  public static final int XOR = 11;
  public static final int ADD = 12;
  public static final int SATURATE = 13;
  public static final int MAXIMUM = 14;



  // render opcode 8 - composite
  /**
   * @param operation
   * {@link #CLEAR}
   * {@link #SRC}
   * {@link #DST}
   * {@link #OVER}
   * {@link #OVER_REVERSE}
   * {@link #IN}
   * {@link #IN_REVERSE}
   * {@link #OUT}
   * {@link #OUT_REVERSE}
   * {@link #ATOP}
   * {@link #ATOP_REVERSE}
   * {@link #XOR}
   * {@link #ADD}
   * {@link #SATURATE}
   * {@link #MAXIMUM}
   * 
   * @see <a href="XRenderComposite.html">XRenderComposite</a>
   */
  public void composite (int op, Picture src, Picture mask, Picture dst, 
    int src_x, int src_y, int mask_x, int mask_y, int dst_x, int dst_y, 
    int width, int height) {

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 8, 9);
      o.write_int8 (op);
      o.skip (3);
      o.write_int32 (src.id);
      o.write_int32 (mask.id);
      o.write_int32 (dst.id);
      o.write_int16 (src_x);
      o.write_int16 (src_y);
      o.write_int16 (mask_x);
      o.write_int16 (mask_y);
      o.write_int16 (dst_x);
      o.write_int16 (dst_y);
      o.write_int16 (width);
      o.write_int16 (height);
      o.send ();
    }
  }

  public void composite_glyphs (int op, Picture src, Picture dst,
                                PictFormat mask_format, GlyphSet glyphset,
                                int src_x, int src_y, int dst_x, int dst_y,
                                Glyph[] glyphs) {
    // TODO: Replace int op with some enum type.
    // TODO: Implement 8 and 16 bit versions.
    RequestOutputStream o = display.out;
    synchronized (o) {
      int len = 9 + glyphs.length;
      o.begin_request (major_opcode, 25, len);
      o.write_int8 (op);
      o.skip (3);
      o.write_int32 (src.id ());
      o.write_int32 (dst.id ());
      o.write_int32 (mask_format.id ());
      o.write_int32 (glyphset.id ());
      o.write_int16 (src_x);
      o.write_int16 (src_y);
      o.write_int8 (glyphs.length);
      o.skip(3);
      o.write_int16 (dst_x);
      o.write_int16 (dst_y);
      for (int i = 0; i < glyphs.length; i++) {
        o.write_int32 (glyphs [i].get_id ());
      }
    }
  }

  public static final int BAD_PICTURE_FORMAt = 0;
  public static final int BAD_PICTURE = 1;
  public static final int BAD_PICTURE_OPERATIOR = 2;
  public static final int BAD_GLYPH_SET = 3;
  public static final int BAD_GLYPH = 4;


  public static final String [] ERROR_STRINGS = {
    "BAD_RENDER_PICTURE_FORMAT: parameter not a RENDER picture format",
    "BAD_RENDER_PICTURE: parameter not a RENDER picture",
    "BAD_RENDER_PICTURE_OPERATOR: parameter not a RENDER picture operator",
    "BAD_RENDER_GLYPH_SET: parameter not a RENDER glyph set",
    "BAD_RENDER_GLYPH: parameter not a RENDER glyph",
  };


  public Error build (Display display, int code, int seq_no, int bad,
                      int minor_opcode, int major_opcode) {

    String error_string = ERROR_STRINGS [code - first_error];
    return new Error (display, error_string, code, seq_no, bad, 
      minor_opcode, major_opcode);
  }


  /**
   * @see DrawablePicture#Picture(Render, Drawable, DrawablePicture.Format, DrawablePicture.Attributes)
   */  
  public DrawablePicture create_picture (Drawable drawable, PictFormat format,
                                         DrawablePicture.Attributes attr) {
    
    return new DrawablePicture (this, drawable, format, attr);
  }

  /**
   * Creates a Picture instance for a solid fill operation with the specified
   * color. Only the least significant 16 bit of the values are used.
   *
   * @param red the red component
   * @param green the green component
   * @param blue the blue component
   * @param alpha the alpha component
   *
   * @return a solid fill 
   */
  public Picture create_solid_fill (int red, int green, int blue, int alpha) {
    // Opcode 33.
    RequestOutputStream o = display.out;
    Picture pic = new Picture (display);
    synchronized (o) {
      o.begin_request (major_opcode, 33, 4);
      o.write_int32 (pic.id ());
      o.write_int16 (red);
      o.write_int16 (green);
      o.write_int16 (blue);
      o.write_int16 (alpha);
      o.send ();
    }
    return pic;
  }

  /**
   * @see <a href="XRenderFindFormat.html">XRenderFindFormat</a>
   */
  public PictFormat picture_format (PictFormat.Template template, 
                                    boolean must) {

    PictFormat [] pfs = picture_formats ();
    for (int i=0; i<pfs.length; i++) {
      if (pfs [i].match (template)) {
        return pfs [i];
      }
    }

    if (! must) {
      return null;
    }
    throw new Error ("No matching: " + template);
  }


  public String more_string () {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + server_major_version + "." + server_minor_version;
  }
}
