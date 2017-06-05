package gnu.x11.extension.render;

import gnu.x11.RequestOutputStream;


/** GlyphSet in RENDER. */
public class GlyphSet extends gnu.x11.Resource {

  private Render render;

  private boolean disposed = false;

  // render opcode 17 - create glyph set
  /**
   * @see <a href="XRenderCreateGlyphSet.html">XRenderCreateGlyphSet</a>
   */
  public GlyphSet (Render render, PictFormat format) {
    super (render.display);
    this.render = render;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (render.major_opcode, 17, 3);
      o.write_int32 (id);
      o.write_int32 (format.id ());
      o.send ();
    }
  }


  // render opcode 18 - reference glyph set
  /**
   * @see <a href="XRenderReferenceGlyphSet.html">
   * XRenderReferenceGlyphSet</a>
   */
  public GlyphSet (GlyphSet src) {
    super (src.display);
    render = src.render;

    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (render.major_opcode, 18, 3);
      o.write_int32 (id);
      o.write_int32 (src.id);
      o.send ();
    }
  }


  // render opcode 19 - free glyph set
  /**
   * Frees the glyph set in the X server. If this is only a reference as
   * created by {@link #GlyphSet(GlyphSet)}, this will only free this reference
   * but not the original glyph set.
   *
   * This method is also called by {@link #finalize()} when this object gets
   * garbage collected, so it isn't strictly necessary to call this explicitly.
   * However, if it can be determined when this object will no longer be used
   * it is recommended to explicitly call this method, so the garbage collector
   * won't be bothered with possible network lags.
   *
   * @see <a href="XRenderFreeGlyphSet.html">XRenderFreeGlyphSet</a>
   */
  public void free () {
    // Only free the GlyphSet once.
    if (! disposed) {
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (render.major_opcode, 19, 2);
        o.write_int32 (id);
        o.send ();
      }
    }
  }

  /**
   * Called when the GlyphSet object is finalized. In this case we free this
   * GlyphSet (if it hasn't already been freed).
   */
  protected void finalize () {
    free ();
  }

  /**
   * Adds glyphs to this glyph set. This makes these glyphs available
   * for rendering.
   *
   * @param glyph_reqs
   *
   * @return
   */
  public Glyph [] add_glyphs (GlyphRequest [] glyph_reqs) {
    // Determine length.
    int numGlyphs = glyph_reqs.length;
    int imageBytes = 0;
    for (int i = 0; i < numGlyphs; i++) {
      imageBytes += glyph_reqs [i].get_image ().length;
    }
    imageBytes += RequestOutputStream.pad (imageBytes);
    int len = numGlyphs * 4 + imageBytes / 4 + 3;
    RequestOutputStream o = display.out;
    Glyph [] glyphs = new Glyph [numGlyphs];
    synchronized (o) {
      o.begin_request (render.major_opcode, 20, len);
      o.write_int32 (id);
      o.write_int32 (numGlyphs);
      for (int i = 0; i < numGlyphs; i++) {
        int id = glyph_reqs [i].get_id ();
        o.write_int32 (id);
        glyphs [i] = new Glyph (this, id);
      }
      for (int i = 0; i < numGlyphs; i++) {
        GlyphRequest gr = glyph_reqs [i];
        o.write_int16 (gr.get_width ());
        o.write_int16 (gr.get_height ());
        o.write_int16 (gr.get_x ());
        o.write_int16 (gr.get_y ());
        o.write_int16 (gr.get_offs_x ());
        o.write_int16 (gr.get_offs_y ());
      }
      for (int i = 0; i < numGlyphs; i++) {
        byte [] image = glyph_reqs [i].get_image ();
        o.write (image);
      }
      o.send ();
    }
    return glyphs;
  }

  /**
   * Frees the specified glyphs in this glyphset. The releases any allocated
   * X server resources for these glyphs.
   *
   * Glyph instances get automatically freed when the garbage collector
   * determines that there no more reference to such an instance. However,
   * it is preferable to explicitly free glyphs so that the garbage collector
   * isn't burdened with network activity and locking.
   *
   * @param glyphs the glyphs to be freed
   */
  public void free_glyphs (Glyph [] glyphs) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      int numGlyphs = 0;
      for (int i = 0; i < glyphs.length; i++) {
        if (! glyphs [i].disposed) {
          numGlyphs++;
        }
      }
      if (numGlyphs > 0) {
        o.begin_request (render.major_opcode, 22, 2 + numGlyphs);
        o.write_int32 (id);
        for (int i = 0; i < glyphs.length; i++) {
          if (! glyphs [i].disposed) {
            o.write_int32 (glyphs [i].get_id ());
          }
        }
        o.send ();
      }
    }
  }
}
