package gnu.x11.extension.render;

/**
 * Represents one glyph in the X Render Extension. This can be used to cache
 * glyphs in a HashMap or similar data structure.
 *
 * Glyph instances cannot be created directly. They must be create by
 * adding glyphs to a {@link GlyphSet}. It is important to store the created
 * glyph instances somewhere, otherwise they get garbage collected and the
 * X server frees the resources for that glyph.
 */
public final class Glyph {

  /**
   * The glyph set to which this glyph belongs.
   */
  private GlyphSet glyph_set;

  /**
   * The ID of this glyph, as specified when this glyph was constructed in
   * {@link GlyphSet#add_glyphs(GlyphRequest[])}.
   */
  private int id;

  /**
   * True when this glyph has already been freed from the X server.
   */
  boolean disposed;

  /**
   * Creates a new instance with the specified ID.
   *
   * @param id2 the ID for this glyph
   */
  Glyph (GlyphSet gs, int id2) {
    glyph_set = gs;
    id = id2;
    disposed = false;
  }

  /**
   * Returns the ID of this glyph.
   *
   * @return the ID of this glyph
   */
  public int get_id () {
    return id;
  }

  /**
   * Compares this object with another object. This object is only equal
   * to the other object if the other object is also a Glyph instance and
   * if both have the same ID and the same parent glyph set.
   *
   * @param o the other object
   *
   * @return <code>true</code> when both objects are equal, <code>false</code>
   *         otherwise
   */
  public boolean equals (Object o) {
    return o instanceof Glyph && ((Glyph) o).glyph_set == glyph_set
           && ((Glyph) o).id ==id;
  }

  /**
   * Computes a hashcode.
   */
  public int hashCode () {
    return id ^ glyph_set.hashCode ();
  }

  /**
   * Frees this glyph instance if it hasn't already been freed explicitly.
   */
  protected void finalize () {
    if (! disposed) {
      glyph_set.free_glyphs(new Glyph [] { this });
    }
  }
}
