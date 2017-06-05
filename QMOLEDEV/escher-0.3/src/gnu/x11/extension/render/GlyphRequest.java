package gnu.x11.extension.render;

public class GlyphRequest {

  /**
   * The requested ID for the glyph.
   */
  private int id;

  /**
   * The width of the glyph image in pixels.
   */
  private int width;

  /**
   * The height of the glyph image in pixels.
   */
  private int height;

  /**
   * The X translation of the glyph. This value will be substracted from
   * the current X location when rendering this glyph.
   */
  private int x;

  /**
   * The Y translation of the glyph. This value will be substracted from
   * the current Y location when rendering this glyph.
   */
  private int y;

  /**
   * The X offset to the next glyph position. After rendering this glyph
   * the X position will be advanced by that offset.
   */ 
  private int offs_x;

  /**
   * The Y offset to the next glyph position. After rendering this glyph
   * the Y position will be advanced by that offset.
   */ 
  private int offs_y;

  /**
   * The image data for the glyph, in the format specified by the GlyphSet.
   */
  private byte[] image;

  /**
   * Creates a new glyph request.
   *
   * @param id the ID for the new glyph
   * @param width the width of the glyph image
   * @param height the height of the glyph image
   * @param x the X position of the glyph for rendering
   * @param y the Y position of the glyph for rendering
   * @param offs_x the X offset to the next glyph
   * @param offs_y the Y offset to the next glyph
   * @param image the image data in the format of this glyph set
   */
  public GlyphRequest (int id, int width, int height, int x, int y, int offs_x,
                       int offs_y, byte [] image) {
    this.id = id;
    this.width = width;
    this.height = height;
    this.x = x;
    this.y = y;
    this.offs_x = offs_x;
    this.offs_y = offs_y;
    this.image = image;
  }

  int get_id () {
    return id;
  }

  int get_width () {
    return width;
  }

  int get_height () {
    return height;
  }

  int get_x () {
    return x;
  }

  int get_y () {
    return y;
  }

  int get_offs_x () {
    return offs_x;
  }

  int get_offs_y () {
    return offs_y;
  }

  byte [] get_image () {
    return image;
  }
}
