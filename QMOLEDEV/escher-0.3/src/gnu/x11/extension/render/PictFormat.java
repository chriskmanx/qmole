package gnu.x11.extension.render;

import gnu.x11.Colormap;
import gnu.x11.ResponseInputStream;


/**
 * Format specification for Picture instances. This class cannot be
 * instantiated directly. Instead you can query the available formats
 * by using {@link Render#get_picture_formats()} and use one of those.
 * In order to make format selection easier, you can create a {@link Template}
 * and use
 * {@link Render#picture_format(gnu.x11.extension.render.PictFormat.Template, boolean)}
 * to select a matching format.
 */
public class PictFormat {

  /**
   * The different types of formats.
   */
  public enum Type {

    /**
     * Pictures of that format use an indexed (aka pseudo) color model.
     */
    INDEXED,

    /**
     * Pictures of that format use a direct (aka true) color model.
     */
    DIRECT
  };

  /**
   * Parameters for direct color models to describe the data layout of pixel
   * samples.
   */
  public static class Direct {

    /**
     * The shift of the red component.
     */
    private int red_shift;

    /**
     * The mask of the red component.
     */
    private int red_mask;

    /**
     * The shift of the green component.
     */
    private int green_shift;

    /**
     * The mask of the green component.
     */
    private int green_mask;

    /**
     * The shift of the blue component.
     */
    private int blue_shift;

    /**
     * The mask of the blue component.
     */
    private int blue_mask;

    /**
     * The shift of the alpha component.
     */
    private int alpha_shift;

    /**
     * The mask of the alpha component.
     */
    private int alpha_mask;

    /**
     * Reads the direct color parameters from the response stream.
     *
     * @param i the stream to read from
     */
    Direct (ResponseInputStream i) {
      red_shift = i.read_int16();
      red_mask = i.read_int16();
      green_shift = i.read_int16();
      green_mask = i.read_int16();
      blue_shift = i.read_int16();
      blue_mask = i.read_int16();
      alpha_shift = i.read_int16();
      alpha_mask = i.read_int16();
    }

    /**
     * Checks if the direct parameters in the template matches our
     * parameters.
     *
     * @param temp the template to check
     *
     * @return <code>true</code> when the parameters match, <code>false</code>
     *         otherwise
     */
    boolean match (Template temp) {
      return temp.red_shift == red_shift && temp.red_mask == red_mask
           && temp.green_shift == green_shift && temp.green_mask == green_mask
           && temp.blue_shift == blue_shift && temp.blue_mask == blue_mask
           && temp.alpha_shift == alpha_shift && temp.alpha_mask == alpha_mask;
    }

    /**
     * Returns a string representation of this instance.
     *
     * @return a string representation of this instance
     */
    public String toString () {
      return "Direct[red_shift=" + red_shift + ", red_mask=" + red_mask
             + ", green_shift=" + green_shift + ", green_mask=" + green_mask
             + ", blue_shift=" + blue_shift + ", blue_mask=" + blue_mask
             + ", alpha_shift=" + alpha_shift + ", alpha_mask=" + alpha_mask
             + "]";
    }
  }

  /**
   * A template for selecting PictFormat instances in
   * {@link Render#picture_format(gnu.x11.extension.render.PictFormat.Template, boolean)}.
   */
  public static class Template {

    private static final int TYPE_MASK = 1 << 0;
    private static final int DEPTH_MASK = 1 << 1;
    private static final int DIRECT_MASK = 1 << 2;

    /**
     * The format type.
     */
    private Type type;

    /**
     * The format depth.
     */
    private int depth;

    /**
     * The shift for the red component in direct formats.
     */
    private int red_shift;

    /**
     * The mask for the red component in direct formats.
     */
    private int red_mask;

    /**
     * The shift for the green component in direct formats.
     */
    private int green_shift;
    /**
     * The mask for the green component in direct formats.
     */
    private int green_mask;

    /**
     * The shift for the blue component in direct formats.
     */
    private int blue_shift;

    /**
     * The mask for the blue component in direct formats.
     */
    private int blue_mask;

    /**
     * The shift for the alpha component in direct formats.
     */
    private int alpha_shift;

    /**
     * The mask for the alpha component in direct formats.
     */
    private int alpha_mask;

    /**
     * Indicates which fields should be matched.
     */
    private int field_mask;

    /**
     * Creates a template with no fields set.
     */
    public Template () {
      field_mask = 0;
    }

    /**
     * Creates a new template for format selection. The shift and mask
     * arguments only need to be specified for direct formats (otherwise,
     * leave them 0). Of the shifts and masks arguments, only the least 16
     * significant bits are used. 
     *
     * @param type the format type
     * @param depth the depth in bits / pixel
     * @param red_shift the shift for the red component
     * @param red_mask the mask for the red component
     * @param green_shift the shift for the green component
     * @param green_mask the mask for the green component
     * @param blue_shift the shift for the blue component
     * @param blue_mask the mask for the blue component
     * @param alpha_shift the shift for the alpha component
     * @param alpha_mask the mask for the alpha component
     */
    public Template (Type type, int depth, int red_shift, int red_mask,
                     int green_shift, int green_mask, int blue_shift,
                     int blue_mask, int alpha_shift, int alpha_mask) {
      this.type = type;
      this.depth = depth;
      this.red_shift = red_shift;
      this.red_mask = red_mask;
      this.green_shift = green_shift;
      this.green_mask = green_mask;
      this.blue_shift = blue_shift;
      this.blue_mask = blue_mask;
      this.alpha_shift = alpha_shift;
      this.alpha_mask = alpha_mask;
    }

    public void set_depth (int depth) {
      this.depth = depth;
      field_mask |= DEPTH_MASK;
    }

    public void set_type (Type type) {
      this.type = type;
      field_mask |= TYPE_MASK;
    }

    public void set_direct (int red_shift, int red_mask,
                            int green_shift, int green_mask, int blue_shift,
                            int blue_mask, int alpha_shift, int alpha_mask) {

      this.red_mask = red_mask;
      this.red_shift = red_shift;
      this.green_mask = green_mask;
      this.green_shift = green_shift;
      this.blue_mask = blue_mask;
      this.blue_shift = blue_shift;
      this.alpha_mask = alpha_mask;
      this.alpha_shift = alpha_shift;
      field_mask |= DIRECT_MASK;
    }

    public void clear () {
      field_mask = 0;
    }
  }

  /**
   * Indicates that no format should be used. This is only valid in some
   * operationes, for example in {@link Render#composite_glyphs}.
   */
  public static final PictFormat NONE = new PictFormat (0);

  /**
   * The ID of this pict format.
   */
  private int id;

  /**
   * The format type (indexed or direct color).
   */
  private Type type;

  /**
   * The depth in bits/pixel.
   */
  private int depth;

  /**
   * The direct parameters. Only valid for direct formats.
   */
  private Direct direct;

  /**
   * The colormap. This is used only for indexed formats.
   */
  private Colormap colormap;

  /**
   * Creates a PictFormat with a predefined ID. This is only used for the
   * {@link #NONE} PictFormat constant.
   *
   * @param id the predefined id
   */
  private PictFormat (int id) {
    id = 0;
  }

  /**
   * Reads a PictFormat from the stream.
   *
   * @param i the PictFormat to read from
   */
  PictFormat (ResponseInputStream i) {
    id = i.read_int32 ();
    type = i.read_int8 () == 0 ? Type.INDEXED : Type.DIRECT;
    depth = i.read_int8 ();
    i.skip (2);
    direct = new Direct(i);
    colormap = new Colormap(i.read_int32 ());
  }

  /**
   * Returns the ID of this PictFormat.
   *
   * @return the ID of this PictFormat
   */
  int id () {
    return id;
  }

  public int depth () {
    return depth;
  }

  /**
   * Returns <code>true</code> if the template matches this format instance,
   * <code>false</code> otherwise.
   *
   * @param temp the template to check
   *
   * @return <code>true</code> if the template matches this format instance,
   *         <code>false</code> otherwise
   */
  boolean match (Template temp) {
    return (((temp.field_mask & Template.TYPE_MASK) != 0) && temp.type == type)
           && (((temp.field_mask & Template.DEPTH_MASK) != 0)
               && temp.depth == depth)
           && (((temp.field_mask & Template.DIRECT_MASK) != 0)
               && direct.match (temp));
  }

  /**
   * Returns a string representation of this PictFormat instance.
   */
  public String toString () {
    return "PictFormat[type=" + type + ", depth=" + depth
           + ", direct=" + direct + "]";
  }

  public Direct direct_format () {
    return direct;
  }
}
