package gnu.x11;


/** X Screen. */
public class Screen {

  public Display display;

  public int root_id;

  public int default_colormap_id;

  public int white_pixel;

  public int black_pixel;

  public int current_input_masks;

  public int width;

  public int height;

  public int width_in_mm;

  public int height_in_mm;

  public int min_installed_maps;

  public int max_installed_maps;

  public int root_visual_id;

  public int backing_stores;

  public boolean save_unders;

  public int root_depth;

  public Depth[] allowed_depths;

  public Screen (Display display, ResponseInputStream in) { 
    this.display = display;

    root_id = in.read_int32 ();
    default_colormap_id = in.read_int32 ();
    white_pixel = in.read_int32 ();
    black_pixel = in.read_int32 ();
    current_input_masks = in.read_int32 ();
    width = in.read_int16 ();
    height = in.read_int16 ();
    width_in_mm = in.read_int16 ();
    height_in_mm = in.read_int16 ();
    min_installed_maps = in.read_int16 ();
    max_installed_maps = in.read_int16 ();
    root_visual_id = in.read_int32 ();
    backing_stores = in.read_int8 ();
    save_unders = in.read_bool ();
    root_depth = in.read_int8 ();
    int num_depths = in.read_int8 ();
    allowed_depths = new Depth [num_depths];
    for (int i = 0; i < num_depths; i++) {
      allowed_depths [i] = new Depth (in);
    }
  }

  public static final int NEVER = 0;
  public static final int WHEN_MAPPED = 1;
  public static final int ALWAYS = 2;


  public static final String [] BACKING_STORES_STRINGS = {
    "never", "when-mapped", "always"
  };

  private GC default_gc_cache;

  /** Shared, read-only resource in general. */
  public GC default_gc () {
    if (default_gc_cache == null) {
      GC.Values gv = new GC.Values ();
      gv.set_foreground (black_pixel);
      gv.set_background (white_pixel);

      default_gc_cache = new GC (display, gv);
    }

    return default_gc_cache;
  }

  public Window root () { 
    return (Window) Window.intern (display, root_id);
  }

  public int root_visual_id () {
    return root_visual_id;
  }

  public int root_depth() {
    return root_depth;
  }

  public Colormap default_colormap () {
    return new Colormap (display, default_colormap_id);
  }

  public String toString () { 
    return "#Screen"
      + "\n  root-id: " + root_id
      + "\n  default-colormap-id: " + default_colormap_id
      + "\n  white-pixel: " + white_pixel
      + "\n  black-pixel: " + black_pixel
      + "\n  width: " + width
      + "\n  height: " + height
      + "\n  width-mm: " + width_in_mm
      + "\n  height-mm: " + height_in_mm
      + "\n  min-installed-maps: " + min_installed_maps
      + "\n  max-installed-maps: " + max_installed_maps
      + "\n  root-visual-id: " + root_visual_id
      + "\n  backing-stores: " + BACKING_STORES_STRINGS [backing_stores]
      + "\n  save-unders: " + save_unders
      + "\n  root-depth: " + root_depth
      + "\n  allowed-depth-count: " + allowed_depths.length;
  }
}
