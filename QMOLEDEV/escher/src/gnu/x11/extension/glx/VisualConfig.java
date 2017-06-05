package gnu.x11.extension.glx;

import gnu.x11.Data;
import gnu.x11.ResponseInputStream;

/**
 *
 */
public class VisualConfig {
    
    public static final int VISUAL_BIT = 1 << 0;
    public static final int CLASS_BIT = 1 << 1;
    public static final int RGBA_BIT = 1 << 2;
    public static final int RED_SIZE_BIT = 1 << 3;
    public static final int GREEN_SIZE_BIT = 1 << 4;
    public static final int BLUE_SIZE_BIT = 1 << 5;
    public static final int ALPHA_SIZE_BIT = 1 << 6;
    public static final int ACCUM_RED_SIZE_BIT = 1 << 7;
    public static final int ACCUM_GREEN_SIZE_BIT = 1 << 8;
    public static final int ACCUM_BLUE_SIZE_BIT = 1 << 9;
    public static final int ACCUM_ALPHA_SIZE_BIT = 1 << 10;
    public static final int DOUBLE_BUFFER_BIT = 1 << 11;
    public static final int STEREO_BIT = 1 << 12;
    public static final int BUFFER_SIZE_BIT = 1 << 13;
    public static final int DEPTH_SIZE_BIT = 1 << 14;
    public static final int STENCIL_SIZE_BIT = 1 << 15;
    public static final int AUX_BUFFERS_BIT = 1 << 16;
    public static final int LEVEL_BIT = 1 << 17;

    public int bitmask;
    public int count;

    private int visual_id = (int) GLXConstants.GLX_DONT_CARE;
    private int clazz = (int) GLXConstants.GLX_DONT_CARE;
    private boolean rgba = false;
    private boolean careRGBA = false;
    private int red_size = (int) GLXConstants.GLX_DONT_CARE;
    private int green_size = (int) GLXConstants.GLX_DONT_CARE;
    private int blue_size = (int) GLXConstants.GLX_DONT_CARE;
    private int alpha_size = (int) GLXConstants.GLX_DONT_CARE;
    private int accum_red_size = (int) GLXConstants.GLX_DONT_CARE;
    private int accum_green_size = (int) GLXConstants.GLX_DONT_CARE;
    private int accum_blue_size = (int) GLXConstants.GLX_DONT_CARE;
    private int accum_alpha_size = (int) GLXConstants.GLX_DONT_CARE;
    private boolean double_buffer = false;
    private boolean careDoubleBuffer = false;
    private boolean stero = false;
    private boolean careStereo = false;
    private int buffer_size = (int) GLXConstants.GLX_DONT_CARE;
    private int depth_size = (int) GLXConstants.GLX_DONT_CARE;
    private int stencil_size = (int) GLXConstants.GLX_DONT_CARE;
    private int aux_buffers = (int) GLXConstants.GLX_DONT_CARE;
    private int level;

    private int[] more_prop_types;
    private int[] more_prop_values;

    /** Writing. */
    public VisualConfig() { /* no-op */}

    public VisualConfig(int [] attributeList) { 
         
        for (int attribute = 0; attribute < attributeList.length;
             attribute++) {
            switch (attributeList[attribute]) {
            case GLXConstants.GLX_RGBA:
                this.rgba = true;
                this.careRGBA = true;
                break;
            case GLXConstants.GLX_BUFFER_SIZE:
                this.buffer_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_LEVEL:
                this.level = attributeList[++attribute];
                break;
            case GLXConstants.GLX_DOUBLEBUFFER:
                this.double_buffer = true;
                this.careDoubleBuffer = true;
                break;
            case GLXConstants.GLX_STEREO:
                this.stero = true;
                this.careStereo = true;
                break;
            case GLXConstants.GLX_AUX_BUFFERS:
                this.aux_buffers = attributeList[++attribute];
                break;
            case GLXConstants.GLX_RED_SIZE:
                this.red_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_GREEN_SIZE:
                this.green_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_BLUE_SIZE:
                this.blue_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_ALPHA_SIZE:
                this.alpha_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_DEPTH_SIZE:
                this.depth_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_STENCIL_SIZE:
                this.stencil_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_ACCUM_RED_SIZE:
                this.accum_red_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_ACCUM_GREEN_SIZE:
                this.accum_green_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_ACCUM_BLUE_SIZE:
                this.accum_blue_size= attributeList[++attribute];
                break;
            case GLXConstants.GLX_ACCUM_ALPHA_SIZE:
                this.accum_alpha_size = attributeList[++attribute];
                break;
            case GLXConstants.GLX_VISUAL_CAVEAT_EXT:
                break;
            case GLXConstants.GLX_NONE: case 0:
                attribute = attributeList.length;
                break;
            default:
                System.out.println("IGNORED CASE!! " + attributeList[attribute]);
                break;
            }
        }        
    }
    
    /** Reading. */
    public VisualConfig(ResponseInputStream i, int count) {

        this.count = count;
        
        // first 18 numbered properties
        visual_id = i.readInt32();
        clazz = i.readInt32();
        rgba = i.readInt32() == 1;
        red_size = i.readInt32();
        green_size = i.readInt32();
        blue_size = i.readInt32();
        alpha_size = i.readInt32();
        accum_red_size = i.readInt32();
        accum_green_size = i.readInt32();
        accum_blue_size = i.readInt32();
        accum_alpha_size = i.readInt32();
        double_buffer = i.readInt32() == 1;
        stero = i.readInt32() == 1;
        buffer_size = i.readInt32();
        depth_size = i.readInt32();
        stencil_size = i.readInt32();
        aux_buffers = i.readInt32();
        level = i.readInt32();

        // 
        more_prop_types = new int[count];
        more_prop_values = new int[count];
        for (int index = 0; index < count; index++) {
            more_prop_types[index] = i.readInt32();
            more_prop_values[index] = i.readInt32();
        }
    }

    // -- reading

    @Deprecated
    public int visual_id() {

        return visual_id;
    }

    /**
     * Return the visual ID of thios VisualConfig.
     * 
     * @return
     */
    public int getVisualID() {

        return this.visual_id();
    }

    /**
     * @deprecated Use {@link #getVisualClass()} instead
     */
    public int clazz() {
        return clazz;
    }

    public int getVisualClass() {

        return clazz();
    }

    /**
     * @deprecated Use {@link #getRGBA()} instead
     */
    public boolean rgba() {
        
        return rgba;
    }

    public boolean getRGBA() {

        return rgba;
    }

    /**
     * @deprecated Use {@link #getRedSize()} instead
     */
    public int red_size() {
        
        return red_size;
    }

    public int getRedSize() {

        return red_size();
    }

    /**
     * @deprecated Use {@link #getGreenSize()} instead
     */
    public int green_size() {
        
        return green_size;
    }

    public int getGreenSize() {

        return green_size();
    }

    /**
     * @deprecated Use {@link #getBlueSize()} instead
     */
    public int blue_size() {
        
        return blue_size;
    }

    public int getBlueSize() {

        return blue_size();
    }

    /**
     * @deprecated Use {@link #getAlphaSize()} instead
     */
    public int alpha_size() {
        
        return alpha_size;
    }

    public int getAlphaSize() {

        return alpha_size();
    }

    public int accum_red_size() {

        return accum_red_size;
    }

    public int accum_green_size() {

        return accum_green_size;
    }

    public int accum_blue_size() {

        return accum_blue_size;
    }

    public int accum_alpha_size() {

        return accum_alpha_size;
    }

    public boolean double_buffer() {

        return double_buffer;
    }

    public boolean stero() {

        return stero;
    }

    public int buffer_size() {

        return buffer_size;
    }

    public int depth_size() {

        return depth_size;
    }

    public int stencil_size() {

        return stencil_size;
    }

    public int aux_buffers() {

        return aux_buffers;
    }

    public int level() {

        return level;
    }

    public int more_property_type(int i) {

        return more_prop_types[i];
    }

    public int more_property_value(int i) {

        return more_prop_values[i];
    }

    // -- writing

    public void set_visual_id(int i) {

        visual_id = i;
        set(0);
    }

    public void set_clazz(int i) {

        clazz = i;
        set(1);
    }

    public void set_rgba() {

        set(2);
    }

    public void set_red_size(int i) {

        red_size = i;
        set(3);
    }

    public void set_green_size(int i) {

        green_size = i;
        set(4);
    }

    public void set_blue_size(int i) {

        blue_size = i;
        set(5);
    }

    public void set_alpha_size(int i) {

        alpha_size = i;
        set(6);
    }

    public void set_accum_red_size(int i) {

        accum_red_size = i;
        set(7);
    }

    public void set_accum_green_size(int i) {

        accum_green_size = i;
        set(8);
    }

    public void set_accum_blue_size(int i) {

        accum_blue_size = i;
        set(9);
    }

    public void set_accum_alpha_size(int i) {

        accum_alpha_size = i;
        set(10);
    }

    public void set_double_buffer() {

        set(11);
    }

    public void set_stero() {

        set(12);
    }

    public void set_buffer_size(int i) {

        buffer_size = i;
        set(13);
    }

    public void set_depth_size(int i) {

        depth_size = i;
        set(14);
    }

    public void set_stencil_size(int i) {

        stencil_size = i;
        set(15);
    }

    public void set_aux_buffers(int i) {

        aux_buffers = i;
        set(16);
    }

    public void set_level(int i) {

        level = i;
        set(17);
    }

    public void clear() {

        bitmask = 0;
    }

    public int length() {

        return 4 * count;
    }

    public void set_accum_rgb_size(int i) {

        set_accum_red_size(i);
        set_accum_green_size(i);
        set_accum_blue_size(i);
    }
    
    /**
     * See if the current VisualConfig is compatible the given attributeList
     * @param attributeList
     */
    public boolean compatible (VisualConfig template) {
        
        // so it's smaller to write and the compiler don't complain about
        // GLX_DONT_CARE being long (when in fact would be better to have as int)
        int DONT_CARE = (int) GLXConstants.GLX_DONT_CARE;
        
        /* **** DON'T CARE ***** */
        if (template.careDoubleBuffer && double_buffer != template.double_buffer)
            return false;
        if (template.careStereo && stero != template.stero)
            return false;
        if (template.clazz != DONT_CARE && clazz != template.clazz)
            return false;
       
        /* ***** MINIMUM ***** */
        if (template.buffer_size != DONT_CARE && template.buffer_size > buffer_size)
            return false;
        if (template.aux_buffers != DONT_CARE && template.aux_buffers > aux_buffers)
            return false;
        if (template.red_size != DONT_CARE && template.red_size > red_size)
            return false;
        if (template.green_size != DONT_CARE && template.green_size > green_size)
            return false; 
        if (template.blue_size != DONT_CARE && template.blue_size > blue_size)
            return false;
        if (template.alpha_size != DONT_CARE && template.alpha_size > alpha_size)
            return false;
        if (template.depth_size != DONT_CARE && template.depth_size > depth_size)
            return false;
        if (template.stencil_size != DONT_CARE && template.stencil_size > stencil_size)
            return false;
        if (template.accum_red_size != DONT_CARE && template.accum_red_size > accum_red_size)
            return false;
        if (template.accum_green_size != DONT_CARE && template.accum_green_size > accum_green_size)
            return false;
        if (template.accum_blue_size != DONT_CARE && template.accum_blue_size > accum_blue_size)
            return false;
        if (template.accum_alpha_size != DONT_CARE && template.accum_alpha_size > accum_alpha_size)
            return false;

        /* ***** EXACT ***** */
        if (level != template.level)
            return false;
        
        return true;
    }
    
    /**
     * Return -1 if {@code this}. VisualConfig is less then {@code template},
     * 0 is they are considered equal, and 1 if template is considered greater
     * than {@code this}.
     * 
     * The algorithm used to determine how a VisualConfig is supposed to be
     * less, greater or equal to another is pure magic and won't be explained
     * here. But you can give a look at the GLX 1.3 spec and ARB_multisample,
     * read the source code, or the MESA source code...
     */
    public int compare (VisualConfig template) {
        
        // FIXME: this is only a subset, and the implementation from MESA
        // clearly states that this code is not 100% correct.
        // we should fix it and try to follow the GLX spec.
        
        // the order of comparision is fixed, don't change it.
        
        if (this.red_size != template.red_size) {
           return preferLargerOrZero(this.red_size, template.red_size);
        }
        
        if (this.green_size != template.green_size) {
            return preferLargerOrZero(this.green_size, template.green_size);
        }
        
        if (this.blue_size != template.blue_size) {
            return preferLargerOrZero(this.blue_size, template.blue_size);
        }
        
        if (this.alpha_size != template.alpha_size) {
            return preferLargerOrZero(this.alpha_size, template.alpha_size);
        }
        
        if (this.buffer_size != template.buffer_size) {
            return preferSmaller(this.buffer_size, template.buffer_size);
        }
        
        if (this.double_buffer != template.double_buffer) {
            //Prefer single-buffer.
            return (this.double_buffer) ? 1 : -1;
        }
        
        if (this.aux_buffers != template.aux_buffers) {
            return preferSmaller(this.aux_buffers, template.aux_buffers);
        }
        
        if (this.depth_size != template.depth_size) {
            return preferLargerOrZero(this.depth_size, template.depth_size);
        }
        
        if (this.stencil_size != template.stencil_size) {
            return preferSmaller(this.stencil_size, template.stencil_size);
        }
        
        if (this.accum_red_size != template.accum_red_size) {
            return preferLargerOrZero(this.accum_red_size, template.accum_red_size);
        }
         
        if (this.accum_green_size != template.accum_green_size) {
            return preferLargerOrZero(this.accum_green_size, template.accum_green_size);
        }

        if (this.accum_blue_size != template.accum_blue_size) {
            return preferLargerOrZero(this.accum_blue_size, template.accum_blue_size);
        }

        if (this.accum_alpha_size != template.accum_alpha_size) {
            return preferLargerOrZero(this.accum_alpha_size, template.accum_alpha_size);
        }

        if (this.clazz != template.clazz) {
            return preferSmaller(this.clazz, template.clazz);
        }
        
        return 0;
    }
    
    private int preferLargerOrZero(int a, int b) {
       
        if (a == 0)
            return -1;
        else if (b == 0)
            return 1;
        else
            return (b - a);
    }
    
    private int preferSmaller(int a, int b) {
        return (a - b);
    }
    
    private int preferLarger(int a, int b) {
        return (b - a);
    }
    
    public String toString() {

        return "#VisualConfig" + "\n  visual-id: " + visual_id()
                + "\n  class: " + getVisualClass() + "\n  rgba: " + getRGBA()
                + "\n  red-size: " + getRedSize() + "\n  green-size: "
                + getGreenSize() + "\n  blue-size: " + getBlueSize()
                + "\n  alpha-size: " + getAlphaSize() + "\n  accum-red-size: "
                + accum_red_size() + "\n  accum-green-size: "
                + accum_green_size() + "\n  accum-blue-size: "
                + accum_blue_size() + "\n  accum-alpha-size: "
                + accum_alpha_size() + "\n  double-buffer: " + double_buffer()
                + "\n  stero: " + stero() + "\n  buffer-size: " + buffer_size()
                + "\n  depth-size: " + depth_size() + "\n  stencil-size: "
                + stencil_size() + "\n  aux-buffers: " + aux_buffers()
                + "\n  level: " + level() + "\n  property-count: " + count;
    }

    private void set(int i) {

        bitmask |= 1 << i;
    }
}
