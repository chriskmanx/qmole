package gnu.x11;

/** X window attributes. */
public class WindowAttributes extends ValueList {

    public WindowAttributes(int count) {
        super(count);
    }

    public final static WindowAttributes EMPTY = new WindowAttributes();

    public WindowAttributes() {

        super(15);
    }

    /**
     * @param p
     *            possible: {@link Pixmap#NONE} (default),
     *            {@link Pixmap#PARENT_RELATIVE}
     */
    public void setBackground(Pixmap p) {

        set(0, p.id);
    }

    /**
     * @see #setBackground(int)
     */
    public void setBackground(Color c) {

        setBackground(c.getPixel());
    }

    public void setBackground(int pixel) {

        set(1, pixel);
    }

    /**
     * @param p
     *                possible: {@link Pixmap#COPY_FROM_PARENT} (default)
     */
    public void setBorder(Pixmap p) {

        set(2, p.id);
    }

    /**
     * @see #setBorder(int)
     */
    public void setBorder(Color c) {

        setBorder(c.getPixel());
    }

    public void setBorder(int pixel) {

        set(3, pixel);
    }

    public enum Gravity {
        FORGET(0),
        NORTH_WEST(1),
        NORTH(2),
        NORTH_EAST(3),
        WEST(4),
        CENTER(5),
        EAST(6),
        SOUTH_WEST(7),
        SOUTH(8),
        SOUTH_EAST(9),
        STATIC(10);
        
        private int code;
        
        
        private Gravity(int code) {
            this.code = code;
        }
        
        
        public int getCode() {

            return code;
        }
    }

    /**
     * @param gravity
     *                valid: {@link #FORGET} (default), {@link #NORTH_WEST},
     *                {@link #NORTH}, {@link #NORTH_EAST}, {@link #WEST},
     *                {@link #CENTER}, {@link #EAST}, {@link #SOUTH_WEST},
     *                {@link #SOUTH}, {@link #SOUTH_EAST}, {@link #STATIC}
     */
    public void setWinGravity(Gravity gravity) {

        set(5, gravity.getCode());
    }

    /**
     * @param i
     *                valid: {@link #NOT_USEFUL}(default),
     *                {@link #WHEN_MAPPED}, {@link #ALWAYS}
     */
    public void setBackingStore(Screen.BackingStore bs) {

        set(6, bs.getCode());
    }

    /**
     * @param i
     *                default: all ones
     */
    public void setBackingPlane(int i) {

        set(7, i);
    }

    /**
     * #set_backing(int)
     */
    public void setBacking(Color c) {

        setBacking(c.getPixel());
    }

    /**
     * @param i
     *                default: zero
     */
    public void setBacking(int pixel) {

        set(8, pixel);
    }

    /**
     * @param b
     *                default: false
     */
    public void setOverrideRedirect(boolean b) {

        set(9, b);
    }

    /**
     * @param b
     *                default: false
     */
    public void setSaveUnder(boolean b) {

        set(10, b);
    }

    /**
     * @param i
     *                default: {}
     */
    public void setEventMask(int i) {

        set(11, i);
    }

    public void addEventMask(int i) {

        setEventMask(eventMask() | i);
    }

    public int eventMask() {

        return data[11];
    }

    /**
     * @param i
     *                default: {}
     */
    public void setDoNotPropagateMask(int i) {

        set(12, i);
    }

    /**
     * @param c
     *                possible: {@link Colormap#COPY_FROM_PARENT} (default)
     */
    public void setColormap(Colormap c) {

        set(13, c.id);
    }

    /**
     * @param c
     *                possible: {@link Cursor#NONE}
     */
    public void setCursor(Cursor c) {

        set(14, c.id);
    }

    public Object clone() {

        WindowAttributes attr = new WindowAttributes();
        attr.copy(this);
        return attr;
    }
}
