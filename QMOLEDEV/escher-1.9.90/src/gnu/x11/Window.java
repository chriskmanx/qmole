
package gnu.x11;

import gnu.x11.event.Event;
import gnu.x11.extension.glx.GLXDrawable;

/** X window. */
public class Window extends Drawable implements GLXDrawable {

    /**
     * Predefined windows.
     * 
     * <p>
     * All predefined resources are not "properly" initialized, in the sense
     * that member variable <code>display</code> is <code>null</code>. That
     * is, they are not connected to any X server, and cannot be used for server
     * interaction (because <code>display == null</code>, resulting in
     * <code>NullPointerException</code>).
     * 
     * <p>
     * For special operations like setting selection owner to {@link #NONE},
     * do:
     * 
     * <pre><code>
     *   Window.NONE.setDisplay = ...;
     *   Window.NONE.setSelectionOwner (...);
     * </code></pre>
     * 
     * <p>
     * We could make these predefined resources members of <code>Display</code>
     * class, and could initialize them properly for each <code>Display</code>
     * instance. It would prevent programmers from the mistake described above.
     * However, predefined resources are not designed in the first place for
     * server interaction. For example, you cannot draw a line on
     * <code>Window.POINTER_WINDOW</code>. Second, it is a better
     * classification to put predefined resources in their respective classes.
     * Third, it is cheaper to have predefined resources as
     * <code>static final</code> objects.
     * 
     * <p>
     * Note also that, consequently, predefined resources are not interned to
     * <code>Display.resource</code>. For example, <code>Window.intern
     * (this, 0)</code>
     * will not return static variable {@link #NONE}. Instead it creates a new
     * <code>Window</code> object which sequential intern calls will return.
     * As a side effect, <code>Window</code> equality should not be tested
     * with <code>==</code> operator; it should be tested easily as
     * <code>this_window.id == that_window.id</code>.
     */
    public static final Window NONE = new Window(0);

    private int x, y;

    
    private Window parent;
    
    
    public enum WinClass {
        COPY_FROM_PARENT(0),
        INPUT_OUTPUT(1),
        INPUT_ONLY(2);
        
        private int code;
        
        WinClass(int cd) {
            this.code = cd;
        }
        
        public int getCode() {
            return code;
        }
        
        public static WinClass getByID(int id) {
            switch (id)
            {
                case 0: return COPY_FROM_PARENT;
                case 1: return INPUT_OUTPUT;
                case 2: return INPUT_ONLY;
                default: return COPY_FROM_PARENT;
            }
        }
    }
    
    
    public enum MapState {
        UNMAPPED(0),
        UNVIEWABLE(1),
        VIEWABLE(2);
        
        private int code;
        
        
        private MapState(int code) {
            this.code = code;
        }
        
        public int getCode() {
            return code;
        }
        
        public static MapState getByCode(int code) {
            switch (code) {
                case 0: return UNMAPPED;
                case 1: return UNVIEWABLE;
                case 2: return VIEWABLE;
                default: return UNMAPPED;
            }
        }
    }
    
    
    public enum CirculateDirection {
        RAISE_LOWEST(0),
        LOWER_HIGHEST(1);
        
        private int code;
        
        CirculateDirection(int code) {
            this.code = code;
        }
        
        public int getCode() {
            return code;
        }
    }
    
    
    public enum PropertyMode {
        REPLACE(0),
        PREPEND(1),
        APPEND(2);
        
        private int code;
        
        PropertyMode(int code) {
            this.code = code;
        }
        
        public int getCode() {
            return code;
        }
    }
    
    
    public enum GrabMode {
        SYNCHRONOUS(0),
        ASYNCHRONOUS(1);
        
        private int code;
        
        GrabMode(int code) {
            this.code = code;
        }
        
        public int getCode() {
            return code;
        }
    }
    
    
    public enum GrabStatus {
        SUCCESS(0),
        ALREADY_GRABBED(1),
        INVALID_TIME(2),
        NOT_VIEWABLE(3),
        FROZEN(4);        
        
        private int code;
        
        GrabStatus(int code) {
            this.code = code;
        }
        
        public int getCode() {
            return code;
        }
        
        public static GrabStatus getByCode(int code) {
            switch (code) {
                case 0: return SUCCESS;
                case 1: return ALREADY_GRABBED;
                case 2: return INVALID_TIME;
                case 3: return NOT_VIEWABLE;
                case 4: return FROZEN;
                default: return SUCCESS;
            }
        }
    }
    
    
    public enum RevertTo {
        TO_NONE(0),
        TO_POINTER_ROOT(1),
        TO_PARENT(2);
        
        private int code;
        
        RevertTo(int code) {
            this.code = code;
        }
        
        public int getCode() {
            return code;
        }
    }

    
    public enum WMInitialState {
        WITHDRAWN(0),
        NORMAL(1),
        ICONIC(3);
        
        private int code;
        
        WMInitialState(int code) {
            this.code = code;
        }
        
        public int getCode() {
            return code;
        }
        
        public static WMInitialState getByCode(int code) {
            switch (code) {
                case 0: return WITHDRAWN;
                case 1: return NORMAL;
                case 2: return ICONIC;
                default: return WITHDRAWN;
            }
        }
    }

    
    /** X window changes. */
    public static class Changes extends ValueList {

        public Changes() {

            super(7);
        }

        public void setX(int i) {

            set(0, i);
        }

        public void setY(int i) {

            set(1, i);
        }

        public void setWidth(int i) {

            set(2, i);
        }

        public void setHeight(int i) {

            set(3, i);
        }

        public void borderWidth(int i) {

            set(4, i);
        }

        public void sibling_id(int i) {

            set(5, i);
        }

        public void sibling(Window window) {

            sibling_id(window.id);
        }

        public enum StackMode {
            ABOVE(0),
            BELOW(1),
            TOP_IF(2),
            BOTTOM_IF(3),
            OPPOSITE(4);
            
            private int code;
            
            StackMode(int code) {
                this.code = code;
            }
            
            public int getCode() {

                return code;
            }
            
            public static StackMode getByCode(int code) {
                switch (code) {
                    case 0: return ABOVE;
                    case 1: return BELOW;
                    case 2: return TOP_IF;
                    case 3: return BOTTOM_IF;
                    case 4: return OPPOSITE;
                    default: return ABOVE;
                }
            }
        }

        /**
         * @param i
         *                valid: {@link #ABOVE}, {@link #BELOW},
         *                {@link #TOP_IF}, {@link #BOTTOM_IF},
         *                {@link #OPPOSITE}
         */
        public void stackMode(StackMode mode) {

            set(6, mode.getCode());
        }

        public Object clone() {

            Changes changes = new Changes();
            changes.copy(this);
            return changes;
        }
    }
    
   
    /** Reply of {@link #tree()}. */
    public class TreeInfo {

        Window root;

        Window parent;

        private Window[] children;

        TreeInfo(ResponseInputStream i) {

            root = (Window) intern(display, i.readInt32());
            int parent_id = i.readInt32();
            if (parent_id != 0)
                parent = (Window) intern(display, parent_id);
            else
                parent = null;

            int numWindows = i.readInt16();
            i.skip(14);
            children = new Window[numWindows];
            for (int j = 0; j < numWindows; j++) {
                int id = i.readInt32();
                children[j] = (Window) intern(display, id);
            }
        }

        public Window[] children() {

            return children;
        }
    }

    
    /** Reply of {@link #property(boolean, Atom, Atom, int, int)}. */
    public class Property {

        private int format;

        private int typeID;

        private int bytesAfter;

        private int length;

        private byte[] value;

        Property(ResponseInputStream i) {

            format = i.readInt8();
            i.skip(6);
            typeID = i.readInt32();
            bytesAfter = i.readInt32();
            length = i.readInt32();
            i.skip(12);
            int num_bytes;
            switch (format) {
            case 8:
                num_bytes = length;
                break;
            case 16:
                num_bytes = length * 2;
                break;
            case 32:
                num_bytes = length * 4;
                break;
            default:
                num_bytes = 0;
            }
            value = new byte[num_bytes];
            i.readData(value);
            int p = RequestOutputStream.pad(num_bytes);
            if (p > 0)
                i.skip(p);
        }

        public int format() {

            return format;
        }

        public int typeID() {

            return typeID;
        }

        /**
         * Returns the value at index <code>i</code>. This interprets the
         * underlying byte data according to the format of this property.
         * 
         * @param i
         *                the index
         * 
         * @return the value at the specified index
         */
        public int value(int i) {

            int v;
            switch (format) {
            case 8:
                v = value[i];
                break;
            case 16:
                v = ((0xff & value[i * 2]) << 8) | (0xff & value[i * 2 + 1]);
                break;
            case 32:
                v = ((0xff & value[i * 4]) << 24)
                        | ((0xff & value[i * 4 + 1]) << 16)
                        | ((0xff & value[i * 4 + 2] << 8))
                        | (0xff & ((int) value[i * 4 + 3]));
                break;
            default:
                throw new ArrayIndexOutOfBoundsException();
            }
            return v;
        }

        /**
         * Returns the property value as string.
         * 
         * @return the property value as string
         */
        public String stringValue() {

            return new String(value);
        }
    }
    
    
    /** Reply of {@link #getAttributes()}. */
    public static class AttributesReply {

        private Screen.BackingStore backingStore;

        private Visual visualID;

        private WinClass windowClass;

        private int bitGravity;

        private int winGravity;

        private int backingPlanes;

        private int backingPixel;

        private boolean saveUnder;

        private boolean mapIsInstalled;

        private MapState mapState;

        private boolean overrideRedirect;

        private int colormapID;

        private int allEventMasks;

        private int yourEventMask;

        private int doNotPropagateMask;

        /**
         * Reads the AttributesReply data from the specified input stream.
         */
        public AttributesReply(ResponseInputStream in) {

            int code = in.readInt8();
            assert code == 1 : "Errors and events should be catched in Connection";

            backingStore = Screen.BackingStore.getCode(in.readInt8());

            in.readInt16(); // Sequence number, not needed.

            in.readInt32(); // Reply length, not needed.

            visualID = Visual.getVisual(in.readInt32());
            windowClass = WinClass.getByID(in.readInt16());
            bitGravity = in.readInt8();
            winGravity = in.readInt8();
            backingPlanes = in.readInt32();
            backingPixel = in.readInt32();
            saveUnder = in.readBool();
            mapIsInstalled = in.readBool();
            mapState = MapState.getByCode(in.readInt8());
            overrideRedirect = in.readBool();
            colormapID = in.readInt32();
            allEventMasks = in.readInt32();
            yourEventMask = in.readInt32();
            doNotPropagateMask = in.readInt16();
            in.skip(2); // Unused.
        }

        /**
         * @return valid: {@link MapState}
         */
        public MapState mapState() {
            return mapState;
        }

        public boolean overrideRedirect() {
            return overrideRedirect;
        }
    }

    
    /** Reply of {@link #pointer()}. */
    public class PointerInfo {

        private boolean sameScreen;

        private Window root;

        private Window child;

        private int rootX;

        private int rootY;

        private int winX;

        private int winY;

        private int mask;

        PointerInfo(ResponseInputStream i) {

            sameScreen = i.readBool();
            i.skip(6);

            int root_id = i.readInt32();
            root = (Window) intern(display, root_id);

            int child_id = i.readInt32();
            if (child_id != 0)
                child = (Window) intern(display, root_id);
            else
                child = null;

            rootX = i.readInt16();
            rootY = i.readInt16();
            winX = i.readInt16();
            winY = i.readInt16();
            mask = i.readInt16();
        }

        public Point rootPosition() {
            return new Point(rootX, rootY);
        }
    }
    
    
    /** Reply of {@link #getMotionEvents(int, int)} */
    public static class TimeCoord {

        public long timestamp;

        public int x;

        public int y;

        TimeCoord(ResponseInputStream i) {

            timestamp = i.readInt32();
            x = i.readInt16();
            y = i.readInt16();
        }
    }
    
    
    /** Reply of {@link #translateCoordinates(Window, int, int)}. */
    public class Coordinates {

        private boolean sameScreen;

        private Window child;

        private int x;

        private int y;

        Coordinates(ResponseInputStream i) {

            sameScreen = i.readBool();
            i.skip(6);
            int child_id = i.readInt32();
            if (child_id != 0)
                child = (Window) intern(display, child_id);
            else
                child = null;
            x = i.readInt16();
            y = i.readInt16();
        }
    }

    
    /** X window manager class hint. */
    public static class WMClassHint {

        public String res;

        public int middle;

        public WMClassHint(Data data) {

            int len = data.read4(16) - 1;
            res = data.readString(32, len);
            middle = res.indexOf(0);
        }

        public boolean classEquals(String res_class) {

            if (res_class == null)
                return false;
            return res.endsWith(res_class);
        }

        public boolean classEquals(WMClassHint hint) {

            if (hint == null)
                return false;
            return classEquals(hint.resClass());
        }

        public boolean equals(WMClassHint hint) {

            if (hint == null)
                return false;
            return res.equals(hint.res);
        }

        public boolean equals(String resName, String resClass) {

            if (resName == null || resClass == null)
                return false;
            if (resName.length() + resClass.length() != res.length() - 1)
                return false;

            String res0 = resName + "\0" + resClass;
            return res.equals(res0);
        }

        public String resName() {

            return res.substring(0, middle);
        }

        public String resClass() {

            return res.substring(middle + 1, res.length());
        }

        public String toString() {

            return "[" + resName() + " " + resClass() + "]";
        }
    }
    
    /** X window manager hints. */
    public static class WMHints extends Data {

        public WMHints(Data data) {

            super(data);
        }

        public static final int INPUT_HINT_MASK = 1 << 0;

        public static final int STATE_HINT_MASK = 1 << 1;

        public static final int ICON_PIXMAP_HINT_MASK = 1 << 2;

        public static final int ICON_WINDOW_HINT_MASK = 1 << 3;

        public static final int ICON_POSITION_HINT_MASK = 1 << 4;

        public static final int ICON_MASK_HINT_MASK = 1 << 5;

        public static final int WINDOW_GROUP_HINT_MASK = 1 << 6;

        public static final int URGENCY_HINT_MASK = 1 << 8;

        /**
         * @return valid: {@link #INPUT_HINT_MASK}, {@link #STATE_HINT_MASK},
         *         {@link #ICON_PIXMAP_HINT_MASK},
         *         {@link #ICON_WINDOW_HINT_MASK},
         *         {@link #ICON_POSITION_HINT_MASK},
         *         {@link #ICON_MASK_HINT_MASK},
         *         {@link #WINDOW_GROUP_HINT_MASK}, {@link #URGENCY_HINT_MASK}
         */
        public int flags() {

            return read4(32);
        }


        /**
         * @return valid: {@link WMInitialState.WITHDRAWN}, {@link WMInitialState.NORMAL}, {@link WMInitialState.ICONIC}
         */
        public WMInitialState initialState() {

            return WMInitialState.getByCode(read4(40));
        }
    }

    
    /** X window manager state. */
    public static class WMState extends Data {

        private Display display;

        public WMState(Display display, Data data) {

            super(data);
            this.display = display;
        }

        /**
         * @return valid:
         * {@link WMInitialState.WITHDRAWN},
         * {@link WMInitialState.NORMAL},
         * {@link WMInitialState.ICONIC}
         */
        public WMInitialState state() {

            return WMInitialState.getByCode(read4(32));
        }

        public int iconID() {

            return read4(36);
        }

        public Window icon() {

            return (Window) intern(display, iconID());
        }
    }
    
    
    /** Predefined. */
    public Window(int id) {

        super(id);
    }

    /** Intern. */
    public Window(Display display, int id) {

        super(display, id);
    }

    /**
     * @see #Window(Window, int, int, int, int)
     */
    public Window(Window parent, Rectangle geometry) {

        this(parent, geometry.getX(), geometry.getY(), geometry.getWidth(), geometry.getHeight());
    }

    /**
     * @see #Window(Window, int, int, int, int, int, Window.Attributes)
     */
    public Window(Window parent, Rectangle geometry, int borderWidth,
            WindowAttributes attr) {

        this(parent, geometry.getX(), geometry.getY(), geometry.getWidth(),
             geometry.getHeight(), borderWidth, attr);
    }

    /**
     * Initialize member fields only without creating object in X server.
     */
    public Window(Window parent, int x, int y, int width, int height) {

        super(parent.display);

        this.parent = parent;
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
    }

    /**
     * @see #create(int, int, int, int, Window.Attributes)
     */
    public Window(Window parent, int x, int y, int width, int height,
            int borderWidth, WindowAttributes attr) {

        this(parent, x, y, width, height);
        create(borderWidth, attr);
    }

    // opcode 1 - create window
    /**
     * @param depth
     *                possible: {@link WinClass.COPY_FROM_PARENT}
     * 
     * @param klass
     *                valid: {@link WinClass}
     * 
     * @param visualID
     *                possible: {@link WinClass.COPY_FROM_PARENT} or {@link Visual}
     *                
     * @see <a href="XCreateWindow.html">XCreateWindow</a>
     */
    public void create(int borderWidth, int depth, WinClass klass, int visualID,
                       WindowAttributes attr) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(1, depth, 8 + attr.count());
            o.writeInt32(id);
            o.writeInt32(parent.id);
            o.writeInt16(x);
            o.writeInt16(y);
            o.writeInt16(width);
            o.writeInt16(height);
            o.writeInt16(borderWidth);
            o.writeInt16(klass.getCode());
            o.writeInt32(visualID);
            o.writeInt32(attr.bitmask);
            attr.write(o);
            o.send();
        }
    }

    
    /**
     * @see #create(int, int, int, int, WindowAttributes)
     */
    public void create(int borderWidth, WindowAttributes attr) {

        create(borderWidth, WinClass.COPY_FROM_PARENT.getCode(), 
               WinClass.COPY_FROM_PARENT, WinClass.COPY_FROM_PARENT.getCode(),
               attr);
    }

    
    /**
     * @see #create(int, Window.Attributes)
     */
    public void create() {

        create(0, WindowAttributes.EMPTY);
    }

    
    // opcode 2 - change window attributes
    /**
     * This request will be aggregated.
     * 
     * @see <a href="XChangeAttributes.html"> XChangeAttributes</a>
     * 
     * @see Request.Aggregate aggregation
     */
    public void changeAttributes(WindowAttributes attr) {

        // FIXME: Implement aggregation.
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(2, 0, 3 + attr.count());
            o.writeInt32(id);
            o.writeInt32(attr.bitmask);
            attr.write(o);
            o.send();
        }
    }

    
    // opcode 3 - get window attributes
    /**
     * @see <a href="XGetAttributes.html">XGetAttributes</a>
     */
    public AttributesReply getAttributes() {

        RequestOutputStream o = display.getResponseOutputStream();
        AttributesReply r;
        synchronized (o) {
            o.beginRequest(3, 0, 2);
            o.writeInt32(id);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                r = new AttributesReply(i);
            }
        }
        return r;
    }

    
    // opcode 4 - destroy window
    /**
     * @see <a href="XDestroyWindow.html">XDestroyWindow</a>
     */
    public void destroyWindow() {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(4, 0, 2);
            o.writeInt32(id);
            o.send();
        }
    }

    
    // opcode 5 - destroy subwindows
    /**
     * @see <a href="XDestroySubwindows.html">XDestroySubwindows</a>
     */
    public void destroySubwindows() {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(5, 0, 2);
            o.writeInt32(id);
            o.send();
        }
    }

    
    // opcode 6 - change save set
    /**
     * @param mode
     *                valid: {@link Host.ChangeOperation.INSERT},
     *                {@link Host.ChangeOperation.DELETE}
     * 
     * @see <a href="XChangeSaveSet.html">XChangeSaveSet</a>
     */
    public void changeSaveSet(Host.ChangeOperation mode) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(6, mode.getCode(), 2);
            o.writeInt32(id);
            o.send();
        }
    }

    
    // opcode 7 - reparent window
    /**
     * @see <a href="XReparentWindow.html">XReparentWindow</a>
     */
    public void reparentWindow(Window parent, int x, int y) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(7, 0, 4);
            o.writeInt32(id);
            o.writeInt32(parent.id);
            o.writeInt16(x);
            o.writeInt16(y);
            o.send();
        }
    }

    
    // opcode 8 - map window
    /**
     * @see <a href="XMapWindow.html">XMapWindow</a>
     */
    public void map() {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(8, 0, 2);
            o.writeInt32(id);
            o.send();
            o.flush();
        }
    }

    
    // opcode 9 - map subwindows
    /**
     * @see <a href="XMapSubwindows.html">XMapSubwindows</a>
     */
    public void mapSubwindows() {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(9, 0, 2);
            o.writeInt32(id);
            o.send();
        }
    }

    
    // opcode 10 - unmap window
    /**
     * @see <a href="XUnmapWindow.html">XUnmapWindow</a>
     */
    public void unmap() {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(10, 0, 2);
            o.writeInt32(id);
            o.send();
        }
    }

    
    
    // opcode 11 - unmap subwindows
    /**
     * @see <a href="XUnmapSubwindows.html">XUnmapSubwindows</a>
     */
    public void unmapSubwindows() {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(11, 0, 2);
            o.writeInt32(id);
            o.send();
        }
    }


    // opcode 12 - configure window
    /**
     * This request will be aggregated.
     * 
     * @see <a href="XConfigureWindow.html">XConfigureWindow</a>
     * @see Request.Aggregate aggregation
     */
    public void configure(Changes changes) {

        // FIXME: Implement aggregation.
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(12, 0, 3 + changes.count());
            o.writeInt32(id);
            o.writeInt16(changes.bitmask);
            o.skip(2);
            changes.write(o);
            o.send();
        }
    }

    
    // opcode 13 - circulate window
    /**
     * @param direction
     *                valid: {@link #RAISE_LOWEST}, {@link #LOWER_HIGHEST}
     * 
     * @see <a href="XCirculateSubwindows.html">XCirculateSubwindows</a>
     */
    public void circulateWindow(CirculateDirection direction) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(13, direction.getCode(), 2);
            o.writeInt32(id);
            o.send();
        }
    }

    
    // opcode 15 - query tree
    /**
     * @see <a href="XQueryTree.html">XQueryTree</a>
     */
    public TreeInfo tree() {
        TreeInfo info;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(15, 0, 2);
            o.writeInt32(id);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(8);
                info = new TreeInfo(i);
            }
        }
        return info;
    }

    
    // opcode 18 - change property
    /**
     * Extra parameters (offset and data_format) are used to support Data class
     * as parameter for writing. See setWMNormalHints ().
     * 
     * @param mode
     *                valid: {@link PropertyMode.REPLACE}, {@link PropertyMode.PREPEND},
     *                {@link PropertyMode.APPEND}
     * 
     * @param format:
     *                valid: <code>8</code>, <code>16</code>,
     *                <code>32</code>
     * 
     * @param data_format:
     *                valid: <code>8</code>, <code>16</code>,
     *                <code>32</code>
     * 
     * @see <a href="XChangeProperty.html">XChangeProperty</a>
     */
    public void changeProperty(PropertyMode mode, Atom property, Atom type,
                               int format, Object data, int offset, int data_format) {

        byte[] byteData;
        switch (format) {
        case 8:
            byteData = (byte[]) data;
            break;
        case 16:
            short[] shortData = (short[]) data;
            byteData = new byte[shortData.length * 2];
            for (int i = 0; i < shortData.length; i++) {
                byteData[i * 2] = (byte) (shortData[i] >> 8);
                byteData[i * 2 + 1] = (byte) (shortData[i]);
            }
            break;
        case 32:
            int[] intData = (int[]) data;
            byteData = new byte[intData.length * 4];
            for (int i = 0; i < intData.length; i++) {
                byteData[i * 4] = (byte) (intData[i] >> 24);
                byteData[i * 4 + 1] = (byte) (intData[i] >> 16);
                byteData[i * 4 + 2] = (byte) (intData[i] >> 8);
                byteData[i * 4 + 3] = (byte) (intData[i]);
            }
            break;
        default:
            throw new IllegalArgumentException("Illegal format argument: "
                    + format);
        }
        int len = 0;
        int n = byteData.length;
        switch (format) {
        case 8:
            len = n;
            break;
        case 16:
            len = n / 2;
            break;
        case 32:
            len = n / 4;
            break;
        default:
            len = 0;
        }

        int p = RequestOutputStream.pad(n);

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(18, mode.getCode(), 6 + (n + p) / 4);
            o.writeInt32(id);
            o.writeInt32(property.getID());
            o.writeInt32(type.getID());
            o.writeInt8(format);
            o.skip(3);
            o.writeInt32(len); // data length in format unit
            o.write(byteData);
            o.skip(p);
            o.send();
        }
    }

    
    // opcode 19 - delete property
    /**
     * @see <a href="XDeleteProperty.html">XDeleteProperty</a>
     */
    public void deleteProperty(Atom property) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(19, 0, 3);
            o.writeInt32(id);
            o.writeInt32(property.getID());
            o.send();
        }
    }

    
    // opcode 20 - get property
    /**
     * @see <a href="XGetWindowProperty.html">XGetWindowProperty</a>
     */
    public Property getProperty(boolean delete, Atom property, Atom type,
                                 int offset, int length) {

        Property prop;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(20, delete ? 1 : 0, 6);
            o.writeInt32(id);
            o.writeInt32(property.getID());
            o.writeInt32(type.getID());
            o.writeInt32(offset);
            o.writeInt32(length);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(1);
                prop = new Property(i);
            }
        }
        return prop;
    }

    
    // opcode 21 - list properties
    /**
     * @return valid: {@link Enum#next()} of type {@link Atom},
     *         {@link Enum#next4()}
     * 
     * @see <a href="XRotateWindowProperties.html"> XRotateWindowProperties</a>
     */
    public Atom[] rotateWindowProperties() {

        Atom[] atoms;
        RequestOutputStream o = display.getResponseOutputStream();
        int[] atomIds;
        synchronized (o) {
            o.beginRequest(21, 0, 2);
            o.writeInt32(id);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(8);
                int numAtoms = i.readInt16();
                atomIds = new int[numAtoms];
                i.skip(22);
                for (int j = 0; j < numAtoms; j++) {
                    atomIds[j] = i.readInt32();
                }
            }
        }
        atoms = new Atom[atomIds.length];
        for (int i = 0; i < atomIds.length; i++)
            atoms[i] = (Atom) Atom.intern(display, atomIds[i]);
        return atoms;
    }

    
    // opcode 22 - set selection owner
    /**
     * @param time
     *                possible: {@link Display#CURRENT_TIME}
     * @see <a href="XSetSelectionOwner.html">XSetSelectionOwner</a>
     */
    public void setSelectionOwner(Atom selection, int time) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(22, 0, 4);
            o.writeInt32(id);
            o.writeInt32(selection.getID());
            o.writeInt32(time);
            o.send();
        }
    }

    
    // opcode 24 - convert selection
    /**
     * @param time
     *                possible: {@link Display#CURRENT_TIME}
     * @see <a href="XConvertSelection.html">XConvertSelection</a>
     */
    public void convertSelection(Atom selection, Atom target, Atom property,
                                  int time) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(24, 0, 6);
            o.writeInt32(id);
            o.writeInt32(selection.getID());
            o.writeInt32(target.getID());
            o.writeInt32(property.getID());
            o.writeInt32(time);
            o.send();
        }
    }

    
    // opcode 25 - send event
    /**
     * @see <a href="XSendEvent.html">XSendEvent</a>
     */
    public void sendEvent(boolean propagate, int event_mask, Event event) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(25, propagate ? 1 : 0, 11);
            o.writeInt32(id);
            o.writeInt32(event_mask);
            event.write(o);
            o.send();
        }
    }

    
    // opcode 26 - grab pointer
    /**
     * @param pointerMode
     *                valid: {@link GrabMode.SYNCHRONOUS}, {@link GrabMode.ASYNCHRONOUS}
     * 
     * @param keyboardMode
     *                valid: {@link GrabMode.SYNCHRONOUS}, {@link GrabMode.ASYNCHRONOUS}
     * 
     * @param confineTo
     *                possible: {@link #NONE}
     * @param cursor
     *                possible: {@link Cursor#NONE}
     * @param time
     *                possible: {@link Display#CURRENT_TIME}
     * 
     * @return valid: {@link GrabStatus.SUCCESS}, {@link GrabStatus.ALREADY_GRABBED},
     *         {@link GrabStatus.FROZEN}, {@link GrabStatus.INVALID_TIME}, {@link GrabStatus.NOT_VIEWABLE}
     * 
     * @see <a href="XGrabPointer.html">XGrabPointer</a>
     */
    public GrabStatus grabPointer(boolean owner_events, int eventMask,
                            GrabMode pointerMode, GrabMode keyboardMode,
                            Window confineTo, Cursor cursor, int time) {

        int status;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(26, owner_events ? 1 : 0, 6);
            o.writeInt32(id);
            o.writeInt16(eventMask);
            o.writeInt16(pointerMode.getCode());
            o.writeInt16(keyboardMode.getCode());
            o.writeInt32(confineTo.getID());
            o.writeInt32(cursor.getID());
            o.writeInt32(time);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(1);
                status = i.readInt8();
                i.skip(30);
            }
        }
        return GrabStatus.getByCode(status);
    }

    
    public static final int ANY_BUTTON = 0;

    
    public static final int ANY_MODIFIER = 0x8000;

    
    // opcode 28 - grab button
    /**
     * @param button
     *                possible: {@link #ANY_BUTTON}
     * @param modifiers
     *                possible: {@link #ANY_MODIFIER}
     * @param pointerMode
     *                valid: {@link GrabMode.SYNCHRONOUS},
     *                       {@link GrabMode.ASYNCHRONOUS}
     * 
     * @param keyboardMode
     *                valid: {@link GrabMode.SYNCHRONOUS},
     *                       {@link GrabMode.ASYNCHRONOUS}
     * 
     * @param confineTo
     *                possible: {@link #NONE}
     * @param cursor
     *                possible: {@link Cursor#NONE}
     * @see <a href="XGrabButton.html">XGrabButton</a>
     */
    public void grabButton(int button, int modifiers, boolean ownerEvents,
                            int eventMask, GrabMode pointerMode, 
                            GrabMode keyboardMode, Window confineTo,
                            Cursor cursor) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(28, ownerEvents ? 1 : 0, 6);
            o.writeInt32(id);
            o.writeInt16(eventMask);
            o.writeInt8(pointerMode.getCode());
            o.writeInt8(keyboardMode.getCode());
            o.writeInt32(confineTo.getID());
            o.writeInt32(cursor.getID());
            o.writeInt8(button);
            o.skip(1);
            o.writeInt16(modifiers);
            o.send();
        }
    }

    
    // opcode 29 - ungrab button
    /**
     * @param button
     *                possible: {@link #ANY_BUTTON}
     * @param modifiers
     *                possible: {@link #ANY_MODIFIER}
     * @see <a href="XUngrabButton.html">XUngrabButton</a>
     */
    public void ungrabButton(int button, int modifiers) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(29, button, 3);
            o.writeInt32(id);
            o.writeInt16(modifiers);
            o.skip(2);
            o.send();
        }
    }

    
    // opcode 31 - grab keyboard
    /**
     * @param pointerMode
     *                valid: {@link GrabMode.SYNCHRONOUS}, {@link GrabMode.ASYNCHRONOUS}
     * 
     * @param keyboardMode
     *                valid: {@link GrabMode.SYNCHRONOUS}, {@link GrabMode.ASYNCHRONOUS}
     * 
     * @param time
     *                possible: {@link Display#CURRENT_TIME}
     * 
     * @return valid: {@link GrabStatus.SUCCESS}, {@link GrabStatus.ALREADY_GRABBED},
     *         {@link GrabStatus.FROZEN}, {@link GrabStatus.INVALID_TIME}, {@link GrabStatus.NOT_VIEWABLE}
     * 
     * @see <a href="XGrabKeyboard.html">XGrabKeyboard</a>
     */
    public GrabStatus grabKeyboard(boolean ownerEvents, GrabMode pointerMode,
                                   GrabMode keyboardMode, int time) {

        int status;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(31, ownerEvents ? 1 : 0, 4);
            o.writeInt32(id);
            o.writeInt32(time);
            o.writeInt8(pointerMode.getCode());
            o.writeInt8(keyboardMode.getCode());
            o.skip(2);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(1);
                status = i.readInt8();
                i.skip(30);
            }
        }
        return GrabStatus.getByCode(status);
    }

    
    // opcode 33 - grab key
    /**
     * @param modifiers
     *                possible: {@link #ANY_MODIFIER}
     * @param pointerMode
     *                valid: {@link GrabMode.SYNCHRONOUS},
     *                       {@link GrabMode.ASYNCHRONOUS}
     * 
     * @param keyboardMode
     *                valid: {@link GrabMode.SYNCHRONOUS},
     *                       {@link GrabMode.ASYNCHRONOUS}
     * 
     * @see <a href="XGrabKey.html">XGrabKey</a>
     */
    public void grabKey(int keysym, int modifiers, boolean ownerEvents,
                        GrabMode pointerMode, GrabMode keyboardMode) {

        int keycode = display.getInput().keysymToKeycode(keysym);


        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(33, ownerEvents ? 1 : 0, 4);
            o.writeInt32(id);
            o.writeInt16(modifiers);
            o.writeInt8(keycode);
            o.writeInt8(pointerMode.getCode());
            o.writeInt8(keyboardMode.getCode());
            o.send();
        }
    }

    
    public static final int ANY_KEY = 0;

    
    // opcode 34 - ungrab key
    /**
     * @param key
     *                possible: {@link #ANY_KEY}
     * @param modifiers
     *                possible: {@link #ANY_MODIFIER}
     * @see <a href="XUngrabKey.html">XUngrabKey</a>
     */
    public void ungrabKey(int keysym, int modifiers) {

        int keycode = keysym == 0 ? 0 : display.getInput().keysymToKeycode(keysym);

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(34, keycode, 3);
            o.writeInt32(id);
            o.writeInt16(modifiers);
            o.skip(2);
            o.send();
        }
    }
    

    // opcode 38 - query pointer
    /**
     * @see <a href="XQueryPointer.html">XQueryPointer</a>
     */
    public PointerInfo queryPointer() {

        PointerInfo info;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(38, 0, 2);
            o.writeInt32(id);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(1);
                info = new PointerInfo(i);
                i.skip(6);
            }
        }
        return info;
    }
    

    // opcode 39 - get motion events
    /**
     * @param start
     *                possible: {@link Display#CURRENT_TIME}
     * @param stop
     *                possible: {@link Display#CURRENT_TIME}
     * @see <a href="XGetMotionEvents.html">XGetMotionEvents</a>
     */
    public TimeCoord[] getMotionEvents(int start, int stop) {

        TimeCoord[] timecoords;

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(39, 0, 4);
            o.writeInt32(id);
            o.writeInt32(start);
            o.writeInt32(stop);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(8);
                int len = i.readInt32();
                timecoords = new TimeCoord[len];
                i.skip(20);
                for (TimeCoord t : timecoords)
                    t = new TimeCoord(i);
            }
        }
        return timecoords;
    }

    
    // opcode 40 - translate coordinates
    /**
     * @see <a href="XTranslateCoordinates.html">XTranslateCoordinates</a>
     */
    public Coordinates translateCoordinates(Window src, int srcX, int srcY) {

        Coordinates coords;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(40, 0, 4);
            o.writeInt32(src.id);
            o.writeInt32(id);
            o.writeInt16(srcX);
            o.writeInt16(srcY);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(1);
                coords = new Coordinates(i);
                i.skip(16);
            }
        }
        return coords;
    }

    
    // opcode 41 - warp pointer
    /**
     * @param src
     *                possible: {@link #NONE}
     * @see <a href="XWarpPointer.html">XWarpPointer</a>
     */
    public void warpPointer(Window src, int srcX, int srcY, int srcWidth,
                             int srcHeight, int destX, int destY) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(41, 0, 6);
            o.writeInt32(src.getID());
            o.writeInt32(id);
            o.writeInt16(srcX);
            o.writeInt16(srcY);
            o.writeInt16(srcWidth);
            o.writeInt16(srcHeight);
            o.writeInt16(destX);
            o.writeInt16(destY);
            o.send();
        }
    }

    
    /**
     * @see #changeAttributes(Window.Attributes)
     * @see Attributes#setBackground(Color)
     */
    public void setBackground(Color c) {

        WindowAttributes attr = new WindowAttributes();
        attr.setBackground(c);
        changeAttributes(attr);
    }

    
    /**
     * @see #changeAttributes(Window.Attributes)
     * @see Attributes#setBackground(int)
     */
    public void setBackground(int pixel) {

        WindowAttributes attr = new WindowAttributes();
        attr.setBackground(pixel);
        changeAttributes(attr);
    }

    
    /**
     * @see #changeAttributes(Window.Attributes)
     * @see Attributes#setBackground(Pixmap)
     */
    public void setBackground(Pixmap p) {

        WindowAttributes attr = new WindowAttributes();
        attr.setBackground(p);
        changeAttributes(attr);
    }

    
    /**
     * @see #changeAttributes(Window.Attributes)
     * @see Attributes#setBorder(Color)
     */
    public void setBorder(Color c) {

        WindowAttributes attr = new WindowAttributes();
        attr.setBorder(c);
        changeAttributes(attr);
    }

    
    /**
     * @see #changeAttributes(Window.Attributes)
     * @see Attributes#setBorder(int)
     */
    public void setBorder(int pixel) {

        WindowAttributes attr = new WindowAttributes();
        attr.setBorder(pixel);
        changeAttributes(attr);
    }

    
    /**
     * @see #changeAttributes(Window.Attributes)
     * @see Attributes#setBorder(Pixmap)
     */
    public void setBorder(Pixmap p) {

        WindowAttributes attr = new WindowAttributes();
        attr.setBorder(p);
        changeAttributes(attr);
    }

    
    /**
     * @see #changeAttributes(Window.Attributes)
     * @see Attributes#setColormap(Colormap)
     */
    public void setColormap(Colormap cmap) {

        WindowAttributes attr = new WindowAttributes();
        attr.setColormap(cmap);
        changeAttributes(attr);
    }

    
    public static final Window POINTER_ROOT = new Window(1);

    // opcode 42 - set input focus
    /**
     * @param mode
     *                valid: {@link RevertTo.TO_NONE}, {@link RevertTo.TO_POINTER_ROOT},
     *                {@link RevertTo.TO_PARENT}
     * 
     * @param time
     *                possible: {@link Display#CURRENT_TIME}
     * @see <a href="XSetInputFocus.html">XSetInputFocus</a>
     */
    public void setInputFocus(RevertTo revertTo, int time) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginX11CoreRequest(X11CoreCommand.SetInputFocus, revertTo.getCode());
            o.writeInt32(id);
            o.writeInt32(time);
            o.send();
        }
    }

    
    // opcode 61 - clear area
    /**
     * @see <a href="XClearArea.html">XClearArea</a>
     */
    public void clearArea(int x, int y, int width, int height,
                           boolean exposures) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(61, exposures ? 1 : 0, 4);
            o.writeInt32(id);
            o.writeInt16(x);
            o.writeInt16(y);
            o.writeInt16(width);
            o.writeInt16(height);
            o.send();
        }
    }

    
    // opcode 83 - list installed colormaps
    /**
     * @return valid: {@link Enum#next()} of type {@link Colormap},
     *         {@link Enum#next4()}
     * 
     * @see <a href="XListInstalledColormaps.html"> XListInstalledColormaps</a>
     */
    public Colormap[] listInstalledColormaps() {

        Colormap[] maps;
        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(83, 0, 2);
            o.writeInt32(id);
            ResponseInputStream i = display.getResponseInputStream();
            synchronized (i) {
                i.readReply(o);
                i.skip(8);
                int numMaps = i.readInt16();
                maps = new Colormap[numMaps];
                i.skip(22);
                for (Colormap map : maps) {
                    int id = i.readInt32();
                    map = (Colormap) Colormap.intern(display, id);
                }
            }
        }
        return maps;
    }

    
    // opcode 114 - rotate properties
    /**
     * @see <a href="XRotateWindowProperties.html"> XRotateWindowProperties</a>
     */
    public void rotateProperties(Atom[] properties, int delta) {

        RequestOutputStream o = display.getResponseOutputStream();
        synchronized (o) {
            o.beginRequest(114, 0, 3 + properties.length);
            o.writeInt32(id);
            o.writeInt16(properties.length);
            o.writeInt16(delta);

            for (Atom atom : properties)
                o.writeInt32(atom.getID());

            o.send();
        }
    }

    
    /**
     * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
     */
    public void changeProperty(Atom property, Atom type, int data) {

        changeProperty(PropertyMode.REPLACE, property, type, 32, new int[] {
            data
        }, 0, 32);
    }

    
    /**
     * @see <a href="XClearWindow.html">XClearWindow</a>
     * @see #clearArea(int, int, int, int, boolean)
     */
    public void clear(boolean exposures) {

        clearArea(0, 0, width, height, exposures);
    }

    
    public void delete() {

        // FIXME: Re-think WM -messages.
        // if (!(wm_protocol ("WM_DELETE_WINDOW"))) return;
        //
        // ClientMessage event = new ClientMessage (display);
        // Atom wm_protocols = (Atom) Atom.intern (display, "WM_PROTOCOLS");
        // Atom wm_delete_window = (Atom) Atom.intern (display,
        // "WM_DELETE_WINDOW");
        //
        // event.set_format (32);
        // event.set_window (this);
        // event.set_type (wm_protocols);
        // event.set_wm_data (wm_delete_window.id);
        // event.set_wm_time (Display.CURRENT_TIME);
        // send_event (false, Event.NO_EVENT_MASK, event);
    }

    
    /**
     * @see #configure(Window.Changes)
     */
    public void flip() {

        Changes changes = new Changes();
        changes.stackMode(Changes.StackMode.OPPOSITE);
        configure(changes);
    }

    
    /**
     * Grab button ignoring caps lock (LOCK), num lock (MOD2), and scroll lock
     * (MOD5).
     * 
     * @see #grabButton(int, int, boolean, int, GrabMode, GrabMode, Window, Cursor)
     */
    public void grabButtonIgnoreLocks(int button, int modifiers,
                                         boolean ownerEvents, int eventMask,
                                         GrabMode pointerMode,
                                         GrabMode keyboardMode,
                                         Window confineTo, Cursor cursor) {

        // Are there a portable way to do this?
        // Sawfish and Icewm use the same technique as well.
        // TODO highly inefficient (many X requests)
        for (int i = 0; i < Input.LOCK_COMBINATIONS.length; i++)
            grabButton(button, modifiers | Input.LOCK_COMBINATIONS[i],
                        ownerEvents, eventMask, pointerMode, keyboardMode,
                        confineTo, cursor);
    }

    
    /**
     * Grab key ignoring caps lock (LOCK), num lock (MOD2), and scroll lock
     * (MOD5).
     * 
     * @see #grabKey(int, int, boolean, int, int)
     * 
     * @see #grabButtonIgnoreLocks(int, int, boolean, int, GrabMode, GrabMode,
     *  Window, Cursor)
     */
    public void grabKeyIgnoreLocks(int keysym, int modifiers,
                                      boolean ownerEvents, 
                                      GrabMode pointerMode,
                                      GrabMode keyboard_mode) {

        for (int i = 0; i < Input.LOCK_COMBINATIONS.length; i++)
            grabKey(keysym, modifiers | Input.LOCK_COMBINATIONS[i],
                     ownerEvents, pointerMode, keyboard_mode);
    }

    
    /**
     * @see <a href="XIconifyWindow.html">XIconifyWindow</a>
     * @see <a href="icccm.html#4.1.4">ICCCM Section 4.1.4</a>
     * @see #sendEvent(boolean, int, Event)
     */
    public void iconify() {
        // TODO: Imp
        // FIXME: Re-think WM -messages.
        // Atom wm_change_state = (Atom) Atom.intern (display,
        // "WM_CHANGE_STATE");
        //
        // ClientMessage event = new ClientMessage (display);
        // event.set_format (32);
        // event.set_window (this);
        // event.set_type (wm_change_state);
        // event.set_wm_data (WMHints.ICONIC);
        // send_event (false, Event.SUBSTRUCTURE_REDIRECT_MASK
        // | Event.SUBSTRUCTURE_NOTIFY_MASK, event);
    }

    
    public static Object intern(Display display, int id) {
        
        Object value = display.getResources().get(new Integer(id));
        if (value != null)
            return value;
        return new Window(display, id);
    }

    
    /**
     * @see <a href="XLowerWindow.html">XLowerWindow</a>
     * @see #configure(Window.Changes)
     */
    public void lower() {

        Changes changes = new Changes();
        changes.stackMode(Changes.StackMode.BELOW);
        configure(changes);
    }

    
    /**
     * @see <a href="XMoveWindow.html">XMoveWindow</a>
     * @see #configure(Window.Changes)
     */
    public void move() {

        Changes changes = new Changes();
        changes.setX(x);
        changes.setY(y);
        configure(changes);
    }

    
    /**
     * @see <a href="XMoveWindow.html">XMoveWindow</a>
     * @see #configure(Window.Changes)
     */
    public void move(int x, int y) {

        if (this.x == x && this.y == y)
            return;

        this.x = x;
        this.y = y;
        move();
    }

    
    /**
     * @see <a href="XMoveResizeWindow.html">XMoveResizeWindow</a>
     * @see #configure(Window.Changes)
     */
    public void moveResize() {

        move();
        resize();
    }

    
    /**
     * @see <a href="XMoveResizeWindow.html">XMoveResizeWindow</a>
     * @see #configure(Window.Changes)
     */
    public void moveResize(int x, int y, int width, int height) {

        move(x, y);
        resize(width, height);
    }

    
    /**
     * @see <a href="XMoveResizeWindow.html">XMoveResizeWindow</a>
     * @see #configure(Window.Changes)
     */
    public void moveResize(Rectangle rectangle) {

        moveResize(rectangle.getX(), rectangle.getY(), rectangle.getWidth(), 
                    rectangle.getHeight());
    }

    
    /**
     * @see Display#killClient(Resource)
     */
    public void kill() {

        display.killClient(this);
    }

    
    /**
     * @see #configure(Window.Changes)
     */
    public void raise() {

        Changes changes = new Changes();
        changes.stackMode(Changes.StackMode.ABOVE);
        configure(changes);
    }


    public Rectangle rectangle() {

        return new Rectangle(x, y, width, height);
    }

    
    /**
     * @see <a href="XResizeWindow.html">XResizeWindow</a>
     * @see #configure(Window.Changes)
     */
    public void resize() {

        Changes changes = new Changes();

        // width/height == 0 causes BAD_VALUE Error
        if (width != 0)
            changes.setWidth(width);
        if (height != 0)
            changes.setHeight(height);
        if (changes.bitmask != 0)
            configure(changes);
    }

    
    /**
     * @see <a href="XResizeWindow.html">XResizeWindow</a>
     * @see #configure(Window.Changes)
     */
    public void resize(int width, int height) {

        if (this.width == width && this.height == height)
            return;

        this.width = width;
        this.height = height;
        resize();
    }

    
    public boolean resized(Rectangle r) {
        return r.getWidth() != width || r.getHeight() != height;
    }

    
    /**
     * @deprecated Use {@link #getScreen()} instead
     */
    @Deprecated
    public Screen screen() {

        for (Screen screen : display.getScreens()) {
            if (screen.getRootID() == id)
                return screen;
        }

        return null;
    }

  
    public Screen getScreen() {

        return this.screen();
    }

  
    /**
     * @see <a href="XSelectInput.html">XSelectInput</a>
     * @see #changeAttributes(Window.Attributes)
     */
    public void selectInput(int eventMask) {

        WindowAttributes attr = new WindowAttributes();
        attr.setEventMask(eventMask);
        changeAttributes(attr);
    }

    
    /**
     * @see #setInputFocus(int, int)
     */
    public void setInputFocus() {

        setInputFocus(RevertTo.TO_POINTER_ROOT, Display.CURRENT_TIME);
    }
    

    public void setGeometryCache(Rectangle r) {

        x = r.getX();
        y = r.getY();
        width = r.getWidth();
        height = r.getHeight();
    }
    

    /**
     * A standard way to set wm class hint and name in Java.
     * 
     * @see #setWMClassHint(String, String)
     * @see #setWMName(String)
     */
    public void setWM(Object app, String topic) {

        String resClass = app.getClass().getName();
        setWMClassHint(topic, resClass);
        setWMName(topic + " - " + resClass);
    }
    

    /**
     * @see #setWMClassHint(String, String)
     */
    public void setWMClassHint(WMClassHint class_hint) {

        setWMClassHint(class_hint.resName(), class_hint.resClass());
    }

    
    /**
     * @see <a href="XSetClassHint.html">XSetClassHint</a>
     * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
     */
    public void setWMClassHint(String res_name, String res_class) {

        String wm_class = res_name + '\0' + res_class + '\0';

        changeProperty(PropertyMode.REPLACE, Atom.WM_CLASS, Atom.STRING, 8, wm_class
                .getBytes(), 0, 8);
    }

    
    /**
     * @see <a href="XSetWMHints.html">XSetWMHints</a>
     * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
     */
    public void set_wm_hints(WMHints wm_hints) {

        changeProperty(PropertyMode.REPLACE, Atom.WM_HINTS, Atom.WM_HINTS, 8,
                        wm_hints.getData(), 32, 8);
    }

    /** X window manager size hints. */
    public static class WMSizeHints extends Data {

        public WMSizeHints(Data data) {

            super(data);
        }

        // reading

        public final static int USPOSITION_MASK = 1 << 0;

        public final static int USSIZE_MASK = 1 << 1;

        public final static int PPOSITION_MASK = 1 << 2;

        public final static int PSIZE_MASK = 1 << 3;

        public final static int PMIN_SIZE_MASK = 1 << 4;

        public final static int PMAX_SIZE_MASK = 1 << 5;

        public final static int PRESIZE_INC_MASK = 1 << 6;

        public final static int PASPECT_MASK = 1 << 7;

        public final static int PBASE_SIZE_MASK = 1 << 8;

        public final static int PWIN_GRAVITY_MASK = 1 << 9;

        /**
         * @return valid: {@link #USPOSITION_MASK}, {@link #USSIZE_MASK},
         *         {@link #PPOSITION_MASK}, {@link #PSIZE_MASK},
         *         {@link #PMIN_SIZE_MASK}, {@link #PMAX_SIZE_MASK},
         *         {@link #PRESIZE_INC_MASK}, {@link #PASPECT_MASK},
         *         {@link #PBASE_SIZE_MASK}, {@link #PWIN_GRAVITY_MASK}
         */
        public int flags() {

            return read4(32);
        }

        // skip 4 paddings in July 27, 1988 draft of icccm?
        // but apps still use it, and how otherwise can we get these info?
        public boolean user_position() {

            return (flags() & USPOSITION_MASK) != 0;
        }

        public boolean user_size() {

            return (flags() & USSIZE_MASK) != 0;
        }

        public boolean program_position() {

            return (flags() & PPOSITION_MASK) != 0;
        }

        public boolean program_size() {

            return (flags() & PSIZE_MASK) != 0;
        }

        public int x() {

            return read4(36);
        }

        public int y() {

            return read4(40);
        }

        public int width() {

            return read4(44);
        }

        public int height() {

            return read4(48);
        }

        public int min_width() {

            return read4(52);
        }

        public int min_height() {

            return read4(56);
        }

        // writing

        public void x(int i) {

            write4(36, i);
        }
    }

    /**
     * @see <a href="XSetWMNormalHints.html">XSetWMNormalHints</a>
     * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
     */
    public void setWMNormalHints(WMSizeHints sizeHints) {

        changeProperty(PropertyMode.REPLACE, Atom.WM_NORMAL_HINTS,
                       Atom.WM_SIZE_HINTS, 32, sizeHints.getData(), 32, 8);
    }

    /**
     * @see <a href="XSetWMName.html">XSetWMName</a>
     * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
     */
    public void setWMName(String wmName) {

        changeProperty(PropertyMode.REPLACE, Atom.WM_NAME, Atom.STRING,
                       8, wmName.getBytes(), 0, 8); // support other types?
    }

    /** 
     * @see #setWMProtocol(String)
     */
    public void setWMDeleteWindow() {

        setWMProtocol("WM_DELETE_WINDOW");
    }

    /** 
     * @see #changeProperty(Atom, Atom, int)
     */
    public void setWMProtocol(String name) {

        Atom wmProtocols = (Atom) Atom.intern(display, "WM_PROTOCOLS");
        Atom protocol = (Atom) Atom.intern(display, name);

        changeProperty(wmProtocols, Atom.ATOM, protocol.getID());
    }

    /** 
     * @see #setWMState(int, Window)
     */
    public void setWMState(WMInitialState state) {

        setWMState(state, NONE);
    }


    /** 
     * @see #setWMState(int, Window)
     */
    public void setWMState(WMState state) {

        setWMState(state.state(), state.icon());
    }

    /** 
     * @see #change_property(int, int, Atom, Atom, int, Object, int, int)
     */
    public void setWMState(WMInitialState state, Window icon) {

        // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
        // not in the core protocol.

        Atom wmState = (Atom) Atom.intern(display, "WM_STATE");
        int[] data = {
                        state.getCode(), icon.id
        };

        changeProperty(PropertyMode.REPLACE, wmState, wmState, 32, data, 0, 32);
    }

    public String toString() {

        return "#Window " + id + " "
                + (new Rectangle(x, y, width, height)).spec();
    }

    public static final int MAX_WM_LENGTH = 1000;

    /**
     * @see <a href="XGetClassHint.html">XGetClassHint</a>
     * @see #property(boolean, Atom, Atom, int, int)
     */
    public WMClassHint wmClassHint() {

        // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
        // not in the core protocol.
        return null;
        //    PropertyReply pi = property (false, Atom.WM_CLASS, 
        //      Atom.STRING, 0, MAX_WM_LENGTH); // support other types?
        //
        //    if (pi.format () != 8 || pi.type_id () != Atom.STRING.id)
        //      return null;
        //
        //    return new WMClassHint (pi);
    }

    /**
     * @see <a href="XGetWMHints.html">XGetWMHints</a>
     * @see #property(boolean, Atom, Atom, int, int)
     */
    public WMHints wmHints() {

        // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
        // not in the core protocol.
        return null;
        //    PropertyReply pi = property (false, Atom.WM_HINTS, Atom.WM_HINTS, 0, 8);
        //    
        //    if (pi.format () != 32 || pi.type_id () != Atom.WM_HINTS.id
        //      || pi.length () != 8) return null;
        //
        //    return new WMHints (pi);
    }

    /**
     * @see <a href="XGetWMName.html">XGetWMName</a>
     * @see #property(boolean, Atom, Atom, int, int)
     */
    public String wmName() {

        Property pi = getProperty(false, Atom.WM_NAME, Atom.STRING, 0,
                                   MAX_WM_LENGTH); // support other types?

        if (pi.format() != 8 || pi.typeID() != Atom.STRING.getID())
            return null;

        return pi.stringValue();
    }

    /**
     * @see <a href="XGetWMNormalHints.html">XGetWMNormalHints</a>
     * @see #property(boolean, Atom, Atom, int, int)
     */
    public WMSizeHints wmNormalHints() {

        // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
        // not in the core protocol.
        return null;
        //    PropertyReply pi = property (false, Atom.WM_NORMAL_HINTS,
        //      Atom.WM_SIZE_HINTS, 0, 18);
        //
        //    if (pi.format () != 32 
        //      || pi.type_id () != Atom.WM_SIZE_HINTS.id
        //      || pi.length () != 18) return null;
        //   
        //    return new WMSizeHints (pi);
    }

    /**
     * @see #wmProtocols()
     */
    public boolean wmProtocol(String name) {

        Atom protocol = (Atom) Atom.intern(display, name);
        int[] list = wmProtocols();

        for (int i : list) {
            if (i == protocol.getID())
                return true;
        }

        return false;
    }

    /** 
     * @return valid:
     * {@link Enum#next()} of type {@link Atom},
     * {@link Enum#next4()}
     *
     * @see #property(boolean, Atom, Atom, int, int)
     */
    public int[] wmProtocols() {

        // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
        // not in the core protocol.
        return new int[0];
        //    Atom wm_protocols = (Atom) Atom.intern (display, "WM_PROTOCOLS");
        //    PropertyReply pi = property (false, wm_protocols, Atom.ATOM, 0,
        //      MAX_WM_LENGTH/4);
        //
        //    if (pi.byte_after () != 0)
        //      throw new RuntimeException ("Number of WM protocol exceeds " +
        //	MAX_WM_LENGTH/4); 
        //
        //    return new Enum (pi, 32, pi.length ()) {
        //      public Object next () {
        //        return Atom.intern (display, next4 ());
        //      }
        //    };
    }

    /** 
     * @see #property(boolean, Atom, Atom, int, int)
     */
    public WMState wmState() {

        // FIXME: Re-think WM -stuff. Maybe do outside of Window as this is
        // not in the core protocol.
        return null;
        //    Atom wm_state = (Atom) Atom.intern (display, "WM_STATE");    
        //    PropertyReply pi = property (false, wm_state, wm_state, 0, 2);
        //
        //    if (pi.format () != 32 
        //      || pi.type_id () != wm_state.id
        //      || pi.length () != 2) return null;
        //
        //    return new WMState (display, pi);
    }

    /**
     * @see #warpPointer(Window, int, int, int, int, int, int)
     */
    public void warpPointer(int x, int y) {

        warpPointer(NONE, 0, 0, 0, 0, x, y);
    }

    /**
     * @see #warpPointer(int, int)
     */
    public void warpPointer(Point position) {

        warpPointer(position.getX(), position.getY());
    }

    public int id() {

        return id;
    }

    
    public int getX() {

        return x;
    }
    
    
    public int getY() {

        return y;
    }
    
}
