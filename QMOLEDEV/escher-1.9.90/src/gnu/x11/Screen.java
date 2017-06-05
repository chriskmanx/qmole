
package gnu.x11;

/** X Screen. */
public class Screen {

    private Display display;

    private int rootID;

    private int defaultColormapID;

    private int whitePixel;

    private int blackPixel;

    private int currentInputMasks;

    private int width;

    private int height;

    private int widthInMM;

    private int heightInMM;

    private int minInstalledMaps;

    private int maxInstalledMaps;

    private int rootVisualID;

    private BackingStore backingStores;

    private boolean saveUnders;

    private int rootDepth;

    private Depth[] allowedDepths;

    public enum BackingStore {
        NEVER(0) { public String toString() { return "never"; } },
        WHEN_MAPPED(1) { public String toString() { return "when-mapped"; } },
        ALWAYS(2) { public String toString() { return "always"; }};
        
        private int code;
        
        private BackingStore(int code) {
            this.code = code;
        }
        
        public abstract String toString();
        
        public static BackingStore getCode(int code) {
            switch (code) {
                case 0:
                    return NEVER;
                case 1:
                    return WHEN_MAPPED;
                case 2:
                    return ALWAYS;
                default:
                    return NEVER;
            }
        }
        
        public int getCode() {
            return this.code;
        }
    }
    
    private GC defaultGCCache;
    
    public Screen(ResponseInputStream in, Display display) {

        this.display = display;
        
        rootID = in.readInt32();
        defaultColormapID = in.readInt32();
        whitePixel = in.readInt32();
        blackPixel = in.readInt32();
        currentInputMasks = in.readInt32();
        width = in.readInt16();
        height = in.readInt16();
        widthInMM = in.readInt16();
        heightInMM = in.readInt16();
        minInstalledMaps = in.readInt16();
        maxInstalledMaps = in.readInt16();
        rootVisualID = in.readInt32();
        backingStores = BackingStore.getCode(in.readInt8());
        saveUnders = in.readBool();
        rootDepth = in.readInt8();
        
        int num_depths = in.readInt8();
        allowedDepths = new Depth[num_depths];
        for (int i = 0; i < num_depths; i++) {
            allowedDepths[i] = new Depth(in, this);
        }
    }
    
    /** Shared, read-only resource in general. */
    public GC defaultGC() {

        if (defaultGCCache == null) {
            GC.Values gv = new GC.Values();
            gv.setForeground(blackPixel);
            gv.setBackground(whitePixel);

            defaultGCCache = new GC(display, gv);
        }

        return defaultGCCache;
    }

    public Window root() {

        return (Window) Window.intern(display, rootID);
    }

    public int rootVisualID() {

        return rootVisualID;
    }

    public int rootDepth() {

        return rootDepth;
    }

    public Colormap defaultColormap() {

        return new Colormap(display, defaultColormapID);
    }

    public Depth[] getDepths()
    {
        return this.allowedDepths;
    }

    public Display getDisplay() {
        
        return this.display;
    }
    
    public String toString() {

        return "#Screen" + "\n  root-id: " + rootID
                + "\n  default-colormap-id: " + defaultColormapID
                + "\n  white-pixel: " + whitePixel + "\n  black-pixel: "
                + blackPixel + "\n  width: " + width + "\n  height: " + height
                + "\n  width-mm: " + widthInMM + "\n  height-mm: "
                + heightInMM + "\n  min-installed-maps: "
                + minInstalledMaps + "\n  max-installed-maps: "
                + maxInstalledMaps + "\n  root-visual-id: " + rootVisualID
                + "\n  backing-stores: "
                + backingStores.toString() + "\n  save-unders: "
                + saveUnders + "\n  root-depth: " + rootDepth
                + "\n  allowed-depth-count: " + allowedDepths.length;
    }

    // Get and Sets
    
    public int getRootID() {
    
        return rootID;
    }

    
    public int getBlackPixel() {
    
        return blackPixel;
    }

    
    public int getHeight() {

        return height;
    }
    
    public int getHeightInMM() {
    
        return heightInMM;
    }

    
    public int getRootVisualID() {
    
        return rootVisualID;
    }

    
    public int getRootDepth() {
    
        return rootDepth;
    }
    
    
    public int getWhitePixel() {

        return whitePixel;
    }
    
    public int getWidth() {

        return width;
    }
    
    
    public int getWidthInMM() {

        return widthInMM;
    }
}
