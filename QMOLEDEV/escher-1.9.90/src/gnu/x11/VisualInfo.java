/**
 * 
 */

package gnu.x11;

/**
 * This Class represents an XVisualInfo.
 * 
 * Quoting from the XLib manual:
 * "A single display can support multiple screens.
 * Each screen can have several different visual types supported at
 * different depths."
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public class VisualInfo {

    protected static final int MASK = 0x01;
    
    private int id;
    private int visualClassID;
    private Visual visual;
    private int bitsPerRGBValue;
    private int colormapEntries;
    private int redMask;
    private int greenMask;
    private int blueMask;
    private int depth;
    private Screen screen = null;
    
    private int visualInfoMask = VisualInfoMask.VisualNoMask;
    
    /*
     * These are used by ColorMapper and ZPixmaps to calculate the
     * correct color shifting for different color depths.
     */
    /**
     * Contains the number of bit to shift before getting to the
     * red bit set.
     * This value is referring to the number of zero at the left
     * of the bitmask.
     */
    private int redShiftCount = 0;
    
    /**
     * Contains the number of bit to shift before getting to the
     * green bit set.
     * This value is referring to the number of zero at the left
     * of the bitmask.
     */
    private int greenShiftCount = 0;
    
    /**
     * Contains the number of bit to shift before getting to the
     * blue bit set.
     * This value is referring to the number of zero at the left
     * of the bitmask.
     */
    private int blueShiftCount = 0;
    
    /**
     * The real number of bits used by any component.
     */
    protected int redBits = 0;
    protected int greenBits = 0;
    protected int blueBits = 0;
    
    /**
     * 
     */
    public VisualInfo() {

        // TODO Auto-generated constructor stub
    }

    VisualInfo(ResponseInputStream in, Depth depth) {

        this.depth = depth.getDepth();
        this.screen = depth.getScreen();

        id = in.readInt32();
        visualClassID = in.readInt8();
        bitsPerRGBValue = in.readInt8();
        colormapEntries = in.readInt16();
        redMask = in.readInt32();
        greenMask = in.readInt32();
        blueMask = in.readInt32();

        in.skip(4);

        visual = Visual.getVisual(visualClassID);
        
        initVisualShiftCount();
    }

    private void initVisualShiftCount() {
        
        // red           
        int red = getRedMask();            
        if(red > 0) {
            while ((red & MASK) != MASK) {
                red = (red >> 1);
                redShiftCount++;
            }
        }
                    
        // green
        int green = getGreenMask();           
        if (green > 0) {
            while ((green & MASK) != MASK) {
                green = (green >> 1);
                greenShiftCount++;
            }
        }
            
      
        // blue
        int blue = getBlueMask();           
        if (blue > 0) {
            while ((blue & MASK) != MASK) {
                blue = (blue >> 1);
                blueShiftCount++;
            }
        }
        
        redBits = Integer.bitCount(getRedMask());
        greenBits = Integer.bitCount(getGreenMask());
        blueBits = Integer.bitCount(getBlueMask());
    }
    
    /**
     * @param id the id to set
     */
    public void setID(int id) {
    
        this.id = id;
    }
 
    /**
     * @param visual the visual to set
     */
    public void setVisualClass(Visual visual) {
    
        this.visual = visual;
    }

    /**
     * @param bitsPerRGBValue the bitsPerRGBValue to set
     */
    public void setBitsPerRGBValue(int bitsPerRGBValue) {
    
        this.bitsPerRGBValue = bitsPerRGBValue;
    }

    /**
     * @param colormapEntries the colormapEntries to set
     */
    public void setColormapEntries(int colormapEntries) {
    
        this.colormapEntries = colormapEntries;
    }
    
    /**
     * @param redMask the redMask to set
     */
    public void setRedMask(int redMask) {
    
        this.redMask = redMask;
    }
    
    /**
     * @param greenMask the greenMask to set
     */
    public void setGreenMask(int greenMask) {
    
        this.greenMask = greenMask;
    }

    /**
     * @param blueMask the blueMask to set
     */
    public void setBlueMask(int blueMask) {
    
        this.blueMask = blueMask;
    }

    /**
     * @param depth the depth to set
     */
    public void setDepth(int depth) {
    
        this.depth = depth;
    }

    public void setVisualInfoMask(int visualInfoMask) {
        this.visualInfoMask = visualInfoMask;
    }
    
    /**
     * @param screen the screen to set
     */
    public void setScreen(Screen screen) {
    
        this.screen = screen;
    }

    public int getID() {

        return id;
    }

    public Visual getVisualClass() {

        return visual;
    }

    public int getBitsPerRGBValue() {

        return bitsPerRGBValue;
    }

    public int getColormapEntries() {

        return colormapEntries;
    }

    public int getRedMask() {

        return redMask;
    }

    public int getGreenMask() {

        return greenMask;
    }

    public int getBlueMask() {

        return blueMask;
    }

    public int getDepth() {
        
        return this.depth;
    }
    
    public Screen getScreen() {
        
        return this.screen;
    }
    
    
    public int getVisualInfoMask() {
        return this.visualInfoMask;
    }
    
    public String toString() {

        return "#Visual" + "\n  id: " + id + "\n  class: " + visual
                + "\n  bits-per-rgb-value: " + bitsPerRGBValue
                + "\n  colormap-entries: " + colormapEntries
                + "\n  red-mask: 0x" + Integer.toHexString(redMask)
                + "\n  green-mask: 0x" + Integer.toHexString(greenMask)
                + "\n  blue-mask: 0x" + Integer.toHexString(blueMask);
    }

    
    /**
     * @return the redShiftCount
     */
    public int getRedShiftCount() {
    
        return redShiftCount;
    }

    
    /**
     * @return the greenShiftCount
     */
    public int getGreenShiftCount() {
    
        return greenShiftCount;
    }

    
    /**
     * @return the blueShiftCount
     */
    public int getBlueShiftCount() {
    
        return blueShiftCount;
    }

    
    /**
     * @return the redBits
     */
    public int getRedBits() {
    
        return redBits;
    }

    
    /**
     * @return the greenBits
     */
    public int getGreenBits() {
    
        return greenBits;
    }

    
    /**
     * @return the blueBits
     */
    public int getBlueBits() {
    
        return blueBits;
    }
}
