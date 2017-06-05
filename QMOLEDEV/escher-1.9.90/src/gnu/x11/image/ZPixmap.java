
package gnu.x11.image;

import gnu.x11.Color;
import gnu.x11.Display;
import gnu.x11.EscherUnsupportedScreenBitDepthException;
import gnu.x11.Pixmap;
import gnu.x11.RGB;
import gnu.x11.VisualInfo;
import gnu.x11.color.ColorMapper;
import gnu.x11.image.Image.Format;

public class ZPixmap extends Image {

    protected int imageByteOrder;

    protected VisualInfo xVisual = null;

    protected Display display;

    protected ZPixmapDelegate delegate = null;

    /**
     * Creates a ZPixmap using the specified Visual.
     * 
     * @param display
     * @param xVisual
     * @throws EscherUnsupportedScreenBitDepthException
     */
    ZPixmap(Display display, VisualInfo xVisual)
            throws EscherUnsupportedScreenBitDepthException {

        super(Format.ZPIXMAP, display.getDefaultPixmapFormat());

        imageByteOrder = display.getImageByteOrder();
        this.xVisual = xVisual;
        this.delegate = getDelegate(display.getDefaultPixmapFormat().getDepth());
    }

    /**
     * Creates a ZPixmap with the specified size and format, and initializes the
     * underlying data array with an existing one.
     * 
     * @param display
     *                the
     * @param width
     * @param height
     * @param format
     * @param data
     * @throws EscherUnsupportedScreenBitDepthException 
     * @throws UnsupportedBitDepthException
     */
    public ZPixmap(Display display, int width, int height,
                   Pixmap.Format format, byte[] data, VisualInfo xVisual)
            throws EscherUnsupportedScreenBitDepthException {

        this(display, width, height, format, xVisual);
        this.data = data;
    }
    
    /**
     * Creates a new ZPixmap with the specified width and height and with the
     * default pixmap format from the display.
     * 
     * @param display
     *                the display on which to create the pixmap
     * @param width
     *                the width in pixels
     * @param height
     *                the height in pixels
     * @throws EscherUnsupportedScreenBitDepthException
     * @throws UnsupportedBitDepthException
     */
    public ZPixmap(Display display, int width, int height, VisualInfo xVisual)
            throws EscherUnsupportedScreenBitDepthException {
        
        this(display, width, height, display.getDefaultPixmapFormat(), xVisual);
    }

    public ZPixmap(Display display, int width, int height,
                   Pixmap.Format format, VisualInfo xVisual)
            throws EscherUnsupportedScreenBitDepthException {

        super(width, height, Format.ZPIXMAP, format);

        imageByteOrder = display.getImageByteOrder();
        this.xVisual = xVisual;
        this.delegate = getDelegate(format.getDepth());
    }

    private ZPixmapDelegate getDelegate(int bitDepth)
            throws EscherUnsupportedScreenBitDepthException {

        switch (bitDepth) {
        case 24:
            return new ZPixmap24();

        case 16:
            return new ZPixmap16();

        case 8:
            return new ZPixmap8();
            
        default:
            throw new EscherUnsupportedScreenBitDepthException("Unsupported "
                    + "Screen Depth for creating Pixmaps: " + bitDepth);
        }

    }

    /**
     * Puts the image data into this image. This data must be in the same format
     * as specified in this image.
     * 
     * @param imageData the data to set
     */
    public void setData(int[] imageData) {

        int pixelByteCount = pixmapFormat.getBitsPerPixel() / 8;
        int len = pixelByteCount * width * height;
        len = Math.min(len, data.length);
        System.arraycopy(imageData, 0, data, 0, len);
    }

    /**
     * Sets a data element in the ZPixmap directly.
     * 
     * This manipulates the underlying data directly. It is recommended to use
     * one of the other accessor methods instead.
     * 
     * @param index
     *                the index of the data element to set
     * @param val
     *                the value
     */
    public void setDataElement(int index, byte val) {

        data[index] = val;
    }

    /**
     * Returns the data element at the specified index.
     * 
     * This manipulates the underlying data directly. It is recommended to use
     * one of the other accessor methods instead.
     * 
     * @param index
     *                the index of the data element
     * 
     * @return the data element at the specified index
     */
    public byte getDataElement(int index) {

        return data[index];
    }

    /**
     * Returns the length of the underlying data array.
     * 
     * @return the length of the underlying data array
     */
    public int getDataLength() {

        return data.length;
    }

    public void putPixel(int x, int y, int pixel) {

        delegate.putPixel(x, y, pixel);
    }

    @Deprecated
    public void putPixel(int x, int y, Color color) {

        delegate.putPixel(x, y, color.getPixel());
    }

    public void putRGB(int x, int y, int r, int g, int b) {

        int pixel = delegate.getPixelFromRGB(r, g, b);
        delegate.putPixel(x, y, pixel);
    }
    
    public void putRGB(int x, int y, RGB rgb) {

        int pixel = delegate.getPixelFromRGB(rgb.getRed(), rgb.getGreen(), rgb.getBlue());
        delegate.putPixel(x, y, pixel);
    }

    public int getPixel(int x, int y) {
        
        return delegate.getPixel(x, y);
    }
    
    public RGB getRGB(int x, int y) {
        
        int pixel = getPixel(x, y);
        return delegate.getRGBFromPixel(pixel);
    }
    
    /* ***** delegate classes ***** */

    abstract class ZPixmapDelegate {

        protected static final int MASK = 0x01;
                       
        /**
         * How many bits we have to shift to get the real number of
         * bits for a give bit depth in one byte.
         */
        protected int redScaleShift = 0;
        protected int greenScaleShift = 0;
        protected int blueScaleShift = 0;
        
        public ZPixmapDelegate() {

            this.redScaleShift = 8 - ZPixmap.this.xVisual.getRedBits();
            this.greenScaleShift = 8 - ZPixmap.this.xVisual.getGreenBits();
            this.blueScaleShift = 8 - ZPixmap.this.xVisual.getBlueBits();
        }
        
        public void putPixel(int x, int y, int pixel) {

            int bytesPerPixel = getBytesPerPixel();            
            int i = (y * getLineByteCount()) + (x * bytesPerPixel);
            
            // outside for loop for speed
            if (imageByteOrder == LSB_FIRST) {
                for (int j = 0; j < bytesPerPixel; j++)
                    data[i + j] = (byte) (0xff & (pixel >> (j * 8)));
            } else {
                // MSB_FIRST
                for (int j = 0; j < bytesPerPixel; j++)
                    data[i + j] = 
                        (byte) (0xff & (pixel >> (bytesPerPixel - 1 - j) * 8));
            }
        }
     
        public int getPixel(int x, int y) {

            int pixel = 0;
            int bytesPerPixel = getBytesPerPixel();

            int i = (y * getLineByteCount()) + (x * bytesPerPixel);
            
            // outside for loop for speed
            if (imageByteOrder == LSB_FIRST) {
                for (int j = 0; j < bytesPerPixel; j++) {
                    pixel |=  (((int) data[i + j]) & 0xff) << (j * 8);
                }
            } else {
                // MSB_FIRST
                for (int j = 0; j < bytesPerPixel; j++) {
                    pixel |= (((int) data[i + j]) & 0xff)
                                << ((bytesPerPixel - 1 - j) * 8);
                }
            }

            return pixel;
        }
        
        public RGB getRGBFromPixel(int pixel) {
            
            int red = (pixel & ZPixmap.this.xVisual.getRedMask()) >>>
                    ZPixmap.this.xVisual.getRedShiftCount();
                
            int green = (pixel & ZPixmap.this.xVisual.getGreenMask()) >>>
                    ZPixmap.this.xVisual.getGreenShiftCount();
                
            int blue = (pixel & ZPixmap.this.xVisual.getBlueMask()) >>>
                    ZPixmap.this.xVisual.getBlueShiftCount();
        
            return new RGB(red, green, blue);
        }
        
        abstract public int getPixelFromRGB(int r, int g, int b);        
        abstract public int getBytesPerPixel();
    }

    /**
     * Implements handling of 24 bit images in ZPixmap format.
     * 
     * @author Mario Torre <neugens@aics.com>
     */
    class ZPixmap24 extends ZPixmapDelegate {
  
        @Override
        public int getPixelFromRGB(int r, int g, int b) {

            return (r <<  ZPixmap.this.xVisual.getRedShiftCount())
                    | (g << ZPixmap.this.xVisual.getGreenShiftCount())
                    | (b << ZPixmap.this.xVisual.getBlueShiftCount());
        }
        
        @Override
        public int getBytesPerPixel() {
          
            return pixmapFormat.getBitsPerPixel() / 8;
        }
    }

    /**
     * Implements handling of 16 bit images in ZPixmap format.
     * 
     * @author Mario Torre <neugens@aics.com>
     */
    class ZPixmap16 extends ZPixmapDelegate {
   
        @Override
        public RGB getRGBFromPixel(int pixel) {
         
            RGB rgb = super.getRGBFromPixel(pixel);
            
            rgb.setRed(rgb.getRed() << this.redScaleShift);
            rgb.setGreen(rgb.getGreen() << this.greenScaleShift);
            rgb.setBlue(rgb.getBlue() << this.blueScaleShift);
            
            return rgb;
        }
        
        public int getPixelFromRGB(int r, int g, int b) {
             
            int red = (r >>> this.redScaleShift) <<
                ZPixmap.this.xVisual.getRedShiftCount();
            
            int green = (g >>> this.greenScaleShift) <<
                ZPixmap.this.xVisual.getGreenShiftCount();
            
            int blue = (b >>> this.blueScaleShift) <<
                ZPixmap.this.xVisual.getBlueShiftCount();
            
            return (red | green | blue);
        }
        
        @Override
        public int getBytesPerPixel() {

            return 2;
        }
    }
    
    /**
     * Implements handling of 8 bit images in ZPixmap format.
     * 
     * @author Mario Torre <neugens@aics.com>
     */
    class ZPixmap8 extends ZPixmapDelegate {

        public RGB getRGBFromPixel(int pixel) {
            
            throw new UnsupportedOperationException("Not yet implemented");
        }
        
        public int getPixelFromRGB(int r, int g, int b) {

            throw new UnsupportedOperationException("Not yet implemented");
        }
        
        @Override
        public int getBytesPerPixel() {

            return 1;
        }
    }
}
