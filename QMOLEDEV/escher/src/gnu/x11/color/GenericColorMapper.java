package gnu.x11.color;

import gnu.x11.VisualInfo;
import gnu.x11.image.ZPixmap;

/**
 * Implements default conversion from ARGB to 16 bits RGB.
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public class GenericColorMapper extends ColorMapper {
     
    @Override
    public int convertToNativePixel(int pixel, VisualInfo visual) {

        int redBits = 24 - (visual.getRedBits() + visual.getRedShiftCount());
        int greenBits =
            16 - (visual.getGreenBits() + visual.getGreenShiftCount());
        int blueBits = 8 - (visual.getBlueBits() + visual.getBlueShiftCount());
            
        int r, g, b;
        
        r = (pixel >>> redBits) & visual.getRedMask();
        g = (pixel >>> greenBits) & visual.getGreenMask();
        b = (pixel >>> blueBits) & visual.getBlueMask();
        
        return (0xff000000 | r | g | b);
    }

    @Override
    public int convertToTargetPixel(int pixel, VisualInfo visual) {

        throw new UnsupportedOperationException("Not implemented yet");
    }

}
