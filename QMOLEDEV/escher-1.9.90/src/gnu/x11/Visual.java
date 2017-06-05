
package gnu.x11;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

/** X visual. */
public enum Visual {
    
    /** special value that means to copy the Visual from parents components. */
    CopyFromParent(0),

    StaticGray(0),
    GraySscale(1),
    StaticColor(2),
    PseudoColor(3),
    TrueColor(4),
    DirectColor(5);

    private static final Map<Integer, Visual> visualsByID = new HashMap<Integer, Visual>();

    static {
        
        for (Visual visual : EnumSet.allOf(Visual.class)) {
            // CopyFromParent has a special meaning
            if (visual != CopyFromParent)
                visualsByID.put(Integer.valueOf(visual.id), visual);
        }
    }

    private int id = 0;

    /**
     * @param id
     */
    Visual(int id) {

        this.id = id;
    }

    /**
     * Returns the ID associated to this enum,
     * @return
     */
    public int getID() {
        
        return this.id;
    }
    
    /**
     * Get the Visual represented by the given visualClassID or null
     * if there is not matching Visual.
     * 
     * @param visualClassID
     * @return
     */
    public static Visual getVisual(int visualClassID) {

        return visualsByID.get(Integer.valueOf(visualClassID));
    }

    // public static final String [] CLASS_STRINGS = {
    // "static-gray", "gray-scale", "static-color", "pseudo-color",
    // "true-color", "direct-color"
    // };
}
