
package gnu.x11;

/** X depth. */
public class Depth {

    private int depth;
    private VisualInfo[] visualTypes;
    
    private Screen screen = null;
    
    public Depth(ResponseInputStream in, Screen screen) {

        this.screen = screen;
        
        depth = in.readInt8();
        in.skip(1);
        int visualCount = in.readInt16();
        in.skip(4);

        visualTypes = new VisualInfo[visualCount];
        
        for (int i = 0; i < visualCount; i++) {
            visualTypes[i] = new VisualInfo(in, this);
            screen.getDisplay().addVisual(visualTypes[i]);
        }
    }

    public int getDepth() {

        return this.depth;
    }

    public Screen getScreen() {
        
        return this.screen;
    }
    
    public VisualInfo[] getVisuals() {
        
        return this.visualTypes;
    }
     
    public String toString() {

        return "#Depth" + "\n  depth: " + depth + "\n  visual-count: "
                + visualTypes.length;
    }
}
