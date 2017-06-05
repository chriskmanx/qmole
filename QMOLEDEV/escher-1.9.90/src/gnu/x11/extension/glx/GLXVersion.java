package gnu.x11.extension.glx;


public class GLXVersion {

    /** The major version number of the OpenGL X11 implementation. */
    public final int major;
    
    /** The minor version number of the OpenGL X11 implementation. */
    public final int minor;
    
    /** package protected */
    GLXVersion(int major, int minor) {

        this.major = major;
        this.minor = minor;
    }
    
    /**
     * Return a String of the form
     * {@code this.major + "." + this.minor}
     */
    @Override
    public String toString() {
    
        return this.major + "." + this.minor;
    }
}
