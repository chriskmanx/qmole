package gnu.x11.extension.glx;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

/**
 * List all OpenGL Extension known to Escher and collect them with OpenGL
 * specific version, so that they can be enabled depending on the version of
 * OpenGL available on the server.
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public enum GLXExtension {
    
    GL_EXT_abgr(new GLXVersion(1, 1))
    
    ;

    private static final Map<GLXVersion, ArrayList<GLXExtension>> extensions 
        = new HashMap<GLXVersion, ArrayList<GLXExtension>>();
    
    static {
        for (GLXExtension extension : GLXExtension.values()) {
            extensions.get(extension.getVersion()).add(extension);
        }
    }

    private final GLXVersion version;
    GLXExtension(GLXVersion version) {
     
        this.version = version;
    }
    
    public GLXVersion getVersion() {
        return this.version;
    }
}
