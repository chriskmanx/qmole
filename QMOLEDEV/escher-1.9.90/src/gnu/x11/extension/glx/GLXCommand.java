package gnu.x11.extension.glx;

import gnu.x11.X11Command;

/**
 * Enum class describing GLX and OpenGL commands.
 * 
 * Each command is associated with a Command ID (<code>opcode</code>)
 * and a <code>length</code> field.
 * 
 * The name of each field reflect the name defined in the "GLX Extensions For
 * OpenGL Protocol Specification" manual, if the extension refers to
 * a GLX specific command, instead of an OpenGL one, it is prefixed by "GLX".
 * 
 * See <a href='http://www.opengl.org/documentation/specs/'>
 * http://www.opengl.org/documentation/specs/</a> for details.
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public enum GLXCommand implements X11Command {

    // Please, try to keep the list in alphabetical order, and GLX commands
    // before OpenGL commands.
    
    /* ***** GLX Commands ***** */
    
    GLXCreateContext(3, 6),
    GLXGetVisualConfigs(14, 2),
    GLXQueryExtensionString(18, 2),
    GLXQueryServerString(19, 3),
    GLXQueryVersion(7, 3),
    GLXMakeCurrent(5, 4),
    
    /* ***** OpenGL Commands ***** */
    
    EndList(102, 2),
    Flush(143, 2),
    GenLists(104, 3),
    GetIntegerv(117, 3),
    GetLightfv(118, 4),
    GetPixelMapuiv(126, 3),
    GetString(129, 3),
    IsTexture(146, 3),
    NewList(101, 4),
    PixelStorei(110, 4),
    ReadPixels(111, 9);
    
    private final int opcode;
    private final int length;
    
    GLXCommand(int opcode, int length) {
        
        this.opcode = opcode;
        this.length = length;
    }

    // @Override
    public int getLength() {

        return this.length;
    }

    // @Override
    public int getOpcode() {

        return this.opcode;
    }
}
