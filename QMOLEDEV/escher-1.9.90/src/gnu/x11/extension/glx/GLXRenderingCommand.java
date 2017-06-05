package gnu.x11.extension.glx;

import gnu.x11.X11Command;

/**
 * Enum class describing GLX Rendering commands.
 * 
 * Each command is associated with a Command ID (<code>opcode</code>)
 * and a <code>length</code> field.
 * 
 * The name of each field reflect the name defined in the "GLX Extensions For
 * OpenGL Protocol Specification" manual.
 * 
 * See <a href='http://www.opengl.org/documentation/specs/'>
 * http://www.opengl.org/documentation/specs/</a> for details.
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public enum GLXRenderingCommand implements X11Command {
    
    // Please, try to keep the list in alphabetical order.
    // NOTE: The first argument is length, the second argument is opcode
    // this reflect the GLX specification for rendering commands, where the
    // GLX command opcode is the minor opcode and comes after the length in
    // the request stream.
    
    Begin(8, 4),
    CallList(8, 1),
    Clear(8, 127),
    ClearColor(20, 130),
    ClearDepth(12, 132),
    Color3fv(16, 8),
    DepthMask(8, 135),
    Enable(8, 139),
    End(4, 23),
    Frustum(52, 175),
    LoadIdentity(4, 176),
    Materialfv(12, 97),
    MatrixMode(8, 179),
    MultMatrixd(132, 181),
    Normal3fv(16, 30),
    Normal3iv(16, 31),
    Ortho(52, 182),
    PopMatrix(4, 183),
    PushMatrix(4, 184),
    ShadeModel(8, 104),
    ReadBuffer(8, 171),
    Rotatef(20, 186),
    Translatef(16, 190),
    Vertex3fv(16, 70),
    Viewport(20, 191);
    
    private final int opcode;
    private final int length;
    
    GLXRenderingCommand(int length, int opcode)
    {
        this.opcode = opcode;
        this.length = length;
    }

    //@Override
    public int getLength() {
        return this.length;
    }

    //@Override
    public int getOpcode() {

        return this.opcode;
    }
}
