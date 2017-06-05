package gnu.x11;

/**
 * Enum class describing X11 Core commands.
 * 
 * Each command is associated with a Command ID (<code>opcode</code>)
 * and a <code>length</code> field.
 * 
 * The name of each field reflect the name defined in the "X Protocol Reference
 * Manual, for version X 11" manual.
 * 
 * See <a href='http://www.x.org/wiki/'>
 * http://www.x.org/wiki/</a> for details.
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public enum X11CoreCommand implements X11Command
{
    GetImage(73, 5),
    SetInputFocus(42, 3);

    private final int opcode;
    private final int length;
    
    X11CoreCommand(int opcode, int length) {
        
        this.opcode = opcode;
        this.length = length;
    }
    
//    @Override
    public int getLength()
    {
        return this.length;
    }

//    @Override
    public int getOpcode()
    {
        return this.opcode;
    }

}
