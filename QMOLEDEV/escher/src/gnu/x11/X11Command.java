/**
 * 
 */
package gnu.x11;

/**
 * Interface implement by the various X11 command enums.
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
public interface X11Command {

    /**
     * Return the opcode of this X11Command.
     */
    int getOpcode();
    
    /**
     * Return the base length of this X11Command.
     * Some X11 commands can have a variable command length, so the final length
     * is the sum of the value returned by this method and the additional
     * data that the command send.
     */
    int getLength();
}
