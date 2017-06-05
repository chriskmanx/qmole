package gnu.x11.extension;

import gnu.x11.Display;


/** Base class for X extension. */
abstract public class Extension {
  protected Display display;
  protected int firstEvent, firstError, majorOpcode;
  protected String name;


  protected Extension (Display display, String name, 
   String[] minorOpcodeStrings) throws NotFoundException {

    this (display, name, minorOpcodeStrings, 0, 0);
  }


  protected Extension (Display display, String name, String[] minorOpcodeStrings,
                       int errorCount, int eventCount) throws NotFoundException {
    this.display = display;
    this.name = name;
    
    Display.ExtensionInfo er = display.queryExtension (name);
    if (!er.present ()) throw new NotFoundException (name);

    firstEvent = er.firstEvent ();
    firstError = er.firstError ();
    majorOpcode = er.majorOpcode ();

    // register opcode strings
    display.extensionOpcodeStrings [majorOpcode - 128] = name;
    display.extensionMinorOpcodeStrings [majorOpcode - 128] 
      = minorOpcodeStrings;

    // register error factory
    for (int i=0; i<errorCount; i++)
      display.extensionErrorFactories [firstError - 128 + i]
        = (ErrorFactory) this;

    // register event factory
    for (int i=0; i<eventCount; i++)
      display.extensionEventFactories [firstEvent - 64 + i]
        = (EventFactory) this;
  }


  /**
   * Additional information such as client version and server version to
   * display in <code>toString()</code>.
   */
  public String moreString () {
    return "";
  }


  public String toString () {
    return "#Extension \"" + name + "\" "
      + "\n  major-opcode: " + majorOpcode
      + "\n  first-event: " + firstEvent
      + "\n  first-error: " + firstError
      + moreString ();    
  }
  
  
  public Display getDisplay() {
    return display;
  }


  public int getMajorOpcode() {
    return majorOpcode;
  }

}
