package gnu.x11.extension;

import gnu.x11.Display;


/** Base class for X extension. */
abstract public class Extension {
  public Display display;
  public int first_event, first_error, major_opcode;
  public String name;


  protected Extension (Display display, String name, 
    String [] minor_opcode_strings) throws NotFoundException {

    this (display, name, minor_opcode_strings, 0, 0);
  }


  protected Extension (Display display, String name, 
                       String [] minor_opcode_strings, int error_count,
                       int event_count)
    throws NotFoundException {

    this.display = display;
    this.name = name;
    
    Display.ExtensionInfo er = display.query_extension (name);
    if (!er.present ()) throw new NotFoundException (name);

    first_event = er.first_event ();
    first_error = er.first_error ();
    major_opcode = er.major_opcode ();

    // register opcode strings
    display.extension_opcode_strings [major_opcode - 128] = name;
    display.extension_minor_opcode_strings [major_opcode - 128] 
      = minor_opcode_strings;

    // register error factory
    for (int i=0; i<error_count; i++)
      display.extension_error_factories [first_error - 128 + i]
        = (ErrorFactory) this;

    // register event factory
    for (int i=0; i<event_count; i++)
      display.extension_event_factories [first_event - 64 + i]
        = (EventFactory) this;
  }


  /**
   * Additional information such as client version and server version to
   * display in <code>toString()</code>.
   */
  public String more_string () {
    return "";
  }


  public String toString () {
    return "#Extension \"" + name + "\" "
      + "\n  major-opcode: " + major_opcode
      + "\n  first-event: " + first_event
      + "\n  first-error: " + first_error
      + more_string ();    
  }
}
