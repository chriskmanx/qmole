package gnu.x11.extension;

import gnu.x11.Data;
import gnu.x11.Display;
import gnu.x11.Error;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;
import gnu.x11.Window;


/**
 * X Print Service Extension. The specification can be found <a href=
 * "http://escher.sourceforge.net/etc/specification/print-library.ps.gz"
 * >here</a> (<a href=
 * "http://escher.sourceforge.net/etc/specification/print-protocol.ps.gz"
 * >protocol</a>; <a href=
 * "http://escher.sourceforge.net/etc/specification/print-server.ps.gz"
 * >server</a>).
 */
public class Print extends gnu.x11.extension.Extension 
  implements gnu.x11.extension.ErrorFactory,
  gnu.x11.extension.EventFactory {

  public static final String [] MINOR_OPCODE_STRINGS = {
    "QueryVersion",             // 0
    "GetPrinterList",           // 1
    "CreateContext",            // 2
    "SetContext",               // 3
    "GetContext",               // 4
    "DestroyContext",           // 5
    "GetContextScreen",         // 6
    "StartJob",                 // 7
    "EndJob",                   // 8
    "StartDoc",                 // 9
    "EndDoc",                   // 10
    "PutDocumentData",          // 11
    "GetDocumentData",          // 12
    "StartPage",                // 13
    "EndPage",                  // 14
    "SelectInput",              // 15
    "InputSelected",            // 16
    "GetAttributes",            // 17
    "SetAttributes",            // 18
    "GetOneAttribute",          // 19
    "RehashPrinterList",        // 20
    "GetPageDimensions",        // 21
    "QueryScreens",             // 22
    "SetImageResolution",       // 23
    "GetImageResolution",       // 24
  };


  public static final int CLIENT_MAJOR_VERSION = 1;
  public static final int CLIENT_MINOR_VERSION = 0;


  public String locale = "";
  public int server_major_version, server_minor_version;


  public class Context extends gnu.x11.Resource {
    /** Intern. */
    Context (int id) {
      super (Print.this.display, id);
    }
  
    
    // print 2 - create context
    /**
     * @see <a href="XpCreateContext.html">XpCreateContext</a>
     */
    public Context (String name) {
      super (Print.this.display);

      int len = 4 + Data.unit (name) + Data.unit (locale);
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 2, len);
        o.write_int32 (id);
        o.write_int32 (name.length ());
        o.write_int32 (locale.length ());
        o.write_string8 (name);
        o.write_string8 (locale);
        o.send ();
      }
    }
  
  
    // print 3 - set context
    /**
     * @see <a href="XpSetContext.html">XpSetContext</a>
     */
    public void set () {
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 3, 2);
        o.write_int32 (id);
        o.send ();
      }
    }
  
  
    // print 5 - destroy context
    /**
     * @see <a href="XpDestroyContext.html">XpDestroyContext</a>
     */
    public void destroy () {
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 5, 2);
        o.write_int32 (id);
        o.send ();
      }
    }
  
  
    // print 6 - get screen of context
    /**
     * @see <a href="XpGetScreenOfContext.html">XpGetScreenOfContext</a>
     */
    public Window screen () {
      int root_id;
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 6, 2);
        // error in spec: missing context field
        o.write_int32 (id);
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.read_reply (o);
          i.skip (8);
          root_id = i.read_int32 ();
          i.skip (16);
        }
      }
      return (Window) Window.intern (this.display, root_id);
    }
  
  
    // print 15 - select input
    /**
     * @see <a href="XpSelectInput.html">XpSelectInput</a>
     */
    public void select_input (int event_mask) {
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 15, 3);
        o.write_int32 (id);
        o.write_int32 (event_mask);
        o.send ();
      }
    }
  
  
    /** Reply of {@link #input_selected(int)}. */
    public class InputSelectedInfo {
      public int event_mask;
      public int all_event_masks;
      InputSelectedInfo (ResponseInputStream i) {
        event_mask = i.read_int32 ();
        all_event_masks = i.read_int32 ();
      }
      public int event_mask () { return event_mask; }
      public int all_events_mask () { return all_event_masks; }
    }
  
  
    // print 16 - input selected
    /**
     * @see <a href="XpInputSelected.html">XpInputSelected</a>
     */
    public InputSelectedInfo input_selected (int event_mask) {

      InputSelectedInfo info;
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 16, 2);
        o.write_int32 (id);
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.read_reply (o);
          i.skip (8);
          info = new InputSelectedInfo (i);
          i.skip (16);
        }
      }
      return info;
    }
  
  
    public static final int JOB_ATTRIBUTE_POOL = 1;
    public static final int DOC_ATTRIBUTE_POOL = 2;
    public static final int PAGE_ATTRIBUTE_POOL = 3;
    public static final int PRINTER_ATTRIBUTE_POOL = 4;
    public static final int SERVER_ATTRIBUTE_POOL = 5;
    public static final int MEDIUM_ATTRIBUTE_POOL = 6;
    public static final int SPOOLER_ATTRIBUTE_POOL = 7;
  
  
    // print 17 - get attributes
    /**
     * @param pool valid:
     * {@link #JOB_ATTRIBUTE_POOL},
     * {@link #DOC_ATTRIBUTE_POOL},
     * {@link #PAGE_ATTRIBUTE_POOL},
     * {@link #PRINTER_ATTRIBUTE_POOL},
     * {@link #SERVER_ATTRIBUTE_POOL},
     * {@link #MEDIUM_ATTRIBUTE_POOL},
     * {@link #SPOOLER_ATTRIBUTE_POOL}
     *
     * @see <a href="XpGetAttributes.html">XpGetAttributes</a>
     */
    public String attributes (int pool) {

      String atts;
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 17, 3);
        o.write_int32 (id);
        o.write_int8 (pool);
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.read_reply (o);
          i.skip (8);
          int strlen = i.read_int32 ();
          i.skip (20);
          atts = i.read_string8 (strlen);
        }
      }
      return atts;
    }   
  
  
    public static final int ATTRIBUTE_REPLACE = 1;
    public static final int ATTRIBUTE_MERGE = 2;
  
  
    // print 18 - set attributes
    /**
     * @param pool valid:
     * {@link #JOB_ATTRIBUTE_POOL},
     * {@link #DOC_ATTRIBUTE_POOL},
     * {@link #PAGE_ATTRIBUTE_POOL},
     * {@link #PRINTER_ATTRIBUTE_POOL},
     * {@link #SERVER_ATTRIBUTE_POOL}
     *
     * @see <a href="XpSetAttributes.html">XpSetAttributes</a>
     */
    public void set_attributes (int pool, int rule, String attributes) {
      int len = 4 + Data.unit (attributes);
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 18, len);
        o.write_int32 (id);
        o.write_int32 (attributes.length ());
        o.write_int8 (pool);
        o.write_int8 (rule);
        o.skip (2);
        o.write_string8 (attributes);
        o.send ();
      }
    };   
  
  
    // print 19 - get one attribute
    /**
     * @param pool valid:
     * {@link #JOB_ATTRIBUTE_POOL},
     * {@link #DOC_ATTRIBUTE_POOL},
     * {@link #PAGE_ATTRIBUTE_POOL},
     * {@link #PRINTER_ATTRIBUTE_POOL},
     * {@link #SERVER_ATTRIBUTE_POOL}
     *
     * @param rule valid:
     * {@link #ATTRIBUTE_REPLACE},
     * {@link #ATTRIBUTE_MERGE}
     *
     * @see <a href="XpGetOneAttribute.html">XpGetOneAttribute</a>
     */
    public String one_attribute (int pool, String name) {
      String attr;
      int len = 4 + Data.unit (name);
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 19, len);
        o.write_int32 (id);
        o.write_int32 (name.length ());
        o.write_int8 (pool);
        o.skip (3);
        o.write_string8 (name);
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.read_reply (o);
          i.skip (8);
          int strlen = i.read_int32 ();
          i.skip (20);
          attr = i.read_string8 (strlen);
          i.skip (RequestOutputStream.pad (strlen));
        }
      }
      return attr;
    }
  
  
    /** Reply of {@link #page_dimensions()}. */  
    public class PageDimensions {
      public int width;
      public int height;
      public int offset_x;
      public int offset_y;
      public int reproducible_width;
      public int reproducible_height;
      PageDimensions (ResponseInputStream i) {
        width = i.read_int16 ();
        height = i.read_int16 ();
        offset_x = i.read_int16 ();
        offset_y = i.read_int16 ();
        reproducible_width = i.read_int16 ();
        reproducible_height = i.read_int16 ();
      }

      public int width () { return width; }
      public int height () { return height; }
      public int offset_x () { return offset_x; }
      public int offset_y () { return offset_y; }
      public int reproducible_x () { return reproducible_width; }
      public int reproducible_y () { return reproducible_height; }
  
  
      public String toString () {
        return "#PageDimensionsReply"
          + "\nwidth: " + width ()
          + "\nheight: " + height ()
          + "\noffset-x: " + offset_x ()
          + "\noffset-x: " + offset_y ()
          + "\nreproducible-x: " + reproducible_x ()
          + "\nreproducible-y: " + reproducible_y ();
      }
    }
  
  
    /** Reply of {@link #page_dimensions()}. */  
    public class SetImageResolutionInfo {
      public boolean status;
      public int previous_resolution;
      SetImageResolutionInfo (ResponseInputStream i) {
        status = i.read_bool ();
        i.skip (6);
        previous_resolution = i.read_int16 ();
      }
      public boolean status () { return status; }
      public int previous_resolution () { return previous_resolution; }
    }
  
  
    // print 21 - get page dimensions
    /**
     * @see <a href="XpGetPageDimensions.html">XpGetPageDimensions</a>
     */
    public PageDimensions page_dimensions () {
      PageDimensions dim;
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 21, 2);
        o.write_int32 (id);
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.read_reply (o);
          i.skip (8);
          dim = new PageDimensions (i);
          i.skip (12);
        }
      }
      return dim;
    }
  
  
    // print 23 - set image resolution
    /**
     * @see <a href="XpSetImageResolution.html">XpSetImageResolution</a>
     */
    public SetImageResolutionInfo set_image_resolution (int resolution) {

      SetImageResolutionInfo info;
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 23, 3);
        o.write_int32 (id);
        o.write_int16 (resolution);
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.skip (1);
          info = new SetImageResolutionInfo (i);
          i.skip (22);
        }
      }
      return info;
    }
      
  
    // print 24 - get image resolution
    /**
     * @see <a href="XpGetImageResolution.html">XpGetImageResolution</a>
     */
    public int image_resolution () {

      int res;
      RequestOutputStream o = display.out;
      synchronized (o) {
        o.begin_request (major_opcode, 24, 2);
        o.write_int32 (id);
        ResponseInputStream i = display.in;
        synchronized (i) {
          i.skip (8);
          res = i.read_int16 ();
          i.skip (22);
        }
      }
      return res;
    }
      
  
    /**
     * All possible attributes.
     *
     * @see #attributes(int)
     */
    public String attributes () {
      return attributes (JOB_ATTRIBUTE_POOL)
        + attributes (DOC_ATTRIBUTE_POOL)
        + attributes (PAGE_ATTRIBUTE_POOL)
        + attributes (PRINTER_ATTRIBUTE_POOL)
        + attributes (SERVER_ATTRIBUTE_POOL);
    }
  
  
    public String toString () {
      return "#Context\n" 
        + attributes ();
    }
  }
  
  
    // print opcode 0 - query version
  /**
   * @see <a href="XpQueryVersion.html">XpQueryVersion</a>
   */
  public Print (Display display) 
    throws gnu.x11.extension.NotFoundException { 

    super (display, "XpExtension", MINOR_OPCODE_STRINGS, 2, 2);

    // check version before any other operations
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 0, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        server_major_version = i.read_int16 ();
        server_minor_version = i.read_int16 ();
        i.skip (20);
      }
    }
  }


  /** Reply of {@link #printer_list(String)}. */
  public class Printer {

    public String name;
    public String desc;
    Printer (ResponseInputStream i) {
      int name_len = i.read_int32 ();
      name = i.read_string8 (name_len);
      i.skip (RequestOutputStream.pad (name_len));
      int desc_len = i.read_int32 ();
      desc = i.read_string8 (desc_len);
      i.skip (RequestOutputStream.pad (desc_len));
    }

    public String name () { return name; }
    public String description () {
      return desc;
    }


    /**
     * @see #create_context(String)
     */
    public Context context () {
      return create_context (name ());
    }

    public String toString () {
      return "#Printer"
        + "\nprinter-name: " + name ()
        + "\nprinter-description: "  + description ();
    }
  }

  
  // print opcode 1 - get printer list
  /**
   * @return valid: {@link Enum#next()}
   * @see <a href="XpGetPrinterList.html">XpGetPrinterList</a>
   */
  public Printer [] printer_list (String name) {

    Printer [] printers;
    int len = 3 + Data.unit (name) + Data.unit (locale);
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request(major_opcode, 1, len);
      o.write_int32 (name.length ());
      o.write_int32 (locale.length ());
      o.write_string8 (name);
      o.write_string8 (locale);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.skip (8);
        int num = i.read_int32 ();
        printers = new Printer [num];
        for (int j = 0; j < num; j++) {
          printers [j] = new Printer (i);
        }
      }
    }
    return printers;
  }


  // print 4 - get context
  /**
   * @see <a href="XpGetContext.html">XpGetContext</a>
   */
  public Context context () {
    int id;
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 4, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        id = i.read_int32 ();
        i.skip (16);
      }
    }
    return new Context (id);
  }


  public static final int SPOOL = 1;
  public static final int GET_DATA = 2;


  // print 7 - start job
  /**
   * @param output_mode valid:
   * {@link #SPOOL},
   * {@link #GET_DATA}
   *
   * @see <a href="XpStartJob.html">XpStartJob</a>
   */
  public void start_job (int output_mode) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 7, 2);
      o.write_int8 (output_mode);
      o.send ();
    }
  }


  // print 8 - end job
  /**
   * @see <a href="XpEndJob.html">XpEndJob</a>
   */
  public void end_job (boolean cancel) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 8, 2);
      o.write_bool (cancel);
      o.send ();
    }
  }


  // print 9 - start doc
  /**
   * @see <a href="XpStartDoc.html">XpStartDoc</a>
   */
  public void start_doc (int type) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 9, 2);
      o.write_int8 (type);
      o.send ();
    }
  }


  // print 10 - end doc
  /**
   * @see <a href="XpEndDoc.html">XpEndDoc</a>
   */
  public void end_doc (boolean cancel) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 10, 2);
      o.write_bool (cancel);
      o.send ();
    }
  }


  // print 13 - start page
  /**
   * @see <a href="XpStartPage.html">XpStartPage</a>
   */
  public void start_page (Window window) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 13, 2);
      o.write_int32 (window.id);
      o.send ();
    }
  }


  // print 14 - end page
  /**
   * @see <a href="XpEndPage.html">XpEndPage</a>
   */
  public void end_page (boolean cancel) {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 14, 2);
      o.write_bool (cancel);
      o.send ();
    }
  }


  // print opcode 20 - rehash printer list
  /**
   * @see <a href="XpRehashPrinterList.html">XpRehashPrinterList</a>
   */
  public void rehash_printer_list () {
    RequestOutputStream o = display.out;
    synchronized (o) {
      o.begin_request (major_opcode, 20, 1);
      o.send ();
    }
  }


  // printer opcode 22 - query screens
  /**
   * @return valid:
   * {@link Enum#next()} of type {@link Window},
   * {@link Enum#next4()}
   *
   * @see <a href="XpQueryScreens.html">XpQueryScreens</a>
   */
  public Window [] screens () {
    int [] ids;
    RequestOutputStream o = display.out;
    synchronized (o) {
      // error in spec: request length = 1 vs. 2
      o.begin_request (major_opcode, 22, 1);
      ResponseInputStream i = display.in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        int count = i.read_int32 ();
        ids = new int [count + 1];
        i.skip (20);
        for (int j = 0; j <= count; j++)
          ids [j] = i.read_int32 ();
      }
    }
    Window [] windows = new Window [ids.length];
    for (int j = 0; j < windows.length; j++)
      windows [j] = (Window) Window.intern (display, ids [j]);
    return windows;
  }
    

  public static final String [] ERROR_STRINGS = {
    "BAD_PRINT_CONTEXT: parameter not a PRINT context",
    "BAD_PRINT_SEQUENCE: illegal sequence of PRINT operations",
    "BAD_PRINT_RESOURCE_ID: parameter not a X-resource ID"
  };


  public Error build (Display display, int code, int seq_no, int bad,
                      int minor_opcode, int major_opcode) {

    String error_string = ERROR_STRINGS [code - first_error];
    return new Error (display, error_string, code, seq_no, bad, 
      minor_opcode, major_opcode);
  }


  /** PRINT attribute notify event. */
  public class AttributeNotifyEvent extends gnu.x11.event.Event {
    public static final int code = 0;
  

    public int context_id;
    AttributeNotifyEvent (Display display, ResponseInputStream in) {
      super (display, in);
      context_id = in.read_int32 ();
      in.skip (24);
    }
  
  
    /**
     * @return valid:
     * {@link Print.Context#JOB_ATTRIBUTE_POOL},
     * {@link Print.Context#DOC_ATTRIBUTE_POOL},
     * {@link Print.Context#PAGE_ATTRIBUTE_POOL},
     * {@link Print.Context#PRINTER_ATTRIBUTE_POOL},
     * {@link Print.Context#SERVER_ATTRIBUTE_POOL},
     * {@link Print.Context#MEDIUM_ATTRIBUTE_POOL},
     * {@link Print.Context#SPOOLER_ATTRIBUTE_POOL}
     */
    public int detail () { return detail; }
    public int context_id () { return context_id; }
  }  
  
  
  /** PRINT print notify event. */
  public class PrintNotifyEvent extends gnu.x11.event.Event {
    public static final int code = 0;
  
  
    public int context_id;
    public boolean cancel;
    public PrintNotifyEvent (Display display, ResponseInputStream i) { 
      super (display, i);
      context_id = i.read_int32 ();
      cancel = i.read_bool ();
      i.skip (23);
    }
  
  
    public static final int START_JOB_NOTIFY = 0;
    public static final int END_JOB_NOTIFY = 1;
    public static final int START_DOC_NOTIFY = 2;
    public static final int END_DOC_NOTIFY = 3;
    public static final int START_PAGE_NOTIFY = 4;
    public static final int END_PAGE_NOTIFY = 5;
  
  
    /**
     * @return valid:
     * {@link #START_JOB_NOTIFY},
     * {@link #END_JOB_NOTIFY},
     * {@link #START_DOC_NOTIFY},
     * {@link #END_DOC_NOTIFY},
     * {@link #START_PAGE_NOTIFY},
     * {@link #END_PAGE_NOTIFY}
     */
    public int detail () { return detail; }
  
  
    public int context_id () { return context_id; }
    public boolean cancel () { return cancel; }
  }
  
  
  public gnu.x11.event.Event build (Display display, 
                                    ResponseInputStream i, int code) {

    return new PrintNotifyEvent (display, i);
  }


  // javadoc bug? should be Context#Context(String)
  /**
   * Create print context.
   *
   * @param name if zero-length, default (first) printer name is used
   *
   * @see Context
   */
  public Context create_context (String name) {
    if (name.length () == 0) {
      Printer printer = (Printer) printer_list () [0];
      name = printer.name ();
    }

    return new Context (name);    
  }


  /**
   * @see #end_doc(boolean)
   */
  public void end_doc () { 
    end_doc (false); 
  }


  /**
   * @see #end_job(boolean)
   */
  public void end_job () { 
    end_job (false); 
  }


  /**
   * @see #end_page(boolean)
   */
  public void end_page () { 
    end_page (false); 
  }


  public String more_string () {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + server_major_version + "." + server_minor_version;
  }


  /**
   * @see #printer_list(String)
   */
  public Printer [] printer_list () {
    return printer_list ("");
  }
}
