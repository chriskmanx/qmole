package gnu.x11.extension;

import gnu.x11.Data;
import gnu.x11.Display;
import gnu.x11.Error;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;
import gnu.x11.Window;
import gnu.x11.Error.ErrorCode;


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

  private static final String [] MINOR_OPCODE_STRINGS = {
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
  public int serverMajorVersion, serverMinorVersion;

  
  public enum EventMask {
      XPNoEventMask(0),
      XPPrintMask(1),
      XPAttributeMask(2);
      
      private int code;
      
      EventMask(int code) {
          this.code = code;
      }
      
      public int getCode() {
          return this.code;
      }
      
      public boolean onMask(int mask) {
          return (this.code & mask) != 0;
      }
  }
  
    
  public enum OutputMode {
      SPOOL(1),
      GET_DATA(2);
      
      private int code;
      
      OutputMode(int code) {
          this.code = code;
      }
      
      public int getCode() {
          return this.code;
      }
  }
  
  
  public enum Attributes {
      JOB_ATTRIBUTE_POOL(1),
      DOC_ATTRIBUTE_POOL(2),
      PAGE_ATTRIBUTE_POOL(3),
      PRINTER_ATTRIBUTE_POOL(4),
      SERVER_ATTRIBUTE_POOL(5),
      MEDIUM_ATTRIBUTE_POOL(6),
      SPOOLER_ATTRIBUTE_POOL(7);
      
      private int code;
      
      Attributes(int code) {
          this.code = code;
      }
      
      public int getCode() {
          return this.code;
      }
  }
  
  
  public enum Rule {
      ATTRIBUTE_REPLACE(1),
      ATTRIBUTE_MERGE(2);
      
      private int code;
      
      Rule(int code) {
          this.code = code;
      }
      
      public int getCode() {
          return this.code;
      }
  }
  
  
  public enum Notify {
      START_JOB_NOTIFY(0),
      END_JOB_NOTIFY(1),
      START_DOC_NOTIFY(2),
      END_DOC_NOTIFY(3),
      START_PAGE_NOTIFY(4),
      END_PAGE_NOTIFY(5);

      private int code;
      
      Notify(int code) {
          this.code = code;
      }
      
      public int getCode() { 
          return this.code;
      }
  }
  
  
  public class Context extends gnu.x11.Resource {
    /** Intern. */
    Context(int id) {
      super(Print.this.display, id);
    }
  
    
    // print 2 - create context
    /**
     * @see <a href="XpCreateContext.html">XpCreateContext</a>
     */
    public Context(String name) {
      super(Print.this.display);

      int len = 4 + Data.unit(name) + Data.unit(locale);
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 2, len);
        o.writeInt32(id);
        o.writeInt32(name.length());
        o.writeInt32(locale.length());
        o.writeString8(name);
        o.writeString8(locale);
        o.send();
      }
    }
  
  
    // print 3 - set context
    /**
     * @see <a href="XpSetContext.html">XpSetContext</a>
     */
    public void set() {
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 3, 2);
        o.writeInt32(id);
        o.send();
      }
    }
  
  
    // print 5 - destroy context
    /**
     * @see <a href="XpDestroyContext.html">XpDestroyContext</a>
     */
    public void destroy() {
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 5, 2);
        o.writeInt32(id);
        o.send();
      }
    }
  
  
    // print 6 - get screen of context
    /**
     * @see <a href="XpGetScreenOfContext.html">XpGetScreenOfContext</a>
     */
    public Window screen() {
      int rootID;
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 6, 2);
        // error in spec: missing context field
        o.writeInt32(id);
        ResponseInputStream i = display.getResponseInputStream();
        synchronized(i) {
          i.readReply(o);
          i.skip(8);
          rootID = i.readInt32();
          i.skip(16);
        }
      }
      return (Window) Window.intern(this.display, rootID);
    }


    
    // print 15 - select input
    /**
     * @see <a href="XpSelectInput.html">XpSelectInput</a>
     */
    public void selectInput(EventMask eventMask) {
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 15, 3);
        o.writeInt32(id);
        o.writeInt32(eventMask.getCode());
        o.send();
      }
    }
  
  
    /** Reply of {@link #inputSelected(int)}. */
    public class InputSelectedInfo {
      public EventMask eventMask;
      public int allEventMasks;
      InputSelectedInfo(ResponseInputStream i) {
        eventMask = EventMask.values()[i.readInt32()];
        allEventMasks = i.readInt32();
      }
      public EventMask eventMask() { return eventMask; }
      public int allEventsMask() { return allEventMasks; }
    }
  
  
    // print 16 - input selected
    /**
     * @see <a href="XpInputSelected.html">XpInputSelected</a>
     */
    public InputSelectedInfo inputSelected(EventMask eventMask) {

      InputSelectedInfo info;
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 16, 2);
        o.writeInt32(id);
        ResponseInputStream i = display.getResponseInputStream();
        synchronized(i) {
          i.readReply(o);
          i.skip(8);
          info = new InputSelectedInfo(i);
          i.skip(16);
        }
      }
      return info;
    }
  
    // print 17 - get attributes
    /**
     * @param pool valid:
     * {@link Attributes}
     *
     * @see <a href="XpGetAttributes.html">XpGetAttributes</a>
     */
    public String attributes(Attributes pool) {

      String atts;
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 17, 3);
        o.writeInt32(id);
        o.writeInt8(pool.getCode());
        ResponseInputStream i = display.getResponseInputStream();
        synchronized(i) {
          i.readReply(o);
          i.skip(8);
          int strlen = i.readInt32();
          i.skip(20);
          atts = i.readString8(strlen);
        }
      }
      return atts;
    }   
  

  
  
    // print 18 - set attributes
    /**
     * @param pool valid:
     * {@link Attributes}
     *
     * @see <a href="XpSetAttributes.html">XpSetAttributes</a>
     */
    public void set_attributes(Attributes pool, Rule rule, String attributes) {
      int len = 4 + Data.unit(attributes);
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 18, len);
        o.writeInt32(id);
        o.writeInt32(attributes.length());
        o.writeInt8(pool.getCode());
        o.writeInt8(rule.getCode());
        o.skip(2);
        o.writeString8(attributes);
        o.send();
      }
    };   
  
  
    // print 19 - get one attribute
    /**
     * @param pool valid:
     * {@link Attributes},
     *
     * @param rule valid:
     * {@link Rule}
     *
     * @see <a href="XpGetOneAttribute.html">XpGetOneAttribute</a>
     */
    public String oneAttribute(Attributes pool, String name) {
      String attr;
      int len = 4 + Data.unit(name);
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 19, len);
        o.writeInt32(id);
        o.writeInt32(name.length());
        o.writeInt8(pool.getCode());
        o.skip(3);
        o.writeString8(name);
        ResponseInputStream i = display.getResponseInputStream();
        synchronized(i) {
          i.readReply(o);
          i.skip(8);
          int strlen = i.readInt32();
          i.skip(20);
          attr = i.readString8(strlen);
          i.skip(RequestOutputStream.pad(strlen));
        }
      }
      return attr;
    }
  
  
    /** Reply of {@link #pageDimensions()}. */  
    public class PageDimensions {
      private int width;
      private int height;
      private int offsetX;
      private int offsetY;
      private int reproducibleWidth;
      private int reproducibleHeight;
      PageDimensions(ResponseInputStream i) {
        width = i.readInt16();
        height = i.readInt16();
        offsetX = i.readInt16();
        offsetY = i.readInt16();
        reproducibleWidth = i.readInt16();
        reproducibleHeight = i.readInt16();
      }

      public int width() { return width; }
      public int height() { return height; }
      public int offsetX() { return offsetX; }
      public int offsetY() { return offsetY; }
      public int reproducibleX() { return reproducibleWidth; }
      public int reproducibleY() { return reproducibleHeight; }
  
  
      public String toString() {
        return "#PageDimensionsReply"
          + "\nwidth: " + width()
          + "\nheight: " + height()
          + "\noffset-x: " + offsetX()
          + "\noffset-x: " + offsetY()
          + "\nreproducible-x: " + reproducibleX()
          + "\nreproducible-y: " + reproducibleY();
      }
    }
  
  
    /** Reply of {@link #pageDimensions()}. */  
    public class SetImageResolutionInfo {
      private boolean status;
      private int previousResolution;
      SetImageResolutionInfo(ResponseInputStream i) {
        status = i.readBool();
        i.skip(6);
        previousResolution = i.readInt16();
      }
      public boolean status() { return status; }
      public int previousResolution() { return previousResolution; }
    }
  
  
    // print 21 - get page dimensions
    /**
     * @see <a href="XpGetPageDimensions.html">XpGetPageDimensions</a>
     */
    public PageDimensions pageDimensions() {
      PageDimensions dim;
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 21, 2);
        o.writeInt32(id);
        ResponseInputStream i = display.getResponseInputStream();
        synchronized(i) {
          i.readReply(o);
          i.skip(8);
          dim = new PageDimensions(i);
          i.skip(12);
        }
      }
      return dim;
    }
  
  
    // print 23 - set image resolution
    /**
     * @see <a href="XpSetImageResolution.html">XpSetImageResolution</a>
     */
    public SetImageResolutionInfo setImageResolution(int resolution) {

      SetImageResolutionInfo info;
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 23, 3);
        o.writeInt32(id);
        o.writeInt16(resolution);
        ResponseInputStream i = display.getResponseInputStream();
        synchronized(i) {
          i.skip(1);
          info = new SetImageResolutionInfo(i);
          i.skip(22);
        }
      }
      return info;
    }
      
  
    // print 24 - get image resolution
    /**
     * @see <a href="XpGetImageResolution.html">XpGetImageResolution</a>
     */
    public int imageResolution() {

      int res;
      RequestOutputStream o = display.getResponseOutputStream();
      synchronized(o) {
        o.beginRequest(majorOpcode, 24, 2);
        o.writeInt32(id);
        ResponseInputStream i = display.getResponseInputStream();
        synchronized(i) {
          i.skip(8);
          res = i.readInt16();
          i.skip(22);
        }
      }
      return res;
    }
      
  
    /**
     * All possible attributes.
     *
     * @see #attributes(int)
     */
    public String attributes() {
      return attributes(Attributes.JOB_ATTRIBUTE_POOL)
        + attributes(Attributes.DOC_ATTRIBUTE_POOL)
        + attributes(Attributes.PAGE_ATTRIBUTE_POOL)
        + attributes(Attributes.PRINTER_ATTRIBUTE_POOL)
        + attributes(Attributes.SERVER_ATTRIBUTE_POOL);
    }
  
  
    public String toString() {
      return "#Context\n" 
        + attributes();
    }
  }
  
  
    // print opcode 0 - query version
  /**
   * @see <a href="XpQueryVersion.html">XpQueryVersion</a>
   */
  public Print(Display display) 
    throws gnu.x11.extension.NotFoundException { 

    super(display, "XpExtension", MINOR_OPCODE_STRINGS, 2, 2);

    // check version before any other operations
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 0, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        serverMajorVersion = i.readInt16();
        serverMinorVersion = i.readInt16();
        i.skip(20);
      }
    }
  }


  /** Reply of {@link #printerList(String)}. */
  public class Printer {

    private String name;
    private String desc;
    
    Printer(ResponseInputStream i) {
      int nameLength = i.readInt32();
      name = i.readString8(nameLength);
      i.skip(RequestOutputStream.pad(nameLength));
      int descLength = i.readInt32();
      desc = i.readString8(descLength);
      i.skip(RequestOutputStream.pad(descLength));
    }

    public String name() { return name; }
    public String description() { return desc; }


    /**
     * @see #createContext(String)
     */
    public Context context() {
      return createContext(name());
    }

    public String toString() {
      return "#Printer"
        + "\nprinter-name: " + name()
        + "\nprinter-description: "  + description();
    }
  }

  
  // print opcode 1 - get printer list
  /**
   * @return valid: {@link Enum#next()}
   * @see <a href="XpGetPrinterList.html">XpGetPrinterList</a>
   */
  public Printer[] printerList(String name) {

    Printer[] printers;
    int len = 3 + Data.unit(name) + Data.unit(locale);
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 1, len);
      o.writeInt32(name.length());
      o.writeInt32(locale.length());
      o.writeString8(name);
      o.writeString8(locale);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.skip(8);
        int num = i.readInt32();
        printers = new Printer[num];
        
        for (Printer p : printers) 
          p = new Printer(i);
      }
    }
    return printers;
  }


  // print 4 - get context
  /**
   * @see <a href="XpGetContext.html">XpGetContext</a>
   */
  public Context context() {
    int id;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 4, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        id = i.readInt32();
        i.skip(16);
      }
    }
    return new Context(id);
  }



  // print 7 - start job
  /**
   * @param outputMode valid:
   * {@link OutputMode}
   * 
   * @see <a href="XpStartJob.html">XpStartJob</a>
   */
  public void startJob(OutputMode outputMode) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 7, 2);
      o.writeInt8(outputMode.getCode());
      o.send();
    }
  }


  // print 8 - end job
  /**
   * @see <a href="XpEndJob.html">XpEndJob</a>
   */
  public void endJob(boolean cancel) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 8, 2);
      o.writeBool(cancel);
      o.send();
    }
  }


  // print 9 - start doc
  /**
   * @see <a href="XpStartDoc.html">XpStartDoc</a>
   */
  public void startDoc(int type) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 9, 2);
      o.writeInt8(type);
      o.send();
    }
  }


  // print 10 - end doc
  /**
   * @see <a href="XpEndDoc.html">XpEndDoc</a>
   */
  public void endDoc(boolean cancel) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 10, 2);
      o.writeBool(cancel);
      o.send();
    }
  }


  // print 13 - start page
  /**
   * @see <a href="XpStartPage.html">XpStartPage</a>
   */
  public void startPage(Window window) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 13, 2);
      o.writeInt32(window.getID());
      o.send();
    }
  }


  // print 14 - end page
  /**
   * @see <a href="XpEndPage.html">XpEndPage</a>
   */
  public void endPage(boolean cancel) {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 14, 2);
      o.writeBool(cancel);
      o.send();
    }
  }


  // print opcode 20 - rehash printer list
  /**
   * @see <a href="XpRehashPrinterList.html">XpRehashPrinterList</a>
   */
  public void rehashPrinterList() {
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      o.beginRequest(majorOpcode, 20, 1);
      o.send();
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
  public Window [] screens() {
    int [] ids;
    RequestOutputStream o = display.getResponseOutputStream();
    synchronized(o) {
      // error in spec: request length = 1 vs. 2
      o.beginRequest(majorOpcode, 22, 1);
      ResponseInputStream i = display.getResponseInputStream();
      synchronized(i) {
        i.readReply(o);
        i.skip(8);
        int count = i.readInt32();
        ids = new int [count + 1];
        i.skip(20);
        for (int j = 0; j <= count; j++)
          ids[j] = i.readInt32();
      }
    }
    Window[] windows = new Window [ids.length];

    for (int j = 0; j < windows.length; j++)
      windows [j] =(Window) Window.intern(display, ids [j]);
    return windows;
  }
    

  public static final String [] ERROR_STRINGS = {
    "BAD_PRINT_CONTEXT: parameter not a PRINT context",
    "BAD_PRINT_SEQUENCE: illegal sequence of PRINT operations",
    "BAD_PRINT_RESOURCE_ID: parameter not a X-resource ID"
  };


  public Error build(Display display, int code, int seq_no, int bad,
                      int minor_opcode, int major_opcode) {

    String error_string = ERROR_STRINGS [code - firstError];
    return new Error(display, error_string, ErrorCode.getError(code), seq_no, bad, 
      minor_opcode, major_opcode);
  }


  /** PRINT attribute notify event. */
  public class AttributeNotifyEvent extends gnu.x11.event.Event {
    public static final int code = 0;
  

    public int contextID;
    AttributeNotifyEvent(Display display, ResponseInputStream in) {
      super(display, in);
      contextID = in.readInt32();
      in.skip(24);
    }
  
  
    /**
     * @return valid:
     * {@link Attributes}
     */
    public Attributes detail() { return Attributes.values()[detail + 1]; }
    public int contextID() { return contextID; }
  }  
  
  
  /** PRINT print notify event. */
  public class ContextID extends gnu.x11.event.Event {
    public static final int code = 0;
  
  
    public int contextID;
    public boolean cancel;
    public ContextID(Display display, ResponseInputStream i) { 
      super(display, i);
      contextID = i.readInt32();
      cancel = i.readBool();
      i.skip(23);
    }
  

    /**
     * @return valid:
     * {@link Notify},
     */
    public Notify detail() { return Notify.values()[detail]; }
  
  
    public int getContextID() { return contextID; }
    public boolean cancel() { return cancel; }
  }
  
  
  public gnu.x11.event.Event build(Display display, 
                                    ResponseInputStream i, int code) {

    return new ContextID(display, i);
  }


  // javadoc bug? should be Context#Context(String)
  /**
   * Create print context.
   *
   * @param name if zero-length, default (first) printer name is used
   *
   * @see Context
   */
  public Context createContext(String name) {
    if (name.length() == 0) {
      Printer printer = (Printer) printerList() [0];
      name = printer.name();
    }

    return new Context(name);    
  }


  /**
   * @see #endDoc(boolean)
   */
  public void endDoc() { 
    endDoc(false); 
  }


  /**
   * @see #endJob(boolean)
   */
  public void endJob() { 
    endJob(false); 
  }


  /**
   * @see #endPage(boolean)
   */
  public void endPage() { 
    endPage(false); 
  }


  public String moreString() {
    return "\n  client-version: " 
      + CLIENT_MAJOR_VERSION + "." + CLIENT_MINOR_VERSION
      + "\n  server-version: "
      + serverMajorVersion + "." + serverMinorVersion;
  }


  /**
   * @see #printerList(String)
   */
  public Printer[] printerList() {
    return printerList("");
  }
}
