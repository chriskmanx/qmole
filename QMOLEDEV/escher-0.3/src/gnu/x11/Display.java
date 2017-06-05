package gnu.x11;

import gnu.x11.event.Event;
import gnu.x11.extension.ErrorFactory;
import gnu.x11.extension.EventFactory;
import gnu.x11.extension.BigRequests;
import gnu.x11.extension.NotFoundException;
import gnu.x11.extension.XCMisc;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.Hashtable;


/** X server connection. */
public class Display {
  public static final int CURRENT_TIME = 0;


  /**
   * The output stream.
   */
  public RequestOutputStream out;

  /**
   * The input stream.
   */
  public ResponseInputStream in;

  /**
   * The socket.
   */
  private Socket socket;

  /**
   * The hostname to this display.
   */
  public String hostname;

  /**
   * The display number.
   */
  public int display_no;

  public Input input;

  /**
   * Indicates if this display is connected or not.
   */
  public boolean connected;


  // server information
  public int release_no;
  public String vendor;
  public int maximum_request_length;
  public Screen [] screens;
  public Pixmap.Format [] pixmap_formats;
  public int image_byte_order;
  public int bitmap_format_bit_order;
  public int bitmap_format_scanline_unit;
  public int bitmap_format_scanline_pad;
  public int resource_base;
  public int resource_mask;


  // defaults
  public Color default_black, default_white;
  public Colormap default_colormap;
  public int default_depth; 
  public Pixmap.Format default_pixmap_format;
  public Window default_root;
  public Screen default_screen;
  public int default_screen_no;

  int min_keycode;
  int max_keycode;

  /** 
   * @see Screen#default_gc()
   */
  public GC default_gc;


  // resources
  public Hashtable resources = new Hashtable (257);
  public int resource_index;
  public Hashtable atom_ids = new Hashtable (257);
  public Hashtable atom_names = new Hashtable (257);
  

  // xcmisc
  public XCMisc xcmisc;
  public boolean use_xcmisc;
  public int xcmisc_resource_base;
  public int xcmisc_resource_count;


  // extension

  public boolean big_requests_present;
  public int extended_maximum_request_length;


  /**
   * Major opcodes 128 through 255 are reserved for extensions,
   * totally 128. 
   */
  public String [] extension_opcode_strings = new String [128];
  public String [] [] extension_minor_opcode_strings = new String [128] [];


  /** 
   * Event codes 64 through 127 are reserved for extensiones, 
   * totally 64. 
   */
  public EventFactory [] extension_event_factories = new EventFactory [64];


  /** 
   * Error codes 128 through 255 are reserved for extensiones,
   * totally 128. 
   */
  public ErrorFactory [] extension_error_factories = new ErrorFactory [128];

  /**
   * #Display(String, int, int)
   */
  public Display () {
    this ("", 0, 0);
  }


  /** X display name. */
  public static class Name {
    public String hostname = "";
    public int display_no, screen_no;


    public Name (String display_name) {
      if (display_name == null) return;
      int i = display_name.indexOf (':');

      // case 1: display_name = hostname
      if (i == -1) {
        hostname = display_name;
        return;
      }
      
      hostname = display_name.substring (0, i);
      int j = display_name.indexOf ('.', i);
  
      if (j == -1) {
        // case 2: display_name = hostname:display_no
        display_no = Integer.parseInt (
          display_name.substring (i+1, display_name.length ())); 
        return;
      }
  
      // case 3: display_name = hostname:display_no.screen_no
      display_no = Integer.parseInt (
        display_name.substring (i+1, j));
      screen_no = Integer.parseInt (
        display_name.substring (j+1, display_name.length ()));      
    }


    public Name (String hostname, int display_no, int screen_no) {
      this.hostname = hostname;
      this.display_no = display_no;
      this.screen_no = screen_no;
    }

    
    public String toString () {
      return hostname + ":" + display_no + "." + screen_no;
    }
  }


  /**
   * #Display(String, int, int)
   */
  public Display (Name name) {        
    this (name.hostname, name.display_no, name.screen_no);
  }


  /**
   * #Display(String, int, int)
   */
  public Display (String hostname, int display_no) {
    this (hostname, display_no, 0);
  }

  /**
   * Sets up a display using a connection over the specified
   * <code>socket</code>. This should be used when there is a need to use
   * non-TCP sockets, like connecting to an X server via Unix domain sockets.
   * You need to provide an implementation for this kind of socket though.
   *
   * @param socket the socket to use for that connection
   * @param hostname the hostname to connect to
   * @param display_no the display number
   * @param screen_no the screen number
   */
  public Display (Socket socket, String hostname, int display_no,
                  int screen_no) {
    default_screen_no = screen_no;
    this.hostname = hostname;
    this.display_no = display_no;
    this.socket = socket;
    init_streams ();
    init();
  }

  /**
   * @see <a href="XOpenDisplay.html">XOpenDisplay</a>
   */
  public Display (String hostname, int display_no, int screen_no) {
    default_screen_no = screen_no;
    this.display_no = display_no;
    this.hostname = hostname;
    try {
      socket = new Socket (hostname, 6000 + display_no);
    } catch (IOException ex) {
      handle_exception (ex);
    }
    init_streams ();
    init ();
  }

  private void init() {

    // authorization protocol
    XAuthority xauth = get_authority ();

    byte[] auth_name;
    byte[] auth_data;
    if (xauth != null) {
      auth_name = xauth.protocol_name;
      auth_data = xauth.protocol_data;
    } else {
      // In case the X authority couldn't be established...
      auth_name = new byte[0];
      auth_data = new byte[0];
    }

    RequestOutputStream o = out;
    synchronized (o) {
      o.write_int8 ('B');
      o.write_int8 (0); // Unused.
      o.write_int16 (11);// major version
      o.write_int16 (0);// minor version
      o.write_int16 (auth_name.length);
      o.write_int16 (auth_data.length);
      o.write_int16 (0); // Unuse.
      o.write_bytes (auth_name);
      o.write_pad (auth_name.length);
      o.write_bytes (auth_data);
      o.write_pad (auth_data.length);
      o.flush ();
      ResponseInputStream i = in;
      synchronized (i) {
        // Don't do read_reply() here, this is not needed and doesn't work
        // during connection setup.
        connected = true;
        init_server_info (i);
      }
    }
    maximum_request_length = out.set_buffer_size (maximum_request_length);
    init_keyboard_mapping ();
    init_defaults ();
    init_big_request_extension ();
    //System.err.println("Connection to X server established");
  }

  // opcode 23 - get selection owner
  /**
   * @see <a href="XGetSelectionOwner.html">XGetSelectionOwner</a>
   */
  public Window selection_owner (Atom selection) {

    RequestOutputStream o = out;
    int owner_id = -1;
    synchronized (o) {
      o.begin_request(23, 0, 2);
      o.write_int32 (selection.id);
      ResponseInputStream i = in;
      synchronized (i) {
        i.read_reply(o);
        i.skip (8);
        owner_id = i.read_int32 ();
        i.skip (20);
      }
    }
    return (Window) Window.intern (this, owner_id);
  }


  // opcode 36 - grab server
  public synchronized void grab_server () {
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (36, 0, 1);
      o.send ();
    }
  }


  // opcode 37 - ungrab server
  public void ungrab_server () {
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request(37, 0, 1);
      o.send();
    }
  }


  // opcode 49 - list fonts
  /**
   * @return valid:
   * {@link Enum#next()} of type {@link Font},
   * {@link Enum#next_string()}
   * 
   * @see <a href="XListFonts.html">XListFonts</a>
   */
  public Font[] fonts (String pattern, int max_name_count) {

    int n = pattern.length();
    int p = RequestOutputStream.pad (n); 

    RequestOutputStream o = out;
    Font[] fonts = null;
    synchronized (o) {
      o.begin_request(49, 0, 2 + (n + p) / 4);
      o.write_int16 (max_name_count);
      o.write_int16 (n);
      o.write_string8 (pattern);
      o.skip (p);

      ResponseInputStream i = in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (4);
        int len = i.read_int32 () * 4; // Number of bytes for the reply.
        int num_strings = i.read_int16 ();
        i.skip (22);
        fonts = new Font[num_strings];
        for (int j = 0; j < num_strings; j++) {
          int strlen = i.read_int8 ();
          String str = i.read_string8 (strlen);
          len -= strlen + 1;
          fonts [j] = new Font (this, str);
        }
        i.skip (len); // Pad the remaining bytes.
      }
    }
    return fonts;
  }


  // opcode 50 - list fonts with info
  /**
   * @see <a href="XListFontsWithInfo.html">XListFontsWithInfo</a>
   */
  public Data fonts_with_info (String pattern, 
    int max_name_count) {

    // FIXME: Implement.
    return null;
  }


  // opcode 51 - set font path
  /**
   * @see <a href="XSetFontPath.html">XSetFontPath</a>
   */
  public void set_font_path (int count, String[] path) {

    int n = 0;
    for (int i = 0; i < path.length; i++) {
      n += path.length + 1;
    }
    int p = RequestOutputStream.pad (n);

    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (51, 0, 2 + (n + p) / 4);
      o.write_int16 (path.length);
      o.skip (2);
      for (int i = 0; i < path.length; i++) {
        o.write_int8 (path [i].length());
        o.write_string8 (path [i]);
      }
      o.skip (p);
      o.send ();
    }
  }

  // opcode 52 - get font path
  /**
   * Returns the current search path for fonts.
   *
   * @return the current search path for fonts
   *
   * @see #set_font_path(int, String[])
   * @see <a href="XGetFontPath.html">XGetFontPath</a>
   */
  public String[] font_path () {

    RequestOutputStream o = out;
    String[] path;
    synchronized (o) {
      o.begin_request (52, 0, 1);
      ResponseInputStream i = in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (4);
        int reply_length = i.read_int32 () * 4;
        int num_strings = i.read_int16 ();
        i.skip (22);
        path = new String[num_strings];
        int bytes_read = 0;
        for (int j = 0; j < num_strings; j++) {
          int num_chars = i.read_int8 ();
          path [j] = i.read_string8 (num_chars);
          bytes_read += num_chars + 1;
        }
        i.skip (reply_length - bytes_read);
      }
    }
    return path;
  }

  
  /**
   * Information about an X extension.
   *
   * @see Display#query_extension
 . */
  public static class ExtensionInfo {

    private boolean present;
    private int major_opcode;
    private int first_event;
    private int first_error;

    ExtensionInfo (ResponseInputStream in) {
      present = in.read_bool ();
      major_opcode = in.read_int8 ();
      first_event = in.read_int8 ();
      first_error = in.read_int8 ();
      //System.err.println("first error: " + first_error);
      //Thread.dumpStack ();
    }

    public boolean present () {
      return present;
    }

    public int major_opcode () {
      return major_opcode;
    }

    public int first_event () {
      return first_event;
    }

    public int first_error () {
      return first_error;
    }
  }
  
  
  // opcode 98 - query extension
  /**
   * Determines if the named extension is present. If so, the major opcode for the extension is returned,
   * if it has one. Otherwise zero is returned. Any minor opcode or the request formats are specific
   * to the extension. If the extension involves additional event types, the base event type code is
   * returned. Otherwise zero is returned. The format of the events is specific to the extension.
   * If the extension involves additional error codes, the base error code is returned. The format
   * of additional data in the errors is specific to the extension.
   *
   * The name should use ISO-Latin1 encoding, and uppercase and lowercase do matter.
   *
   * @param name the name of the extension to query
   *
   * @return
   *
   * @see <a href="XQueryExtension.html">XQueryExtension</a>
   */
  public ExtensionInfo query_extension (String name) {

    int n = name.length ();
    int p = RequestOutputStream.pad (n);

    ExtensionInfo info;
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (98, 0, 2 + (n + p) / 4);
      o.write_int16 (n);
      o.skip (2);
      o.write_string8 (name);
      o.skip (p);
      ResponseInputStream i = in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        info = new ExtensionInfo (i);
        i.skip (20);
      }
    }
    return info;
  }

  // opcode 99 - list extensions
  /**
   * Returns a list of all extensions supported by the server.
   *
   * @return a list of all extensions supported by the server
   *
   * @see <a href="XListExtensions.html">XListExtensions</a>
   */
  public String[] extensions () {

    String [] exts;
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (99, 9, 1);
      ResponseInputStream i = in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        int num_strs = i.read_int8 ();
        i.skip (2);
        int reply_length = i.read_int32 () * 4;
        exts = new String [num_strs];
        i.skip (24);
        int bytes_read = 0;
        for (int j = 0; j < num_strs; j++) {
          int len = i.read_int8 ();
          exts [j] = i.read_string8 (len);
          bytes_read += len + 1;
        }
        i.skip (reply_length - bytes_read);
      }
    }
    return exts;
  }


  // opcode 104 - bell
  /**
   * Rings the bell on the keyboard at a volume relative to the base volume
   * of the keyboard, if possible.  Percent can range from -100 to +100
   * inclusive (or a Value error results). The volume at which the bell is rung
   * when percent is nonnegative is:
   *
   * base - [(base * percent) / 100] + percent
   *
   * When percent is negative, it is:
   *
   * base + [(base * percent) / 100]
   *
   * @param volume, see above
   *
   * @see <a href="XBell.html">XBell</a>
   */
  public void bell (int percent) {
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (104, percent, 1);
      o.send ();
    }
  }


  public static final int NO = 0;
  public static final int YES = 1;
  public static final int DEFAULT = 2;


  public static final String [] SCREEN_SAVER_STRINGS
    = {"no", "yes", "default"};


  // opcode 107 - set screen saver
  /**
   * 
   * @param prefer_blanking valid:
   * {@link #NO},
   * {@link #YES},
   * {@link #DEFAULT}
   * 
   * @param allow_exposures valid: 
   * {@link #NO},
   * {@link #YES},
   * {@link #DEFAULT}
   * 
   * @see <a href="XSetScreenSaver.html">XSetScreenSaver</a>
   */
  public void set_screen_saver (int timeout, int interval, 
                                int prefer_blanking, int allow_exposures) {

    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (107, 0, 3);
      o.write_int16 (timeout);
      o.write_int16 (interval);
      o.write_int8 (prefer_blanking);
      o.write_int8 (allow_exposures);
      o.skip (2);
      o.send ();
    }
  }


  /**
   * Informations about the screensaver.
   *
   * @see {@link Display#get_screen_saver()}.
   */
  public static class ScreenSaverInfo {

    private int timeout;
    private int interval;
    private boolean prefer_blanking;
    private boolean allow_exposures;

    ScreenSaverInfo (ResponseInputStream in) {
      timeout = in.read_int16 ();
      interval = in.read_int16 ();
      prefer_blanking = in.read_bool ();
      allow_exposures = in.read_bool ();
    }

    public int timeout () {
      return timeout;
    }

    public int interval () {
      return interval;
    }
  
    public boolean prefer_blanking () {
      return prefer_blanking;
    }
  
    public boolean allow_exposures () {
      return allow_exposures;
    }
  
    public String toString () {
      return "#ScreenSaverReply"
        + "\n  timeout: " + timeout ()
        + "\n  interval: " + interval ()
        + "\n  prefer-blanking: "
        + prefer_blanking ()
        + "\n  allow-exposures: "
        + allow_exposures ();
    }
  }
  
  
  // opcode 108 - get screen saver
  /**
   * Returns the screensaver control values.
   *
   * @return the screensaver control values
   *
   * @see <a href="XGetScreenSaver.html">XGetScreenSaver</a>
   */
  public ScreenSaverInfo screen_saver () {

    ScreenSaverInfo info;
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (108, 0, 1);
      ResponseInputStream i = in;      
      synchronized (i) {
        i.read_reply (o);
        i.skip (8);
        info = new ScreenSaverInfo (i);
        i.skip (18);
      }
    }
    return info;
  }


  public static final int INSERT = 0;
  public static final int DELETE = 1;


  // opcode 109 - change hosts
  /**
   * @param mode valid:
   * {@link #INSERT},
   * {@link #DELETE}
   * 
   * @see <a href="XAddHost.html">XAddHost</a>
   * @see <a href="XRemoveHost.html">XRemoveHost</a>
   */
  public void change_hosts (int mode, int family, byte [] host) {
    
    int n = host.length;
    int p = RequestOutputStream.pad (n);

    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (109, mode, 2 + (n + p) / 4);
      o.write_int8 (family);
      o.skip (1);
      o.write_int16 (n);
      o.write_bytes (host);
      o.skip (p);
      o.send ();
    }
  }


  /**
   * Information about a host.
   *
   * @see Display#list_hosts()
   */
  public static class Host {
    public static final int INTERNET = 0;
    public static final int DECNET = 1;
    public static final int CHAOS = 2;

    public int family;
    public byte[] address;

    /**
     * Reads one Host instance from a ResponseInputStream.
     *
     * @param in the input stream to read from
     */
    Host (ResponseInputStream in) {
      family = in.read_int8 ();
      in.skip (1);
      int add_len = in.read_int16 ();
      address = new byte [add_len];
      in.read_data (address);
      in.pad (add_len);
    }
  }

  /**
   * Hosts currently on the access control list and whether use of
   * the list at connection setup is currently enabled or disabled.
   *
   * @see Display#list_hosts
   */
  public static class HostsInfo {

    public boolean mode;

    Host[] hosts;

    HostsInfo (ResponseInputStream in) {
      mode = in.read_bool ();
      in.skip (6);
      int num_hosts = in.read_int16 ();
      in.skip (22);
      hosts = new Host [num_hosts];
      for (int i = 0; i < num_hosts; i++)
        hosts [i] = new Host (in);
    }

  }
  
  
  // opcode 110 - list hosts
  /**
   * Returns the hosts currently on the access control list and whether use of
   * the list at connection setup is currently enabled or disabled.
   *
   * @see <a href="XListHosts.html">XListHosts</a>
   */
  public HostsInfo list_hosts () {
    HostsInfo info;
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (110, 0, 1);
      ResponseInputStream i = in;
      synchronized (i) {
        i.read_reply (o);
        i.skip (1);
        info = new HostsInfo (i);
      }
    }
    return info;
  }

  public static final int ENABLE = 0;
  public static final int DISABLE = 1;


  // opcode 111 - set access control
  /**
   * @param mode valid:
   * {@link #ENABLE},
   * {@link #DISABLE}
   * 
   * @see <a href="XSetAccessControl.html">XSetAccessControl</a>
   */
  public void set_access_control (int mode) {
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (111, mode, 1);
      o.send ();
    }
  }

  
  // opcode 113 - kill client
  /**
   * @see <a href="XKillClient.html">XKillClient</a>
   */
  public void kill_client (Resource resource) {
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (113, 0, 2);
      o.write_int32 (resource.id);
      o.send ();
    }
  }


  public static final int DESTROY = 0;
  public static final int RETAIN_PERMANENT = 1;
  public static final int RETAIN_TEMPORARY = 2;


  // opcode 112 - set close down mode
  /**
   * @param mode valid:
   * {@link #DESTROY},
   * {@link #RETAIN_PERMANENT},
   * {@link #RETAIN_TEMPORARY}
   * 
   * @see <a href="XSetCloseDownMode.html">XSetCloseDownMode</a>
   */
  public void set_close_down_mode (int mode) {
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (112, mode, 1);
      o.send ();
    }
  }
    

  public static final int ACTIVATE = 0;
  public static final int RESET = 1;


  // opcode 115 - force screen saver
  /**
   * @param mode valid:
   * {@link #ACTIVATE},
   * {@link #RESET}
   * 
   * @see <a href="XForceScreenSaver.html">XForceScreenSaver</a>
   */
  public void force_screen_saver (int mode) {
    RequestOutputStream o = out;
    synchronized (o) {
      o.begin_request (115, mode, 1);
      o.send ();
    }
  }


  public int allocate_id (Object object) {
    /* From XC-MISC extension specification:
     * 
     * When an X client connects to an X server, it receives a fixed range
     * of resource IDs to use to identify the client's resources inside the
     * X server. Xlib hands these out sequentially as needed. When it
     * overruns the end of the range, an IDChoice protocol error results. 
     * Long running clients, or clients that use resource IDs at a rapid
     * rate, may encounter this circumstance. When it happens, there are
     * usually many resource IDs available, but Xlib doesn't know about
     * them.
     *
     * One approach to solving this problem would be to have Xlib notice
     * when a resource is freed and recycle its ID for future use. This
     * strategy runs into difficulties because sometimes freeing one
     * resource causes others to be freed (for example, when a window is
     * destroyed, so are its children). To do a complete job, Xlib would
     * have to maintain a large amount of state that currently resides only
     * in the server (the entire window tree in the above example). Even if
     * a less comprehensive strategy was adopted, such as recycling only
     * those IDs that Xlib can identify without maintaining additional
     * state, the additional bookkeeping at resource creation and
     * destruction time would likely introduce unacceptable overhead.
     *
     * To avoid the problems listed above, the server's complete knowledge
     * of all resource IDs in use by a client is leveraged. This extension
     * provides two ways for Xlib to query the server for available
     * resource IDs. Xlib can use these extension requests behind the
     * scenes when it has exhausted its current pool of resource IDs.
     */

    /* If XC-MISC is present, we use it. Otherwise, we fall back to
     * allocate X resource ID sequentially to the end without recycling ID
     * (just as xlib does).
     *
     * Sample values:
     *   resource base: 0x04000000 or 00000100000000000000000000000000b
     *   resource mask: 0x003FFFFF or 00000000001111111111111111111111b
     */

    if (!use_xcmisc)
      // check if basic allocation fails
      use_xcmisc = (resource_index+1 & ~resource_mask) != 0;


    if (!use_xcmisc) {
      int id = resource_index++ | resource_base;
      resources.put (new Integer (id), object);
      return id;
    }

    
    if (xcmisc == null) 
      try {
        xcmisc = new XCMisc (this);
      } catch (NotFoundException e) {
        throw new RuntimeException ("Failed to allocate new resource id");
      }

    
    if (xcmisc_resource_count == 0) {
      // first time, or used up
      gnu.x11.extension.XCMisc.XIDRange rr = xcmisc.xid_range ();
      xcmisc_resource_base = rr.start_id;
      xcmisc_resource_count = rr.count;
    }

    
    // give out in descending order
    xcmisc_resource_count--;
    return xcmisc_resource_base+xcmisc_resource_count;   
  }


  /**
   * @see <a href="XCloseDisplay.html">XCloseDisplay</a>
   */  
  public void close () {
    // FIXME: Implement more sensible shutdown.
    try {
    in.close ();
    out.close ();
    socket.close ();
    } catch (IOException ex) {
      handle_exception (ex);
    }
    connected = false;
  }

  public void init_big_request_extension () {
    /* From Big Requests extension specification:
     *
     * It is desirable for core Xlib, and other extensions, to use this
     * extension internally when necessary. It is also desirable to make
     * the use of this extension as transparent as possible to the X
     * client. For example, if enabling of the extension were delayed until
     * the first time it was needed, an application that used XNextRequest
     * to determine the sequence number of a request would no longer get
     * the correct sequence number. As such, XOpenDisplay will determine if
     * the extension is supported by the server and, if it is, enable
     * extended-length encodings.
     *
     * The core Xlib functions XDrawLines, XDrawArcs, XFillPolygon,
     * XChangeProperty, XSetClipRectangles, and XSetRegion are required to
     * use extended-length encodings when necessary, if supported by the
     * server. Use of extended-length encodings in other core Xlib
     * functions (XDrawPoints, XDrawRectangles, XDrawSegments, XFillArcs,
     * XFillRectangles, XPutImage) is permitted but not required; an Xlib
     * implementation may choose to split the data across multiple smaller
     * requests instead.
     */
    try {
      BigRequests big = new BigRequests (this);
      big_requests_present = true;
      extended_maximum_request_length = big.enable ();
    } catch (NotFoundException e) {
      big_requests_present = false;
    }
  }


  public void init_defaults () {
    default_screen = screens [default_screen_no];
    default_root = default_screen.root (); // before init default_gc
    default_depth = default_screen.root_depth;
    default_colormap = default_screen.default_colormap ();
    default_gc = default_screen.default_gc ();
    default_black = new Color (default_screen.black_pixel);
    default_white = new Color (default_screen.white_pixel);

    for (int i=pixmap_formats.length-1; i>=0; i--)
      if (pixmap_formats [i].depth == default_depth) {
        default_pixmap_format = pixmap_formats [i];
        break;
      }
  }


  /**
   * Reads the server information after connection setup. The information
   * is read from the connection's ResponseInputStream.
   */
  private void init_server_info (ResponseInputStream i) {

    int accepted = i.read_int8();
    if (accepted == 0) { System.err.println ("failed"); }
    if (accepted == 2) { System.err.println ("more auth data not yet implemented"); }

    i.skip (1); // Unused.
    i.skip (2); // protocol-major-version.
    i.skip (2); // protocol-minor-version.
    i.skip (2); // Length.

    release_no = i.read_int32 ();
    resource_base = i.read_int32 ();
    resource_mask = i.read_int32 ();
    i.skip (4); // motion-buffer-size.

    int vendor_length = i.read_int16 ();
    extended_maximum_request_length 
      = maximum_request_length = i.read_int16 ();
    int screen_count = i.read_int8 ();
    int pixmap_format_count = i.read_int8 ();

    image_byte_order = i.read_int8 ();
    bitmap_format_bit_order = i.read_int8 ();
    bitmap_format_scanline_unit = i.read_int8 ();
    bitmap_format_scanline_pad = i.read_int8 ();

    min_keycode = i.read_int8 ();
    max_keycode = i.read_int8 ();
    i.skip (4); // Unused.

    vendor = i.read_string8 (vendor_length);
    i.pad (vendor_length);

    // pixmap formats
    pixmap_formats = new Pixmap.Format [pixmap_format_count];
    for (int j = 0; j < pixmap_format_count; j++) {
      pixmap_formats [j] = new Pixmap.Format (i);
    }

    // screens

    if (default_screen_no < 0 || default_screen_no >= screen_count)    
      throw new RuntimeException ("Invalid screen number (screen-count "
        + screen_count + "): " + default_screen_no);

    screens = new Screen [screen_count];
    for (int j = 0; j < screen_count; j++) {
      screens [j] = new Screen (this, i);
      //System.err.println("screen: " + screens [j]);
    }

  }

  /**
   * Initializes the keyboard mapping.
   */
  private void init_keyboard_mapping () {
  
    input = new Input (this, min_keycode, max_keycode);
    input.keyboard_mapping ();
  }


  public Event next_event () {
    return in.read_event ();
  }

  public String toString () {
    return "#Display"
      + "\n  default-screen-number: " + default_screen_no
      + "\n  vendor: " + vendor
      + "\n  release-number: " + release_no
      + "\n  maximum-request-length: " + maximum_request_length;
  }

  /**
   * Fetches the XAuthority that matches this display.
   *
   * @return the XAuthority that matches this display
   */
  private XAuthority get_authority () {

    XAuthority[] auths = XAuthority.get_authorities();

    // Fetch hostname.
    if (hostname == null || hostname.equals("")
        ||hostname.equals ("localhost")) {
      // Translate localhost hostnames to the real hostname of this host.
      try {
        InetAddress local = InetAddress.getLocalHost ();
        hostname = local.getHostName ();
      } catch (UnknownHostException ex) {
        ex.printStackTrace();
      }
    }

    // Fetch display no.
    String display_no_str = String.valueOf (display_no);

    // Find the XAuthority that matches the hostname and display no.
    XAuthority found = null;
    for (int i = 0; i < auths.length; i++) {
      XAuthority auth = auths[i];
      try {
        if (auth.hostname != null && auth.display.equals (display_no_str)
            && InetAddress.getByName(auth.hostname)
                 .equals(InetAddress.getByName(hostname))) {
          found = auth;
          break;
        }
      } catch (UnknownHostException ex) {
         System.err.println("warning unknown host :"+auth.hostname);
      }
    }
    return found;
  }

  public void check_error () {
    // `XSync' function in `xc/lib/X11/Sync.c' uses the same technique.
    try {
      input.input_focus ();

    } catch (Error e) {
      /* When an X error occurs, Java throws an `gnu.x11.Error' exception,
       * the normal execution order is disrupted; the reply of
       * `input_focus()' resides in network buffer while nobody wants it. 
       * In case someone (`gnu.x11.test.Shape') catches the error and
       * continues to work, we should discard the input focus reply (by
       * clearing the socket input stream).
       *
       * TODO Should I be careful not to clear other packets after the
       * reply of input focus? Some event may come after that?
       */
      try {
        in.skip (in.available ());
      } catch (IOException ie) {
        throw new java.lang.Error ("Failed to clear socket input stream: " + ie);
      }

      throw e;
    }
  }

  /**
   * Initializes the input and output streams.
   */
  private void init_streams () {
    
    try {
      // TODO: Evaluate if we gain performance by using BufferedOutputStream
      // here.
      OutputStream o = socket.getOutputStream ();
      //BufferedOutputStream buf_out = new BufferedOutputStream (o, 512);
      out = new RequestOutputStream (o, this);

      // Create buffered response input stream.
      InputStream sock_in = socket.getInputStream();
      // Buffer space for 4 response messages. More are hardly needed I'd
      // think.
      BufferedInputStream buf_in = new BufferedInputStream (sock_in, 128);
      in = new ResponseInputStream (buf_in, this);
    } catch (IOException ex) {
      handle_exception (ex);
    }
  }

  public void flush () {
    synchronized (out) {
      out.send ();
      out.flush();
    }
  }

  private void handle_exception (Throwable ex) {
    ex.printStackTrace ();
  }
}
