
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
import java.util.ArrayList;
import java.util.HashMap;

import java.util.Hashtable;
import java.util.List;
import java.util.logging.Level;

/** X server connection. */
// TODO Support Multiple Screens
public class Display {

    private static java.util.logging.Logger logger;
    static {

        logger = java.util.logging.Logger.getLogger("gnu.x11.Display");
        logger.setLevel(Level.ALL);
    }

    public static final int CURRENT_TIME = 0;

    // TODO better handling debug flag
    public static final boolean DEBUG = false;

    /**
     * The output stream.
     */
    private RequestOutputStream outputStream;

    /**
     * The input stream.
     */
    private ResponseInputStream inputStream;

    /**
     * The socket.
     */
    private Socket socket;

    /**
     * The hostname to this display.
     */
    private String hostname;

    /**
     * The display number.
     */
    private int displayNumber;
    
    /**
     *  The display input (Keyboard/mouse) 
     */
    private Input input;

    /**
     * Indicates if this display is connected or not.
     */
    private boolean connected;

    // Server information
    private int releaseNumber;

    private String vendor;

    private int maximumRequestLength;

    /** Contains an array view of the Screens supported by this Display. */
    private Screen[] screens;
    
    /**
     * Contains all the visual info mapped by the ID.
     */
    private HashMap<Integer, VisualInfo> visuals = new HashMap<Integer, VisualInfo>();
       
    private Pixmap.Format[] pixmapFormats;

    private int imageByteOrder;

    private int bitmapFormatBitOrder;

    private int bitmapFormatScanlineUnit;

    private int bitmapFormatScanlinePad;

    private int resourceBase;

    private int resourceMask;

    // Defaults
    private Color defaultBlack, defaultWhite;

    private Colormap defaultColormap;

    private int defaultDepth;

    private Pixmap.Format defaultPixmapFormat;

    private Window defaultRoot;

    private Screen defaultScreen;

    private int defaultScreenNumber;

    private int minKeycode;

    private int maxKeycode;

    /**
     * @see Screen#default_gc()
     */
    private GC defaultGC;

    // Resources
    private Hashtable<Integer, Resource> resources = new Hashtable<Integer, Resource>(257);

    private int resourceIndex;

    private Hashtable<Integer, Atom> atomIDs = new Hashtable<Integer, Atom>(257);

    private Hashtable<String, Atom> atoms = new Hashtable<String, Atom>(257);

    // XCMisc
    private XCMisc xcmisc;

    private boolean useXcmisc;

    private int xcmiscResourceBase;

    private int xcmiscResourceCount;

    // Extension
    private boolean bigRequestsPresent;

    private int extendedMaximumRequestLength;

    // ScreenSaver blanking
    
    public enum ScreenSaverBlanking {
        NO(0),
        YES(1),
        DEFAULT(2);
        
        private int codeID;
        
        ScreenSaverBlanking(int opID) {
            this.codeID = opID;
        }
        
        public int getCodeID() {
            return codeID;
        }
        
        public static ScreenSaverBlanking getOption(boolean b) {
            return (b) ? ScreenSaverBlanking.NO : ScreenSaverBlanking.YES;
        }
    }
    
    public enum ScreenSaverExposures {
        NO(0),
        YES(1),
        DEFAULT(2);
        
        private int codeID;
        
        ScreenSaverExposures(int opID) {
            this.codeID = opID;
        }
        
        public int getCodeID() {
            return codeID;
        }
        
        public static ScreenSaverExposures getOption(boolean b) {
            return (b) ? ScreenSaverExposures.NO : ScreenSaverExposures.YES;
        }
    }
    
    /**
     * Major opcodes 128 through 255 are reserved for extensions, totally 128.
     */
    public String[] extensionOpcodeStrings = new String[128];

    public String[][] extensionMinorOpcodeStrings = new String[128][];

    /**
     * Event codes 64 through 127 are reserved for extensiones, totally 64.
     */
    public EventFactory[] extensionEventFactories = new EventFactory[64];

    /**
     * Error codes 128 through 255 are reserved for extensiones, totally 128.
     */
    public ErrorFactory[] extensionErrorFactories = new ErrorFactory[128];

    // <-- Constructors -->
    
    /**
     * Sets up a display using a connection over the specified <code>socket</code>.
     * This should be used when there is a need to use non-TCP sockets, like
     * connecting to an X server via Unix domain sockets. You need to provide an
     * implementation for this kind of socket though.
     * 
     * @param socket
     *            the socket to use for that connection
     * @param hostname
     *            the hostname to connect to
     * @param displayNumber
     *            the display number
     * @param screenNumber
     *            the screen number
     * @throws EscherServerConnectionException
     */
    public Display(Socket socket, String hostname, int displayNumber, int screenNumber)
            throws EscherServerConnectionException {

        setDefaultScreenNumber(screenNumber);
        setHostname(hostname);
        setDisplayNumber(displayNumber);
        setSocket(socket);
        
        init_streams();
        init();
    }

    /**
     * @throws EscherServerConnectionException
     * @see <a href="XOpenDisplay.html">XOpenDisplay</a>
     */
    public Display(String hostname, int displayNumber, int screenNumber)
            throws EscherServerConnectionException {
        
        setDefaultScreenNumber(screenNumber);
        setHostname(hostname);
        setDisplayNumber(displayNumber);
        try {
            System.out.println("Debugging");
            System.out.println(hostname + ":" + displayNumber + "." + screenNumber);
            socket = new Socket(hostname, 6000 + displayNumber);
            setSocket(socket);
        } catch (IOException ex) {
            handleException(ex);
        }
        init_streams();
        init();
    }
    
    /**
     * Performs client connection to the XServer, initialising all
     * the internal datastructures.
     * 
     * @throws EscherServerConnectionException
     */
    private void init() throws EscherServerConnectionException {

        // authorization protocol
        XAuthority xauth = getAuthority();

        byte[] authName;
        byte[] authData;
        if (xauth != null) {
            authName = xauth.getProtocolName();
            authData = xauth.getProtocolData();
        } else {
            // In case the X authority couldn't be established...
            authName = new byte[0];
            authData = new byte[0];
        }

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.writeInt8('B');
            o.writeInt8(0); // Unused.
            o.writeInt16(11);// major version
            o.writeInt16(0);// minor version
            o.writeInt16(authName.length);
            o.writeInt16(authData.length);
            o.writeInt16(0); // Unused.
            o.writeBytes(authName);
            o.writePad(authName.length);
            o.writeBytes(authData);
            o.writePad(authData.length);
            o.flush();
            ResponseInputStream i = inputStream;
            synchronized (i) {
                // Don't do read_reply() here, this is not needed and
                // doesn't work during connection setup.
                connected = true;
                initServerInfo(i);
            }
        }

        maximumRequestLength = outputStream.setBufferSize(maximumRequestLength);
        initKeyboarMapping();
        initDefaults();
        initBigRequestExtension();
    }
    
    /**
     * This method returns a list of visual information that match the
     * specified attributes given in the visual information
     * template.
     * 
     * The list can be empty if no visual match the given template.
     * 
     * If template is null, it returns a list of all the visual information
     * for the given screen, behaving like {@link #getVisualInfo()}.
     * 
     * 
     * @param screenNo
     * @param template
     * @return
     */
    public List<VisualInfo> getVisualInfo(VisualInfo template,
                                          int visualInfoMask) {
        
        List<VisualInfo> visuals = new ArrayList<VisualInfo>();
        
        // A visual is contained into a Depth object
        // and this is contained into the Screen object.
        // Xlib get the list of visuals by first checking if the user
        // query for a specific screen, and then lopping through all the depth
        // for each sreeen.
        
        Screen [] screens = null;
        if ((visualInfoMask & VisualInfoMask.VisualScreenMask) != 0) {
            
            screens = new Screen[1];
            screens[0] = template.getScreen();
        } else {
           
            screens = getScreens();
        }
        
        for (Screen screen : screens) {
            for (Depth depth : screen.getDepths()) {
                
                // Roman, this is why I hate the sun coding conventions
                // of 4 space tabs
                if (((visualInfoMask & VisualInfoMask.VisualDepthMask) != 0)
                        && (template.getDepth() == depth.getDepth()))
                    continue;

                for (VisualInfo visual : depth.getVisuals()) {
                    
                    if (((visualInfoMask & VisualInfoMask.VisualIDMask) != 0)
                            && (template.getID() != visual.getID())) 
                        continue;
                    
                    if (((visualInfoMask & VisualInfoMask.VisualClassMask) != 0)
                            && (!template.getVisualClass().
                                    equals(visual.getVisualClass())))
                        continue;
                    
                    if (((visualInfoMask & VisualInfoMask.VisualRedMaskMask)
                                != 0)
                            && (template.getRedMask() != visual.getRedMask()))
                        continue;
                    
                    if (((visualInfoMask & VisualInfoMask.VisualGreenMaskMask)
                                != 0)
                            && (template.getGreenMask()
                                    != visual.getGreenMask()))
                        continue;
                    
                    if (((visualInfoMask & VisualInfoMask.VisualBlueMaskMask)
                                != 0)
                            && (template.getBlueMask() != visual.getBlueMask()))
                        continue;
                    
                    if (((visualInfoMask & VisualInfoMask.VisualBlueMaskMask)
                                != 0)
                            && (template.getBlueMask() != visual.getBlueMask()))
                        continue;
                    
                    if (((visualInfoMask & VisualInfoMask.VisualBlueMaskMask)
                                != 0)
                            && (template.getBlueMask() != visual.getBlueMask()))
                        continue;
                
                    if (((visualInfoMask & 
                            VisualInfoMask.VisualColormapSizeMask)
                                != 0)
                            && (template.getColormapEntries()
                                    != visual.getColormapEntries()))
                      continue;
                  
                    if (((visualInfoMask & VisualInfoMask.VisualBitsPerRGBMask)
                                != 0)
                           && (template.getBitsPerRGBValue()
                                   != visual.getBitsPerRGBValue()))
                      continue;
                  
                  visuals.add(visual);
                }
            }
        }
        
        return visuals;
    }

    /**
     * Get all screens associated with this display
     * @return a array of {@link Screen}s
     */
    public Screen[] getScreens() {

        return this.screens;
    }

    /**
     * Returns a list of all the Visuals supported by this Display.
     * @return
     */
    public List<VisualInfo> getVisualInfo() {
        
        return this.getVisualInfo(null, VisualInfoMask.VisualNoMask);
    }
    
    /**
     * Returns a list of all the Visuals supported by this Display that
     * match this template.
     * 
     * The default mask is {@code VisualInfoMask.VisualNoMask}.
     */
    public List<VisualInfo> getVisualInfo(VisualInfo template) {
        
        return this.getVisualInfo(template, template.getVisualInfoMask());
    }
    
    /**
     * Return a single Visual associated to the given id. This is
     * an optimized version of {@link #getVisualInfo(VisualInfo, int)},
     * when the id is known in advance. This method may return {@code null},
     * as opposed to the other {@code getVisualInfo()}, which always
     * return at least a List with no entries.
     */
    public VisualInfo getVisualInfo(int id) {
        
        return this.visuals.get(id);
    }
    
    /**
     * Return the default VisualInfo of the given screen.
     */
    public VisualInfo getDefaultVisual(Screen screen) {
     
        return this.getVisualInfo(screen.rootVisualID());
    }
    
    /**
     * Return the default VisualInfo of the default screen.
     */
    public VisualInfo getDefaultVisual() {
     
        return this.getVisualInfo(this.defaultScreen.rootVisualID());
    }
    
    // opcode 23 - get selection owner
    /**
     * @see <a href="XGetSelectionOwner.html">XGetSelectionOwner</a>
     */
    public Window selectionOwner(Atom selection) {

        RequestOutputStream o = outputStream;
        int owner_id = -1;
        synchronized (o) {
            o.beginRequest(23, 0, 2);
            o.writeInt32(selection.getID());
            ResponseInputStream i = inputStream;
            synchronized (i) {
                i.readReply(o);
                i.skip(8);
                owner_id = i.readInt32();
                i.skip(20);
            }
        }
        return (Window) Window.intern(this, owner_id);
    }

    // opcode 36 - grab server
    public synchronized void grabServer() {

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(36, 0, 1);
            o.send();
        }
    }

    // opcode 37 - ungrab server
    public void ungrabServer() {

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(37, 0, 1);
            o.send();
        }
    }

    // opcode 49 - list fonts
    /**
     * @return valid: {@link Enum#next()} of type {@link Font},
     *         {@link Enum#next_string()}
     * @see <a href="XListFonts.html">XListFonts</a>
     */
    public Font[] fonts(String pattern, int maxNameCount) {

        int n = pattern.length();
        int p = RequestOutputStream.pad(n);

        RequestOutputStream o = outputStream;
        Font[] fonts = null;
        synchronized (o) {
            o.beginRequest(49, 0, 2 + (n + p) / 4);
            o.writeInt16(maxNameCount);
            o.writeInt16(n);
            o.writeString8(pattern);
            o.skip(p);

            ResponseInputStream i = inputStream;
            synchronized (i) {
                i.readReply(o);
                i.skip(4);
                int len = i.readInt32() * 4; // Number of bytes for the reply.
                int numStrings = i.readInt16();
                i.skip(22);
                fonts = new Font[numStrings];
                for (int j = 0; j < numStrings; j++) {
                    int strlen = i.readInt8();
                    String str = i.readString8(strlen);
                    len -= strlen + 1;
                    fonts[j] = new Font(this, str);
                }
                i.skip(len); // Pad the remaining bytes.
            }
        }
        return fonts;
    }

    // opcode 50 - list fonts with info
    /**
     * @see <a href="XListFontsWithInfo.html">XListFontsWithInfo</a>
     */
    public Data fontsWithInfo(String pattern, int maxNameCount) {

        // FIXME: Implement.
        return null;
    }

    // opcode 51 - set font path
    /**
     * @see <a href="XSetFontPath.html">XSetFontPath</a>
     */
    public void setFontPath(int count, String[] path) {

        int n = 0;
        // TODO: Verify this
        for (int i = 0; i < path.length; i++) {
            n += path.length + 1;
        }
        int p = RequestOutputStream.pad(n);

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(51, 0, 2 + (n + p) / 4);
            o.writeInt16(path.length);
            o.skip(2);
            for (String ph : path) {
                o.writeInt8(ph.length());
                o.writeString8(ph);
            }
            o.skip(p);
            o.send();
        }
    }

    // opcode 52 - get font path
    /**
     * Returns the current search path for fonts.
     * 
     * @return the current search path for fonts
     * @see #setFontPath(int, String[])
     * @see <a href="XGetFontPath.html">XGetFontPath</a>
     */
    public String[] fontPath() {

        RequestOutputStream o = outputStream;
        String[] path;
        synchronized (o) {
            o.beginRequest(52, 0, 1);
            ResponseInputStream i = inputStream;
            synchronized (i) {
                i.readReply(o);
                i.skip(4);
                int reply_length = i.readInt32() * 4;
                int num_strings = i.readInt16();
                i.skip(22);
                path = new String[num_strings];
                int bytes_read = 0;
                for (int j = 0; j < num_strings; j++) {
                    int num_chars = i.readInt8();
                    path[j] = i.readString8(num_chars);
                    bytes_read += num_chars + 1;
                }
                i.skip(reply_length - bytes_read);
            }
        }
        return path;
    }

    /**
     * Information about an X extension.
     * 
     * @see Display#query_extension .
     */
    public static class ExtensionInfo {

        private boolean present;

        private int majorOpcode;

        private int firstEvent;

        private int firstError;

        ExtensionInfo(ResponseInputStream in) {

            present = in.readBool();
            majorOpcode = in.readInt8();
            firstEvent = in.readInt8();
            firstError = in.readInt8();
            // System.err.println("first error: " + first_error);
            // Thread.dumpStack ();
        }

        public boolean present() {

            return present;
        }

        public int majorOpcode() {

            return majorOpcode;
        }

        public int firstEvent() {

            return firstEvent;
        }

        public int firstError() {

            return firstError;
        }
    }

    // opcode 98 - query extension
    /**
     * Determines if the named extension is present. If so, the major opcode for
     * the extension is returned, if it has one. Otherwise zero is returned. Any
     * minor opcode or the request formats are specific to the extension. If the
     * extension involves additional event types, the base event type code is
     * returned. Otherwise zero is returned. The format of the events is specific
     * to the extension. If the extension involves additional error codes, the
     * base error code is returned. The format of additional data in the errors is
     * specific to the extension. The name should use ISO-Latin1 encoding, and
     * uppercase and lowercase do matter.
     * 
     * @param name
     *            the name of the extension to query
     * @return
     * @see <a href="XQueryExtension.html">XQueryExtension</a>
     */
    public ExtensionInfo queryExtension(String name) {

        int n = name.length();
        int p = RequestOutputStream.pad(n);

        ExtensionInfo info;
        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(98, 0, 2 + (n + p) / 4);
            o.writeInt16(n);
            o.skip(2);
            o.writeString8(name);
            o.skip(p);
            ResponseInputStream i = inputStream;
            synchronized (i) {
                i.readReply(o);
                i.skip(8);
                info = new ExtensionInfo(i);
                i.skip(20);
            }
        }
        return info;
    }

    // opcode 99 - list extensions
    /**
     * Returns a list of all extensions supported by the server.
     * 
     * @return a list of all extensions supported by the server
     * @see <a href="XListExtensions.html">XListExtensions</a>
     */
    public String[] extensions() {

        String[] exts;
        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(99, 9, 1);
            ResponseInputStream i = inputStream;
            synchronized (i) {
                i.readReply(o);
                i.skip(1);
                int num_strs = i.readInt8();
                i.skip(2);
                int reply_length = i.readInt32() * 4;
                exts = new String[num_strs];
                i.skip(24);
                int bytes_read = 0;
                for (int j = 0; j < num_strs; j++) {
                    int len = i.readInt8();
                    exts[j] = i.readString8(len);
                    bytes_read += len + 1;
                }
                i.skip(reply_length - bytes_read);
            }
        }
        return exts;
    }

    // opcode 104 - bell
    /**
     * Rings the bell on the keyboard at a volume relative to the base volume of
     * the keyboard, if possible. Percent can range from -100 to +100 inclusive
     * (or a Value error results). The volume at which the bell is rung when
     * percent is nonnegative is: base - [(base * percent) / 100] + percent When
     * percent is negative, it is: base + [(base * percent) / 100]
     * 
     * @param volume,
     *            see above
     * @see <a href="XBell.html">XBell</a>
     */
    public void bell(int percent) {

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(104, percent, 1);
            o.send();
        }
    }

    

    // opcode 107 - set screen saver
    /**
     * @param preferBlanking
     *            valid: {@link #NO}, {@link #YES}, {@link #DEFAULT}
     * @param allowExposures
     *            valid: {@link #NO}, {@link #YES}, {@link #DEFAULT}
     * @see <a href="XSetScreenSaver.html">XSetScreenSaver</a>
     */
    public void setScreenSaver(int timeout, int interval,
                                 ScreenSaverBlanking preferBlanking,
                                 ScreenSaverExposures allowExposures) {

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(107, 0, 3);
            o.writeInt16(timeout);
            o.writeInt16(interval);
            o.writeInt8(preferBlanking.getCodeID());
            o.writeInt8(allowExposures.getCodeID());
            o.skip(2);
            o.send();
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

        private ScreenSaverBlanking preferBlanking;

        private ScreenSaverExposures allowExposures;

        ScreenSaverInfo(ResponseInputStream in) {

            timeout = in.readInt16();
            interval = in.readInt16();
            preferBlanking = ScreenSaverBlanking.getOption(in.readBool()); 
            allowExposures = ScreenSaverExposures.getOption(in.readBool()); 
        }

        public int timeout() {

            return timeout;
        }

        public int interval() {

            return interval;
        }

        public ScreenSaverBlanking preferBlanking() {

            return preferBlanking;
        }

        public ScreenSaverExposures allowExposures() {

            return allowExposures;
        }

        public String toString() {

            return "#ScreenSaverReply" + "\n  timeout: " + timeout()
                            + "\n  interval: " + interval()
                            + "\n  prefer-blanking: " + preferBlanking()
                            + "\n  allow-exposures: " + allowExposures();
        }
    }

    // opcode 108 - get screen saver
    /**
     * Returns the screensaver control values.
     * 
     * @return the screensaver control values
     * @see <a href="XGetScreenSaver.html">XGetScreenSaver</a>
     */
    public ScreenSaverInfo screenSaver() {

        ScreenSaverInfo info;
        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(108, 0, 1);
            ResponseInputStream i = inputStream;
            synchronized (i) {
                i.readReply(o);
                i.skip(8);
                info = new ScreenSaverInfo(i);
                i.skip(18);
            }
        }
        return info;
    }

    // opcode 109 - change hosts
    /**
     * @param mode
     *            valid: {@link #INSERT}, {@link #DELETE}
     * @see <a href="XAddHost.html">XAddHost</a>
     * @see <a href="XRemoveHost.html">XRemoveHost</a>
     */
    public void changeHosts(Host.ChangeOperation mode, Host.InternetFamily family, byte[] host) {

        int n = host.length;
        int p = RequestOutputStream.pad(n);

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(109, mode.getCode(), 2 + (n + p) / 4);
            o.writeInt8(family.getCode());
            o.skip(1);
            o.writeInt16(n);
            o.writeBytes(host);
            o.skip(p);
            o.send();
        }
    }


    // opcode 110 - list hosts
    /**
     * Returns the hosts currently on the access control list and whether use of
     * the list at connection setup is currently enabled or disabled.
     * 
     * @see <a href="XListHosts.html">XListHosts</a>
     */
    public HostsInfo listHosts() {

        HostsInfo info;
        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(110, 0, 1);
            ResponseInputStream i = inputStream;
            synchronized (i) {
                i.readReply(o);
                i.skip(1);
                info = new HostsInfo(i);
            }
        }
        return info;
    }

    // opcode 111 - set access control
    /**
     * @param mode
     *            valid: {@link #ENABLE}, {@link #DISABLE}
     * @see <a href="XSetAccessControl.html">XSetAccessControl</a>
     */
    public void setAccessControl(Host.AccessControl mode) {

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(111, mode.getCode(), 1);
            o.send();
        }
    }

    // opcode 113 - kill client
    /**
     * @see <a href="XKillClient.html">XKillClient</a>
     */
    public void killClient(Resource resource) {

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(113, 0, 2);
            o.writeInt32(resource.id);
            o.send();
        }
    }

    // opcode 112 - set close down mode
    /**
     * @param mode
     *            valid: {@link #DESTROY}, {@link #RETAIN_PERMANENT},
     *            {@link #RETAIN_TEMPORARY}
     * @see <a href="XSetCloseDownMode.html">XSetCloseDownMode</a>
     */
    public void setCloseDownMode(Host.Shape mode) {

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(112, mode.getCode(), 1);
            o.send();
        }
    }

    // opcode 115 - force screen saver
    /**
     * @param mode
     *            valid: {@link #ACTIVATE}, {@link #RESET}
     * @see <a href="XForceScreenSaver.html">XForceScreenSaver</a>
     */
    public void forceScreenSaver(Host.ForceScreenSaver mode) {

        RequestOutputStream o = outputStream;
        synchronized (o) {
            o.beginRequest(115, mode.getCode(), 1);
            o.send();
        }
    }

    /**
     * From XC-MISC extension specification: When an X client connects to an X
     * server, it receives a fixed range of resource IDs to use to identify the
     * client's resources inside the X server. Xlib hands these out sequentially
     * as needed. When it overruns the end of the range, an IDChoice protocol
     * error results. Long running clients, or clients that use resource IDs at
     * a rapid rate, may encounter this circumstance. When it happens, there are
     * usually many resource IDs available, but Xlib doesn't know about them.
     * One approach to solving this problem would be to have Xlib notice when a
     * resource is freed and recycle its ID for future use. This strategy runs
     * into difficulties because sometimes freeing one resource causes others to
     * be freed (for example, when a window is destroyed, so are its children).
     * To do a complete job, Xlib would have to maintain a large amount of state
     * that currently resides only in the server (the entire window tree in the
     * above example). Even if a less comprehensive strategy was adopted, such
     * as recycling only those IDs that Xlib can identify without maintaining
     * additional state, the additional bookkeeping at resource creation and
     * destruction time would likely introduce unacceptable overhead. To avoid
     * the problems listed above, the server's complete knowledge of all
     * resource IDs in use by a client is leveraged. This extension provides two
     * ways for Xlib to query the server for available resource IDs. Xlib can
     * use these extension requests behind the scenes when it has exhausted its
     * current pool of resource IDs.
     */
    public int allocateID(Object object) {

        /*
         * If XC-MISC is present, we use it. Otherwise, we fall back to allocate X
         * resource ID sequentially to the end without recycling ID (just as xlib
         * does). Sample values: resource base: 0x04000000 or
         * 00000100000000000000000000000000b resource mask: 0x003FFFFF or
         * 00000000001111111111111111111111b
         */

        if (!useXcmisc)
            // check if basic allocation fails
            useXcmisc = (resourceIndex + 1 & ~resourceMask) != 0;

        if (!useXcmisc) {
            int id = resourceIndex++ | resourceBase;
            resources.put(id, (Resource) object);
            return id;
        }

        if (xcmisc == null) {
            try {
                xcmisc = new XCMisc(this);
            } catch (NotFoundException e) {
                throw new RuntimeException("Failed to allocate new resource id");
            }
        }

        if (xcmiscResourceCount == 0) {
            // first time, or used up
            gnu.x11.extension.XCMisc.XIDRange rr = xcmisc.xidRange();
            xcmiscResourceBase = rr.startID;
            xcmiscResourceCount = rr.count;
        }

        // give out in descending order
        xcmiscResourceCount--;
        return xcmiscResourceBase + xcmiscResourceCount;
    }

    /**
     * @see <a href="XCloseDisplay.html">XCloseDisplay</a>
     */
    public void close() {

        // FIXME: Implement more sensible shutdown.
        try {
            inputStream.close();
            outputStream.close();
            socket.close();
        } catch (IOException ex) {
            handleException(ex);
        }
        connected = false;
    }

    /**
     * From Big Requests extension specification: It is desirable for core Xlib,
     * and other extensions, to use this extension internally when necessary. It
     * is also desirable to make the use of this extension as transparent as
     * possible to the X client. For example, if enabling of the extension were
     * delayed until the first time it was needed, an application that used
     * XNextRequest to determine the sequence number of a request would no longer
     * get the correct sequence number. As such, XOpenDisplay will determine if
     * the extension is supported by the server and, if it is, enable
     * extended-length encodings. The core Xlib functions XDrawLines, XDrawArcs,
     * XFillPolygon, XChangeProperty, XSetClipRectangles, and XSetRegion are
     * required to use extended-length encodings when necessary, if supported by
     * the server. Use of extended-length encodings in other core Xlib functions
     * (XDrawPoints, XDrawRectangles, XDrawSegments, XFillArcs, XFillRectangles,
     * XPutImage) is permitted but not required; an Xlib implementation may choose
     * to split the data across multiple smaller requests instead.
     */
    public void initBigRequestExtension() {

        try {
            BigRequests big = new BigRequests(this);
            bigRequestsPresent = true;
            extendedMaximumRequestLength = big.enable();

        } catch (NotFoundException e) {
            bigRequestsPresent = false;
        }
    }

    public void initDefaults() {

        defaultScreen = screens[defaultScreenNumber];
        defaultRoot = defaultScreen.root(); // before init default_gc
        defaultDepth = defaultScreen.getRootDepth();
        defaultColormap = defaultScreen.defaultColormap();
        defaultGC = defaultScreen.defaultGC();
        defaultBlack = new Color(defaultScreen.getBlackPixel());
        defaultWhite = new Color(defaultScreen.getWhitePixel());

        for (Pixmap.Format px : pixmapFormats) {
            if (px.getDepth() == defaultDepth) {
                defaultPixmapFormat = px;
                break;
            }
        }
    }

    /**
     * Reads the server information after connection setup. The information is
     * read from the connection's ResponseInputStream.
     * 
     * @throws EscherServerConnectionException
     */
    private void initServerInfo(ResponseInputStream i) throws EscherServerConnectionException {

        int accepted = i.readInt8();
        boolean connectionFailed = false;
        StringBuilder debugMessage = new StringBuilder();
        switch (accepted) {
        
        case 0:
            debugMessage.append("Connection to the XServer failed.\n");
            debugMessage.append("Try to set DISPLAY variable or to give " +
                                "proper\n");
            debugMessage.append("permissions (eg. edit .Xauthority or " +
                                "run \"xhost +\")\n");
            debugMessage.append("NOTE: leaving \"xhost +\" and allowing " +
                                "remote tcp\n");
            debugMessage.append("connections to the XServer can be a " +
                                "potential\n");
            debugMessage.append("security risk.\n");

            connectionFailed = true;

            break;
        
        case 1:
            logger.warning("more auth data not yet implemented");
            break;

        default:
            logger.warning("init_server_info::Unknown server reply!");
            break;
        }

        int failedLength = 0;
        if (connectionFailed) {
            // length of reason
            failedLength = i.readByte();

        } else {
            // Unused.
            i.skip(1);
        }

        i.skip(2); // protocol-major-version.
        i.skip(2); // protocol-minor-version.
        i.skip(2); // Length.

        // try to print some more (maybe not so) meaningful messages to
        // understand the failure
        if (connectionFailed) {

            if (DEBUG) {

                String codes = (failedLength > 1 ? "codes" : "code");

                debugMessage.append("XServer returned " + failedLength +
                                    " error ");
                debugMessage.append(codes);

                int reason = 0;
                for (int n = 0; n < failedLength; n++) {
                    reason = i.readInt32();

                    debugMessage.append("XServer returned error code: " +
                                        reason + "\n");
                }

                debugMessage.append("XServer are allowed to use non " +
                                     "standard errors codes\n");
                debugMessage.append("Please, consult the manual of your " +
                                    "XServer\n" +
                                    "to map the error codes to human " +
                                    "readable values.\n");
            }

            logger.severe(debugMessage.toString());

            throw new EscherServerConnectionException("Connection to the " +
                                                      "XServer failed.");
        }

        releaseNumber = i.readInt32();
        resourceBase = i.readInt32();
        resourceMask = i.readInt32();
        i.skip(4); // motion-buffer-size.

        int vendorLength = i.readInt16();
        maximumRequestLength = i.readInt16();
        extendedMaximumRequestLength = maximumRequestLength;
        
        int screen_count = i.readInt8();
        int pixmap_format_count = i.readInt8();

        imageByteOrder = i.readInt8();
        
        bitmapFormatBitOrder = i.readInt8();
        bitmapFormatScanlineUnit = i.readInt8();
        bitmapFormatScanlinePad = i.readInt8();

        minKeycode = i.readInt8();
        maxKeycode = i.readInt8();
        i.skip(4); // Unused.

        vendor = i.readString8(vendorLength);
        i.pad(vendorLength);

        /* ***** FORMAT ***** */
        pixmapFormats = new Pixmap.Format[pixmap_format_count];
        for (int j = 0; j < pixmap_format_count; j++) {
            pixmapFormats[j] = new Pixmap.Format(i);
        }

        /* ***** SCREEN ***** */
        if (defaultScreenNumber < 0 || defaultScreenNumber >= screen_count)
            throw new RuntimeException("Invalid screen number (screen-count "
                            + screen_count + "): " + defaultScreenNumber);

        screens = new Screen[screen_count];
        for (int j = 0; j < screen_count; j++) {
            screens[j] = new Screen(i, this);
        }

    }

    /**
     * Initializes the keyboard mapping.
     */
    private void initKeyboarMapping() {

        input = new Input(this, minKeycode, maxKeycode);
        input.keyboardMapping();
    }

    public Event nextEvent() {

        return inputStream.readEvent();
    }

    public String toString() {

        return "#Display" + "\n  default-screen-number: " + defaultScreenNumber
                        + "\n  vendor: " + vendor + "\n  release-number: "
                        + releaseNumber + "\n  maximum-request-length: "
                        + maximumRequestLength;
    }

    /**
     * Fetches the XAuthority that matches this display.
     * 
     * @return the XAuthority that matches this display
     */
    private XAuthority getAuthority() {

        XAuthority[] auths = XAuthority.getAuthorities();

        // Fetch hostname.
        if (hostname == null || hostname.equals("")
                        || hostname.equals("localhost")) {
            // Translate localhost hostnames to the real hostname of this host.
            try {
                InetAddress local = InetAddress.getLocalHost();
                hostname = local.getHostName();
            } catch (UnknownHostException ex) {
                ex.printStackTrace();
            }
        }

        // Fetch display no.
        String displayNo = String.valueOf(displayNumber);

        // Find the XAuthority that matches the hostname and display no.
        XAuthority found = null;
        for (int i = 0; i < auths.length; i++) {
            XAuthority auth = auths[i];
            try {
                if (auth.getHostname() != null
                    && auth.getDisplayNumber().equals(displayNo)
                    && InetAddress.getByName(auth.getHostname()).equals(
                       InetAddress.getByName(hostname))) {
                    found = auth;
                    break;
                }
            } catch (UnknownHostException ex) {
                System.err.println("warning unknown host :" + auth.getHostname());
            }
        }
        return found;
    }

    public void check_error() {

        // `XSync' function in `xc/lib/X11/Sync.c' uses the same technique.
        try {
            input.inputFocus();

        } catch (Error e) {
            /*
             * When an X error occurs, Java throws an `gnu.x11.Error' exception, the
             * normal execution order is disrupted; the reply of `input_focus()'
             * resides in network buffer while nobody wants it. In case someone
             * (`gnu.x11.test.Shape') catches the error and continues to work, we
             * should discard the input focus reply (by clearing the socket input
             * stream). TODO Should I be careful not to clear other packets after
             * the reply of input focus? Some event may come after that?
             */
            try {
                inputStream.skip(inputStream.available());
            } catch (IOException ie) {
                throw new java.lang.Error(
                                          "Failed to clear socket input stream: "
                                                          + ie);
            }

            throw e;
        }
    }

    /**
     * Initializes the input and output streams.
     */
    private void init_streams() {

        String _debug = System.getProperty("escher.debug_streams", null);

        try {
            // TODO: Evaluate if we gain performance by using BufferedOutputStream
            // here.
            OutputStream o = socket.getOutputStream();
            //BufferedOutputStream buf_out = new BufferedOutputStream (o, 512);
            if (_debug != null)
                outputStream = new DebugRequestOutputStream(o, this);
            else
                outputStream = new RequestOutputStream(o, this);

            // Create buffered response input stream.
            InputStream sock_in = socket.getInputStream();
            // Buffer space for 4 response messages. More are hardly needed I'd
            // think.
            BufferedInputStream buf_in = new BufferedInputStream(sock_in, 128);
            inputStream = new ResponseInputStream(buf_in, this);
            
        } catch (IOException ex) {
            handleException(ex);
        }
    }

    public void flush() {

        synchronized (outputStream) {
            outputStream.send();
            outputStream.flush();
        }
    }

   
    public Window getRootWindow() {

        return defaultRoot;
    }
    
    private void handleException(Throwable ex) {

        ex.printStackTrace();
    }
    
    // Accessors
    
    synchronized void addVisual(VisualInfo xVisual) {
        
        this.visuals.put(xVisual.getID(), xVisual);
    }
    
    synchronized void addAtom(int id, Atom atom) {
        this.atomIDs.put(id, atom);
    }
    
    synchronized void addAtom(String name, Atom atom) {
        this.atoms.put(name, atom);
    }
    
    synchronized Atom getAtom(int id) {
        return atomIDs.get(id);
    }
    
    synchronized Atom getAtom(String name) {
        return atomIDs.get(name);
    }    
    
    public ResponseInputStream getResponseInputStream() {

        return inputStream;
    }

    public RequestOutputStream getResponseOutputStream() {

        return outputStream;
    }

    // Gets and Set's    

    public Socket getSocket() {
    
        return socket;
    }

    
    public String getHostname() {
    
        return hostname;
    }

    
    public int getDisplayNumber() {
    
        return displayNumber;
    }

    
    public Input getInput() {
    
        return input;
    }

    
    public boolean isConnected() {
    
        return connected;
    }

    
    public int getReleaseNumber() {
    
        return releaseNumber;
    }

    
    public String getVendor() {
    
        return vendor;
    }

    
    public int getMaximumRequestLength() {
    
        return maximumRequestLength;
    }

    
    public HashMap<Integer, VisualInfo> getVisuals() {
    
        return visuals;
    }

    
    public Pixmap.Format[] getPixmapFormats() {
    
        return pixmapFormats;
    }

    
    public int getImageByteOrder() {
    
        return imageByteOrder;
    }

    
    public int getBitmapFormatBitOrder() {
    
        return bitmapFormatBitOrder;
    }

    
    public int getBitmapFormatScanlineUnit() {
    
        return bitmapFormatScanlineUnit;
    }

    
    public int getBitmapFormatScanlinePad() {
    
        return bitmapFormatScanlinePad;
    }

    
    public int getResourceBase() {
    
        return resourceBase;
    }

    
    public int getResourceMask() {
    
        return resourceMask;
    }

    
    public Color getDefaultBlack() {
    
        return defaultBlack;
    }

    
    public Color getDefaultWhite() {
    
        return defaultWhite;
    }

    
    public Colormap getDefaultColormap() {
    
        return defaultColormap;
    }

    
    public int getDefaultDepth() {
    
        return defaultDepth;
    }

    
    public Pixmap.Format getDefaultPixmapFormat() {
    
        return defaultPixmapFormat;
    }

    
    public Window getDefaultRoot() {
    
        return defaultRoot;
    }

    
    public Screen getDefaultScreen() {
    
        return defaultScreen;
    }

    
    public int getDefaultScreenNumber() {
    
        return defaultScreenNumber;
    }

    
    public int getMinKeycode() {
    
        return minKeycode;
    }

    
    public int getMaxKeycode() {
    
        return maxKeycode;
    }

    
    public GC getDefaultGC() {
    
        return defaultGC;
    }

    
    public Hashtable<Integer, Resource> getResources() {
    
        return resources;
    }

    
    public int getResourceIndex() {
    
        return resourceIndex;
    }

    
    public XCMisc getXcmisc() {
    
        return xcmisc;
    }

    
    public boolean isUseXcmisc() {
    
        return useXcmisc;
    }

    
    public int getXcmiscResourceBase() {
    
        return xcmiscResourceBase;
    }

    
    public int getXcmiscResourceCount() {
    
        return xcmiscResourceCount;
    }

    
    public boolean isBigRequestsPresent() {
    
        return bigRequestsPresent;
    }

    
    public int getExtendedMaximumRequestLength() {
    
        return extendedMaximumRequestLength;
    }

    
    public String[] getExtensionOpcodeStrings() {
    
        return extensionOpcodeStrings;
    }

    
    public String[][] getExtensionMinorOpcodeStrings() {
    
        return extensionMinorOpcodeStrings;
    }

    
    public EventFactory[] getExtensionEventFactories() {
    
        return extensionEventFactories;
    }

    
    public ErrorFactory[] getExtensionErrorFactories() {
    
        return extensionErrorFactories;
    }

    
    public void setSocket(Socket socket) {
    
        this.socket = socket;
    }

    
    public void setHostname(String hostname) {
    
        this.hostname = hostname;
    }

    
    public void setDisplayNumber(int displayNumber) {
    
        this.displayNumber = displayNumber;
    }

    
    public void setInput(Input input) {
    
        this.input = input;
    }

    
    public void setConnected(boolean connected) {
    
        this.connected = connected;
    }

    
    public void setReleaseNumber(int releaseNumber) {
    
        this.releaseNumber = releaseNumber;
    }

    
    public void setVendor(String vendor) {
    
        this.vendor = vendor;
    }

    
    public void setMaximumRequestLength(int maximumRequestLength) {
    
        this.maximumRequestLength = maximumRequestLength;
    }

    
    public void setScreens(Screen[] screens) {
    
        this.screens = screens;
    }

    
    public void setVisuals(HashMap<Integer, VisualInfo> visuals) {
    
        this.visuals = visuals;
    }

    
    public void setPixmapFormats(Pixmap.Format[] pixmapFormats) {
    
        this.pixmapFormats = pixmapFormats;
    }

    
    public void setImageByteOrder(int imageByteOrder) {
    
        this.imageByteOrder = imageByteOrder;
    }

    
    public void setBitmapFormatBitOrder(int bitmapFormatBitOrder) {
    
        this.bitmapFormatBitOrder = bitmapFormatBitOrder;
    }

    
    public void setBitmapFormatScanlineUnit(int bitmapFormatScanlineUnit) {
    
        this.bitmapFormatScanlineUnit = bitmapFormatScanlineUnit;
    }

    
    public void setBitmapFormatScanlinePad(int bitmapFormatScanlinePad) {
    
        this.bitmapFormatScanlinePad = bitmapFormatScanlinePad;
    }

    
    public void setResourceBase(int resourceBase) {
    
        this.resourceBase = resourceBase;
    }

    
    public void setResourceMask(int resourceMask) {
    
        this.resourceMask = resourceMask;
    }

    
    public void setDefaultBlack(Color defaultBlack) {
    
        this.defaultBlack = defaultBlack;
    }

    
    public void setDefaultWhite(Color defaultWhite) {
    
        this.defaultWhite = defaultWhite;
    }

    
    public void setDefaultColormap(Colormap defaultColormap) {
    
        this.defaultColormap = defaultColormap;
    }

    
    public void setDefaultDepth(int defaultDepth) {
    
        this.defaultDepth = defaultDepth;
    }

    
    public void setDefaultPixmapFormat(Pixmap.Format defaultPixmapFormat) {
    
        this.defaultPixmapFormat = defaultPixmapFormat;
    }

    
    public void setDefaultRoot(Window defaultRoot) {
    
        this.defaultRoot = defaultRoot;
    }

    
    public void setDefaultScreen(Screen defaultScreen) {
    
        this.defaultScreen = defaultScreen;
    }

    
    public void setDefaultScreenNumber(int defaultScreenNumber) {
    
        this.defaultScreenNumber = defaultScreenNumber;
    }

    
    public void setMinKeycode(int minKeycode) {
    
        this.minKeycode = minKeycode;
    }

    
    public void setMaxKeycode(int maxKeycode) {
    
        this.maxKeycode = maxKeycode;
    }

    
    public void setDefaultGC(GC defaultGC) {
    
        this.defaultGC = defaultGC;
    }

    
    public void setResources(Hashtable<Integer, Resource> resources) {
    
        this.resources = resources;
    }

    
    public void setResourceIndex(int resourceIndex) {
    
        this.resourceIndex = resourceIndex;
    }

    
    public void setXcmisc(XCMisc xcmisc) {
    
        this.xcmisc = xcmisc;
    }

    
    public void setUseXcmisc(boolean useXcmisc) {
    
        this.useXcmisc = useXcmisc;
    }

    
    public void setXcmiscResourceBase(int xcmiscResourceBase) {
    
        this.xcmiscResourceBase = xcmiscResourceBase;
    }

    
    public void setXcmiscResourceCount(int xcmiscResourceCount) {
    
        this.xcmiscResourceCount = xcmiscResourceCount;
    }

    
    public void setBigRequestsPresent(boolean bigRequestsPresent) {
    
        this.bigRequestsPresent = bigRequestsPresent;
    }

    
    public void setExtendedMaximumRequestLength(int extendedMaximumRequestLength) {
    
        this.extendedMaximumRequestLength = extendedMaximumRequestLength;
    }

    
    public void setExtensionOpcodeStrings(String[] extensionOpcodeStrings) {
    
        this.extensionOpcodeStrings = extensionOpcodeStrings;
    }

    
    public void setExtensionMinorOpcodeStrings(String[][] extensionMinorOpcodeStrings) {
    
        this.extensionMinorOpcodeStrings = extensionMinorOpcodeStrings;
    }

    
    public void setExtensionEventFactories(EventFactory[] extensionEventFactories) {
    
        this.extensionEventFactories = extensionEventFactories;
    }

    
    public void setExtensionErrorFactories(ErrorFactory[] extensionErrorFactories) {
    
        this.extensionErrorFactories = extensionErrorFactories;
    }
    
    
}
