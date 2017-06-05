package gnu.x11;

import static gnu.x11.Error.ErrorCode.BAD_COLORMAP;

import java.util.HashMap;
import java.util.Map;


/**
 * X server core error.
 *
 * This class is based on <code>Error</code> instead of
 * <code>Exception</code> or <code>RuntimeException</code> because X server
 * error should be regarded as fatal.
 *
 * Instead of setting error handler with <code>XSetErrorHandler</code> in C
 * programming, we should utilize the exception facility of Java
 * programming.
 */
public class Error extends java.lang.Error {

  public ErrorCode code;
  public int seq_no, bad, minor_opcode, major_opcode;
  
  /**
   * This enum represents an Error Code from a request with X server.
   */
  public enum ErrorCode {      
    SUCCESS(0, "SUCCESS: everything okay"),

    /**
     * The major or minor opcode does not specify a valid request. This usually
     * is an Xlib or server error.
     */
    BAD_REQUEST(1, "BAD_REQUEST: bad request code"),

    /**
     * Some numeric value falls outside of the range of values accepted by the
     * request. Unless a specific range is specified for an argument, the full
     * range defined by the argument's type is accepted. Any argument defined
     * as a set of alternatives typically can generate this error (due to the
     * encoding).
     */
    BAD_VALUE(2, "BAD_VALUE: parameter out of range"),
     
    /**
     * A value for a window argument does not name a defined window.
     */
    BAD_WINDOW(3, "BAD_WINDOW: parameter not a window"),

    /**
     * A value for a pixmap argument does not name a defined pixmap.
     */
    BAD_PIXMAP(4, "BAD_PIXMAP: parameter not a pixmap"),

    /**
     * A value for an Atom argument does not name a defined Atom.
     */
    BAD_ATOM(5, "BAD_ATOM: parameter not an atom"),

    /**
     * A value for a cursor argument does not name a defined cursor.
     */
     BAD_CURSOR(6, "BAD_CURSOR: parameter not a cursor"),

    /**
     * A value for a font argument does not name a defined font (or, in some
     * cases, GContext).
     */
    BAD_FONT(7, "BAD_FONT: parameter not a font"),

    /**
     * In a graphics request, the root and depth of the graphics context do not
     * match those of the drawable. An <em>InputOnly</em> window is used as a
     * drawable. Some argument or pair of arguments has the correct type and
     * range, but it fails to match in some other way required by the request.
     * An <em>InputOnly</em> window lacks this attribute.
     */
    BAD_MATCH(8, "BAD_MATCH: parameter mismatch"),

    /**
     * A value for a drawable argument does not name a defined window or pixmap.
     */
    BAD_DRAWABLE(9, "BAD_DRAWABLE: parameter not a pixmap or a window"),

    /** 
     * A client attempts to grab a key/button combination already grabbed by
     * another client.
     *
     * <p>A client attempts to free a colormap entry that it had not already
     * allocated or to free an entry in a colormap that was created with all
     * entries writable.
     *
     * <p>A client attempts to store into a read-only or unallocated colormap
     * entry.
     *
     * <p>A client attempts to modify the access control list from other than
     * the local (or otherwise authorized) host.
     *
     * <p>A client attempts to select an event type that another client has
     * already selected.
     */
    BAD_ACCESS(10, "BAD_ACCESS: operation right denied"),  

    /**
     * The server fails to allocate the requested resource. Note that the
     * explicit listing of <em>BadAlloc</em> errors in requests only covers
     * allocation errors at a very coarse level and is not intended to (nor can
     * it in practice hope to) cover all cases of a server running out of
     * allocation space in the middle of service. The semantics when a server
     * runs out of allocation space are left unspecified, but a server may
     * generate a <em>BadAlloc</em> error on any request for this reason, and
     * clients should be prepared to receive such errors and handle or discard
     * them. 
     */
    BAD_ALLOC(11, "BAD_ALLOC: insufficient resources"),

    /**
     * A value for a Colormap argument does not name a defined Colormap.
     */
    BAD_COLORMAP(12, "BAD_COLORMAP: parameter not a colormap"),

    /**
     * A value for a <em>GContext</em> argument does not name a defined
     * <em>GContext</em>.
     */
    BAD_GC(13, "BAD_GC: parameter not a GC"),

    /**
     * The value chosen for a resource identifier either is not included in the
     * range assigned to the client or is already in use. Under normal
     * circumstances, this cannot occur and should be considered a server or
     * Xlib error.
     */
    BAD_ID_CHOICE(14, "BAD_ID_CHOICE: id choice not in range or already used"),

    /**
     * A font or color of the specified name does not exist.
     */
    BAD_NAME(15, "BAD_NAME: font or color name non-existent"),

    /**
     * The length of a request is shorter or longer than that required to
     * contain the arguments. This is an internal Xlib or server error. The
     * length of a request exceeds the maximum length accepted by the server.
     */
    BAD_LENGTH(16, "BAD_LENGTH: request length incorrect"),

    /**
     * The server does not implement some aspect of the request. A server that
     * generates this error for a core request is deficient. As such, this
     * error is not listed for any of the requests, but clients should be
     * prepared to receive such errors and handle or discard them.
     */
    BAD_IMPLEMENTATION(17, "BAD_IMPLEMENTATION: server defective"),
    
    /**
     * Unkown Error Code
     */
    UNKNOWN_ERROR(-1, "UNKNOWN ERROR: unknown error");
    
    private int errorID;
    private String errorMsg;
    private static Map<Integer, ErrorCode> errorCodes;
    
    private static Map<Integer, ErrorCode> getErrorCodes() {
        if (errorCodes == null) {
            errorCodes = new HashMap<Integer, ErrorCode>();
            for (ErrorCode code : ErrorCode.values())
                errorCodes.put(code.errorID, code);
        }
        
        return errorCodes;
    }
    
    public static ErrorCode getError(int id) {
      ErrorCode error = getErrorCodes().get(id);
      return (error == null) ? error : UNKNOWN_ERROR;
    }
    
    ErrorCode(int id, String errorMS) {
      this.errorID = id;
      this.errorMsg = errorMS;
    }
    
    public int getErrorId() {
      return this.errorID;
    }
    
    public String getErrorMessage() {
      return this.errorMsg;
    }
    
  }

  public static final String [] OPCODE_STRINGS = {
    null,                       // 0
    "CreateWindow",             // 1
    "ChangeWindowAttributes",   // 2
    "GetWindowAttributes",      // 3
    "DestroyWindow",            // 4
    "DestroySubwindows",        // 5
    "ChangeSaveSet",            // 6
    "ReparentWindow",           // 7
    "MapWindow",                // 8
    "MapSubwindows",            // 9
    "UnmapWindow",              // 10
    "UnmapSubwindows",          // 11
    "ConfigureWindow",          // 12
    "CirculateWindow",          // 13
    "GetGeometry",              // 14
    "QueryTree",                // 15
    "InternAtom",               // 16
    "GetAtomName",              // 17
    "ChangeProperty",           // 18
    "DeleteProperty",           // 19
    "GetProperty",              // 20
    "ListProperties",           // 21
    "SetSelectionOwner",        // 22
    "GetSelectionOwner",        // 23
    "ConvertSelection",         // 24
    "SendEvent",                // 25
    "GrabPointer",              // 26
    "UngrabPointer",            // 27
    "GrabButton",               // 28
    "UngrabButton",             // 29
    "ChangeActivePointerGrab",  // 30
    "GrabKeyboard",             // 31
    "UngrabKeyboard",           // 32
    "GrabKey",                  // 33
    "UngrabKey",                // 34
    "AllowEvents",              // 35
    "GrabServer",               // 36
    "UngrabServer",             // 37
    "QueryPointer",             // 38
    "GetMotionEvents",          // 39
    "TranslateCoords",          // 40
    "WarpPointer",              // 41
    "SetInputFocus",            // 42
    "GetInputFocus",            // 43
    "QueryKeymap",              // 44
    "OpenFont",                 // 45
    "CloseFont",                // 46
    "QueryFont",                // 47
    "QueryTextExtents",         // 48
    "ListFonts",                // 49
    "ListFontsWithInfo",        // 50
    "SetFontPath",              // 51
    "GetFontPath",              // 52
    "CreatePixmap",             // 53
    "FreePixmap",               // 54
    "CreateGC",                 // 55
    "ChangeGC",                 // 56
    "CopyGC",                   // 57
    "SetDashes",                // 58
    "SetClipRectangles",        // 59
    "FreeGC",                   // 60
    "ClearArea",                // 61
    "CopyArea",                 // 62
    "CopyPlane",                // 63
    "PolyPoint",                // 64
    "PolyLine",                 // 65
    "PolySegment",              // 66
    "PolyRectangle",            // 67
    "PolyArc",                  // 68
    "FillPoly",                 // 69
    "PolyFillRectangle",        // 70
    "PolyFillArc",              // 71
    "PutImage",                 // 72
    "GetImage",                 // 73
    "PolyText8",                // 74
    "PolyText16",               // 75
    "ImageText8",               // 76
    "ImageText16",              // 77
    "CreateColormap",           // 78
    "FreeColormap",             // 79
    "CopyColormapAndFree",      // 80
    "InstallColormap",          // 81
    "UninstallColormap",        // 82
    "ListInstalledColormaps",   // 83
    "AllocColor",               // 84
    "AllocNamedColor",          // 85
    "AllocColorCells",          // 86
    "AllocColorPlanes",         // 87
    "FreeColors",               // 88
    "StoreColors",              // 89
    "StoreNamedColor",          // 90
    "QueryColors",              // 91
    "LookupColor",              // 92
    "CreateCursor",             // 93
    "CreateGlyphCursor",        // 94
    "FreeCursor",               // 95
    "RecolorCursor",            // 96
    "QueryBestSize",            // 97
    "QueryExtension",           // 98
    "ListExtensions",           // 99
    "ChangeKeyboardMapping",    // 100
    "GetKeyboardMapping",       // 101
    "ChangeKeyboardControl",    // 102
    "GetKeyboardControl",       // 103
    "Bell",                     // 104
    "ChangePointerControl",     // 105
    "GetPointerControl",        // 106
    "SetScreenSaver",           // 107
    "GetScreenSaver",           // 108
    "ChangeHosts",              // 109
    "ListHosts",                // 110
    "SetAccessControl",         // 111
    "SetCloseDownMode",         // 112
    "KillClient",               // 113
    "RotateProperties",         // 114
    "ForceScreenSaver",         // 115
    "SetPointerMapping",        // 116
    "GetPointerMapping",        // 117
    "SetModifierMapping",       // 118
    "GetModifierMapping",       // 119
    null,                       // 120
    null,                       // 121
    null,                       // 122
    null,                       // 123
    null,                       // 124
    null,                       // 125
    null,                       // 126
    "NoOperation"               // 127
  };

  // <--- Constructors ----> //
  
  public Error (String s) { super (s); }

  public Error (Display display, String error_string, ErrorCode code, int seq_no, 
    int bad, int minor_opcode, int major_opcode) {

    super (init (display, error_string, code, seq_no, bad, 
      minor_opcode, major_opcode));
    this.code = code;
    this.seq_no = seq_no;
    this.bad = bad;
    this.minor_opcode = minor_opcode;
    this.major_opcode = major_opcode;
  }  
  
  // <---- Methods ---> //
  public static String init (Display display, String error_string, ErrorCode code,
    int seq_no, int bad, int minor_opcode, int major_opcode) {

    //-- code
    String code_string = "\n  code: " + code.getErrorId() + " " + error_string;


    //-- sequence number
    String seq_no_string = "\n  sequence-number: " + seq_no;


    //-- bad

    String bad_string = "";
    if (code == BAD_COLORMAP
      || code == ErrorCode.BAD_CURSOR
      || code == ErrorCode.BAD_DRAWABLE
      || code == ErrorCode.BAD_FONT
      || code == ErrorCode.BAD_GC
      || code == ErrorCode.BAD_ID_CHOICE
      || code == ErrorCode.BAD_PIXMAP
      || code == ErrorCode.BAD_WINDOW) {

      Object bad_object = display.getResources().get (new Integer (bad));
      bad_string = bad_object == null ?
        "\n  bad-id: " + bad
        : "\n  bad-object: " + bad_object;

    } else if (code == ErrorCode.BAD_ATOM) {
      Object bad_atom = display.getAtom(bad);
      bad_string = bad_atom == null ?
        "\n  bad-atom-id: " + bad
        : "\n  bad-atom: " + bad_atom;

    } else if (code == ErrorCode.BAD_VALUE) {
      bad_string = "\n  bad-value: " + bad;
    } else if (code == ErrorCode.UNKNOWN_ERROR) {
      bad_string = "\n unkown-error: " + bad;
    }


    //-- major opcode

    String major_opcode_string = "\n  major-opcode: " + major_opcode + " "
      + (major_opcode < 128 ?
        OPCODE_STRINGS [major_opcode]
        : display.extensionOpcodeStrings [major_opcode - 128]);
    

    //-- minor opcode    
    
    String minor_opcode_string = major_opcode < 128 ? ""
      : "\n  minor-opcode: " + minor_opcode
      + " " +
      display.extensionMinorOpcodeStrings[major_opcode - 128][minor_opcode];


    //-- output

    return "#Error"
      + code_string
      + seq_no_string
      + bad_string
      + major_opcode_string
      + minor_opcode_string;
  }
}