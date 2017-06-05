/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 */

/* global.h is included by most of the other source files, just after
 * including config.h and the system header files, but before any other
 * ROX-Filer header files.
 */

#include <glib.h>

//#define UNIT_TESTS

/* We put all the global typedefs here to avoid creating dependencies
 * between header files.
 */

/* Each filer window has one of these all to itself */
typedef struct _FilerWindow FilerWindow;

/* There is one Directory object per cached disk directory inode number.
 * Multiple FilerWindows may share a single Directory. Directories
 * are cached, so a Directory may exist without any filer windows
 * referencing it at all.
 */
typedef struct _Directory Directory;

/* Each item in a directory has a DirItem. This contains information from
 * stat()ing the file, plus a few other bits. There may be several of these
 * for a single file, if it appears (hard-linked) in several directories.
 * Each pinboard and panel icon also has one of these (not shared).
 */
typedef struct _DirItem DirItem;

/* Widgets which can display directories implement the View interface.
 * This should be used in preference to the old collection interface because
 * it isn't specific to a particular type of display.
 */
typedef struct _ViewIface ViewIface;

/* A ViewIter specifies a single item in a View, rather like an index.
 * They can be used to iterate over all the items in a View, and remain
 * valid until the View is changed. If allocated on the stack, they do not need
 * to be freed.
 */
typedef struct _ViewIter ViewIter;

/* This contains the pixbufs for an image, in various sizes.
 * Despite the name, it now contains neither pixmaps nor masks!
 */
typedef struct _MaskedPixmap MaskedPixmap;

/* Each MIME type (eg 'text/plain') has one of these. It contains
 * a link to the image and the type's name (used so that the image can
 * be refreshed, among other things).
 */
typedef struct _MIME_type MIME_type;

/* Icon is an abstract base class for pinboard and panel icons.
 * It contains the name and path of the icon, as well as its DirItem.
 */
typedef struct _Icon Icon;

/* There will be one of these if the pinboard is in use. It contains
 * the name of the pinboard and links to the pinned Icons inside.
 */
typedef struct _Pinboard Pinboard;

/* There is one of these for each panel window open. Panels work rather
 * like little pinboards, but with a more rigid layout.
 */
typedef struct _Panel Panel;

/* Each option has a static Option structure. This is initialised by
 * calling option_add_int() or similar. See options.c for details.
 * This structure is read-only.
 */
typedef struct _Option Option;

/* A filesystem cache provides a quick and easy way to load files.
 * When a cache is created, functions to load and update files are
 * registered to it. Requesting an object from the cache will load
 * or update it as needed, or return the cached copy if the current
 * version of the file is already cached.
 * Caches are used to access directories, images and XML files.
 */
typedef struct _GFSCache GFSCache;

/* Each cached XML file is represented by one of these */
typedef struct _XMLwrapper XMLwrapper;

/* This holds a pre-parsed version of a filename, which can be quickly
 * compared with another CollateKey for intelligent sorting.
 */
typedef struct _CollateKey CollateKey;

/* Like a regular GtkLabel, except that the text can be wrapped to any
 * width. Used for pinboard icons.
 */
typedef struct _WrappedLabel WrappedLabel;

/* A filename where " " has been replaced by "%20", etc.
 * This is really just a string, but we try to catch type errors.
 */
typedef struct _EscapedPath EscapedPath;

/* The minibuffer is a text field which appears at the bottom of
 * a filer window. It has various modes of operation:
 */
typedef enum {
	MINI_NONE,
	MINI_PATH,
	MINI_SHELL,
	MINI_SELECT_IF,
	MINI_FILTER,
	MINI_SELECT_BY_NAME,
} MiniType;

/* The next three correspond to the styles on the Display submenu: */

typedef enum {		/* Values used in options, must start at 0 */
	LARGE_ICONS	= 0,
	SMALL_ICONS	= 1,
	HUGE_ICONS	= 2,
	AUTO_SIZE_ICONS	= 3,
	UNKNOWN_STYLE
} DisplayStyle;

typedef enum {		/* Values used in options, must start at 0 */
	DETAILS_NONE		= 0,
	DETAILS_SIZE		= 2,
	DETAILS_PERMISSIONS	= 3,
	DETAILS_TYPE		= 4,
	DETAILS_TIMES		= 5,
	DETAILS_UNKNOWN		= -1,
} DetailsType;

typedef enum {		/* Values used in options */
	SORT_NAME = 0,
	SORT_TYPE = 1,
	SORT_DATE = 2,
	SORT_SIZE = 3,
	SORT_OWNER = 4,
	SORT_GROUP = 5
} SortType;

/* Each DirItem has a base type with indicates what kind of object it is.
 * If the base_type is TYPE_FILE, then the MIME type field gives the exact
 * type.
 */
enum
{
	/* Base types - this also determines the sort order */
	TYPE_ERROR,
	TYPE_UNKNOWN,		/* Not scanned yet */
	TYPE_DIRECTORY,
	TYPE_PIPE,
	TYPE_SOCKET,
	TYPE_FILE,
	TYPE_CHAR_DEVICE,
	TYPE_BLOCK_DEVICE,
	TYPE_DOOR,

	/* These are purely for colour allocation */
	TYPE_EXEC,
	TYPE_APPDIR,
};

/* The namespaces for the SOAP messages */
#define SOAP_ENV_NS_OLD "http://www.w3.org/2001/06/soap-envelope"
#define SOAP_ENV_NS "http://www.w3.org/2001/12/soap-envelope"
#define SOAP_RPC_NS "http://www.w3.org/2001/12/soap-rpc"
#define ROX_NS "http://rox.sourceforge.net/SOAP/ROX-Filer"

/* Namespace for configuration */
#define SITE "rox.sourceforge.net"

/* Stock icons */
#define ROX_STOCK_SHOW_DETAILS "rox-show-details"
#define ROX_STOCK_SHOW_HIDDEN  "rox-show-hidden"
#define ROX_STOCK_SELECT       "rox-select"
#define ROX_STOCK_MOUNT        "rox-mount"
#define ROX_STOCK_MOUNTED      "rox-mounted"
#define ROX_STOCK_XATTR        "rox-xattr"
#define ROX_STOCK_SYMLINK      "rox-symlink"

/* Re-use an existing icon for a slightly different purpose */
#define ROX_STOCK_BOOKMARKS    GTK_STOCK_JUMP_TO

#include <libxml/tree.h>
