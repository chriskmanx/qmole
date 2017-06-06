/****************************************************************************
 * scream::scream.h
 * routines to connect to screen and or scream daemons.
 * BSD Licence applies.
 * 2002/04/19  Azundris  incept
 ***************************************************************************/



#include "screamcfg.h"

#ifdef NS_HAVE_TWIN
#  include <Tw/Tw.h>
#endif

#define NS_TWIN_PROTO      "twin://"
#define NS_SCREEN_PROTO    "screen://"
#define NS_SCREAM_PROTO    "scream://"

#define NS_DK              0
#define NS_SSH             1
#define NS_LCL             2
#define NS_SU              3

#define NS_MODE_NEGOTIATE -1
#define NS_MODE_NONE       0
#define NS_MODE_SCREEN     1
#define NS_MODE_SCREAM     2
#define NS_MODE_TWIN       3

#define NS_MAGIC_LINE(a)   (((a)==NS_MODE_SCREEN)||((a)==NS_MODE_NEGOTIATE))

#define NS_SUCC          (-1)
#define NS_FAIL            0
#define NS_OOM             1
#define NS_MALFORMED_URL   2
#define NS_UNKNOWN_USER    3
#define NS_INVALID_SESS    4
#define NS_UNKNOWN_PROTO   5
#define NS_CANT_RESOLVE    6
#define NS_CANT_CONNECT    7
#define NS_CANT_LOGIN      8
#define NS_CANT_ATTACH     9
#define NS_UNKNOWN_LOC    10
#define NS_SCREEN_STATUS  11
#define NS_SCREEN_ST_CLR  12
#define NS_EFUN_NOT_SET   13
#define NS_USER_CXL       14
#define NS_NOT_ALLOWED    15

#define NS_ERR_WEIRDSCREEN 1

#define NS_SCREAM_CURR     1
#define NS_SCREAM_PRVS     2
#define NS_SCREAM_UTMP     4
#define NS_SCREAM_BELL     8
#define NS_SCREAM_ACT     16
#define NS_SCREAM_FILE    32
#define NS_SCREAM_SHARED  64
#define NS_SCREAM_ZOMBIE 128
#define NS_SCREAM_BUTTON 0xf00

#define NS_SCREAM_MASK   (~(NS_SCREAM_UTMP|NS_SCREAM_PRVS))

#define NS_HOP_DOWN        0
#define NS_HOP_UP          1

typedef enum {
  NS_ESC_CMDLINE=1,
  NS_ESC_SYSSCREENRC=2,
  NS_ESC_SCREENRC=3,
  NS_ESC_INTERACTIVE=4
} ns_esc_whence;

#define NS_SESS_NO_MON_MSG 1

#define NS_MON_TOGGLE_QUIET 0
#define NS_MON_TOGGLE_NOISY 1



/***************************************************************************/



typedef struct __ns_hop {
  int               localport;
  char             *fw;
  int               fwport;
  int               established;
  int               delay;
  int               refcount;
  struct __ns_sess *sess;    /* first only, others have same host/port */
  struct __ns_hop  *next;
} _ns_hop;



typedef struct __ns_sess {   /* a whole screen-session with many clients */
  char   *name;              /* session name */
  int     where;             /* local/remote */
  int     backend;           /* screen/scream/twin */
  int     nesting;           /* 0=topLevel, 1=screen within a screen etc */
  time_t  timestamp;         /* last updated when? */
  int     delay;             /* initialization delay */

  int     flags;             /* miracle flags, see NS_SESS_* */
  int     fd;                /* fd for communication */
  int     dsbb;              /* default length of scroll-back buffer */

  char   *proto;             /* protocol.  usually "screen" */
  char   *host;              /* host. numeric or symbolic. ("localhost") */
  int     port;              /* port. usually TCP22: SSH */
  int     disp;              /* display (used by twin etc., not screen) */
  char   *user;              /* user. often current local user */
  char   *pass;              /* password. used for su. for remote sessions, a
                                ssh-key should be on the remote machine. */
  char   *rsrc;              /* add'l parameter to screen/scream. URL-enc */

  void   *userdef;           /* the term-app can store a pointer here */

  struct __ns_efuns *efuns;  /* callbacks into the terminal program. */
  struct __ns_hop   *hop;    /* tunnel, if any */
  struct __ns_disp  *dsps;   /* first display (that with the lowest index) */
  struct __ns_disp  *curr;   /* current display (NULL for none) */
  struct __ns_sess  *prvs;   /* previous session in session list */
  struct __ns_sess  *next;   /* next     session in session list */

  char   *home;              /* user's home dir. so we can find .screenrc */
  char   *sysrc;             /* global screen config */
  char    escape,literal;    /* talking to screen: defaults to ^A, a */
  ns_esc_whence escdef;            /* where was the escape sequence defined? */

#ifdef NS_HAVE_TWIN
  tdisplay twin;             /* twin-display */
  char *twin_str;            /* twin-display (string specifier) */
#endif
} _ns_sess;



typedef struct __ns_disp {  /* describing the window of a single client */
  int   index;          /* index 0..n */
  char *name;           /* arbitrary \0-terminated string. humans only. */
  int   sbb;            /* scroll-back buffer. 0 if parent to more screens */
  int   x;              /* rsvd */
  int   y;              /* rsvd */
  int   w;              /* rsvd */
  int   h;              /* rsvd */
  int   flags;          /* current, activity, logging, ... */
  struct __ns_sess  *sess;   /* is child of... (always) */
  struct __ns_sess  *child;  /* if parent of... (rarely) */
  struct __ns_efuns *efuns;  /* NULL to use its parent's efuns */
  struct __ns_disp  *prvs;   /* left neighbour (<index) of this session */
  struct __ns_disp  *next;   /* right neighbour (>index) of this session */
} _ns_disp;



typedef struct __ns_efuns {  /* callbacks into the terminal program */
  int   refcount;            /* use calls below to set this up */
  int   (*set_scroll_x)(void *,int);
  int   (*set_scroll_y)(void *,int);
  int   (*set_scroll_w)(void *,int);
  int   (*set_scroll_h)(void *,int);
  int   (*redraw)(void *);
  int   (*redraw_xywh)(void *,int,int,int,int);
  int   (*expire_buttons)(void *,int);
  int   (*ins_disp)(void *,int,int,char *);
  int   (*del_disp)(void *,int);
  int   (*upd_disp)(void *,int,int,char *);
  int   (*err_msg)(void *,int,char *);
  int   (*execute)(void *,char **);
  int   (*inp_text)(void *,int,char *);
  int   (*inp_dial)(void *,char *,int,char **,int (*)(void *,char *,size_t,size_t));
  int   (*inp_tab)(void *,char *[],int,char *,size_t,size_t);
  int   (*waitstate)(void *,int);
} _ns_efuns;



typedef struct __ns_parse {
  int   screen;
  int   real;
  int   flags;
  char *name;
} _ns_parse;



/***************************************************************************/



/* constructors/destructors */
_ns_efuns *ns_new_efuns(void);
_ns_efuns *ns_dst_efuns(_ns_efuns **);
_ns_efuns *ns_get_efuns(_ns_sess *,_ns_disp *);

/* get session anchor (for iterations) */
_ns_sess *ns_1st_sess(void);

/* transparent attach/detach */
_ns_sess *ns_attach_by_sess(_ns_sess **,int *);
_ns_sess *ns_attach_by_URL(char *,char *,_ns_efuns **,int *,void *);
int ns_detach(_ns_sess **);

/* debug */
void ns_desc_twin(_ns_sess *,char *);

/* convenience */
int ns_run(_ns_efuns *, char *);
int ns_get_ssh_port(void);
int ns_get_twin_port(void);


int disp_get_real_by_screen(_ns_sess *,int);
int disp_get_screen_by_real(_ns_sess *,int);
int ns_magic_disp(_ns_sess **,_ns_disp **);

/* send command to screen */
int ns_screen_command(_ns_sess *, char *);

/* send command to twin */
#ifdef NS_HAVE_TWIN
int ns_twin_command(_ns_sess *,udat,byte *,udat,byte *);
#  define ns_twin_control(s,a,b)   ns_twin_command(s,TW_MSG_USER_CONTROL,(a),(b),NULL)
#  define ns_twin_message(s,a,b,c) ns_twin_command(s,TW_MSG_USER_CLIENTMSG,(a),(b),(c))
#endif


/* send statement to screen (prefixing it with the session's ^A: equiv) */
int ns_screen_xcommand(_ns_sess *,char , char *);

/* parse and forward a screen-statement (from ^A: input or screenrc) */
int ns_parse_screen_cmd(_ns_sess *,char *,ns_esc_whence);

/* parse and forward a screen-hotkey */
int ns_parse_screen_key(_ns_sess *,char);

/* parse and forward a string */
int ns_parse_screen_interactive(_ns_sess *, char *);

/* parse screen escape setup */
char ns_parse_esc(char **);

/* init session (read .screenrc, or whatnot) */
int ns_sess_init(_ns_sess *);

/* what the terminal should call the last line -- screen's "hardstatus"
   changes. submit session, terminal-width, and a pointer to said line. */
int ns_parse_screen(_ns_sess *,int,int,char *);



/* backend abstraction */
/* things the term might ask screen/scream to do ***************************/
int ns_tog_disp(_ns_sess *);
int ns_go2_disp(_ns_sess *,int);
int ns_rel_disp(_ns_sess *,int);
int ns_add_disp(_ns_sess *,int,char *);
int ns_mov_disp(_ns_sess *,int,int);
int ns_rsz_disp(_ns_sess *,int,int,int);
int ns_rem_disp(_ns_sess *,int,int);
int ns_ren_disp(_ns_sess *,int,char *);
int ns_log_disp(_ns_sess *,int,char *);
int ns_mon_disp(_ns_sess *,int,int);
int ns_sbb_disp(_ns_sess *,int);

int ns_tog_region(_ns_sess *,_ns_disp *);
int ns_go2_region(_ns_sess *,_ns_disp *,int);
int ns_rel_region(_ns_sess *,_ns_disp *,int);
int ns_add_region(_ns_sess *,_ns_disp *,int,char *);
int ns_mov_region(_ns_sess *,_ns_disp *,int,int);
int ns_rsz_region(_ns_sess *,_ns_disp *,int,int,int);
int ns_rem_region(_ns_sess *,_ns_disp *,int,int);
int ns_ren_region(_ns_sess *,_ns_disp *,int,char *);
int ns_log_region(_ns_sess *,_ns_disp *,int,char *);
int ns_mon_region(_ns_sess *,_ns_disp *,int);
int ns_sbb_region(_ns_sess *,_ns_disp *,int);
int ns_one_region(_ns_sess *,_ns_disp *,int);

int ns_upd_stat(_ns_sess *);
int ns_scroll2x(_ns_sess *,int);
int ns_scroll2y(_ns_sess *,int);
int ns_inp_dial(_ns_sess *,char *,int,char **,int (*)(void *,char *,size_t,size_t));
int ns_statement(_ns_sess *,char *);
int ns_reset(_ns_sess *,int);
char *ns_get_url(_ns_sess *,int);



/* frontend abstraction */
/* things we might ask the terminal to do (register efuns (callbacks)) *****/
void ns_register_ssx(_ns_efuns *,int (*set_scroll_x)(void *,int));
void ns_register_ssy(_ns_efuns *,int (*set_scroll_y)(void *,int));
void ns_register_ssw(_ns_efuns *,int (*set_scroll_w)(void *,int));
void ns_register_ssh(_ns_efuns *,int (*set_scroll_h)(void *,int));

void ns_register_red(_ns_efuns *,int (*redraw)(void *));
void ns_register_rda(_ns_efuns *,int (*redraw_xywh)(void *,int,int,int,int));
void ns_register_exb(_ns_efuns *,int (*expire_buttons)(void *,int));

void ns_register_ins(_ns_efuns *,int (*ins_disp)(void *,int,int,char *));
void ns_register_del(_ns_efuns *,int (*del_disp)(void *,int));
void ns_register_upd(_ns_efuns *,int (*upd_disp)(void *,int,int,char *));

void ns_register_err(_ns_efuns *,int (*err_msg)(void *,int,char *));

void ns_register_exe(_ns_efuns *,int (*execute)(void *,char **));
void ns_register_txt(_ns_efuns *,int (*inp_text)(void *,int,char *));

void ns_register_inp(_ns_efuns *,int (*)(void *,char *,int,char **,int (*)(void *,char *,size_t,size_t)));
void ns_register_tab(_ns_efuns *,int (*)(void *,char *[],int,char *,size_t,size_t));
void ns_register_fun(_ns_efuns *,int (*)(void *,int));



/***************************************************************************/
