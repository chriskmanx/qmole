/****************************************************************************
 * scream::screamcfg.h
 * user-tunable parameters for the routines to connect to screen and/or
 * scream daemons.
 * BSD Licence applies.
 * 2002/04/19  Azundris  incept
 ***************************************************************************/



/* NS_PARANOID enables checks for deviant "screen" behaviour.
   libscream is a little more efficient (and still stable on my box)
   without it, but leaving the checks out in the official version
   would turn this into the living support nightmare.  Thus, if you
   undef it, you're on your own. */
#define NS_PARANOID



#define NS_MAXCMD            512

#define NS_SSH_CALL          "ssh"
#define NS_SSH_OPTS          "-t -A -X"
#define NS_SSH_TUNNEL_OPTS   "-N"
#define NS_SCREAM_CALL       "scream %s"
#define NS_SCREAM_OPTS       "-xRR"
#define NS_SCREEN_CALL       "screen %s"
#define NS_SCREEN_OPTS       "-RDD"
#define NS_SCREEN_GREP       "grep escape \"$SCREENRC\" 2>/dev/null || grep escape ~/.screenrc 2>/dev/null || grep escape \"$SYSSCREENRC\" 2>/dev/null || grep escape /etc/screenrc 2>/dev/null || grep escape /usr/local/etc/screenrc 2>/dev/null || echo \"escape ^Aa\"\n"
#define NS_SCREEM_CALL       "%s 2>/dev/null || %s"
#define NS_WRAP_CALL         "TERM=vt100; export TERM; screen -wipe; %s"
#define NS_SCREEN_RC         ".screenrc"
#define NS_TWIN_CALL         "twattach %s 2>/dev/null || twin %s"
#define NS_TWIN_PARA         "twin@"
#define NS_TWIN_OPTS         "-hw=tty,TERM=xterm -" NS_TWIN_PARA "%s"

/* this should never change. the escape-char for an out-of-the-box "screen".
   don't change this just because you set something else in your .screenrc */
#define NS_SCREEN_ESCAPE     '\x01'
#define NS_SCREEN_LITERAL    'a'
#define NS_SCREEN_CMD        ':'
#define NS_SCREEN_RENAME     'A'
#define NS_SCREEN_KILL       'k'
#define NS_SCREEN_DEFSBB     100

/* the following must use the char defined in NS_SCREEN_ESCAPE. if something
   else is used in the session, libscream will convert it on the fly. */
/* DO NOT use \005Lw for your status, it breaks older screens!! */
#define NS_SCREEN_UPDATE     "\x01:windows\r"
#define NS_SCREEN_VERSION    "\x01:version\r"
#define NS_SCREEN_SESSION    "\x01:sessionname\r"
#define NS_SCREEN_BELL       "\x01:bell_msg Wuff\r\x01:vbell_msg Wuff\r"
#define NS_SCREEN_INIT       "\x01:msgminwait 0\r\x01:msgwait 0\r\x01:hardstatus on\r\x01:hardstatus alwayslastline \"%w\"\r\x01:defhstatus \"\\005w\"\r\x01:hstatus \"\\005w\"\r\x01:nethack off\r" NS_SCREEN_UPDATE "\r" NS_SCREEN_VERSION NS_SCREEN_SESSION "\x0c" NS_SCREEN_UPDATE "\r" NS_SCREEN_BELL
#define NS_SCREEN_PRVS_REG   "\x01:focus up\r"

#define NS_DFLT_SSH_PORT     22
#define NS_MIN_PORT          47323
#define NS_MAX_PORT          65535

#define NS_MAX_DISPS         512

#define NS_SCREEN_FLAGS      "*-$!@L&Z"

#define NS_SCREEN_DK_CMD_T   "unknown command '"
#define NS_SCREEN_SESS_T     "This session is named '"
#define NS_SCREEN_VERSION_T  "scre%2s %d.%d.%d %16s %32s"
#define NS_SCREEN_NO_DEBUG   "Sorry, screen was compiled without -DDEBUG option."
#define NS_SCREEN_ACT_T      "Activity in window "

/* should be 1s */
#define NS_INIT_DELAY        2

/* how many seconds to wait for an SSH-tunnel to build when using the
   -Z option (tunnel through firewall).  2 for very fast networks,
   much more for slow connections. */
#define NS_TUNNEL_DELAY      30

/* what to call the menu entry for Escreen */
#define NS_MENU_TITLE        "Escreen"

/* prefix for debug info */
#define NS_PREFIX            "screamClient::"



/***************************************************************************/
