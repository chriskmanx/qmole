/* ANSI-C code produced by gperf version 3.0.3 */
/* Command-line: gperf -m 100 vteseq-n.gperf  */
/* Computed positions: -k'1,4,$' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 16 "vteseq-n.gperf"
struct vteseq_n_struct {
	int seq;
	VteTerminalSequenceHandler handler;
};
#include <string.h>
/* maximum key range = 88, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
vteseq_n_hash (register const char *str, register unsigned int len)
{
  static const unsigned char asso_values[] =
    {
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 45, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 20, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97,  0, 45, 30,
       5,  0,  5, 97,  0, 20, 97, 97, 45,  0,
      30,  0, 15, 97, 15,  0,  0, 20, 20, 10,
       5, 15, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
      97, 97, 97, 97, 97, 97
    };
  return len + asso_values[(unsigned char)str[3]] + asso_values[(unsigned char)str[0]] + asso_values[(unsigned char)str[len - 1]];
}

struct vteseq_n_pool_t
  {
    char vteseq_n_pool_str9[sizeof("save-mode")];
    char vteseq_n_pool_str10[sizeof("soft-reset")];
    char vteseq_n_pool_str11[sizeof("decset")];
    char vteseq_n_pool_str13[sizeof("erase-in-line")];
    char vteseq_n_pool_str16[sizeof("erase-characters")];
    char vteseq_n_pool_str17[sizeof("delete-lines")];
    char vteseq_n_pool_str19[sizeof("form-feed")];
    char vteseq_n_pool_str21[sizeof("screen-alignment-test")];
    char vteseq_n_pool_str22[sizeof("delete-characters")];
    char vteseq_n_pool_str24[sizeof("scroll-up")];
    char vteseq_n_pool_str25[sizeof("reset-mode")];
    char vteseq_n_pool_str26[sizeof("save-cursor")];
    char vteseq_n_pool_str27[sizeof("restore-mode")];
    char vteseq_n_pool_str28[sizeof("decreset")];
    char vteseq_n_pool_str30[sizeof("index")];
    char vteseq_n_pool_str31[sizeof("erase-in-display")];
    char vteseq_n_pool_str32[sizeof("insert-lines")];
    char vteseq_n_pool_str33[sizeof("reverse-index")];
    char vteseq_n_pool_str35[sizeof("send-primary-device-attributes")];
    char vteseq_n_pool_str37[sizeof("send-secondary-device-attributes")];
    char vteseq_n_pool_str39[sizeof("next-line")];
    char vteseq_n_pool_str41[sizeof("scroll-down")];
    char vteseq_n_pool_str43[sizeof("insert-blank-characters")];
    char vteseq_n_pool_str44[sizeof("restore-cursor")];
    char vteseq_n_pool_str45[sizeof("device-status-report")];
    char vteseq_n_pool_str46[sizeof("cursor-next-line")];
    char vteseq_n_pool_str47[sizeof("cursor-lower-left")];
    char vteseq_n_pool_str48[sizeof("normal-keypad")];
    char vteseq_n_pool_str49[sizeof("cursor-forward")];
    char vteseq_n_pool_str50[sizeof("cursor-backward")];
    char vteseq_n_pool_str51[sizeof("cursor-preceding-line")];
    char vteseq_n_pool_str52[sizeof("tab-set")];
    char vteseq_n_pool_str53[sizeof("set-mode")];
    char vteseq_n_pool_str54[sizeof("cursor-up")];
    char vteseq_n_pool_str55[sizeof("cursor-character-absolute")];
    char vteseq_n_pool_str57[sizeof("return-terminal-status")];
    char vteseq_n_pool_str58[sizeof("return-terminal-id")];
    char vteseq_n_pool_str59[sizeof("set-icon-title")];
    char vteseq_n_pool_str60[sizeof("full-reset")];
    char vteseq_n_pool_str61[sizeof("set-window-title")];
    char vteseq_n_pool_str62[sizeof("request-terminal-parameters")];
    char vteseq_n_pool_str63[sizeof("iso8859-1-character-set")];
    char vteseq_n_pool_str64[sizeof("window-manipulation")];
    char vteseq_n_pool_str65[sizeof("character-attributes")];
    char vteseq_n_pool_str67[sizeof("line-position-absolute")];
    char vteseq_n_pool_str68[sizeof("application-keypad")];
    char vteseq_n_pool_str69[sizeof("tab-clear")];
    char vteseq_n_pool_str70[sizeof("set-icon-and-window-title")];
    char vteseq_n_pool_str71[sizeof("cursor-down")];
    char vteseq_n_pool_str72[sizeof("character-position-absolute")];
    char vteseq_n_pool_str74[sizeof("dec-device-status-report")];
    char vteseq_n_pool_str75[sizeof("cursor-position")];
    char vteseq_n_pool_str77[sizeof("vertical-tab")];
    char vteseq_n_pool_str80[sizeof("change-cursor-colors")];
    char vteseq_n_pool_str82[sizeof("horizontal-and-vertical-position")];
    char vteseq_n_pool_str84[sizeof("utf-8-character-set")];
    char vteseq_n_pool_str85[sizeof("cursor-forward-tabulation")];
    char vteseq_n_pool_str87[sizeof("change-color")];
    char vteseq_n_pool_str90[sizeof("cursor-back-tab")];
    char vteseq_n_pool_str95[sizeof("set-scrolling-region")];
    char vteseq_n_pool_str96[sizeof("linux-console-cursor-attributes")];
  };
static const struct vteseq_n_pool_t vteseq_n_pool_contents =
  {
    "save-mode",
    "soft-reset",
    "decset",
    "erase-in-line",
    "erase-characters",
    "delete-lines",
    "form-feed",
    "screen-alignment-test",
    "delete-characters",
    "scroll-up",
    "reset-mode",
    "save-cursor",
    "restore-mode",
    "decreset",
    "index",
    "erase-in-display",
    "insert-lines",
    "reverse-index",
    "send-primary-device-attributes",
    "send-secondary-device-attributes",
    "next-line",
    "scroll-down",
    "insert-blank-characters",
    "restore-cursor",
    "device-status-report",
    "cursor-next-line",
    "cursor-lower-left",
    "normal-keypad",
    "cursor-forward",
    "cursor-backward",
    "cursor-preceding-line",
    "tab-set",
    "set-mode",
    "cursor-up",
    "cursor-character-absolute",
    "return-terminal-status",
    "return-terminal-id",
    "set-icon-title",
    "full-reset",
    "set-window-title",
    "request-terminal-parameters",
    "iso8859-1-character-set",
    "window-manipulation",
    "character-attributes",
    "line-position-absolute",
    "application-keypad",
    "tab-clear",
    "set-icon-and-window-title",
    "cursor-down",
    "character-position-absolute",
    "dec-device-status-report",
    "cursor-position",
    "vertical-tab",
    "change-cursor-colors",
    "horizontal-and-vertical-position",
    "utf-8-character-set",
    "cursor-forward-tabulation",
    "change-color",
    "cursor-back-tab",
    "set-scrolling-region",
    "linux-console-cursor-attributes"
  };
#define vteseq_n_pool ((const char *) &vteseq_n_pool_contents)
#ifdef __GNUC__
__inline
#ifdef __GNUC_STDC_INLINE__
__attribute__ ((__gnu_inline__))
#endif
#endif
const struct vteseq_n_struct *
vteseq_n_lookup (register const char *str, register unsigned int len)
{
  enum
    {
      TOTAL_KEYWORDS = 61,
      MIN_WORD_LENGTH = 5,
      MAX_WORD_LENGTH = 32,
      MIN_HASH_VALUE = 9,
      MAX_HASH_VALUE = 96
    };

  static const unsigned char lengthtable[] =
    {
       0,  0,  0,  0,  0,  0,  0,  0,  0,  9, 10,  6,  0, 13,
       0,  0, 16, 12,  0,  9,  0, 21, 17,  0,  9, 10, 11, 12,
       8,  0,  5, 16, 12, 13,  0, 30,  0, 32,  0,  9,  0, 11,
       0, 23, 14, 20, 16, 17, 13, 14, 15, 21,  7,  8,  9, 25,
       0, 22, 18, 14, 10, 16, 27, 23, 19, 20,  0, 22, 18,  9,
      25, 11, 27,  0, 24, 15,  0, 12,  0,  0, 20,  0, 32,  0,
      19, 25,  0, 12,  0,  0, 15,  0,  0,  0,  0, 20, 31
    };
  static const struct vteseq_n_struct wordlist[] =
    {
      {-1}, {-1}, {-1}, {-1}, {-1}, {-1}, {-1}, {-1}, {-1},
#line 33 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str9, VTE_SEQUENCE_HANDLER(vte_sequence_handler_save_mode)},
#line 39 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str10, VTE_SEQUENCE_HANDLER(vte_sequence_handler_soft_reset)},
#line 25 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str11, VTE_SEQUENCE_HANDLER(vte_sequence_handler_decset)},
      {-1},
#line 51 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str13, VTE_SEQUENCE_HANDLER(vte_sequence_handler_erase_in_line)},
      {-1}, {-1},
#line 69 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str16, VTE_SEQUENCE_HANDLER(vte_sequence_handler_erase_characters)},
#line 45 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str17, VTE_SEQUENCE_HANDLER(vte_sequence_handler_delete_lines)},
      {-1},
#line 31 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str19, VTE_SEQUENCE_HANDLER(vte_sequence_handler_form_feed)},
      {-1},
#line 88 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str21, VTE_SEQUENCE_HANDLER(vte_sequence_handler_screen_alignment_test)},
#line 73 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str22, VTE_SEQUENCE_HANDLER(vte_sequence_handler_DC)},
      {-1},
#line 34 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str24, VTE_SEQUENCE_HANDLER(vte_sequence_handler_scroll_up)},
#line 38 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str25, VTE_SEQUENCE_HANDLER(vte_sequence_handler_reset_mode)},
#line 42 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str26, VTE_SEQUENCE_HANDLER(vte_sequence_handler_sc)},
#line 48 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str27, VTE_SEQUENCE_HANDLER(vte_sequence_handler_restore_mode)},
#line 28 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str28, VTE_SEQUENCE_HANDLER(vte_sequence_handler_decreset)},
      {-1},
#line 24 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str30, VTE_SEQUENCE_HANDLER(vte_sequence_handler_index)},
#line 70 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str31, VTE_SEQUENCE_HANDLER(vte_sequence_handler_erase_in_display)},
#line 47 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str32, VTE_SEQUENCE_HANDLER(vte_sequence_handler_insert_lines)},
#line 54 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str33, VTE_SEQUENCE_HANDLER(vte_sequence_handler_reverse_index)},
      {-1},
#line 124 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str35, VTE_SEQUENCE_HANDLER(vte_sequence_handler_send_primary_device_attributes)},
      {-1},
#line 127 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str37, VTE_SEQUENCE_HANDLER(vte_sequence_handler_send_secondary_device_attributes)},
      {-1},
#line 32 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str39, VTE_SEQUENCE_HANDLER(vte_sequence_handler_next_line)},
      {-1},
#line 43 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str41, VTE_SEQUENCE_HANDLER(vte_sequence_handler_scroll_down)},
      {-1},
#line 97 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str43, VTE_SEQUENCE_HANDLER(vte_sequence_handler_insert_blank_characters)},
#line 60 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str44, VTE_SEQUENCE_HANDLER(vte_sequence_handler_rc)},
#line 82 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str45, VTE_SEQUENCE_HANDLER(vte_sequence_handler_device_status_report)},
#line 68 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str46, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_next_line)},
#line 72 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str47, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_lower_left)},
#line 53 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str48, VTE_SEQUENCE_HANDLER(vte_sequence_handler_normal_keypad)},
#line 58 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str49, VTE_SEQUENCE_HANDLER(vte_sequence_handler_RI)},
#line 63 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str50, VTE_SEQUENCE_HANDLER(vte_sequence_handler_LE)},
#line 86 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str51, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_preceding_line)},
#line 27 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str52, VTE_SEQUENCE_HANDLER(vte_sequence_handler_st)},
#line 29 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str53, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_mode)},
#line 30 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str54, VTE_SEQUENCE_HANDLER(vte_sequence_handler_UP)},
#line 110 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str55, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_character_absolute)},
      {-1},
#line 94 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str57, VTE_SEQUENCE_HANDLER(vte_sequence_handler_return_terminal_status)},
#line 76 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str58, VTE_SEQUENCE_HANDLER(vte_sequence_handler_return_terminal_id)},
#line 61 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str59, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_icon_title)},
#line 36 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str60, VTE_SEQUENCE_HANDLER(vte_sequence_handler_full_reset)},
#line 71 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str61, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_window_title)},
#line 116 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str62, VTE_SEQUENCE_HANDLER(vte_sequence_handler_request_terminal_parameters)},
#line 100 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str63, VTE_SEQUENCE_HANDLER(vte_sequence_handler_local_charset)},
#line 79 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str64, VTE_SEQUENCE_HANDLER(vte_sequence_handler_window_manipulation)},
#line 81 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str65, VTE_SEQUENCE_HANDLER(vte_sequence_handler_character_attributes)},
      {-1},
#line 93 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str67, VTE_SEQUENCE_HANDLER(vte_sequence_handler_line_position_absolute)},
#line 74 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str68, VTE_SEQUENCE_HANDLER(vte_sequence_handler_application_keypad)},
#line 35 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str69, VTE_SEQUENCE_HANDLER(vte_sequence_handler_tab_clear)},
#line 113 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str70, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_icon_and_window_title)},
#line 40 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str71, VTE_SEQUENCE_HANDLER(vte_sequence_handler_DO)},
#line 115 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str72, VTE_SEQUENCE_HANDLER(vte_sequence_handler_character_position_absolute)},
      {-1},
#line 107 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str74, VTE_SEQUENCE_HANDLER(vte_sequence_handler_dec_device_status_report)},
#line 64 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str75, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_position)},
      {-1},
#line 50 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str77, VTE_SEQUENCE_HANDLER(vte_sequence_handler_vertical_tab)},
      {-1}, {-1},
#line 80 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str80, VTE_SEQUENCE_HANDLER(vte_sequence_handler_change_cursor_color)},
      {-1},
#line 126 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str82, VTE_SEQUENCE_HANDLER(vte_sequence_handler_horizontal_and_vertical_position)},
      {-1},
#line 78 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str84, VTE_SEQUENCE_HANDLER(vte_sequence_handler_utf_8_charset)},
#line 111 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str85, VTE_SEQUENCE_HANDLER(vte_sequence_handler_ta)},
      {-1},
#line 44 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str87, VTE_SEQUENCE_HANDLER(vte_sequence_handler_change_color)},
      {-1}, {-1},
#line 62 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str90, VTE_SEQUENCE_HANDLER(vte_sequence_handler_bt)},
      {-1}, {-1}, {-1}, {-1},
#line 83 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str95, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_scrolling_region)},
#line 125 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str96, VTE_SEQUENCE_HANDLER(vte_sequence_handler_noop)}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = vteseq_n_hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        if (len == lengthtable[key])
          {
            register const char *s = wordlist[key].seq + vteseq_n_pool;

            if (*str == *s && !memcmp (str + 1, s + 1, len - 1))
              return &wordlist[key];
          }
    }
  return 0;
}
