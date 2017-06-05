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
/* maximum key range = 77, duplicates = 0 */

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
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84,  0, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 10, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 47,  7, 27,
       0,  0, 18, 84,  0, 33, 84, 84,  0,  4,
       8, 43,  2, 84, 29,  2,  0,  2, 42,  5,
      21,  8, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84, 84, 84, 84, 84,
      84, 84, 84, 84, 84, 84
    };
  return len + asso_values[(unsigned char)str[3]] + asso_values[(unsigned char)str[0]] + asso_values[(unsigned char)str[len - 1]];
}

struct vteseq_n_pool_t
  {
    char vteseq_n_pool_str7[sizeof("tab-set")];
    char vteseq_n_pool_str8[sizeof("decset")];
    char vteseq_n_pool_str10[sizeof("set-mode")];
    char vteseq_n_pool_str11[sizeof("save-mode")];
    char vteseq_n_pool_str12[sizeof("soft-reset")];
    char vteseq_n_pool_str14[sizeof("delete-lines")];
    char vteseq_n_pool_str15[sizeof("erase-in-line")];
    char vteseq_n_pool_str16[sizeof("set-icon-title")];
    char vteseq_n_pool_str17[sizeof("next-line")];
    char vteseq_n_pool_str18[sizeof("set-window-title")];
    char vteseq_n_pool_str19[sizeof("delete-characters")];
    char vteseq_n_pool_str20[sizeof("erase-characters")];
    char vteseq_n_pool_str21[sizeof("utf-8-character-set")];
    char vteseq_n_pool_str22[sizeof("line-position-absolute")];
    char vteseq_n_pool_str23[sizeof("screen-alignment-test")];
    char vteseq_n_pool_str24[sizeof("dec-device-status-report")];
    char vteseq_n_pool_str25[sizeof("normal-keypad")];
    char vteseq_n_pool_str26[sizeof("erase-in-display")];
    char vteseq_n_pool_str27[sizeof("set-icon-and-window-title")];
    char vteseq_n_pool_str28[sizeof("full-reset")];
    char vteseq_n_pool_str29[sizeof("set-scrolling-region-to-end")];
    char vteseq_n_pool_str30[sizeof("set-scrolling-region")];
    char vteseq_n_pool_str31[sizeof("form-feed")];
    char vteseq_n_pool_str32[sizeof("window-manipulation")];
    char vteseq_n_pool_str33[sizeof("set-scrolling-region-from-start")];
    char vteseq_n_pool_str34[sizeof("send-primary-device-attributes")];
    char vteseq_n_pool_str35[sizeof("linux-console-cursor-attributes")];
    char vteseq_n_pool_str36[sizeof("send-secondary-device-attributes")];
    char vteseq_n_pool_str37[sizeof("decreset")];
    char vteseq_n_pool_str38[sizeof("tab-clear")];
    char vteseq_n_pool_str39[sizeof("reset-mode")];
    char vteseq_n_pool_str40[sizeof("cursor-up")];
    char vteseq_n_pool_str41[sizeof("restore-mode")];
    char vteseq_n_pool_str42[sizeof("save-cursor")];
    char vteseq_n_pool_str43[sizeof("cursor-forward")];
    char vteseq_n_pool_str44[sizeof("cursor-backward")];
    char vteseq_n_pool_str45[sizeof("cursor-next-line")];
    char vteseq_n_pool_str46[sizeof("cursor-lower-left")];
    char vteseq_n_pool_str47[sizeof("insert-lines")];
    char vteseq_n_pool_str48[sizeof("cursor-down")];
    char vteseq_n_pool_str49[sizeof("return-terminal-id")];
    char vteseq_n_pool_str50[sizeof("cursor-preceding-line")];
    char vteseq_n_pool_str51[sizeof("cursor-back-tab")];
    char vteseq_n_pool_str52[sizeof("cursor-position")];
    char vteseq_n_pool_str53[sizeof("device-status-report")];
    char vteseq_n_pool_str54[sizeof("cursor-character-absolute")];
    char vteseq_n_pool_str55[sizeof("return-terminal-status")];
    char vteseq_n_pool_str56[sizeof("scroll-up")];
    char vteseq_n_pool_str57[sizeof("change-cursor-colors")];
    char vteseq_n_pool_str58[sizeof("insert-blank-characters")];
    char vteseq_n_pool_str59[sizeof("index")];
    char vteseq_n_pool_str60[sizeof("request-terminal-parameters")];
    char vteseq_n_pool_str61[sizeof("vertical-tab")];
    char vteseq_n_pool_str62[sizeof("cursor-forward-tabulation")];
    char vteseq_n_pool_str63[sizeof("reverse-index")];
    char vteseq_n_pool_str64[sizeof("scroll-down")];
    char vteseq_n_pool_str65[sizeof("application-keypad")];
    char vteseq_n_pool_str66[sizeof("iso8859-1-character-set")];
    char vteseq_n_pool_str72[sizeof("restore-cursor")];
    char vteseq_n_pool_str73[sizeof("horizontal-and-vertical-position")];
    char vteseq_n_pool_str76[sizeof("change-color")];
    char vteseq_n_pool_str78[sizeof("character-attributes")];
    char vteseq_n_pool_str83[sizeof("character-position-absolute")];
  };
static const struct vteseq_n_pool_t vteseq_n_pool_contents =
  {
    "tab-set",
    "decset",
    "set-mode",
    "save-mode",
    "soft-reset",
    "delete-lines",
    "erase-in-line",
    "set-icon-title",
    "next-line",
    "set-window-title",
    "delete-characters",
    "erase-characters",
    "utf-8-character-set",
    "line-position-absolute",
    "screen-alignment-test",
    "dec-device-status-report",
    "normal-keypad",
    "erase-in-display",
    "set-icon-and-window-title",
    "full-reset",
    "set-scrolling-region-to-end",
    "set-scrolling-region",
    "form-feed",
    "window-manipulation",
    "set-scrolling-region-from-start",
    "send-primary-device-attributes",
    "linux-console-cursor-attributes",
    "send-secondary-device-attributes",
    "decreset",
    "tab-clear",
    "reset-mode",
    "cursor-up",
    "restore-mode",
    "save-cursor",
    "cursor-forward",
    "cursor-backward",
    "cursor-next-line",
    "cursor-lower-left",
    "insert-lines",
    "cursor-down",
    "return-terminal-id",
    "cursor-preceding-line",
    "cursor-back-tab",
    "cursor-position",
    "device-status-report",
    "cursor-character-absolute",
    "return-terminal-status",
    "scroll-up",
    "change-cursor-colors",
    "insert-blank-characters",
    "index",
    "request-terminal-parameters",
    "vertical-tab",
    "cursor-forward-tabulation",
    "reverse-index",
    "scroll-down",
    "application-keypad",
    "iso8859-1-character-set",
    "restore-cursor",
    "horizontal-and-vertical-position",
    "change-color",
    "character-attributes",
    "character-position-absolute"
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
      TOTAL_KEYWORDS = 63,
      MIN_WORD_LENGTH = 5,
      MAX_WORD_LENGTH = 32,
      MIN_HASH_VALUE = 7,
      MAX_HASH_VALUE = 83
    };

  static const unsigned char lengthtable[] =
    {
       0,  0,  0,  0,  0,  0,  0,  7,  6,  0,  8,  9, 10,  0,
      12, 13, 14,  9, 16, 17, 16, 19, 22, 21, 24, 13, 16, 25,
      10, 27, 20,  9, 19, 31, 30, 31, 32,  8,  9, 10,  9, 12,
      11, 14, 15, 16, 17, 12, 11, 18, 21, 15, 15, 20, 25, 22,
       9, 20, 23,  5, 27, 12, 25, 13, 11, 18, 23,  0,  0,  0,
       0,  0, 14, 32,  0,  0, 12,  0, 20,  0,  0,  0,  0, 27
    };
  static const struct vteseq_n_struct wordlist[] =
    {
      {-1}, {-1}, {-1}, {-1}, {-1}, {-1}, {-1},
#line 27 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str7, VTE_SEQUENCE_HANDLER(vte_sequence_handler_st)},
#line 25 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str8, VTE_SEQUENCE_HANDLER(vte_sequence_handler_decset)},
      {-1},
#line 29 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str10, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_mode)},
#line 33 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str11, VTE_SEQUENCE_HANDLER(vte_sequence_handler_save_mode)},
#line 39 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str12, VTE_SEQUENCE_HANDLER(vte_sequence_handler_soft_reset)},
      {-1},
#line 45 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str14, VTE_SEQUENCE_HANDLER(vte_sequence_handler_delete_lines)},
#line 51 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str15, VTE_SEQUENCE_HANDLER(vte_sequence_handler_erase_in_line)},
#line 61 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str16, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_icon_title)},
#line 32 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str17, VTE_SEQUENCE_HANDLER(vte_sequence_handler_next_line)},
#line 71 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str18, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_window_title)},
#line 73 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str19, VTE_SEQUENCE_HANDLER(vte_sequence_handler_DC)},
#line 69 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str20, VTE_SEQUENCE_HANDLER(vte_sequence_handler_erase_characters)},
#line 78 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str21, VTE_SEQUENCE_HANDLER(vte_sequence_handler_utf_8_charset)},
#line 95 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str22, VTE_SEQUENCE_HANDLER(vte_sequence_handler_line_position_absolute)},
#line 90 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str23, VTE_SEQUENCE_HANDLER(vte_sequence_handler_screen_alignment_test)},
#line 109 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str24, VTE_SEQUENCE_HANDLER(vte_sequence_handler_dec_device_status_report)},
#line 53 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str25, VTE_SEQUENCE_HANDLER(vte_sequence_handler_normal_keypad)},
#line 70 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str26, VTE_SEQUENCE_HANDLER(vte_sequence_handler_erase_in_display)},
#line 115 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str27, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_icon_and_window_title)},
#line 36 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str28, VTE_SEQUENCE_HANDLER(vte_sequence_handler_full_reset)},
#line 85 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str29, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_scrolling_region_to_end)},
#line 83 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str30, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_scrolling_region)},
#line 31 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str31, VTE_SEQUENCE_HANDLER(vte_sequence_handler_form_feed)},
#line 79 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str32, VTE_SEQUENCE_HANDLER(vte_sequence_handler_window_manipulation)},
#line 84 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str33, VTE_SEQUENCE_HANDLER(vte_sequence_handler_set_scrolling_region_from_start)},
#line 126 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str34, VTE_SEQUENCE_HANDLER(vte_sequence_handler_send_primary_device_attributes)},
#line 127 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str35, VTE_SEQUENCE_HANDLER(vte_sequence_handler_noop)},
#line 129 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str36, VTE_SEQUENCE_HANDLER(vte_sequence_handler_send_secondary_device_attributes)},
#line 28 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str37, VTE_SEQUENCE_HANDLER(vte_sequence_handler_decreset)},
#line 35 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str38, VTE_SEQUENCE_HANDLER(vte_sequence_handler_tab_clear)},
#line 38 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str39, VTE_SEQUENCE_HANDLER(vte_sequence_handler_reset_mode)},
#line 30 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str40, VTE_SEQUENCE_HANDLER(vte_sequence_handler_UP)},
#line 48 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str41, VTE_SEQUENCE_HANDLER(vte_sequence_handler_restore_mode)},
#line 42 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str42, VTE_SEQUENCE_HANDLER(vte_sequence_handler_sc)},
#line 58 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str43, VTE_SEQUENCE_HANDLER(vte_sequence_handler_RI)},
#line 63 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str44, VTE_SEQUENCE_HANDLER(vte_sequence_handler_LE)},
#line 68 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str45, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_next_line)},
#line 72 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str46, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_lower_left)},
#line 47 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str47, VTE_SEQUENCE_HANDLER(vte_sequence_handler_insert_lines)},
#line 40 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str48, VTE_SEQUENCE_HANDLER(vte_sequence_handler_DO)},
#line 76 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str49, VTE_SEQUENCE_HANDLER(vte_sequence_handler_return_terminal_id)},
#line 88 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str50, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_preceding_line)},
#line 62 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str51, VTE_SEQUENCE_HANDLER(vte_sequence_handler_bt)},
#line 64 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str52, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_position)},
#line 82 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str53, VTE_SEQUENCE_HANDLER(vte_sequence_handler_device_status_report)},
#line 112 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str54, VTE_SEQUENCE_HANDLER(vte_sequence_handler_cursor_character_absolute)},
#line 96 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str55, VTE_SEQUENCE_HANDLER(vte_sequence_handler_return_terminal_status)},
#line 34 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str56, VTE_SEQUENCE_HANDLER(vte_sequence_handler_scroll_up)},
#line 80 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str57, VTE_SEQUENCE_HANDLER(vte_sequence_handler_change_cursor_color)},
#line 99 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str58, VTE_SEQUENCE_HANDLER(vte_sequence_handler_insert_blank_characters)},
#line 24 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str59, VTE_SEQUENCE_HANDLER(vte_sequence_handler_index)},
#line 118 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str60, VTE_SEQUENCE_HANDLER(vte_sequence_handler_request_terminal_parameters)},
#line 50 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str61, VTE_SEQUENCE_HANDLER(vte_sequence_handler_vertical_tab)},
#line 113 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str62, VTE_SEQUENCE_HANDLER(vte_sequence_handler_ta)},
#line 54 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str63, VTE_SEQUENCE_HANDLER(vte_sequence_handler_reverse_index)},
#line 43 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str64, VTE_SEQUENCE_HANDLER(vte_sequence_handler_scroll_down)},
#line 74 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str65, VTE_SEQUENCE_HANDLER(vte_sequence_handler_application_keypad)},
#line 102 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str66, VTE_SEQUENCE_HANDLER(vte_sequence_handler_local_charset)},
      {-1}, {-1}, {-1}, {-1}, {-1},
#line 60 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str72, VTE_SEQUENCE_HANDLER(vte_sequence_handler_rc)},
#line 128 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str73, VTE_SEQUENCE_HANDLER(vte_sequence_handler_horizontal_and_vertical_position)},
      {-1}, {-1},
#line 44 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str76, VTE_SEQUENCE_HANDLER(vte_sequence_handler_change_color)},
      {-1},
#line 81 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str78, VTE_SEQUENCE_HANDLER(vte_sequence_handler_character_attributes)},
      {-1}, {-1}, {-1}, {-1},
#line 117 "vteseq-n.gperf"
      {(int)(long)&((struct vteseq_n_pool_t *)0)->vteseq_n_pool_str83, VTE_SEQUENCE_HANDLER(vte_sequence_handler_character_position_absolute)}
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
