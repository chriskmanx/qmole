package gnu.x11.keysym;


/**
 * Korean-specific keyboard symbols. Imported from
 * <code>XK_KOREAN</code> section of
 * <code>/usr/include/X11/keysymdef.h</code>. Note that byte 3 = 0x0e.
 */
public class Korean {
  public static final int TOGGLE = 0xff31; /* hangul start/stop(toggle) */
  public static final int START = 0xff32; /* hangul start */
  public static final int END = 0xff33; /* hangul end, english start */
  public static final int HANJA = 0xff34; /* start hangul->hanja conversion */
  public static final int JAMO = 0xff35; /* hangul jamo mode */
  public static final int ROMAJA = 0xff36; /* hangul romaja mode */
  public static final int CODEINPUT = 0xff37; /* hangul code input mode */
  public static final int JEONJA = 0xff38; /* jeonja mode */
  public static final int BANJA = 0xff39; /* banja mode */
  public static final int PRE_HANJA = 0xff3a; /* pre hanja conversion */
  public static final int POST_HANJA = 0xff3b; /* post hanja conversion */
  public static final int SINGLE_CANDIDATE = 0xff3c; /* single candidate */
  public static final int MULTIPLE_CANDIDATE = 0xff3d; /* multiple candidate */
  public static final int PREVIOUS_CANDIDATE = 0xff3e; /* previous candidate */
  public static final int SPECIAL = 0xff3f; /* special symbols */
  public static final int SWITCH = 0xff7e; /* alias for mode_switch */


  /* Hangul Consonant Characters. */

  public static final int KIYEOG = 0xea1;
  public static final int SSANG_KIYEOG = 0xea2;
  public static final int KIYEOG_SIOS = 0xea3;
  public static final int NIEUN = 0xea4;
  public static final int NIEUN_JIEUJ = 0xea5;
  public static final int NIEUN_HIEUH = 0xea6;
  public static final int DIKEUD = 0xea7;
  public static final int SSANG_DIKEUD = 0xea8;
  public static final int RIEUL = 0xea9;
  public static final int RIEUL_KIYEOG = 0xeaa;
  public static final int RIEUL_MIEUM = 0xeab;
  public static final int RIEUL_PIEUB = 0xeac;
  public static final int RIEUL_SIOS = 0xead;
  public static final int RIEUL_TIEUT = 0xeae;
  public static final int RIEUL_PHIEUF = 0xeaf;
  public static final int RIEUL_HIEUH = 0xeb0;
  public static final int MIEUM = 0xeb1;
  public static final int PIEUB = 0xeb2;
  public static final int SSANG_PIEUB = 0xeb3;
  public static final int PIEUB_SIOS = 0xeb4;
  public static final int SIOS = 0xeb5;
  public static final int SSANG_SIOS = 0xeb6;
  public static final int IEUNG = 0xeb7;
  public static final int JIEUJ = 0xeb8;
  public static final int SSANG_JIEUJ = 0xeb9;
  public static final int CIEUC = 0xeba;
  public static final int KHIEUQ = 0xebb;
  public static final int TIEUT = 0xebc;
  public static final int PHIEUF = 0xebd;
  public static final int HIEUH = 0xebe;


  /* Hangul Vowel Characters. */

  public static final int A = 0xebf;
  public static final int AE = 0xec0;
  public static final int YA = 0xec1;
  public static final int YAE = 0xec2;
  public static final int EO = 0xec3;
  public static final int E = 0xec4;
  public static final int YEO = 0xec5;
  public static final int YE = 0xec6;
  public static final int O = 0xec7;
  public static final int WA = 0xec8;
  public static final int WAE = 0xec9;
  public static final int OE = 0xeca;
  public static final int YO = 0xecb;
  public static final int U = 0xecc;
  public static final int WEO = 0xecd;
  public static final int WE = 0xece;
  public static final int WI = 0xecf;
  public static final int YU = 0xed0;
  public static final int EU = 0xed1;
  public static final int YI = 0xed2;
  public static final int I = 0xed3;


  /* Hangul syllable-final (JongSeong) Characters. */

  public static final int J_KIYEOG = 0xed4;
  public static final int J_SSANG_KIYEOG = 0xed5;
  public static final int J_KIYEOG_SIOS = 0xed6;
  public static final int J_NIEUN = 0xed7;
  public static final int J_NIEUN_JIEUJ = 0xed8;
  public static final int J_NIEUN_HIEUH = 0xed9;
  public static final int J_DIKEUD = 0xeda;
  public static final int J_RIEUL = 0xedb;
  public static final int J_RIEUL_KIYEOG = 0xedc;
  public static final int J_RIEUL_MIEUM = 0xedd;
  public static final int J_RIEUL_PIEUB = 0xede;
  public static final int J_RIEUL_SIOS = 0xedf;
  public static final int J_RIEUL_TIEUT = 0xee0;
  public static final int J_RIEUL_PHIEUF = 0xee1;
  public static final int J_RIEUL_HIEUH = 0xee2;
  public static final int J_MIEUM = 0xee3;
  public static final int J_PIEUB = 0xee4;
  public static final int J_PIEUB_SIOS = 0xee5;
  public static final int J_SIOS = 0xee6;
  public static final int J_SSANG_SIOS = 0xee7;
  public static final int J_IEUNG = 0xee8;
  public static final int J_JIEUJ = 0xee9;
  public static final int J_CIEUC = 0xeea;
  public static final int J_KHIEUQ = 0xeeb;
  public static final int J_TIEUT = 0xeec;
  public static final int J_PHIEUF = 0xeed;
  public static final int J_HIEUH = 0xeee;

  
  /* Ancient Hangul Consonant Characters. */

  public static final int RIEUL_YEORIN_HIEUH = 0xeef;
  public static final int SUNKYEONGEUM_MIEUM = 0xef0;
  public static final int SUNKYEONGEUM_PIEUB = 0xef1;
  public static final int PAN_SIOS = 0xef2;
  public static final int KKOGJI_DALRIN_IEUNG = 0xef3;
  public static final int SUNKYEONGEUM_PHIEUF = 0xef4;
  public static final int YEORIN_HIEUH = 0xef5;


  /* Ancient Hangul Vowel Characters. */

  public static final int ARAEA = 0xef6;
  public static final int ARAEAE = 0xef7;


  /* Ancient Hangul syllable-final (JongSeong) Characters. */

  public static final int J_PAN_SIOS = 0xef8;
  public static final int J_KKOGJI_DALRIN_IEUNG = 0xef9;
  public static final int J_YEORIN_HIEUH = 0xefa;


  /* Korean currency symbol. */  
  public static final int WON = 0xeff;
}
