/********************************************************************************
*                                                                               *
*                 R e g u l a r   E x p r e s s i o n   C l a s s               *
*                                                                               *
*********************************************************************************
* Copyright (C) 1999,2006 by Jeroen van der Zijp.   All Rights Reserved.        *
*********************************************************************************
* This library is free software; you can redistribute it and/or                 *
* modify it under the terms of the GNU Lesser General Public                    *
* License as published by the Free Software Foundation; either                  *
* version 2.1 of the License, or (at your option) any later version.            *
*                                                                               *
* This library is distributed in the hope that it will be useful,               *
* but WITHOUT ANY WARRANTY; without even the implied warranty of                *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU             *
* Lesser General Public License for more details.                               *
*                                                                               *
* You should have received a copy of the GNU Lesser General Public              *
* License along with this library; if not, write to the Free Software           *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.    *
*********************************************************************************
* $Id: FXRex.cpp,v 1.98 2006/03/18 01:05:58 fox Exp $                           *
********************************************************************************/
#include "xincs.h"
#include "fxver.h"
#include "fxdefs.h"
#include "fxascii.h"
#include "fxunicode.h"
#include "FXHash.h"
#include "FXStream.h"
#include "FXString.h"
#include "FXRex.h"


/*
  The Story:
  ==========

  As with most regex implementations, this one is inspired by Henry Spencer's
  original implementation.

  This is however an ab-initio implementation, with the following goals:

        o Full C++ implementation, no simple C++ wrapper.
        o Trade speed and memory in favor of speed, but keep it compact where
          possible.
        o Thread-safe:
            - No global variables used during parsing or execution.
            - Multiple threads could use same single FXRex at the same time.
        o Perl-like syntax for character classes.
        o Additional features such as lazy/greedy/possessive closures, counting
          repeats, back references, trailing context.
        o Forward and backward subject string scanning mode.
        o Single line/multi line matching modes.
        o 8-bit safe (you can use it to grep binary data!).
        o No arbitrary limitations.  Patterns can be as long as you need them
          to be, memory allowing.  This is possible due to representing the
          progam as 32-bit integers.
        o When parsing fails, or when created with default constructor, FXRex
          is initialized to a "fallback" program; its thus safe to invoke match
          at any time.
        o The default fallback program will fail to match anything.
        o Observe current locale if your "ctype.h" functions support it, by calling
          all locale-sensitive functions during the match.
        o Convenient feature: disallow empty string matches; this is nice as
          it prevents a common problem, for example searching for "a*" in "bbba";
          without the REX_NOT_EMPTY option, this matches "" and not the "a".
        o Another convenient feature is the ability to compile verbatim strings.
          This is practical as it allows FXRex to be populated with a simple
          string with no interpretation of special characters ^*?+{}()\$.[].
        o Note that FXRex's implementation would naturally move to wide
          character support...

  Because this is completely new implementation of regular expressions, and
  not merely an extension of a previous implementation, some people may want to
  adapt it for use outside of FOX.  This is perfectly OK with me.

  However:

        o The Author is not responsible for the consequences of using this
          software.

        o Recipient should be clearly informed of the origin of the software;
          and if alterations have been made, this must be stated clearly.

        o Software will continue to fall under Lesser GPL, unless specific
          permission from the Author has been obtained.


  Implementation notes:
  =====================

  This implementation does not use "nodes" with "next" pointers; instead, the "next"
  opcode is located implicitly by simple sequentiality.  This has beneficial effect
  on speed, as one can simply add to the program counter instead of performing a
  memory reference.

  Sometimes one needs to jump out of sequence; this is implemented by an
  explicit jump instruction.  Because it works with 32-bit offsets, there
  is no need to distinguish between forward and backward jumps.

  Henry Spencer implemented a trick to "prefix" simple single-character matching
  opcodes with a closure operator, by shifting down already generated code and
  inserting the closure operator in front of it.

  FXRex uses the same trick of shifting down code; but this same trick is also
  useful for branches!

  FXRex only generates a branch node when an alternative has in fact been seen;
  if no alternative is there, we've saved ourselves both a branch node and a
  jump node!

  This has interesting side-effects:

        o The common case of 1 single branch now no longer needs a branch opcode
          and corresponding jump opcode at all!

        o When an alternative is found, we insert a branch node in front and a jump
          node behind the already generated code.  This can be done easily as
          branches and jumps within the shifted block of code are relative, and have
          already been resolved!

        o The matching algorithm for a branch opcode simplifies as well:- either
          it matches the first branch, or it continues after the jump.

        o Its easier to dig out some info from the program, and in fact so easy
          that this digging can be moved to the execute phase.

  When a repeat is prefixed in front of a simple single-character match, counted
  repeats are simplified: {1}, {1,1} is eliminated, {}, {0,} becomes *, {1,} becomes
  +, and {0,1} becomes ?.

  Because single-character repeats are prefixed with a repeat instruction, there is
  no recursion needed; single character repeats are therefore very fast.

  Complex repeats are implemented using branch loop constructs and may involve
  recursions (in fact only the fixed repeat is non-recursive!).  Hence complex repeats
  should be avoided when single-character repeats will suffice.

  OP_BRANCH and OP_BRANCHREV implement alternatives. For OP_BRANCH, first the inline
  code immediately following the offset is executed; if the inline code fails, OP_BRANCH
  takes a jump to the new location and tries the alternative.

  For OP_BRANCHREV, it works the opposite way: OP_BRANCHREV takes the alternative
  first, before trying the inline code.

  Having both OP_BRANCH and OP_BRANCHREV substantially simplifies the design of
  complex, greedy or lazy matches:- the greedy and lazy match turn out to have the
  same code structure, except we're using OP_BRANCHREV for the lazy matches and
  OP_BRANCH for the greedy ones.

  OP_JUMP is an unconditional jump to a new location. OP_JUMPLT and OP_JUMPGT jump
  when the loop counter is less than or greater than the given value, respectively.


  Atomic Matching Groups
  ======================

  For example, trying to match pattern "\d+foo" against subject string "123456bar",
  the matcher will eat all digits up till "6", and then backtrack by trying digits
  up till 5, and so on.  A atomic subgroup match will simply fail if the sub-pattern
  fails to match at the end.  This can be written as: "(?>\d+)foo".  Atomic groups
  are thus more efficient since no repeated tries are being made.


  Greedy, Lazy, and Possessive Matches
  ====================================

  Greedy: the "a*" in pattern "a*ardvark" matching subject string "aardvark" will match
  "aa", then backtrack and match "a", after which the match succeeds.

  Lazy: the "a*?" in pattern "a*?ardvark" will first match "", then try match "a" after
  which the match succeeds.

  Possessive: the "a*+" in pattern "a*+ardvark" will match "aa", then fail without
  backing off.

  Possessive matches and Atomic matching groups are closely related in terms of
  controlling the recursion depth of the matcher.


  Syntax:
  =======

      Special Constructs

      (?i X )   Match sub pattern case insensitive
      (?I X )   Match sub pattern case sensitive
      (?n X )   Match sub pattern with newlines
      (?N X )   Match sub pattern with no newlines
      (?: X )   Non-capturing parentheses
      (?= X )   Zero width positive lookahead
      (?! X )   Zero width negative lookahead
      (?<= X )  Zero width positive lookbehind
      (?<! X )  Zero width negative lookbehind
      (?> X )   Atomic grouping (possessive match)

      Logical Operators

      X Y       X followed by Y
      X | Y     Either X or Y
      ( X )     Sub pattern (capturing if REX_CAPTURE)

      Greedy Quantifiers

      X *       Match 0 or more
      X +       Match 1 or more
      X ?       Match 0 or 1
      X {}      Match 0 or more
      X {n}     Match n times
      X {,m}    Match no more than m times
      X {n,}    Match n or more
      X {n,m}   Match at least n but no more than m times

      Lazy Quantifiers

      X *?      Match 0 or more
      X +?      Match 1 or more
      X ??      Match 0 or 1
      X {}?     Match 0 or more times
      X {n}?    Match n times
      X {,m}?   Match no more than m times
      X {n,}?   Match n or more
      X {n,m}?  Match at least n but no more than m times

      Possessive (non-backtracking) Quantifiers

      X *+      Match 0 or more
      X ++      Match 1 or more
      X ?+      Match 0 or 1
      X {}+     Match 0 or more times
      X {n}+    Match n times
      X {,m}+   Match no more than m times
      X {n,}+   Match n or more
      X {n,m}+  Match at least n but no more than m times

      Boundary Matching
      ^         Match begin of line [if at begin of pattern]
      $         Match end of line [if at end of pattern]
      \<        Begin of word
      \>        End of word
      \b        Word boundary
      \B        Word interior
      \A        Match only beginning of string
      \Z        Match only and end of string

      Character Classes

      [abc]     Match a, b, or c
      [^abc]    Match any but a, b, or c
      [a-zA-Z]  Match upper- or lower-case a through z
      []]       Matches ]
      [-]       Matches -

      Predefined character classes

      .         Match any character
      \d        Digit [0-9]
      \D        Non-digit
      \s        Space
      \S        Non-space
      \w        Word character [a-zA-Z_0-9]
      \W        Non-word character
      \l        Letter [a-zA-Z]
      \L        Non-letter
      \h        Hex digit [0-9a-fA-F]
      \H        Non-hex digit
      \u        Single uppercase character
      \U        Single lowercase character
      \p        Punctuation (not including '_')
      \P        Non punctuation

      Characters

      x           Any character
      \\          Back slash character
      \033        Octal
      \x1b        Hex
      \u1FD1      Unicode U+1FD1 (GREEK SMALL LETTER IOTA WITH MACRON))
      \U00010450  Wide unicode U+10450 (SHAVIAN LETTER PEEP)
      \a          Alarm, bell
      \e          Escape character
      \t          Tab
      \f          Form feed
      \n          Newline
      \r          Return
      \v          Vertical tab
      \cx         Control character

      Back References

      \1        Reference to 1st capturing group
      \2        Reference to 2nd capturing group
      ...
      \9        Reference to 9th capturing group

      POSIX character classes (US-ASCII only)

      \P{name}    Matches anything BUT what \p{name} matches

      \p{Lower}   A lower-case alphabetic character: [a-z]
      \p{Upper}   An upper-case alphabetic character: [A-Z]
      \p{ASCII}   All ASCII: [\x00-\x7F]
      \p{Alpha}   An alphabetic character:[\p{Lower}\p{Upper}]
      \p{Digit}   A decimal digit: [0-9]
      \p{Alnum}   An alphanumeric character: [\p{Alpha}\p{Digit}]
      \p{Punct}   Punctuation: One of !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
      \p{Graph}   A visible character: [\p{Alnum}\p{Punct}]
      \p{Print}   A printable character: [\p{Graph}]
      \p{Blank}   A space or a tab: [ \t]
      \p{Cntrl}   A control character: [\x00-\x1F\x7F]
      \p{XDigit}  A hexadecimal digit: [0-9a-fA-F]
      \p{Space}   A whitespace character: [ \t\n\x0B\f\r]


      Unicode general character categories

      \p{C}     Other (Cc | Cf | Cn | Co | Cs)
      \p{Cc}    Control
      \p{Cf}    Format
      \p{Cn}    Unassigned
      \p{Co}    Private use
      \p{Cs}    Surrogate

      \p{L}     Letter (Ll | Lm | Lo | Lt | Lu)
      \p{Ll}    Lower case letter
      \p{Lm}    Modifier letter
      \p{Lo}    Other letter
      \p{Lt}    Title case letter
      \p{Lu}    Upper case letter

      \p{M}     Mark (Mc | Me | Mn)
      \p{Mc}    Spacing mark
      \p{Me }   Enclosing mark
      \p{Mn}    Non-spacing mark

      \p{N}     Number (Nd | Nl | No)
      \p{Nd}    Decimal number
      \p{Nl}    Letter number
      \p{No}    Other number

      \p{P}     Punctuation (Pc | Pd | Pe | Pf | Pi | Po | Ps)
      \p{Pc}    Connector punctuation
      \p{Pd}    Dash punctuation
      \p{Pe}    Close punctuation
      \p{Pf}    Final punctuation
      \p{Pi}    Initial punctuation
      \p{Po}    Other punctuation
      \p{Ps}    Open punctuation

      \p{S}     Symbol (Sc | Sk | Sm | So)
      \p{Sc}    Currency symbol
      \p{Sk}    Modifier symbol
      \p{Sm}    Mathematical symbol
      \p{So}    Other symbol

      \p{Z}     Separator (Zl | Zp | Zs)
      \p{Zl}    Line separator
      \p{Zp}    Paragraph separator
      \p{Zs}    Space separator


      Unicode script categories

      \p{Arab}  Arabic
      \p{Armn}  Armenian
      \p{Beng}  Bengali
      \p{Bopo}  Bopomofo
      \p{Brai}  Braille
      \p{Bugi}  Buginese
      \p{Buhd}  Buhid
      \p{Cans}  Canadian_Aboriginal
      \p{Cher}  Cherokee
      \p{Copt}  Coptic (Qaac)
      \p{Cprt}  Cypriot
      \p{Cyrl}  Cyrillic
      \p{Deva}  Devanagari
      \p{Dsrt}  Deseret
      \p{Ethi}  Ethiopic
      \p{Geor}  Georgian
      \p{Glag}  Glagolitic
      \p{Goth}  Gothic
      \p{Grek}  Greek
      \p{Gujr}  Gujarati
      \p{Guru}  Gurmukhi
      \p{Hang}  Hangul
      \p{Hani}  Han
      \p{Hano}  Hanunoo
      \p{Hebr}  Hebrew
      \p{Hira}  Hiragana
      \p{Hrkt}  Katakana_Or_Hiragana
      \p{Ital}  Old_Italic
      \p{Kana}  Katakana
      \p{Khar}  Kharoshthi
      \p{Khmr}  Khmer
      \p{Knda}  Kannada
      \p{Laoo}  Lao
      \p{Latn}  Latin
      \p{Limb}  Limbu
      \p{Linb}  Linear_B
      \p{Mlym}  Malayalam
      \p{Mong}  Mongolian
      \p{Mymr}  Myanmar
      \p{Ogam}  Ogham
      \p{Orya}  Oriya
      \p{Osma}  Osmanya
      \p{Qaai}  Inherited
      \p{Runr}  Runic
      \p{Shaw}  Shavian
      \p{Sinh}  Sinhala
      \p{Sylo}  Syloti_Nagri
      \p{Syrc}  Syriac
      \p{Tagb}  Tagbanwa
      \p{Tale}  Tai_Le
      \p{Talu}  New_Tai_Lue
      \p{Taml}  Tamil
      \p{Telu}  Telugu
      \p{Tfng}  Tifinagh
      \p{Tglg}  Tagalog
      \p{Thaa}  Thaana
      \p{Thai}  Thai
      \p{Tibt}  Tibetan
      \p{Ugar}  Ugaritic
      \p{Xpeo}  Old_Persian
      \p{Yiii}  Yi
      \p{Zyyy}  Common


  Grammar:
  ========

      exp        ::= branch { "|" branch }*
      branch     ::= { piece }*
      piece      ::= atom [ rep ]
      rep        ::= ( "*" | "+" | "?" | counts ) [ "?" ]
      counts     ::= "{" digits ["," [ digits] ] "}"
      atom       ::= "(" exp ")" | "[" [^] range "]" | characters
      range      ::= { character | character "-" character } +
      characters ::= { character }*
      digits     ::= { digit }*

  To do:
  ======

  - Look into optimizing character class when possible (e.g.
    collapse [0-9] to OP_DIGIT and [^A] into OP_NOT_CHAR).
  - Should \D, \W, \L match newline?
  - Look behind would be nice...
  - Repeating back references, only possible if capturing parentheses
    are known NOT to match "".
  - Note the \uXXXX is going to be used for UNICODE perhaps:
    See: http://www.unicode.org/unicode/reports/tr18.
  - Implement possessive and atomic groups
*/


// As close to infinity as we're going to get; this seems big
// enough.  We can not make it MAX_INT as this may wrap around when
// added to something else!
#define ONEINDIG 1000000

// Number of capturing sub-expressions allowed
#define NSUBEXP  10

// Size of string buffer
#define MAXCHARS 512

// Set operations
#define EXCL(set,ch) (set[((FXuchar)(ch))>>5]&=~(1<<(((FXuchar)(ch))&31)))
#define INCL(set,ch) (set[((FXuchar)(ch))>>5]|=(1<<(((FXuchar)(ch))&31)))
#define ISIN(set,ch) (set[((FXuchar)(ch))>>5]&(1<<(((FXuchar)(ch))&31)))
#define CLEAR(set)   (set[0]=set[1]=set[2]=set[3]=set[4]=set[5]=set[6]=set[7]=0)

using namespace FX;

/*******************************************************************************/

namespace {

// Opcodes of the engine
enum {
  OP_END           =   0,           // End of pattern reached
  OP_FAIL          =   1,           // Always fail
  OP_SUCCEED       =   2,           // Always succeed
  OP_LINE_BEG      =   3,           // Beginning of line
  OP_LINE_END      =   4,           // End of line
  OP_WORD_BEG      =   5,           // Beginning of word
  OP_WORD_END      =   6,           // End of word
  OP_WORD_BND      =   7,           // Word boundary
  OP_WORD_INT      =   8,           // Word interior
  OP_STR_BEG       =   9,           // Beginning of string
  OP_STR_END       =  10,           // End of string
  OP_ANY_OF        =  11,           // Any character in set
  OP_ANY_BUT       =  12,           // Any character not in set
  OP_ANY           =  13,           // Any character but no newline
  OP_ANY_NL        =  14,           // Any character including newline
  OP_SPACE         =  15,           // White space
  OP_SPACE_NL      =  16,           // White space including newline
  OP_NOT_SPACE     =  17,           // Non-white space
  OP_DIGIT         =  18,           // Digit
  OP_NOT_DIGIT     =  19,           // Non-digit
  OP_NOT_DIGIT_NL  =  20,           // Non-digit including newline
  OP_LETTER        =  21,           // Letter
  OP_NOT_LETTER    =  22,           // Non-letter
  OP_NOT_LETTER_NL =  23,           // Non-letter including newline
  OP_WORD          =  24,           // Word character
  OP_NOT_WORD      =  25,           // Non-word character
  OP_NOT_WORD_NL   =  26,           // Non-word character including newline
  OP_HEX           =  27,           // Hex digit
  OP_NOT_HEX       =  28,           // Non hex digit
  OP_NOT_HEX_NL    =  29,           // Non hex digit including newline
  OP_PUNCT         =  30,           // Punctuation
  OP_NOT_PUNCT     =  31,           // Non punctuation
  OP_NOT_PUNCT_NL  =  32,           // Non punctuation including newline
  OP_CHARS         =  33,           // Match literal string
  OP_CHARS_CI      =  34,           // Match literal string, case insensitive
  OP_CHAR          =  35,           // Single character
  OP_CHAR_CI       =  36,           // Single character, case insensitive
  OP_JUMP          =  37,           // Jump to another location
  OP_BRANCH        =  38,           // Branch: jump after trying following code
  OP_BRANCHREV     =  39,           // Branch: jump before trying following code
  OP_STAR          =  40,           // Greedy * (simple)
  OP_MIN_STAR      =  41,           // Lazy * (simple)
  OP_POS_STAR      =  42,           // Possessive * (simple)
  OP_PLUS          =  43,           // Greedy + (simple)
  OP_MIN_PLUS      =  44,           // Lazy + (simple)
  OP_POS_PLUS      =  45,           // Possessive + (simple)
  OP_QUEST         =  46,           // Greedy ? (simple)
  OP_MIN_QUEST     =  47,           // Lazy ? (simple)
  OP_POS_QUEST     =  48,           // Possessive ? (simple)
  OP_REP           =  49,           // Greedy counted repeat (simple)
  OP_MIN_REP       =  50,           // Lazy counted repeat (simple)
  OP_POS_REP       =  51,           // Possessive counted repeat (simple)
  OP_LOOK_NEG      =  52,           // Negative look ahead
  OP_LOOK_POS      =  53,           // Positive look ahead
  OP_UPPER         =  54,           // Match upper case
  OP_LOWER         =  55,           // Match lower case
  OP_SUB_BEG       =  56,           // Start of substring i
  OP_SUB_END       =  66,           // End of substring i
  OP_REF           =  76,           // Back reference to substring i
  OP_REF_CI        =  86,           // Match substring i case insensitive
  OP_ZERO          =  96,           // Zero count i
  OP_INCR          = 106,           // Increment count i
  OP_JUMPLT        = 116,           // Jump if count i less than value
  OP_JUMPGT        = 126            // JUmp if count i greater than value
  };


// Flags
enum {
  FLG_WORST  = 0,           // Worst case
  FLG_WIDTH  = 1,           // Matches >=1 character
  FLG_SIMPLE = 2            // Simple
  };


// Structure used during matching
struct FXExecute {
  const FXchar  *str;               // String
  const FXchar  *str_beg;           // Begin of string
  const FXchar  *str_end;           // End of string
  FXint         *sub_beg;           // Begin of substring i
  FXint         *sub_end;           // End of substring i
  const FXint   *code;              // Program code
  FXint          npar;              // Number of capturing parentheses
  FXint          count[10];         // Counters for counted repeats
  FXint          mode;              // Match mode

  // Attempt to match
  bool attempt(const FXchar* string);

  // Match at current string position
  bool match(const FXint* prog);

  // Execute
  bool execute(const FXchar* fm,const FXchar* to);
  };


// Structure used during compiling
struct FXCompile {
  const FXchar  *pat;               // Pattern string pointer
  FXint         *code;              // Program code
  FXint         *pc;                // Program counter
  FXint          mode;              // Compile mode
  FXint          nbra;              // Number of counting braces
  FXint          npar;              // Number of capturing parentheses

  // Code generation
  FXint* append(FXint op);
  FXint* append(FXint op,FXint arg);
  FXint* append(FXint op,FXint arg1,FXint arg2);
  FXint* append(FXint op,FXint set[]);
  FXint* append(FXint op,FXint len,FXint *data);
  FXint* insert(FXint *ptr,FXint op);
  FXint* insert(FXint *ptr,FXint op,FXint arg);
  FXint* insert(FXint *ptr,FXint op,FXint arg1,FXint arg2);

  // Patch branches
  void patch(FXint *fm,FXint *to);

  // Parsing
  FXRexError compile(FXint& flags);
  FXRexError expression(FXint& flags);
  FXRexError verbatim(FXint& flags);
  FXRexError alternative(FXint& flags);
  FXRexError piece(FXint& flags);
  FXRexError atom(FXint& flags);
  FXRexError charset();
  };


/*******************************************************************************/

#ifndef NDEBUG

// Dump program
void dump(FXint *prog){
  FXint op,min,max,no,val;
  fxmessage("\n");
  fxmessage("Program:\n");
  fxmessage("%-10p SIZE %d\n",prog,*prog);
  prog++;
  while(1){
    fxmessage("%-10p ",prog);
    op=*prog++;
    switch(op){
      case OP_END:
        fxmessage("OP_END\n");
        goto x;
      case OP_FAIL:
        fxmessage("OP_FAIL\n");
        break;
      case OP_SUCCEED:
        fxmessage("OP_SUCCEED\n");
        break;
      case OP_LINE_BEG:
        fxmessage("OP_LINE_BEG\n");
        break;
      case OP_LINE_END:
        fxmessage("OP_LINE_END\n");
        break;
      case OP_WORD_BEG:
        fxmessage("OP_WORD_BEG\n");
        break;
      case OP_WORD_END:
        fxmessage("OP_WORD_END\n");
        break;
      case OP_WORD_BND:
        fxmessage("OP_WORD_BND\n");
        break;
      case OP_WORD_INT:
        fxmessage("OP_WORD_INT\n");
        break;
      case OP_STR_BEG:
        fxmessage("OP_STR_BEG\n");
        break;
      case OP_STR_END:
        fxmessage("OP_STR_END\n");
        break;
      case OP_ANY_OF:
        fxmessage("OP_ANY_OF   \"");
        for(no=0; no<256; no++){
          if(ISIN(prog,no)){
            if(' '<=no){ fxmessage("%c",no); } else { fxmessage("\\x%02x",no); }
            }
          }
        fxmessage("\"\n");
        prog+=8;
        break;
      case OP_ANY_BUT:
        fxmessage("OP_ANY_BUT  \"");
        for(no=0; no<256; no++){
          if(ISIN(prog,no)){
            if(' '<=no){ fxmessage("%c",no); } else { fxmessage("\\x%02x",no); }
            }
          }
        fxmessage("\"\n");
        prog+=8;
        break;
      case OP_ANY:
        fxmessage("OP_ANY\n");
        break;
      case OP_ANY_NL:
        fxmessage("OP_ANY_NL\n");
        break;
      case OP_SPACE:
        fxmessage("OP_SPACE\n");
        break;
      case OP_SPACE_NL:
        fxmessage("OP_SPACE_NL\n");
        break;
      case OP_NOT_SPACE:
        fxmessage("OP_NOT_SPACE\n");
        break;
      case OP_DIGIT:
        fxmessage("OP_DIGIT\n");
        break;
      case OP_NOT_DIGIT:
        fxmessage("OP_NOT_DIGIT\n");
        break;
      case OP_NOT_DIGIT_NL:
        fxmessage("OP_NOT_DIGIT_NL\n");
        break;
      case OP_LETTER:
        fxmessage("OP_LETTER\n");
        break;
      case OP_NOT_LETTER:
        fxmessage("OP_NOT_LETTER\n");
        break;
      case OP_NOT_LETTER_NL:
        fxmessage("OP_NOT_LETTER_NL\n");
        break;
      case OP_WORD:
        fxmessage("OP_WORD\n");
        break;
      case OP_NOT_WORD:
        fxmessage("OP_NOT_WORD\n");
        break;
      case OP_NOT_WORD_NL:
        fxmessage("OP_NOT_WORD_NL\n");
        break;
      case OP_HEX:
        fxmessage("OP_HEX\n");
        break;
      case OP_NOT_HEX:
        fxmessage("OP_NOT_HEX\n");
        break;
      case OP_NOT_HEX_NL:
        fxmessage("OP_NOT_HEX_NL\n");
        break;
      case OP_PUNCT:
        fxmessage("OP_PUNCT\n");
        break;
      case OP_NOT_PUNCT:
        fxmessage("OP_NOT_PUNCT\n");
        break;
      case OP_NOT_PUNCT_NL:
        fxmessage("OP_NOT_PUNCT_NL\n");
        break;
      case OP_UPPER:
        fxmessage("OP_UPPER\n");
        break;
      case OP_LOWER:
        fxmessage("OP_LOWER\n");
        break;
      case OP_CHARS:
        no=*prog++;
        fxmessage("OP_CHARS     %d,\"",no);
        while(no>0){
          if(' '<=*prog){ fxmessage("%c",*prog); } else { fxmessage("\\x%02x",*prog); }
          prog++;
          no--;
          }
        fxmessage("\"\n");
        break;
      case OP_CHARS_CI:
        no=*prog++;
        fxmessage("OP_CHARS_CI  %d,\"",no);
        while(no>0){
          if(' '<=*prog){ fxmessage("%c",*prog); } else { fxmessage("\\x%02x",*prog); }
          prog++;
          no--;
          }
        fxmessage("\"\n");
        break;
      case OP_CHAR:
        fxmessage("OP_CHAR      \"");
        if(' '<=*prog){ fxmessage("%c",*prog); } else { fxmessage("\\x%02x",*prog); }
        fxmessage("\"\n");
        prog++;
        break;
      case OP_CHAR_CI:
        fxmessage("OP_CHAR_CI   \"");
        if(' '<=*prog){ fxmessage("%c",*prog); } else { fxmessage("\\x%02x",*prog); }
        fxmessage("\"\n");
        prog++;
        break;
      case OP_JUMP:
        fxmessage("OP_JUMP      %-10p\n",*prog ? prog+*prog : 0);
        prog++;
        break;
      case OP_BRANCH:
        fxmessage("OP_BRANCH    %-10p\n",*prog ? prog+*prog : 0);
        prog++;
        break;
      case OP_BRANCHREV:
        fxmessage("OP_BRANCHREV %-10p\n",*prog ? prog+*prog : 0);
        prog++;
        break;
      case OP_STAR:
        fxmessage("OP_STAR\n");
        break;
      case OP_MIN_STAR:
        fxmessage("OP_MIN_STAR\n");
        break;
      case OP_POS_STAR:
        fxmessage("OP_POS_STAR\n");
        break;
      case OP_PLUS:
        fxmessage("OP_PLUS\n");
        break;
      case OP_MIN_PLUS:
        fxmessage("OP_MIN_PLUS\n");
        break;
      case OP_POS_PLUS:
        fxmessage("OP_POS_PLUS\n");
        break;
      case OP_QUEST:
        fxmessage("OP_QUEST\n");
        break;
      case OP_MIN_QUEST:
        fxmessage("OP_MIN_QUEST\n");
        break;
      case OP_POS_QUEST:
        fxmessage("OP_POS_QUEST\n");
        break;
      case OP_REP:
        min=*prog++;
        max=*prog++;
        fxmessage("OP_REP       {%d,%d}\n",min,max);
        break;
      case OP_MIN_REP:
        min=*prog++;
        max=*prog++;
        fxmessage("OP_MIN_REP   {%d,%d}\n",min,max);
        break;
      case OP_POS_REP:
        min=*prog++;
        max=*prog++;
        fxmessage("OP_POS_REP   {%d,%d}\n",min,max);
        break;
      case OP_LOOK_NEG:
        fxmessage("OP_LOOK_NEG  %-10p\n",*prog ? prog+*prog : 0);
        prog++;
        break;
      case OP_LOOK_POS:
        fxmessage("OP_LOOK_POS  %-10p\n",*prog ? prog+*prog : 0);
        prog++;
        break;
      case OP_SUB_BEG+0:
      case OP_SUB_BEG+1:
      case OP_SUB_BEG+2:
      case OP_SUB_BEG+3:
      case OP_SUB_BEG+4:
      case OP_SUB_BEG+5:
      case OP_SUB_BEG+6:
      case OP_SUB_BEG+7:
      case OP_SUB_BEG+8:
      case OP_SUB_BEG+9:
        fxmessage("OP_SUB_BEG%d\n",op-OP_SUB_BEG);
        break;
      case OP_SUB_END+0:
      case OP_SUB_END+1:
      case OP_SUB_END+2:
      case OP_SUB_END+3:
      case OP_SUB_END+4:
      case OP_SUB_END+5:
      case OP_SUB_END+6:
      case OP_SUB_END+7:
      case OP_SUB_END+8:
      case OP_SUB_END+9:
        fxmessage("OP_SUB_END%d\n",op-OP_SUB_END);
        break;
      case OP_REF+0:
      case OP_REF+1:
      case OP_REF+2:
      case OP_REF+3:
      case OP_REF+4:
      case OP_REF+5:
      case OP_REF+6:
      case OP_REF+7:
      case OP_REF+8:
      case OP_REF+9:
        fxmessage("OP_REF%d\n",op-OP_REF);
        break;
      case OP_REF_CI+0:
      case OP_REF_CI+1:
      case OP_REF_CI+2:
      case OP_REF_CI+3:
      case OP_REF_CI+4:
      case OP_REF_CI+5:
      case OP_REF_CI+6:
      case OP_REF_CI+7:
      case OP_REF_CI+8:
      case OP_REF_CI+9:
        fxmessage("OP_REF_CI%d\n",op-OP_REF_CI);
        break;
      case OP_ZERO+0:
      case OP_ZERO+1:
      case OP_ZERO+2:
      case OP_ZERO+3:
      case OP_ZERO+4:
      case OP_ZERO+5:
      case OP_ZERO+6:
      case OP_ZERO+7:
      case OP_ZERO+8:
      case OP_ZERO+9:
        fxmessage("OP_ZERO%d\n",op-OP_ZERO);
        break;
      case OP_INCR+0:
      case OP_INCR+1:
      case OP_INCR+2:
      case OP_INCR+3:
      case OP_INCR+4:
      case OP_INCR+5:
      case OP_INCR+6:
      case OP_INCR+7:
      case OP_INCR+8:
      case OP_INCR+9:
        fxmessage("OP_INCR%d\n",op-OP_INCR);
        break;
      case OP_JUMPLT+0:
      case OP_JUMPLT+1:
      case OP_JUMPLT+2:
      case OP_JUMPLT+3:
      case OP_JUMPLT+4:
      case OP_JUMPLT+5:
      case OP_JUMPLT+6:
      case OP_JUMPLT+7:
      case OP_JUMPLT+8:
      case OP_JUMPLT+9:
        val=*prog++;
        fxmessage("OP_JUMPLT%d   %d,%-10p\n",op-OP_JUMPLT,val,*prog ? prog+*prog : 0);
        prog++;
        break;
      case OP_JUMPGT+0:
      case OP_JUMPGT+1:
      case OP_JUMPGT+2:
      case OP_JUMPGT+3:
      case OP_JUMPGT+4:
      case OP_JUMPGT+5:
      case OP_JUMPGT+6:
      case OP_JUMPGT+7:
      case OP_JUMPGT+8:
      case OP_JUMPGT+9:
        val=*prog++;
        fxmessage("OP_JUMPGT%d   %d,%-10p\n",op-OP_JUMPGT,val,*prog ? prog+*prog : 0);
        prog++;
        break;
      default:
        fxmessage("OP_%d: error\n",op);
        goto x;
      }
    }
x:fxmessage("end\n");
  }

#endif

/*******************************************************************************/

// FXCompile members

// Parse hex escape code
FXint hex(const FXchar*& pat){
  register FXint ch,n;
  for(ch=0,n=2; Ascii::isHexDigit(*pat) && n; n--){
    ch=(ch<<4)+Ascii::digitValue(*pat++);
    }
  return ch;
  }


// Parse octal escape code
FXint oct(const FXchar*& pat){
  register FXint ch,n;
  for(ch=0,n=3; '0'<=*pat && *pat<='7' && n; n--){
    ch=(ch<<3)+(*pat++-'0');
    }
  return ch;
  }


// Compiler main
FXRexError FXCompile::compile(FXint& flags){
  FXRexError err;
  if(*pat=='\0') return REGERR_EMPTY;
  if(mode&REX_VERBATIM)
    err=verbatim(flags);
  else
    err=expression(flags);
  if(err!=REGERR_OK) return err;
  if(*pat!='\0') return REGERR_PAREN;
  append(OP_END);
  return REGERR_OK;
  }


// Parse without interpretation of magic characters
FXRexError FXCompile::verbatim(FXint& flags){
  FXint buf[MAXCHARS],ch,len;
  flags=FLG_WIDTH;
  while(*pat!='\0'){
    len=0;
    do{
      ch=*pat++;
      if(mode&REX_ICASE) ch=Ascii::toLower(ch);
      buf[len++]=(FXuchar)ch;
      }
    while(*pat!='\0' && len<MAXCHARS);
    if(len==1){
      flags|=FLG_SIMPLE;
      append((mode&REX_ICASE)?OP_CHAR_CI:OP_CHAR,buf[0]);
      }
    else{
      append((mode&REX_ICASE)?OP_CHARS_CI:OP_CHARS,len,buf);
      }
    }
  return REGERR_OK;
  }


// Parse expression
FXRexError FXCompile::expression(FXint& flags){
  FXRexError err;
  FXint *at,*jp,flg;
  flags=FLG_WIDTH;
  at=pc;
  jp=NULL;
  err=alternative(flg);
  if(err!=REGERR_OK) return err;
  if(!(flg&FLG_WIDTH)) flags&=~FLG_WIDTH;
  while(*pat=='|'){
    pat++;
    insert(at,OP_BRANCH,pc-at+3);
    append(OP_JUMP,jp?jp-pc-1:0);
    jp=pc-1;
    at=pc;
    err=alternative(flg);
    if(err!=REGERR_OK) return err;
    if(!(flg&FLG_WIDTH)) flags&=~FLG_WIDTH;
    }
  patch(jp,pc);
  return REGERR_OK;
  }


// Parse branch
FXRexError FXCompile::alternative(FXint& flags){
  FXRexError err;
  FXint flg;
  flags=FLG_WORST;
  while(*pat!='\0' && *pat!='|' && *pat!=')'){
    err=piece(flg);
    if(err!=REGERR_OK) return err;
    flags|=flg;
    }
  return REGERR_OK;
  }


// Parse piece
FXRexError FXCompile::piece(FXint& flags){
  FXint ch,rep_min,rep_max,lazy,flg,*ptr;
  FXRexError err;
  ptr=pc;

  // Process atom
  err=atom(flg);

  // Error in atom
  if(err!=REGERR_OK) return err;

  // Followed by repetition
  if((ch=*pat)=='*' || ch=='+' || ch=='?' || ch=='{'){

    // Repeats may not match empty
    if(!(flg&FLG_WIDTH)) return REGERR_NOATOM;

    pat++;
    rep_min=1;
    rep_max=1;

    // Handle repetition type
    switch(ch){
      case '*':                                           // Repeat 0-INF
        rep_min=0;
        rep_max=ONEINDIG;
        break;
      case '+':                                           // Repeat 1-INF
        rep_min=1;
        rep_max=ONEINDIG;
        break;
      case '?':                                           // Repeat 0-1
        rep_min=0;
        rep_max=1;
        break;
      case '{':                                           // Repeat n-m
        rep_min=0;
        rep_max=ONEINDIG;
        if(*pat!='}'){
          while(Ascii::isDigit(*pat)){
            rep_min=10*rep_min+(*pat-'0');
            pat++;
            }
          rep_max=rep_min;
          if(*pat==','){
            pat++;
            rep_max=ONEINDIG;
            if(*pat!='}'){
              rep_max=0;
              while(Ascii::isDigit(*pat)){
                rep_max=10*rep_max+(*pat-'0');
                pat++;
                }
              }
            }
          if(rep_min>rep_max) return REGERR_RANGE;              // Illegal range
          if(rep_min==0 && rep_max==0) return REGERR_COUNT;     // Bad count
          }
        if(*pat!='}') return REGERR_BRACE;                      // Unmatched brace
        pat++;
        break;
      default:
        return REGERR_TOKEN;
      }

    // Handle greedy, lazy, or possessive forms
    if(*pat=='?'){      // Lazy
      lazy=1; pat++;
      }
    else if(*pat=='+'){ // Possessive
      lazy=2; pat++;
      }
    else{               // Greedy
      lazy=0;
      }

    // If zero repetitions are allowed, then may have no width
    if(rep_min==0) flg&=~FLG_WIDTH;

    // Handle only non-trivial cases
    if(!(rep_min==1 && rep_max==1)){

      // For simple repeats we prefix the last operation
      if(flg&FLG_SIMPLE){
        if(rep_min==0 && rep_max==ONEINDIG){
          insert(ptr,OP_STAR+lazy);
          }
        else if(rep_min==1 && rep_max==ONEINDIG){
          insert(ptr,OP_PLUS+lazy);
          }
        else if(rep_min==0 && rep_max==1){
          insert(ptr,OP_QUEST+lazy);
          }
        else{
          insert(ptr,OP_REP+lazy,rep_min,rep_max);
          }
        }

      // For complex repeats we build loop constructs
      else{
        FXASSERT(lazy!=2);                              // FIXME not yet implemented
        if(rep_min==0 && rep_max==ONEINDIG){
          /*    ________
          **   |        \
          ** --B--(...)--J--+--                 (...){0,ONEINDIG}
          **    \___________|
          */
          insert(ptr,lazy?OP_BRANCHREV:OP_BRANCH,pc-ptr+3);
          append(OP_JUMP,ptr-pc-1);
          }
        else if(rep_min==1 && rep_max==ONEINDIG){
          /*    ________
          **   |        \
          ** --+--(...)--B--                    (...){1,ONEINDIG}
          **
          */
          append(lazy?OP_BRANCH:OP_BRANCHREV,ptr-pc-1);
          }
        else if(rep_min==0 && rep_max==1){
          /*
          **
          ** --B--(...)--+--                    (...){0,1}
          **    \________|
          */
          insert(ptr,lazy?OP_BRANCHREV:OP_BRANCH,pc-ptr+1);
          }
        else if(0<rep_min && rep_min==rep_max){
          /*       ___________
          **      |           \
          ** --Z--+--(...)--I--L--              (...){n,n}
          **
          */
          if(nbra>=NSUBEXP) return REGERR_COMPLEX;
          insert(ptr,OP_ZERO+nbra);
          append(OP_INCR+nbra);
          append(OP_JUMPLT+nbra,rep_min,ptr-pc-1);
          nbra++;
          }
        else if(rep_min==0 && rep_max<ONEINDIG){
          /*       ___________
          **      |           \
          ** --Z--B--(...)--I--L--+--           (...){0,n}
          **       \______________|
          */
          if(nbra>=NSUBEXP) return REGERR_COMPLEX;
          insert(ptr,OP_ZERO+nbra);
          insert(ptr+1,lazy?OP_BRANCHREV:OP_BRANCH,pc-ptr+4);
          append(OP_INCR+nbra);
          append(OP_JUMPLT+nbra,rep_max,ptr-pc-1);
          nbra++;
          }
        else if(0<rep_min && rep_max==ONEINDIG){
          /*       ________________
          **      |   ___________  \
          **      |  |           \  \
          ** --Z--+--+--(...)--I--L--B--        (...){n,ONEINDIG}
          */
          if(nbra>=NSUBEXP) return REGERR_COMPLEX;
          insert(ptr,OP_ZERO+nbra);
          append(OP_INCR+nbra);
          append(OP_JUMPLT+nbra,rep_min,ptr-pc-1);
          append(lazy?OP_BRANCH:OP_BRANCHREV,ptr-pc);
          nbra++;
          }
        else{
          /*       ___________________
          **      |   ___________     \
          **      |  |           \     \
          ** --Z--+--+--(...)--I--L--G--B--+--  (...){n,m}
          **                          \____|
          */
          if(nbra>=NSUBEXP) return REGERR_COMPLEX;
          insert(ptr,OP_ZERO+nbra);
          append(OP_INCR+nbra);
          append(OP_JUMPLT+nbra,rep_min,ptr-pc-1);
          append(OP_JUMPGT+nbra,rep_max,3);
          append(lazy?OP_BRANCH:OP_BRANCHREV,ptr-pc);
          nbra++;
          }
        }
      }
    }
  flags=flg&FLG_WIDTH;
  return REGERR_OK;
  }


// Parse atom
FXRexError FXCompile::atom(FXint& flags){
  FXint buf[MAXCHARS],level,save,ch,len,flg,*ptr;
  const FXchar *p;
  FXRexError err;
  flags=FLG_WORST;                                // Assume the worst
  switch(*pat){
    case '(':                                     // Subexpression grouping
      pat++;
      if(*pat=='?'){
        pat++;
        ch=*pat++;
        if(ch==':'){                              // Non capturing parentheses
          err=expression(flg);
          if(err!=REGERR_OK) return err;          // Propagate error
          }
        else if(ch=='=' || ch=='!'){              // Positive or negative look ahead
          append((ch=='=')?OP_LOOK_POS:OP_LOOK_NEG);
          ptr=append(0);
          err=expression(flg);
          if(err!=REGERR_OK) return err;          // Propagate error
          append(OP_SUCCEED);
          patch(ptr,pc);                          // If trailing context matches (fails), go here!
          flg=FLG_WORST;                          // Look ahead has no width!
          }
        else if(ch=='>'){                         // Atomic group
          // FIXME
          }
        else if(ch=='i' || ch=='I' || ch=='n' || ch=='N'){
          save=mode;                              // Save flags
          if(ch=='i') mode|=REX_ICASE;
          if(ch=='I') mode&=~REX_ICASE;
          if(ch=='n') mode|=REX_NEWLINE;
          if(ch=='N') mode&=~REX_NEWLINE;
          err=expression(flg);
          if(err!=REGERR_OK) return err;          // Propagate error
          mode=save;                              // Restore flags
          }
        else{
          return REGERR_TOKEN;
          }
        }
      else if(mode&REX_CAPTURE){                  // Capturing
        level=++npar;
        if(level>=NSUBEXP) return REGERR_COMPLEX; // Expression too complex
        append(OP_SUB_BEG+level);
        err=expression(flg);
        if(err!=REGERR_OK) return err;            // Propagate error
        append(OP_SUB_END+level);
        }
      else{                                       // Normal
        err=expression(flg);
        if(err!=REGERR_OK) return err;            // Propagate error
        }
      if(*pat!=')') return REGERR_PAREN;          // Unmatched parenthesis
      pat++;
      flags=flg&~FLG_SIMPLE;
      break;
    case '.':                                     // Any character
      pat++;
      append((mode&REX_NEWLINE)?OP_ANY_NL:OP_ANY);
      flags=FLG_WIDTH|FLG_SIMPLE;
      break;
    case '^':                                     // Begin of line
      pat++;
      append(OP_LINE_BEG);
      break;
    case '$':                                     // End of line
      pat++;
      append(OP_LINE_END);
      break;
    case '*':                                     // No preceding atom
    case '+':
    case '?':
    case '{':
      return REGERR_NOATOM;
    case '\0':                                    // Technically, this can not happen!
    case '|':
    case ')':
      return REGERR_NOATOM;
    case '}':                                     // Unmatched brace
      return REGERR_BRACE;
    case '[':
      pat++;
      err=charset();
      if(err!=REGERR_OK) return err;              // Bad character class
      if(*pat!=']') return REGERR_BRACK;          // Unmatched bracket
      pat++;
      flags=FLG_WIDTH|FLG_SIMPLE;
      break;
    case ']':                                     // Unmatched bracket
      return REGERR_BRACK;
    case '\\':                                    // Escape sequences which are NOT part of simple character-run
      ch=*(pat+1);
      switch(ch){
        case '\0':                                // Unexpected pattern end
          return REGERR_NOATOM;
        case 'w':                                 // Word character
          append(OP_WORD);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'W':                                 // Non-word character
          append((mode&REX_NEWLINE)?OP_NOT_WORD_NL:OP_NOT_WORD);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 's':                                 // Space
          append((mode&REX_NEWLINE)?OP_SPACE_NL:OP_SPACE);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'S':                                 // Non-space
          append(OP_NOT_SPACE);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'd':                                 // Digit
          append(OP_DIGIT);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'D':                                 // Non-digit
          append((mode&REX_NEWLINE)?OP_NOT_DIGIT_NL:OP_NOT_DIGIT);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'h':                                 // Hex digit
          append(OP_HEX);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'H':                                 // Non-hex digit
          append((mode&REX_NEWLINE)?OP_NOT_HEX_NL:OP_NOT_HEX);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'p':                                 // Punctuation
          append(OP_PUNCT);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'P':                                 // Non-punctuation
          append((mode&REX_NEWLINE)?OP_NOT_PUNCT_NL:OP_NOT_PUNCT);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'l':                                 // Letter
          append(OP_LETTER);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'L':                                 // Non-letter
          append((mode&REX_NEWLINE)?OP_NOT_LETTER_NL:OP_NOT_LETTER);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'u':                                 // Upper case
          append(OP_UPPER);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'U':                                 // Lower case
          append(OP_LOWER);
          pat+=2;
          flags=FLG_WIDTH|FLG_SIMPLE;
          return REGERR_OK;
        case 'b':                                 // Word boundary
          append(OP_WORD_BND);
          pat+=2;
          return REGERR_OK;
        case 'B':                                 // Word interior
          append(OP_WORD_INT);
          pat+=2;
          return REGERR_OK;
        case 'A':                                 // Match only beginning of string
          append(OP_STR_BEG);
          pat+=2;
          return REGERR_OK;
        case 'Z':                                 // Match only and end of string
          append(OP_STR_END);
          pat+=2;
          return REGERR_OK;
        case '<':                                 // Begin of word
          append(OP_WORD_BEG);
          pat+=2;
          return REGERR_OK;
        case '>':                                 // End of word
          append(OP_WORD_END);
          pat+=2;
          return REGERR_OK;
        case '1':                                 // Back reference to previously matched subexpression
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
          if(!(mode&REX_CAPTURE)) return REGERR_BACKREF;  // Can't do backreferences
          level=ch-'0';
          if(level>npar) return REGERR_BACKREF;           // Back reference out of range
          append((mode&REX_ICASE)?(OP_REF_CI+level):(OP_REF+level));
          pat+=2;
          return REGERR_OK;
        }
      /*fall*/
    default:
      len=0;
      do{
        p=pat;                                    // In case we need to back up...
        ch=*pat;
        switch(ch){
          case '^':                               // Bail out on magic characters
          case '$':
          case '.':
          case '(':
          case ')':
          case '[':
          case ']':
          case '|':
            goto x;
          case '\\':
            ch=*(pat+1);
            switch(ch){
              case 'w':                           // Bail out on special matching constructs
              case 'W':
              case 's':
              case 'S':
              case 'd':
              case 'D':
              case 'h':
              case 'H':
              case 'p':
              case 'P':
              case 'l':
              case 'L':
              case 'u':
              case 'U':
              case 'b':
              case 'B':
              case 'A':
              case 'Z':
              case '<':
              case '>':
              case '1':
              case '2':
              case '3':
              case '4':
              case '5':
              case '6':
              case '7':
              case '8':
              case '9':
                goto x;
              case 'a':                           // Bell
                pat+=2;
                ch='\a';
                break;
              case 'e':                           // Escape
                pat+=2;
                ch='\033';
                break;
              case 'f':                           // Form feed
                pat+=2;
                ch='\f';
                break;
              case 'n':                           // Newline
                pat+=2;
                ch='\n';
                break;
              case 'r':                           // Return
                pat+=2;
                ch='\r';
                break;
              case 't':                           // Tab
                pat+=2;
                ch='\t';
                break;
              case 'v':                           // Vertical tab
                pat+=2;
                ch='\v';
                break;
              case 'c':                           // Control character
                pat+=2;
                ch=*pat++;
                if(ch=='\0') return REGERR_NOATOM;// Unexpected pattern end
                ch=Ascii::toUpper(ch)-'@';
                break;
              case '0':                           // Octal digit
                pat+=2;
                ch=oct(pat);
                if(ch>256) return REGERR_TOKEN;   // Characters should be 0..255
                break;
              case 'x':                           // Hex digit
                pat+=2;
                ch=hex(pat);
                if(ch>256) return REGERR_TOKEN;   // Characters should be 0..255
                break;
              case '\0':                          // Unexpected pattern end
                return REGERR_NOATOM;
              default:
                pat+=2;
                break;
              }
            break;
          case '\0':                              // Unexpected pattern end
            return REGERR_NOATOM;
          default:
            pat++;
            break;
          }

        // Make lower case?
        if(mode&REX_ICASE) ch=Ascii::toLower(ch);

        // Add to buffer
        buf[len++]=(FXuchar)ch;
        }
      while(*pat!='\0' && *pat!='*' && *pat!='+' && *pat!='?' && *pat!='{' && len<MAXCHARS);

      // Back up one character if followed by a repetition: aaa* is interpreted as (aa)a*
x:    if(1<len && (*pat=='*' || *pat=='+' || *pat=='?' || *pat=='{')){
        pat=p;
        len--;
        }

      FXASSERT(1<=len);

      // Had at least 1 character
      flags=FLG_WIDTH;

      // Simple only if length is 1
      if(len==1){
        flags|=FLG_SIMPLE;
        append((mode&REX_ICASE)?OP_CHAR_CI:OP_CHAR,buf[0]);
        }

      // Array of characters
      else{
        append((mode&REX_ICASE)?OP_CHARS_CI:OP_CHARS,len,buf);
        }
      break;
    }
  return REGERR_OK;
  }


// True if character is a word character
inline int isword(int ch){
  return Ascii::isAlphaNumeric(ch) || ch=='_';
  }


// True if character is punctuation (delimiter) character
inline int isdelim(int ch){
  return Ascii::isPunct(ch) && ch!='_';
  }


// The new character class structure:
//
//          <N>
//   ( <lo_1> <hi_1> )
//   ( <lo_2> <hi_2> )
//      ...    ...
//   ( <lo_N> <hi_N> )
//
// Parse character class
FXRexError FXCompile::charset(){
  register FXint first,last,op,i;
  FXint set[8];
  CLEAR(set);
  first=-1;
  if(*pat=='^'){                                  // Negated character class
    op=OP_ANY_BUT;
    pat++;
    }
  else{
    op=OP_ANY_OF;
    }
  if(*pat=='-' || *pat==']') goto in;             // '-' and ']' are literal at begin
  while(*pat!='\0' && *pat!=']'){
in: last=*pat++;
    if(last=='\\'){
      last=*pat++;
      switch(last){
        case 'w':
          for(i=0; i<256; i++) {if(isword(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'W':
          for(i=0; i<256; i++){ if(!isword(i)) INCL(set,i); }
          first=-1;
          continue;
        case 's':
          for(i=0; i<256; i++){ if(Ascii::isSpace(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'S':
          for(i=0; i<256; i++){ if(!Ascii::isSpace(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'd':
          for(i=0; i<256; i++){ if(Ascii::isDigit(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'D':
          for(i=0; i<256; i++){ if(!Ascii::isDigit(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'h':
          for(i=0; i<256; i++){ if(Ascii::isHexDigit(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'H':
          for(i=0; i<256; i++){ if(!Ascii::isHexDigit(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'p':
          for(i=0; i<256; i++){ if(isdelim(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'P':
          for(i=0; i<256; i++){ if(!isdelim(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'l':
          for(i=0; i<256; i++){ if(Ascii::isLetter(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'L':
          for(i=0; i<256; i++){ if(!Ascii::isLetter(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'u':
          for(i=0; i<256; i++){ if(Ascii::isUpper(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'U':
          for(i=0; i<256; i++){ if(Ascii::isLower(i)) INCL(set,i); }
          first=-1;
          continue;
        case 'a':                             // Bell
          last='\a';
          break;
        case 'e':                             // Escape
          last='\033';
          break;
        case 'b':                             // Backspace
          last='\b';
          break;
        case 'f':                             // Form feed
          last='\f';
          break;
        case 'n':                             // Newline
          last='\n';
          break;
        case 'r':                             // Return
          last='\r';
          break;
        case 't':                             // Tab
          last='\t';
          break;
        case 'v':                             // Vertical tab
          last='\v';
          break;
        case 'c':                             // Control character
          last=*pat++;
          if(last=='\0') return REGERR_NOATOM;// Unexpected pattern end
          last=Ascii::toUpper(last)-'@';
          break;
        case '0':                             // Octal digit
          last=oct(pat);
          break;
        case 'x':                             // Hex digit
          last=hex(pat);
          break;
        case '\0':
          return REGERR_NOATOM;               // Unexpected pattern end
        }
      }
    if(first==-1){
      if(mode&REX_ICASE){
        INCL(set,Ascii::toLower(last));
        INCL(set,Ascii::toUpper(last));
        }
      else{
        INCL(set,last);
        }
      if(*pat=='-' && *(pat+1)!='\0' && *(pat+1)!=']'){
        first=last;
        pat++;
        }
      }
    else{
      if(first>=last) return REGERR_RANGE;    // Bad range
      if(mode&REX_ICASE){
        for(i=first; i<=last; i++){
          INCL(set,Ascii::toLower(i));
          INCL(set,Ascii::toUpper(i));
          }
        }
      else{
        for(i=first; i<=last; i++){
          INCL(set,i);
          }
        }
      first=-1;
      }
    }

  // Are we matching newlines
  if((op==OP_ANY_BUT) && !(mode&REX_NEWLINE) && !ISIN(set,'\n')){
    INCL(set,'\n');
    }

  // Emit opcode
  append(op,set);
  return REGERR_OK;
  }


// Append opcode
FXint* FXCompile::append(FXint op){
  register FXint *val=pc;
  if(code){
    pc[0]=op;
    }
  pc++;
  return val;
  }


// Append one-argument opcode
FXint* FXCompile::append(FXint op,FXint arg){
  register FXint *val=pc;
  if(code){
    pc[0]=op;
    pc[1]=arg;
    }
  pc+=2;
  return val;
  }


// Append two-argument opcode
FXint* FXCompile::append(FXint op,FXint arg1,FXint arg2){
  register FXint *val=pc;
  if(code){
    pc[0]=op;
    pc[1]=arg1;
    pc[2]=arg2;
    }
  pc+=3;
  return val;
  }


// Append character class opcode
FXint* FXCompile::append(FXint op,FXint set[]){
  register FXint *val=pc;
  if(code){
    pc[0]=op;
    pc[1]=set[0];
    pc[2]=set[1];
    pc[3]=set[2];
    pc[4]=set[3];
    pc[5]=set[4];
    pc[6]=set[5];
    pc[7]=set[6];
    pc[8]=set[7];
    }
  pc+=9;
  return val;
  }


// Append character array
FXint* FXCompile::append(FXint op,FXint len,FXint *data){
  register FXint *val=pc;
  if(code){
    pc[0]=op;
    pc[1]=len;
    memcpy(pc+2,data,sizeof(FXint)*len);
    }
  pc+=len+2;
  return val;
  }


// Insert opcode at ptr
FXint* FXCompile::insert(FXint *ptr,FXint op){
  if(code){
    memmove(ptr+1,ptr,sizeof(FXint)*(pc-ptr));
    ptr[0]=op;
    }
  pc+=1;
  return ptr;
  }


// Insert one-argument opcode at ptr
FXint* FXCompile::insert(FXint *ptr,FXint op,FXint arg){
  if(code){
    memmove(ptr+2,ptr,sizeof(FXint)*(pc-ptr));
    ptr[0]=op;
    ptr[1]=arg;
    }
  pc+=2;
  return ptr;
  }


// Insert two-argument opcode at ptr
FXint* FXCompile::insert(FXint *ptr,FXint op,FXint arg1,FXint arg2){
  if(code){
    memmove(ptr+3,ptr,sizeof(FXint)*(pc-ptr));
    ptr[0]=op;
    ptr[1]=arg1;
    ptr[2]=arg2;
    }
  pc+=3;
  return ptr;
  }



// Patch linked set of branches or jumps
// Example:
//
//      Before:        After:
//      ==========================
//      0:  OP_JUMP    0:  OP_JUMP
//      1:  0          1:  9
//      2:  ....       2:  ....
//      3:  OP_JUMP    3:  OP_JUMP
//      4:  -3         4:  6
//      5:  ....       5:  ....
//      6:  ....       6:  ....
//      7:  OP_JUMP    7:  OP_JUMP
// fm-> 8:  -4         8:  2
//      9:  ....       9:  ....
// to->10:  ....      10:  ....
//
void FXCompile::patch(FXint *fm,FXint *to){
  register FXint delta;
  if(code && fm){
    do{
      delta=*fm;
      *fm=to-fm;
      fm+=delta;
      }
    while(delta);
    }
  }


/*******************************************************************************/

// FXExecute members

// The workhorse
bool FXExecute::match(const FXint* prog){
  register FXint no,keep,rep_min,rep_max,greed,op;
  register const FXchar *save,*beg,*end;
  register FXchar ch;
  for(;;){
    op=*prog++;
    switch(op){
      case OP_END:
        return true;
      case OP_FAIL:           // Fail (sub) pattern
        return false;
      case OP_SUCCEED:        // Succeed (sub) pattern
        return true;
      case OP_JUMP:
        prog+=*prog;
        break;
      case OP_BRANCH:         // Jump after trying following code
        save=str;
        if(match(prog+1)) return true;
        str=save;
        prog+=*prog;
        break;
      case OP_BRANCHREV:      // Jump before trying following code
        save=str;
        if(match(prog+*prog)) return true;
        str=save;
        prog++;
        break;
      case OP_LINE_BEG:       // Must be at begin of line
        if((str==str_beg && (mode&REX_NOT_BOL)) || (str_beg<str && *(str-1)!='\n')) return false;
        break;
      case OP_LINE_END:       // Must be at end of line
        if((str==str_end && (mode&REX_NOT_EOL)) || (str<str_end && *str!='\n')) return false;
        break;
      case OP_WORD_BEG:       // Must be at begin of word
        if(str_beg<str && isword((FXuchar) *(str-1))) return false;
        if(str_end<=str || !isword((FXuchar) *str)) return false;
        break;
      case OP_WORD_END:       // Must be at end of word
        if(str<str_end && isword((FXuchar) *str)) return false;
        if(str<=str_beg || !isword((FXuchar) *(str-1))) return false;
        break;
      case OP_WORD_BND:       // Must be at word boundary
        if(!(((str==str_beg || !isword((FXuchar) *(str-1))) && (str<str_end && isword((FXuchar) *str))) || ((str==str_end || !isword((FXuchar) *str)) && (str_beg<str && isword((FXuchar) *(str-1)))))) return false;
        break;
      case OP_WORD_INT:       // Must be inside a word
        if(str==str_beg || !isword((FXuchar) *(str-1))) return false;
        if(str==str_end || !isword((FXuchar) *str)) return false;
        break;
      case OP_STR_BEG:        // Must be at begin of entire string
        if(str!=str_beg) return false;
        break;
      case OP_STR_END:        // Must be at end of entire string
        if(str!=str_end) return false;
        break;
      case OP_ANY_OF:         // Match a character in a set
        if(str==str_end || !ISIN(prog,*str)) return false;
        prog+=8;
        str++;
        break;
      case OP_ANY_BUT:        // Match a character NOT in a set
        if(str==str_end || ISIN(prog,*str)) return false;
        prog+=8;
        str++;
        break;
      case OP_CHAR:           // Match single character
        if(str==str_end || *prog != *str) return false;
        prog++;
        str++;
        break;
      case OP_CHAR_CI:        // Match single character, disregard case
        if(str==str_end || *prog != Ascii::toLower(*str)) return false;
        prog++;
        str++;
        break;
      case OP_CHARS:          // Match a run of 1 or more characters
        no=*prog++;
        if(str+no>str_end) return false;
        do{
          if(*prog++ != (FXuchar)*str++) return false;
          }
        while(--no);
        break;
      case OP_CHARS_CI:       // Match a run of 1 or more characters, disregard case
        no=*prog++;
        if(str+no>str_end) return false;
        do{
          if(*prog++ != Ascii::toLower(*str++)) return false;
          }
        while(--no);
        break;
      case OP_SPACE:          // Match space
        if(str==str_end || *str=='\n' || !Ascii::isSpace(*str)) return false;
        str++;
        break;
      case OP_SPACE_NL:       // Match space including newline
        if(str==str_end || !Ascii::isSpace(*str)) return false;
        str++;
        break;
      case OP_NOT_SPACE:      // Match non-space
        if(str==str_end || Ascii::isSpace(*str)) return false;
        str++;
        break;
      case OP_DIGIT:          // Match a digit 0..9
        if(str==str_end || !Ascii::isDigit(*str)) return false;
        str++;
        break;
      case OP_NOT_DIGIT:      // Match a non-digit
        if(str==str_end || *str=='\n' || Ascii::isDigit(*str)) return false;
        str++;
        break;
      case OP_NOT_DIGIT_NL:   // Match a non-digit including newline
        if(str==str_end || Ascii::isDigit(*str)) return false;
        str++;
        break;
      case OP_HEX:            // Match a hex digit 0..9A-Fa-f
        if(str==str_end || !Ascii::isHexDigit(*str)) return false;
        str++;
        break;
      case OP_NOT_HEX:        // Match a non-hex digit
        if(str==str_end || *str=='\n' || Ascii::isHexDigit(*str)) return false;
        str++;
        break;
      case OP_NOT_HEX_NL:     // Match a non-hex digit including newline
        if(str==str_end || Ascii::isHexDigit(*str)) return false;
        str++;
        break;
      case OP_PUNCT:          // Match a punctuation
        if(str==str_end || !isdelim((FXuchar) *str)) return false;
        str++;
        break;
      case OP_NOT_PUNCT:      // Match a non-punctuation
        if(str==str_end || *str=='\n' || isdelim((FXuchar) *str)) return false;
        str++;
        break;
      case OP_NOT_PUNCT_NL:   // Match a non-punctuation including newline
        if(str==str_end || isdelim((FXuchar) *str)) return false;
        str++;
        break;
      case OP_LETTER:         // Match a letter a..z, A..Z
        if(str==str_end || !Ascii::isLetter(*str)) return false;
        str++;
        break;
      case OP_NOT_LETTER:     // Match a non-letter
        if(str==str_end || *str=='\n' || Ascii::isLetter(*str)) return false;
        str++;
        break;
      case OP_NOT_LETTER_NL:  // Match a non-letter including newline
        if(str==str_end || Ascii::isLetter(*str)) return false;
        str++;
        break;
      case OP_WORD:           // Match a word character a..z,A..Z,0..9,_
        if(str==str_end || !isword((FXuchar) *str)) return false;
        str++;
        break;
      case OP_NOT_WORD:       // Match a non-word character
        if(str==str_end || *str=='\n' || isword((FXuchar) *str)) return false;
        str++;
        break;
      case OP_NOT_WORD_NL:    // Match a non-word character including newline
        if(str==str_end || isword((FXuchar) *str)) return false;
        str++;
        break;
      case OP_UPPER:          // Match if uppercase
        if(str==str_end || !Ascii::isUpper(*str)) return false;
        str++;
        break;
      case OP_LOWER:          // Match if lowercase
        if(str==str_end || !Ascii::isLower(*str)) return false;
        str++;
        break;
      case OP_ANY:            // Match any character
        if(str==str_end || *str=='\n') return false;
        str++;
        break;
      case OP_ANY_NL:         // Matches any character including newline
        if(str==str_end) return false;
        str++;
        break;
      case OP_MIN_PLUS:       // Lazy one or more repetitions
        rep_min=1;
        rep_max=ONEINDIG;
        greed=0;
        goto rep;
      case OP_POS_PLUS:       // Possessive one or more repetitions
        rep_min=1;
        rep_max=ONEINDIG;
        greed=2;
        goto rep;
      case OP_PLUS:           // Greedy one or more repetitions
        rep_min=1;
        rep_max=ONEINDIG;
        greed=1;
        goto rep;
      case OP_MIN_QUEST:      // Lazy zero or one
        rep_min=0;
        rep_max=1;
        greed=0;
        goto rep;
      case OP_POS_QUEST:      // Possessive zero or one
        rep_min=0;
        rep_max=1;
        greed=2;
        goto rep;
      case OP_QUEST:          // Greedy zero or one
        rep_min=0;
        rep_max=1;
        greed=1;
        goto rep;
      case OP_MIN_REP:        // Lazy bounded repeat
        rep_min=*prog++;
        rep_max=*prog++;
        greed=0;
        goto rep;
      case OP_POS_REP:        // Possessive bounded repeat
        rep_min=*prog++;
        rep_max=*prog++;
        greed=2;
        goto rep;
      case OP_REP:            // Greedy bounded repeat
        rep_min=*prog++;
        rep_max=*prog++;
        greed=1;
        goto rep;
      case OP_MIN_STAR:       // Lazy zero or more repetitions
        rep_min=0;
        rep_max=ONEINDIG;
        greed=0;
        goto rep;
      case OP_POS_STAR:       // Possessive zero or more repetitions
        rep_min=0;
        rep_max=ONEINDIG;
        greed=2;
        goto rep;
      case OP_STAR:           // Greedy zero or more repetitions
        rep_min=0;
        rep_max=ONEINDIG;
        greed=1;

        // We need to match more characters than are available
rep:    if(str+rep_min>str_end) return false;
        beg=str;
        end=beg+rep_max;
        if(end>str_end) end=str_end;
        save=beg;

        // Find out how much could be matched
        op=*prog++;
        switch(op){
          case OP_CHAR:         // For UTF8 we should have OP_CHAR2, OP_CHAR3, ... OP_CHAR6, for  possible UTF8 lengths
            ch=*prog++;
            while(save<end && *save==ch) save++;
            break;
          case OP_CHAR_CI:
            ch=*prog++;
            while(save<end && Ascii::toLower(*save)==ch && *save!='\n') save++;
            break;
          case OP_CHARS:
            ch=*++prog;
            while(save<end && *save==ch) save++;
            prog+=3;
            break;
          case OP_CHARS_CI:
            ch=*++prog;
            while(save<end && Ascii::toLower(*save)==ch) save++;
            prog+=3;
            break;
          case OP_ANY_OF:
            while(save<end && ISIN(prog,*save)) save++;
            prog+=8;
            break;
          case OP_ANY_BUT:
            while(save<end && !ISIN(prog,*save)) save++;
            prog+=8;
            break;
          case OP_SPACE:
            while(save<end && *save!='\n' && Ascii::isSpace(*save)) save++;
            break;
          case OP_SPACE_NL:
            while(save<end && Ascii::isSpace(*save)) save++;
            break;
          case OP_NOT_SPACE:
            while(save<end && !Ascii::isSpace(*save)) save++;
            break;
          case OP_DIGIT:
            while(save<end && Ascii::isDigit(*save)) save++;
            break;
          case OP_NOT_DIGIT:
            while(save<end && *save!='\n' && !Ascii::isDigit(*save)) save++;
            break;
          case OP_NOT_DIGIT_NL:
            while(save<end && !Ascii::isDigit(*save)) save++;
            break;
          case OP_HEX:
            while(save<end && Ascii::isHexDigit(*save)) save++;
            break;
          case OP_NOT_HEX:
            while(save<end && *save!='\n' && !Ascii::isHexDigit(*save)) save++;
            break;
          case OP_NOT_HEX_NL:
            while(save<end && !Ascii::isHexDigit(*save)) save++;
            break;
          case OP_PUNCT:
            while(save<end && isdelim((FXuchar) *save)) save++;
            break;
          case OP_NOT_PUNCT:
            while(save<end && *save!='\n' && !isdelim((FXuchar) *save)) save++;
            break;
          case OP_NOT_PUNCT_NL:
            while(save<end && !isdelim((FXuchar) *save)) save++;
            break;
          case OP_LETTER:
            while(save<end && Ascii::isLetter(*save)) save++;
            break;
          case OP_NOT_LETTER:
            while(save<end && *save!='\n' && !Ascii::isLetter(*save)) save++;
            break;
          case OP_NOT_LETTER_NL:
            while(save<end && !Ascii::isLetter(*save)) save++;
            break;
          case OP_WORD:
            while(save<end && isword((FXuchar) *save)) save++;
            break;
          case OP_NOT_WORD:
            while(save<end && *save!='\n' && !isword((FXuchar) *save)) save++;
            break;
          case OP_NOT_WORD_NL:
            while(save<end && !isword((FXuchar) *save)) save++;
            break;
          case OP_UPPER:
            while(save<end && Ascii::isUpper(*save)) save++;
            break;
          case OP_LOWER:
            while(save<end && Ascii::isLower(*save)) save++;
            break;
          case OP_ANY:
            while(save<end && *save!='\n') save++;
            break;
          case OP_ANY_NL:
            save=end; // Big byte
            break;
          default:
            fxerror("FXRex::match: bad opcode (%d) at: %p on line: %d\n",op,prog-1,__LINE__);
            break;
          }

        // Matched fewer than the minimum desired so bail out
        if(save<beg+rep_min) return false;

        // We must match between beg and end characters
        beg+=rep_min;
        end=save;

        switch(greed){
          case 0:                     // Lazily match the fewest characters
            while(beg<=end){
              str=beg;
              if(match(prog)) return true;
              beg++;
              }
            return false;
          case 1:                     // Greedily match the most characters
            while(beg<=end){
              str=end;
              if(match(prog)) return true;
              end--;
              }
            return false;
          case 2:                     // Possessive match
            return match(prog);
          }
        return false;
      case OP_SUB_BEG+0:              // Capturing open parentheses
      case OP_SUB_BEG+1:
      case OP_SUB_BEG+2:
      case OP_SUB_BEG+3:
      case OP_SUB_BEG+4:
      case OP_SUB_BEG+5:
      case OP_SUB_BEG+6:
      case OP_SUB_BEG+7:
      case OP_SUB_BEG+8:
      case OP_SUB_BEG+9:
        no=op-OP_SUB_BEG;
        if(no>=npar) break;           // Match w/o capture if array too small
        keep=sub_beg[no];             // Keep old value
        sub_beg[no]=str-str_beg;      // Tentatively set new value
        if(match(prog)) return true;  // Match the rest
        sub_beg[no]=keep;             // Restore old value
        return false;
      case OP_SUB_END+0:              // Capturing close parentheses
      case OP_SUB_END+1:
      case OP_SUB_END+2:
      case OP_SUB_END+3:
      case OP_SUB_END+4:
      case OP_SUB_END+5:
      case OP_SUB_END+6:
      case OP_SUB_END+7:
      case OP_SUB_END+8:
      case OP_SUB_END+9:
        no=op-OP_SUB_END;
        if(no>=npar) break;           // Match w/o capture if array too small
        keep=sub_end[no];
        sub_end[no]=str-str_beg;      // Remember capture end for future back reference
        if(match(prog)) return true;
        sub_end[no]=keep;             // Restore old value
        return false;
      case OP_REF+0:                  // Back reference to capturing parentheses
      case OP_REF+1:
      case OP_REF+2:
      case OP_REF+3:
      case OP_REF+4:
      case OP_REF+5:
      case OP_REF+6:
      case OP_REF+7:
      case OP_REF+8:
      case OP_REF+9:
        no=op-OP_REF;
        if(no>=npar) return false;                    // Arrays were too small
        if(sub_beg[no]<0) return false;               // Not captured yet
        if(sub_end[no]<0) return false;               // Not captured yet
        beg=str_beg+sub_beg[no];
        end=str_beg+sub_end[no];
        if(beg<end){                                  // Empty capture matches!
          if(str+(end-beg)>str_end) return false;     // Not enough characters left
          do{
            if(*beg != *str) return false;            // No match
            beg++;
            str++;
            }
          while(beg<end);
          }
        break;
      case OP_REF_CI+0:               // Back reference to capturing parentheses
      case OP_REF_CI+1:
      case OP_REF_CI+2:
      case OP_REF_CI+3:
      case OP_REF_CI+4:
      case OP_REF_CI+5:
      case OP_REF_CI+6:
      case OP_REF_CI+7:
      case OP_REF_CI+8:
      case OP_REF_CI+9:
        no=op-OP_REF_CI;
        if(no>=npar) return false;                    // Arrays were too small
        if(sub_beg[no]<0) return false;               // Not captured yet
        if(sub_end[no]<0) return false;               // Not captured yet
        beg=str_beg+sub_beg[no];
        end=str_beg+sub_end[no];
        if(beg<end){                                  // Empty capture matches!
          if(str+(end-beg)>str_end) return false;     // Not enough characters left
          do{
            if(*beg != Ascii::toLower(*str)) return false;            // No match
            beg++;
            str++;
            }
          while(beg<end);
          }
        break;
      case OP_LOOK_NEG:               // Positive or negative look ahead
      case OP_LOOK_POS:
        save=str;
        keep=match(prog+1);
        str=save;
        if((op-OP_LOOK_NEG)!=keep) return false;      // Didn't get what we expected
        prog=prog+*prog;              // Jump to code after OP_SUCCEED
        break;
      case OP_ZERO+0:                 // Initialize counter for counting repeat
      case OP_ZERO+1:
      case OP_ZERO+2:
      case OP_ZERO+3:
      case OP_ZERO+4:
      case OP_ZERO+5:
      case OP_ZERO+6:
      case OP_ZERO+7:
      case OP_ZERO+8:
      case OP_ZERO+9:
        count[op-OP_ZERO]=0;
        break;
      case OP_INCR+0:                 // Increment counter for counting repeat
      case OP_INCR+1:
      case OP_INCR+2:
      case OP_INCR+3:
      case OP_INCR+4:
      case OP_INCR+5:
      case OP_INCR+6:
      case OP_INCR+7:
      case OP_INCR+8:
      case OP_INCR+9:
        count[op-OP_INCR]++;
        break;
      case OP_JUMPLT+0:               // Jump if counter less than value
      case OP_JUMPLT+1:
      case OP_JUMPLT+2:
      case OP_JUMPLT+3:
      case OP_JUMPLT+4:
      case OP_JUMPLT+5:
      case OP_JUMPLT+6:
      case OP_JUMPLT+7:
      case OP_JUMPLT+8:
      case OP_JUMPLT+9:
        if(count[op-OP_JUMPLT] < *prog++)   // Compare with value
          prog+=*prog;
        else
          prog++;
        break;
      case OP_JUMPGT+0:               // Jump if counter greater than value
      case OP_JUMPGT+1:
      case OP_JUMPGT+2:
      case OP_JUMPGT+3:
      case OP_JUMPGT+4:
      case OP_JUMPGT+5:
      case OP_JUMPGT+6:
      case OP_JUMPGT+7:
      case OP_JUMPGT+8:
      case OP_JUMPGT+9:
        if(count[op-OP_JUMPGT] > *prog++)   // Compare with value
          prog+=*prog;
        else
          prog++;
        break;
      default:
        fxerror("FXRex::match: bad opcode (%d) at: %p on line: %d\n",op,prog-1,__LINE__);
        break;
      }
    }
  return false;
  }


// regtry - try match at specific point; 0 failure, 1 success
bool FXExecute::attempt(const FXchar* string){
  register FXint i=npar;
  str=string;
  do{--i;sub_beg[i]=sub_end[i]=-1;}while(i);          // Possibly move this to FXExecute::execute?
  if(match(code+1)){
    if(string!=str || !(mode&REX_NOT_EMPTY)){         // Match if non-empty or empty is allowed!
      sub_beg[0]=string-str_beg;
      sub_end[0]=str-str_beg;
      return true;
      }
    }
  return false;
  }


// Match subject string, returning number of matches found
bool FXExecute::execute(const FXchar* fm,const FXchar* to){
  register FXchar ch;

  // Simple case
  if(fm==to) return attempt(fm);

  // Match backwards
  if(mode&REX_BACKWARD){
    if(code[1]==OP_STR_BEG){                          // Anchored at string start
      return (fm==str_beg) && attempt(str_beg);
      }
    if(code[1]==OP_LINE_BEG){                         // Anchored at BOL
      while(fm<=to){
        if(((to==str_beg)||(*(to-1)=='\n')) && attempt(to)) return true;
        to--;
        }
      return false;
      }
    if(code[1]==OP_CHAR || code[1]==OP_CHARS){        // Known starting character
      ch=(code[1]==OP_CHAR)?code[2]:code[3];
      if(to==str_end) to--;
      while(fm<=to){
        if(*to==ch && attempt(to)) return true;
        to--;
        }
      return false;
      }
    if(code[1]==OP_CHAR_CI || code[1]==OP_CHARS_CI){  // Known starting character, ignoring case
      ch=(code[1]==OP_CHAR_CI)?code[2]:code[3];
      if(to==str_end) to--;
      while(fm<=to){
        if(Ascii::toLower(*to)==ch && attempt(to)) return true;
        to--;
        }
      return false;
      }
    while(fm<=to){                                    // General case
      if(attempt(to)) return true;
      to--;
      }
    }

  // Match forwards
  else{
    if(code[1]==OP_STR_BEG){                          // Anchored at string start
      return (fm==str_beg) && attempt(str_beg);
      }
    if(code[1]==OP_LINE_BEG){                         // Anchored at BOL
      while(fm<=to){
        if(((fm==str_beg)||(*(fm-1)=='\n')) && attempt(fm)) return true;
        fm++;
        }
      return false;
      }
    if(code[1]==OP_CHAR || code[1]==OP_CHARS){        // Known starting character
      ch=(code[1]==OP_CHAR)?code[2]:code[3];
      if(to==str_end) to--;
      while(fm<=to){
        if(*fm==ch && attempt(fm)) return true;
        fm++;
        }
      return false;
      }
    if(code[1]==OP_CHAR_CI || code[1]==OP_CHARS_CI){  // Known starting character, ignoring case
      ch=(code[1]==OP_CHAR_CI)?code[2]:code[3];
      if(to==str_end) to--;
      while(fm<=to){
        if(Ascii::toLower(*fm)==ch && attempt(fm)) return true;
        fm++;
        }
      return false;
      }
    while(fm<=to){                                   // General case
      if(attempt(fm)) return true;
      fm++;
      }
    }
  return false;
  }

}

/*******************************************************************************/

namespace FX {

// Table of error messages
const FXchar *const FXRex::errors[]={
  "OK",
  "Empty pattern",
  "Unmatched parenthesis",
  "Unmatched bracket",
  "Unmatched brace",
  "Bad character range",
  "Bad escape sequence",
  "Bad counted repeat",
  "No atom preceding repetition",
  "Repeat following repeat",
  "Bad backward reference",
  "Bad character class",
  "Expression too complex",
  "Out of memory",
  "Illegal token"
  };


// Default program always fails
const FXint FXRex::fallback[]={2,OP_FAIL};


// Copy regex object
FXRex::FXRex(const FXRex& orig){
  code=(FXint*)fallback;
  if(orig.code!=fallback){
    FXMEMDUP(&code,orig.code,FXint,orig.code[0]);
    }
  }


// Compile expression from pattern; fail if error
FXRex::FXRex(const FXchar* pattern,FXint mode,FXRexError* error):code((FXint*)fallback){
  FXRexError err=parse(pattern,mode);
  if(error){ *error=err; }
  }


// Compile expression from pattern; fail if error
FXRex::FXRex(const FXString& pattern,FXint mode,FXRexError* error):code((FXint*)fallback){
  FXRexError err=parse(pattern.text(),mode);
  if(error){ *error=err; }
  }


// Assignment
FXRex& FXRex::operator=(const FXRex& orig){
  if(code!=orig.code){
    if(code!=fallback) FXFREE(&code);
    code=(FXint*)fallback;
    if(orig.code!=fallback){
      FXMEMDUP(&code,orig.code,FXint,orig.code[0]);
      }
    }
  return *this;
  }


// Parse pattern
FXRexError FXRex::parse(const FXchar* pattern,FXint mode){
  FXRexError err=REGERR_EMPTY;
  FXCompile cs;
  FXint flags,size;

  // Free old code, if any
  if(code!=fallback) FXFREE(&code);
  code=(FXint*)fallback;

  // Check
  if(pattern){

    // Fill in compile data
    cs.code=NULL;
    cs.pc=NULL;
    cs.pat=pattern;
    cs.mode=mode;
    cs.nbra=0;
    cs.npar=0;

    // Unknown size
    cs.append(0);

    // Check syntax and amount of memory needed
    err=cs.compile(flags);
    if(err==REGERR_OK){

      // Compile code unless only syntax checking
      if(!(mode&REX_SYNTAX)){

        // Allocate new code
        size=cs.pc-((FXint*)NULL);
        if(!FXMALLOC(&code,FXint,size)){
          code=(FXint*)fallback;
          return REGERR_MEMORY;
          }

        // Fill in compile data
        cs.code=code;
        cs.pc=code;
        cs.pat=pattern;
        cs.mode=mode;
        cs.nbra=0;
        cs.npar=0;

        // Size of program
        cs.append(size);

        // Generate program
        err=cs.compile(flags);

        // Dump for debugging
#ifndef NDEBUG
        if(fxTraceLevel>100) dump(code);
#endif
        }
      }
    }
  return err;
  }


// Parse pattern, return error code if syntax error is found
FXRexError FXRex::parse(const FXString& pattern,FXint mode){
  return parse(pattern.text(),mode);
  }


/*******************************************************************************/


// Match subject string, returning number of matches found
bool FXRex::match(const FXchar* string,FXint len,FXint* beg,FXint* end,FXint mode,FXint npar,FXint fm,FXint to) const {
  if(!string || len<0 || npar<1 || NSUBEXP<npar){ fxerror("FXRex::match: bad argument.\n"); }
  if(fm<0) fm=0;
  if(to>len) to=len;
  if(fm<=to){
    FXint abeg[NSUBEXP];
    FXint aend[NSUBEXP];
    FXExecute ms;
    if(!beg) beg=abeg;
    if(!end) end=aend;
    ms.str_beg=string;
    ms.str_end=string+len;
    ms.sub_beg=beg;
    ms.sub_end=end;
    ms.code=code;
    ms.npar=npar;
    ms.mode=mode;
    return ms.execute(string+fm,string+to);
    }
  return false;
  }


// Search for match in string
bool FXRex::match(const FXString& string,FXint* beg,FXint* end,FXint mode,FXint npar,FXint fm,FXint to) const {
  return match(string.text(),string.length(),beg,end,mode,npar,fm,to);
  }


// Return substitution string
FXString FXRex::substitute(const FXchar* string,FXint len,FXint* beg,FXint* end,const FXString& replace,FXint npar){
  register FXint ch,n,i=0;
  FXString result;
  if(!string || len<0 || !beg || !end || npar<1 || NSUBEXP<npar){ fxerror("FXRex::substitute: bad argument.\n"); }
  while((ch=replace[i++])!='\0'){
    if(ch=='&'){
      if(0<=beg[0] && end[0]<=len){result.append(&string[beg[0]],end[0]-beg[0]);}
      }
    else if(ch=='\\' && '0'<=replace[i] && replace[i]<='9'){
      n=replace[i++]-'0';
      if(n<npar && 0<=beg[n] && end[n]<=len){result.append(&string[beg[n]],end[n]-beg[n]);}
      }
    else{
      if(ch=='\\' && (replace[i]=='\\' || replace[i]=='&')){ch=replace[i++];}
      result.append(ch);
      }
    }
  return result;
  }


// Return substitution string
FXString FXRex::substitute(const FXString& string,FXint* beg,FXint* end,const FXString& replace,FXint npar){
  return substitute(string.text(),string.length(),beg,end,replace,npar);
  }


// Equality
bool FXRex::operator==(const FXRex& rex) const {
  return code==rex.code || (code[0]==rex.code[0] && memcmp(code,rex.code,sizeof(FXint)*code[0])==0);
  }


// Inequality
bool FXRex::operator!=(const FXRex& rex) const {
  return !operator==(rex);
  }


// Save
FXStream& operator<<(FXStream& store,const FXRex& s){
  FXint size=s.code[0];
  store << size;
  store.save(s.code+1,size-1);
  return store;
  }


// Load
FXStream& operator>>(FXStream& store,FXRex& s){
  FXint size;
  store >> size;
  FXMALLOC(&s.code,FXint,size);
  store.load(s.code+1,size-1);
  return store;
  }


// Clean up
FXRex::~FXRex(){
  if(code!=fallback) FXFREE(&code);
  }

}
