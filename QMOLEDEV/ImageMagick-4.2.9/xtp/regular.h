/*
  Definitions etc. for RegularExpression(3) routines.
*/
#define EndOfProgram  0
#define MatchBeginningOfLine  1
#define MatchEndOfProgramOfLine  2
#define MatchAnyCharacter  3
#define MatchAnyCharacterOf  4
#define MatchAnyCharacterBut  5
#define MatchThisOrNext  6
#define Back  7
#define MatchExactly  8
#define MatchEmptyString  9
#define MatchZeroOrMore  10
#define MatchOneOrMore  11
#define Open  20
#define Close  30

#define WorstCase  0
#define NonNull  1
#define Simple  2
#define SpecialStart  4

#define Fail(m)  \
{  \
  (void) fprintf(stderr,"RegularExpression: %s\n",m);  \
  return(NULL);  \
}
#define Magick   0234
#define Meta  "^$.[()|?+*\\"
#define MultipleMatches(c) (((c) == '*') || ((c) == '+') || ((c) == '?'))
#define Next(p) (((*((p)+1) & 0377) << 8 )+(*((p)+2) & 0377))
#define NumberSubExpressions  10
#define OpCode(p) (*(p))
#define Operand(p) ((p)+3)

typedef struct _RegularExpression 
{
  char 
    *subpattern[NumberSubExpressions],
    *subpattern_end[NumberSubExpressions],
    start_character,
    anchor,
    *priority_pattern;

  int 
    pattern_length;

  char 
    program[1];
} RegularExpression;

extern RegularExpression 
  *CompileRegularExpression(char *);

extern int 
  ExecuteRegularExpression(RegularExpression *,char *);
