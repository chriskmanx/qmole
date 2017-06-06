/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%             RRRR   EEEEE   GGGG  U   U  L       AAA   RRRR                  %
%             R   R  E      G      U   U  L      A   A  R   R                 %
%             RRRR   EEE    G  GG  U   U  L      AAAAA  RRRR                  %
%             R R    E      G   G  U   U  L      A   A  R R                   %
%             R  R   EEEEE   GGGG   UUU   LLLLL  A   A  R  R                  %
%                                                                             %
%                                                                             %
%                     Regular Expression Interpreter.                         %
%                                                                             %
%                                                                             %
%                                                                             %
%                           Software Design                                   %
%                             John Cristy                                     %
%                              July 1992                                      %
%                                                                             %
%                                                                             %
%  Copyright 1999 E. I. du Pont de Nemours and Company                        %
%                                                                             %
%  Permission is hereby granted, free of charge, to any person obtaining a    %
%  copy of this software and associated documentation files ("ImageMagick"),  %
%  to deal in ImageMagick without restriction, including without limitation   %
%  the rights to use, copy, modify, merge, publish, distribute, sublicense,   %
%  and/or sell copies of ImageMagick, and to permit persons to whom the       %
%  ImageMagick is furnished to do so, subject to the following conditions:    %
%                                                                             %
%  The above copyright notice and this permission notice shall be included in %
%  all copies or substantial portions of ImageMagick.                         %
%                                                                             %
%  The software is provided "as is", without warranty of any kind, express or %
%  implied, including but not limited to the warranties of merchantability,   %
%  fitness for a particular purpose and noninfringement.  In no event shall   %
%  E. I. du Pont de Nemours and Company be liable for any claim, damages or   %
%  other liability, whether in an action of contract, tort or otherwise,      %
%  arising from, out of or in connection with ImageMagick or the use or other %
%  dealings in ImageMagick.                                                   %
%                                                                             %
%  Except as contained in this notice, the name of the E. I. du Pont de       %
%  Nemours and Company shall not be used in advertising or otherwise to       %
%  promote the sale, use or other dealings in ImageMagick without prior       %
%  written authorization from the E. I. du Pont de Nemours and Company.       %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  CompileRegularExpression returns NULL for a failure, where failures are
%  syntax errors, exceeding implementation limits, or applying `+' or `*'
%  to a possibly-null operand.
%
%  This is essentially the same routine written and copywrited by Henry
%  Spencer, University of Toronto.  I made minor programming changes but
%  major variable name changes to improve readability.
%
%  A regular expression is zero or more branches, separated by `|'. It
%  matches anything that matches one of the branches.
%
%  A branch is zero or more pieces, concatenated. It matches a match for
%  the first, followed by a match for the second, etc.
%
%  A piece is an atom possibly followed by `*', `+', or `?'.  An atom
%  followed by `*' matches a sequence of 0 or more matches of the
%  atom.  An atom followed by `+' matches a sequence of 1 or more
%  matches of the atom.  An atom followed by `?' matches a match of
%  the atom, or the null string.
%
%  An atom is a regular expression in parentheses (matching a match
%  for the regular expression), a range (see below), `.' (matching any
%  single character), `^' (matching the null pattern at the beginning of
%  the input string), `$' (matching the null pattern at the end of the
%  input string), a `\' followed by a single character (matching that
%  character), or a single character with no other significance (match-
%  ing that character).
%
%  A range is a sequence of characters enclosed in `[]'. It normally
%  matches any single character from the sequence. If the sequence
%  begins with `^', it matches any single character not from the rest of
%  the sequence.  If two characters in the sequence are separated by `-',
%  this is shorthand for the full list of ASCII characters between
%  them (e.g. `[0-9]' matches any decimal digit). To include a literal
%  `]' in the sequence, make it the first character (following a possible
%  `^').  To include a literal `-', make it the first or last character.
%
%  If a regular expression could match two different parts of a string,
%  it will match the one which begins earliest. If both begin in the
%  same place but match different lengths, or match the same length
%  in different ways, life gets messier, as follows.
%
%  In general, the possibilities in a list of branches are considered in
%  left-to-right order, the possibilities for `*', `+', and `?' are consid-
%  ered longest-first, nested constructs are considered from the outer-
%  most in, and concatenated constructs are considered leftmost-first.
%  The match that will be chosen is the one that uses the earliest possi-
%  bility in the first choice that has to be made. If there is more than
%  one choice, the next will be made in the same manner (earliest pos-
%  sibility) subject to the decision on the first choice.  And so forth.
%
%  For example, `(ab|a)b*c' could match `abc' in one of two ways.
%  The first choice is between `ab' and `a'; since `ab' is earlier, and
%  does lead to a successful overall match, it is chosen. Since the `b' is
%  already spoken for, the `b*' must match its last possibility the empty
%  string since it must respect the earlier choice.
%
%  In the particular case where no `|'s are present and there is only
%  one `*', `+', or `?', the net effect is that the longest possible match
%  will be chosen. So `ab*', presented with `xabbbby', will match
%  `abbbb'.  Note that if `ab*' is tried against `xabyabbbz', it will
%  match `ab' just after `x', due to the begins-earliest rule. (In effect,
%  the decision on where to start the match is the first choice to be
%  made, hence subsequent choices must respect it even if this leads
%  them to less-preferred alternatives.)
%
%
*/

#include "xtp.h"
#include "regular.h"

/*
  Variable declarations.
*/
static char
  *code,
  **subpattern_end,
  *p,
  start_code,
  *start_pattern,
  **subpattern,
  *token;

static int
  number_parenthesis;

static long
  code_size;

/*
  Forward declarations.
*/
static char
  *Atom(int *),
  *Branch(int *),
  *NextToken(register char *),
  *Node(int),
  *Piece(int *),
  *Regular(int,int *);

static int
  Match(char *),
  Repeat(char *),
  Try(RegularExpression *,char *);

static void
  EmitCode(int),
  Insert(int,char *),
  OpTail(char *,char *),
  Tail(char *,char *);

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   A t o m                                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static char *Atom(int *flagp)
{
  int
    flags;

  register char
    *status;

  *flagp=WorstCase;
  switch(*token++)
  {
    case '^':
    {
      status=Node(MatchBeginningOfLine);
      break;
    }
    case '$':
    {
      status=Node(MatchEndOfProgramOfLine);
      break;
    }
    case '.':
    {
      status=Node(MatchAnyCharacter);
      *flagp|=NonNull | Simple;
      break;
    }
    case '[':
    {
      register int
        class,
        class_end;

      if (*token != '^')
        status=Node(MatchAnyCharacterOf);
      else
        {
          /*
            Complement of range.
          */
          status=Node(MatchAnyCharacterBut);
          token++;
        }
      if ((*token == ']') || (*token == '-'))
        EmitCode(*token++);
      while ((*token != '\0') && (*token != ']'))
      {
        if (*token != '-')
          EmitCode(*token++);
         else
          {
            token++;
            if ((*token == ']') || (*token == '\0'))
              EmitCode('-');
            else
              {
                class=((int)*(unsigned char *)(token-2))+1;
                class_end=((int)*(unsigned char *)(token));
                if (class > class_end+1)
                  Fail("invalid [] range");
                for(; class <= class_end; class++)
                  EmitCode((char) class);
                token++;
              }
          }
      }
      EmitCode('\0');
      if (*token != ']')
        Fail("unmatched []");
      token++;
      *flagp|=NonNull | Simple;
      break;
    }
    case '(':
    {
      status=Regular(1,&flags);
      if (status == NULL)
        return(NULL);
      *flagp|=flags & (NonNull | SpecialStart);
      break;
    }
    case '\0':
    case '|':
    case ')':
    {
      Fail("internal urp");
      break;
    }
    case '?':
    case '+':
    case '*':
    {
      Fail("?+* follows nothing");
      break;
    }
    case '\\':
    {
      if (*token == '\0')
        Fail("trailing \\");
      status=Node(MatchExactly);
      EmitCode(*token++);
      EmitCode('\0');
      *flagp|=NonNull | Simple;
      break;
    }
    default:
    {
      register char
        ender;

      register int
        length;

      token--;
      length=strcspn(token,Meta);
      if (length <= 0)
        Fail("internal disaster");
      ender=(*(token+length));
      if (length > 1 && MultipleMatches(ender))
        length--;
      *flagp|=NonNull;
      if (length == 1)
        *flagp|=Simple;
      status=Node(MatchExactly);
      while (length > 0)
      {
        EmitCode(*token++);
        length--;
      }
      EmitCode('\0');
      break;
    }
  }
  return(status);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   B r a n c h                                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function Branch Implements the | operator.
%
%
*/
static char *Branch(int *flagp)
{
  int
    flags;

  register char
    *chain,
    *latest,
    *status;

  *flagp=WorstCase;
  status=Node(MatchThisOrNext);
  chain=NULL;
  while ((*token != '\0') && (*token != '|') && (*token != ')'))
  {
    latest=Piece(&flags);
    if (latest == NULL)
      return(NULL);
    *flagp|=flags & NonNull;
    if (chain == NULL)
      *flagp|=flags & SpecialStart;
    else
      Tail(chain,latest);
    chain=latest;
  }
  if (chain == NULL)
   (void) Node(MatchEmptyString);
  return(status);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E m i t C o d e                                                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static void EmitCode(int opcode)
{
  if (code != &start_code)
    *code++=(char) opcode;
  else
    code_size++;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   I n s e r t                                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function Insert inserts an operator in front of an already-emitted operand.
%
*/
static void Insert(int opcode,char *operand)
{
  register char
    *p,
    *place,
    *q;

  if (code == &start_code)
    {
      code_size+=3;
      return;
    }
  p=code;
  code+=3;
  q=code;
  while (p > operand)
    *--q=(*--p);
  place=operand;
  *place++=opcode;
  *place++='\0';
  *place++='\0';
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   M a t c h                                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static int Match(char *regular_expression)
{
  char
    *next_token;

  register char
    *scan;

  scan=regular_expression;
  while (scan != NULL)
  {
    next_token=NextToken(scan);
    switch(OpCode(scan))
    {
      case MatchBeginningOfLine:
      {
        if (p != start_pattern)
          return(0);
        break;
      }
      case MatchEndOfProgramOfLine:
      {
        if (*p != '\0')
         return(0);
        break;
      }
      case MatchAnyCharacter:
      {
        if (*p == '\0')
          return(0);
        p++;
        break;
      }
      case MatchExactly:
      {
        register char
          *operand;

        register int
          length;

        operand=Operand(scan);
        /*
          Inline the first character for speed.
        */
        if (*operand != *p)
          return(0);
        length=strlen(operand);
        if ((length > 1) && (strncmp(operand,p,length) != 0))
          return(0);
        p+=length;
        break;
      }
      case MatchAnyCharacterOf:
      {
        if ((*p == '\0' || strchr(Operand(scan),*p) == NULL))
          return(0);
        p++;
        break;
      }
      case MatchAnyCharacterBut:
      {
        if ((*p == '\0') || (strchr(Operand(scan),*p) != NULL))
          return(0);
        p++;
        break;
      }
      case MatchEmptyString:
        break;
      case Back:
        break;
      case Open+1:
      case Open+2:
      case Open+3:
      case Open+4:
      case Open+5:
      case Open+6:
      case Open+7:
      case Open+8:
      case Open+9:
      {
        register char
          *save;

        register int
          no;

        no=OpCode(scan)-Open;
        save=p;
        if (!Match(next_token))
          return(0);
        else
          {
            /*
              Don't set subpattern if some later invocation of the same
              parentheses already has.
            */
            if (subpattern[no] == NULL)
              subpattern[no]=save;
            return(1);
          }
        break;
      }
      case Close+1:
      case Close+2:
      case Close+3:
      case Close+4:
      case Close+5:
      case Close+6:
      case Close+7:
      case Close+8:
      case Close+9:
      {
        register char
          *save;

        register int
          no;

        no=OpCode(scan)-Close;
        save=p;
        if (!Match(next_token))
           return(0);
        else
          {
            /*
              Don't set subpattern_end if some later invocation of the same
              parentheses already has.
            */
            if (subpattern_end[no] == NULL)
              subpattern_end[no]=save;
            return(1);
          }
        break;
      }
      case MatchThisOrNext:
      {
        register char
          *save;

        if (OpCode(next_token) != MatchThisOrNext)
          next_token=Operand(scan);
        else
          {
            do
            {
              save=p;
              if (Match(Operand(scan)))
                return(1);
              p=save;
              scan=NextToken(scan);
            } while ((scan != NULL) && (OpCode(scan) == MatchThisOrNext));
            return(0);
          }
        break;
      }
      case MatchZeroOrMore:
      case MatchOneOrMore:
      {
        register char
          next_tokench,
          *save;

        register int
          min,
          no;

        /*
          Lookahead to avoid useless match attempts when we know what
          character comes next_token.
        */
        next_tokench='\0';
        if (OpCode(next_token) == MatchExactly)
          next_tokench=(*Operand(next_token));
        min=(OpCode(scan) == MatchZeroOrMore) ? 0 : 1;
        save=p;
        no=Repeat(Operand(scan));
        while (no >= min)
        {
          /*
            If it could work, try it.
          */
          if ((next_tokench == '\0') || (*p == next_tokench))
            if (Match(next_token))
              return(1);
          /*
            Couldn't or didn't -- back up.
          */
          no--;
          p=save+no;
        }
        return(0);
        break;
      }
      case EndOfProgram:
        return(1);
        break;
      default:
        (void) fprintf(stderr,"Regular(3): %s","memory corruption");
        return(0);
        break;
    }
    scan=next_token;
  }
  (void) fprintf(stderr,"Regular(3): %s","corrupted pointers");
  return(0);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   N e x t T o k e n                                                         %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static char *NextToken(register char *p)
{
  register int
    offset;

  if (p == &start_code)
    return(NULL);
  offset=Next(p);
  if (offset == 0)
    return(NULL);
  if (OpCode(p) == Back)
    return(p-offset);
  else
    return(p+offset);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   N o d e                                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static char *Node(int opcode)
{
  register char
    *ptr,
    *status;

  status=code;
  if (status == &start_code)
    {
      code_size+=3;
      return(status);
    }
  ptr=status;
  *ptr++=(char) opcode;
  *ptr++='\0';
  *ptr++='\0';
  code=ptr;
  return(status);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   O p T a i l                                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static void OpTail(char *p,char *value)
{
  /*
    "Operandless" and "op != MatchThisOrNext" are synonymous in practice.
  */
  if ((p == NULL) || (p == &start_code) || (OpCode(p) != MatchThisOrNext))
    return;
  Tail(Operand(p),value);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   P i e c e                                                                 %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static char *Piece(int *flagp)
{
  int
    flags;

  register char
    *next_token,
    op,
    *status;

  status=Atom(&flags);
  if (status == NULL)
    return(NULL);
  op=(*token);
  if (!MultipleMatches(op))
    {
      *flagp=flags;
      return(status);
    }
  if (!(flags & NonNull) && op != '?')
    Fail("*+ operand could be empty");
  *flagp=(op != '+') ? (WorstCase | SpecialStart) : (WorstCase | NonNull);
  if (op == '*' && (flags & Simple))
    Insert(MatchZeroOrMore,status);
  else
    if (op == '*')
      {
        /*
          Emit x* as (x&|), where & means "self".
        */
        Insert(MatchThisOrNext,status);
        OpTail(status,Node(Back));
        OpTail(status,status);
        Tail(status,Node(MatchThisOrNext));
        Tail(status,Node(MatchEmptyString));
      }
    else
      if ((op == '+') && (flags & Simple))
        Insert(MatchOneOrMore,status);
      else
        if (op == '+')
          {
            /*
              Emit x+ as x (&|), where & means "self".
            */
            next_token=Node(MatchThisOrNext);
            Tail(status,next_token);
            Tail(Node(Back),status);
            Tail(next_token,Node(MatchThisOrNext));
            Tail(status,Node(MatchEmptyString));
          }
        else
          if (op == '?')
            {
              /*
                Emit x? as (x|)
              */
              Insert(MatchThisOrNext,status);
              Tail(status,Node(MatchThisOrNext));
              next_token=Node(MatchEmptyString);
              Tail(status,next_token);
              OpTail(status,next_token);
            }
  token++;
  if (MultipleMatches(*token))
    Fail("nested *?+");
  return(status);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e g u l a r                                                             %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static char *Regular(int parenthesized,int *flagp)
{
  int
    flags;

  register char
    *br,
    *ender,
    *status;

  register int
    count;

  count=0;
  *flagp=NonNull;
  if (!parenthesized)
    status=NULL;
  else
    {
      /*
        Make an Open node.
      */
      if (number_parenthesis >= NumberSubExpressions)
        Fail("too many ()");
      count=number_parenthesis;
      number_parenthesis++;
      status=Node(Open+count);
    }
  /*
    Pick up the branches, linking them together.
  */
  br=Branch(&flags);
  if (br == NULL)
    return(NULL);
  if (status != NULL)
    Tail(status,br);
  else
    status=br;
  if (!(flags & NonNull))
    *flagp&=(~NonNull);
  *flagp|=flags & SpecialStart;
  while (*token == '|')
  {
    token++;
    br=Branch(&flags);
    if (br == NULL)
      return(NULL);
    Tail(status,br);
    if (!(flags & NonNull))
      *flagp &= ~NonNull;
    *flagp|=flags & SpecialStart;
  }
  /*
    Make a closing node and hook it on the end.
  */
  ender=Node((parenthesized) ? Close+count : EndOfProgram);
  Tail(status,ender);
  /*
    Hook the tails of the branches to the closing node.
  */
  for(br=status; br != NULL; br=NextToken(br))
    OpTail(br,ender);
  /*
    Check for proper termination.
  */
  if (parenthesized && (*token++ != ')'))
    Fail("unmatched()")
  else
    if (!parenthesized && (*token != '\0'))
      {
        if (*token == ')')
          Fail("unmatched()")
        else
          Fail("junk on end")
       }
  return(status);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   R e p e a t                                                               %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static int Repeat(char *p)
{
  register char
    *operand,
    *scan;

  register int
    count=0;

  scan=p;
  operand=Operand(p);
  switch(OpCode(p))
  {
    case MatchAnyCharacter:
    {
      count=strlen(scan);
      scan+=count;
      break;
    }
    case MatchExactly:
    {
      while (*operand == *scan)
      {
        count++;
        scan++;
      }
      break;
    }
    case MatchAnyCharacterOf:
    {
      while ((*scan != '\0') && (strchr(operand,*scan) != NULL))
      {
        count++;
        scan++;
      }
      break;
    }
    case MatchAnyCharacterBut:
    {
      while ((*scan != '\0') && (strchr(operand,*scan) == NULL))
      {
        count++;
        scan++;
      }
      break;
    }
    default:
    {
      (void) fprintf(stderr,"Regular(3): %s","internal foulup");
      count=0;
      break;
    }
  }
  p=scan;
  return(count);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   T a i l                                                                   %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static void Tail(char *p,char *val)
{
  register char
    *scan,
    *temp;

  register int
    offset;

  if (p == &start_code)
    return;
  /*
    Find last node.
  */
  scan=p;
  for(;;)
  {
    temp=NextToken(scan);
    if (temp == NULL)
      break;
    scan=temp;
  }
  if (OpCode(scan) == Back)
    offset=scan-val;
  else
    offset=val-scan;
  *(scan+1)=(offset >> 8) & 0377;
  *(scan+2)=offset & 0377;
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   T r y                                                                     %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
*/
static int Try(RegularExpression *regular_expression,char *pattern)
{
  register char
    **ep,
    **sp;

  register int
    i;

  p=pattern;
  subpattern=regular_expression->subpattern;
  subpattern_end=regular_expression->subpattern_end;
  sp=regular_expression->subpattern;
  ep=regular_expression->subpattern_end;
  for(i=NumberSubExpressions; i > 0; i--)
  {
    *sp++=NULL;
    *ep++=NULL;
  }
  if (!Match(regular_expression->program+1))
    return(0);
  else
    {
      regular_expression->subpattern[0]=pattern;
      regular_expression->subpattern_end[0]=p;
      regular_expression->pattern_length=p-pattern;
      return(1);
    }
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   C o m p i l e R e g u l a r E x p r e s s i o n                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function CompileRegularExpression compiles a regular expression into a
%  structure of type RegularExpression and returns a pointer to it.  The space
%  is allocated using function malloc and may be released by function free.
%
%
*/
RegularExpression *CompileRegularExpression(char *regular_expression)
{
  int
    flags;

  register char
    *longest,
    *scan;

  register RegularExpression
    *r;

  register int
    length;

  if (regular_expression == NULL)
    Fail("NULL argument");
  /*
    First pass: determine size.
  */
  token=regular_expression;
  number_parenthesis=1;
  code_size=0L;
  code=(&start_code);
  EmitCode(Magick);
  if (Regular(0,&flags) == NULL)
    return(NULL);
  /*
    Allocate space.
  */
  r=(RegularExpression *)
    malloc((unsigned int) (code_size+sizeof(RegularExpression)));
  if (r == (RegularExpression *) NULL)
    Fail("out of space");
  /*
    Second pass: emit code.
  */
  token=regular_expression;
  number_parenthesis=1;
  code=r->program;
  EmitCode(Magick);
  if (Regular(0,&flags) == NULL)
    return(NULL);
  /*
    Dig out information for optimizations.
  */
  r->start_character='\0';
  r->anchor=0;
  r->priority_pattern=NULL;
  r->pattern_length=0;
  scan=r->program+1;
  if (OpCode(NextToken(scan)) == EndOfProgram)
    {
      scan=Operand(scan);
      if (OpCode(scan) == MatchExactly)
        r->start_character=(*Operand(scan));
      else
        if (OpCode(scan) == MatchBeginningOfLine)
          r->anchor++;
      /*
        If there's something expensive in the regular expression, find the
        longest literal pattern that must appear and make it the
        priority_pattern.
      */
      if (flags & SpecialStart)
        {
          longest=NULL;
          length=0;
          for(; scan != NULL; scan=NextToken(scan))
            if ((OpCode(scan) == MatchExactly) &&
                ((int) strlen(Operand(scan)) >= length))
              {
                longest=Operand(scan);
                length=strlen(Operand(scan));
              }
          r->priority_pattern=longest;
          r->pattern_length=length;
        }
    }
  return(r);
}

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                                                             %
%                                                                             %
%   E x e c u t e R e g u l a r E x p r e s s i o n                           %
%                                                                             %
%                                                                             %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Function ExecuteRegularExpression matches a NULL-terminated pattern against
%  the compiled regular expression in regular-expression.  It returns 1 for
%  success and 0 for failure.
%
%
*/
int ExecuteRegularExpression(register RegularExpression *regular_expression,
  register char *pattern)
{
  register char
    *s;

  if ((regular_expression == (RegularExpression *) NULL) ||
      (pattern == (char *) NULL))
    {
      (void) fprintf(stderr,"Regular(3): %s","NULL parameter\n");
      return(0);
    }
  /*
    Check validity of program.
  */
  if (((int)*(unsigned char *)(regular_expression->program)) != Magick)
    {
      (void) fprintf(stderr,"Regular(3): %s","corrupted program");
      return(0);
    }
  /*
    If there is a "must appear" pattern, look for it.
  */
  if (regular_expression->priority_pattern != NULL)
    {
      s=pattern;
      while ((s=strchr(s,regular_expression->priority_pattern[0])) != NULL)
      {
        if (strncmp(s,regular_expression->priority_pattern,
            regular_expression->pattern_length) == 0)
          break;
        s++;
       }
       if (s == NULL)
         return(0);
    }
  /*
    Mark beginning of line for ^.
  */
  start_pattern=pattern;
  /*
    Simplest case:  anchored match need be tried only once.
  */
  if (regular_expression->anchor)
    return(Try(regular_expression,pattern));
  /*
    Messy cases:  unanchored match.
  */
  s=pattern;
  if (regular_expression->start_character != '\0')
    while ((s=strchr(s,regular_expression->start_character)) != NULL)
    {
      if (Try(regular_expression,s))
        return(1);
      s++;
    }
  else
    do
    {
      if (Try(regular_expression,s))
        return(1);
    } while (*s++ != '\0');
  return(0);
}
