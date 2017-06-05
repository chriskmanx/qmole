/*
 * ROX-Filer, filer for the ROX desktop project
 * By Thomas Leonard, <tal197@users.sourceforge.net>.
 *
 * This rest of this file was taken from GNU FileUtils, with minor
 * modifications.
 */

/* Masks for the `flags' field in a `struct mode_change'. */

#if ! defined MODECHANGE_H_
# define MODECHANGE_H_

/* Affect the execute bits only if at least one execute bit is set already,
   or if the file is a directory. */
# define MODE_X_IF_ANY_X 01

/* If set, copy some existing permissions for u, g, or o onto the other two.
   Which of u, g, or o is copied is determined by which bits are set in the
   `value' field. */
# define MODE_COPY_EXISTING 02

struct mode_change
{
  char op;			/* One of "=+-". */
  char flags;			/* Special operations. */
  unsigned short affected;	/* Set for u/g/o/s/s/t, if to be affected. */
  unsigned short value;		/* Bits to add/remove. */
  struct mode_change *next;	/* Link to next change in list. */
};

/* Masks for mode_compile argument. */
# define MODE_MASK_EQUALS 1
# define MODE_MASK_PLUS 2
# define MODE_MASK_MINUS 4
# define MODE_MASK_ALL (MODE_MASK_EQUALS | MODE_MASK_PLUS | MODE_MASK_MINUS)

struct mode_change *mode_compile(const char *, unsigned);
unsigned short mode_adjust(unsigned, const struct mode_change *);
void mode_free(struct mode_change *);

#endif
