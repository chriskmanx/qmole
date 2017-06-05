/*--------------------------------*-C-*---------------------------------*
 * File:	grkelot.c
 *----------------------------------------------------------------------*
 *
 * All portions of code are copyright by their respective author/s.
 * Copyright (c) 1994,1995	 Angelo Haritsis
 * Copyright (c) 1997,1998	 Oezger Kesim <kesim@math.fu-berlin.de>
 * Copyright (c) 2004		  Jingmin Zhou <jimmyzhou@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *----------------------------------------------------------------------*/
/*
** $Id: grkelot.c,v 1.12 2004/11/11 00:20:28 cvs Exp $
*/

#include "../config.h"
#include "rxvt.h"


#ifdef DEBUG_VERBOSE
#define DEBUG_LEVEL 1
#else 
#define DEBUG_LEVEL 0
#endif

#if DEBUG_LEVEL
#define DBG_MSG(d,x) if(d <= DEBUG_LEVEL) fprintf x
#else
#define DBG_MSG(d,x)
#endif


#ifdef GREEK_SUPPORT

/* --- Macros, Types --------- */
#define MAX_STATES	4	/* max # states for the FSM */
#define MAX_SWITCHER	2U	/* per state */
#define MAX_VAL 	256	/* for temp allocation */

#ifndef HAVE_SYS_TYPES_H
typedef unsigned char u_char;
typedef unsigned int u_int;
typedef unsigned long u_long;
#endif

typedef struct s_xlat {
	u_int		   first, last;
	u_int		  *pval;	/* array of translated values */
} K_XLAT;

typedef struct s_switch {
	u_char		  type;	/* Ascii, Virtual, Scan */
	u_int		   code;
	u_char		  nextstate;
	u_char		  on;		/* current state of key: 0 = off */
} K_SWITCH;

typedef struct s_state {
	u_int		  num_xlat;	/* number of translations */
	K_XLAT		 *xlat;	/* State translations ((dynamic - realloc'ed) */
	u_int		   num_switcher;	/* number of switcher keys */
	K_SWITCH		switcher[MAX_SWITCHER];	/* switcher keys to other states */
	u_char		  life;	/* 0 = until switched by key */
	u_char		  prev_state;	/* filled when jumped to a new state */
} K_STATE;

/* type for each one of the different greek standards (xlat types) */
typedef struct s_xlat_type {
	char		   *plain;
	char		   *accent;
	char		   *accent_xtra;
	char		   *umlaut;
	char		   *acc_uml;
} XLAT_TYPE;

/* --- Local Data ------------ */
static K_STATE  State[MAX_STATES];

/* Current State */
static u_char   nStateNow = 0;
static K_STATE *pStateNow = &State[0];
static int	  GreekMode = GREEK_ELOT928;

/*
 * The following are hard-coded for now. The idea is that such strings would
 * be read from a config file making it possible to change language/encodings
 * more flexibly.
 */
/* elot 928 xlations */
static char	 elot_xlat_plain[] = "65-122:193,194,216,196,197,214,195,199,201,206,202,203,204,205,207,208,81,209,211,212,200,217,87,215,213,198,91,92,93,94,95,96,225,226,248,228,229,246,227,231,233,238,234,235,236,237,239,240,113,241,243,244,232,249,242,247,245,230";

/* c and s give copyright and section sign */
static char	 elot_xlat_acc[] = "65-122:182,194,216,196,184,214,195,185,186,206,202,203,204,205,188,208,81,209,211,212,200,191,87,215,190,198,91,92,93,94,95,96,220,226," /*248 */ "169,228,221,246,227,222,223,238,234,235,236,237,252,240,113,241," /*243 */ "167,244,232,254,242,247,253,230";
static char	 elot_xlat_acc_xtra[] = "46-62:183,47,48,49,50,51,52,53,54,55,56,57,58,59,171,61,187";	/* anw teleia, quotes */
static char	 elot_xlat_uml[] = "65-122:193,194,216,196,197,214,195,199,218,206,202,203,204,205,207,208,81,209,211,212,200,217,87,215,219,198,91,92,93,94,95,96,225,226,248,228,229,246,227,231,250,238,234,235,236,237,239,240,113,241,243,244,232,249,242,247,251,230";
static char	 elot_xlat_umacc[] = "65-122:193,194,216,196,197,214,195,199,201,206,202,203,204,205,207,208,81,209,211,212,200,217,87,215,213,198,91,92,93,94,95,96,225,226,248,228,229,246,227,231,192,238,234,235,236,237,239,240,113,241,243,244,232,249,242,247,224,230";

/* ibm 437 xlations */
static char	 i437_xlat_plain[] = "65-122:128,129,150,131,132,148,130,134,136,141,137,138,139,140,142,143,81,144,145,146,135,151,87,149,147,133,91,92,93,94,95,96,152,153,175,155,156,173,154,158,160,165,161,162,163,164,166,167,113,168,169,171,159,224,170,174,172,157";
static char	 i437_xlat_acc[] = "65-122:234,129,150,131,235,148,130,236,237,141,137,138,139,140,238,143,81,144,145,146,135,240,87,149,239,133,91,92,93,94,95,96,225,153,175,155,226,173,154,227,229,165,161,162,163,164,230,167,113,168,169,171,159,233,170,174,231,157";
static char	 i437_xlat_acc_xtra[] = "46-46:250";	/* anw teleia */
static char	 i437_xlat_uml[] = "65-122:128,129,150,131,132,148,130,134,136,141,137,138,139,140,142,143,81,144,145,146,135,151,87,149,147,133,91,92,93,94,95,96,152,153,175,155,156,173,154,158,228,165,161,162,163,164,166,167,113,168,169,171,159,224,170,174,232,157";
static char	 i437_xlat_umacc[] = "65-122:128,129,150,131,132,148,130,134,136,141,137,138,139,140,142,143,81,144,145,146,135,151,87,149,147,133,91,92,93,94,95,96,152,153,175,155,156,173,154,158,42,165,161,162,163,164,166,167,113,168,169,171,159,224,170,174,42,157";

/*
 * currently ELOT928 and IBM437 are supported; easy to include others
 * (not recommended: stick to just these 2 if not only the ELOT one)
 */
static XLAT_TYPE xlat_type[] =
{
	{elot_xlat_plain, elot_xlat_acc, elot_xlat_acc_xtra, elot_xlat_uml, elot_xlat_umacc},
	{i437_xlat_plain, i437_xlat_acc, i437_xlat_acc_xtra, i437_xlat_uml, i437_xlat_umacc},
};

/* the current trasnaltion type */
static XLAT_TYPE *xlat_now = &xlat_type[GREEK_ELOT928];

#define NUM_XLAT_TYPES	(sizeof(xlat_type) / sizeof(xlat_type[0]))

static void	 kstate_add_xlat(char *str);
static void	 kstate_add_switcher(char *str);
static void	 kstate_set_life(char *str);

/* --- Functions ------------- */
/* INTPROTO */
void
kstate_setcurr(int stateno)
{
	u_char		  prev_state;

	if ((u_int) stateno > (u_int) MAX_STATES)
	return;
	if (pStateNow->life == 1)
	prev_state = pStateNow->prev_state;
	else
	prev_state = nStateNow;
	pStateNow = &State[nStateNow = stateno];
	pStateNow->prev_state = prev_state;
}

/* INTPROTO */
void
kstate_init(void)
{
	pStateNow->num_xlat = pStateNow->num_switcher = pStateNow->life = pStateNow->prev_state = 0;
	pStateNow->xlat = NULL;
}

/* INTPROTO */
void
kstate_end(void)
{
	int			 i;

	for (i = 0; i < pStateNow->num_xlat; i++)
	free(pStateNow->xlat[i].pval);
	if (pStateNow->num_xlat > 0)
	free(pStateNow->xlat);
}

/*
 * Hard coded ELOT-928 translations. Could read these from an rc-type file
 * to support other remappers.
 */
/* INTPROTO */
void
kstate_init_all(int greek_mode)
{
/* the translation tables for the 4 FSM states for ELOT-928 mappings */
	int			 i;

	for (i = 0; i < MAX_STATES; i++) {
	kstate_setcurr(i);
	kstate_init();
	}
	if (greek_mode < 0 || greek_mode >= NUM_XLAT_TYPES)		/* avoid death */
	greek_mode = GREEK_ELOT928;
	xlat_now = &xlat_type[greek_mode];
	kstate_setcurr(0);
	kstate_add_xlat(xlat_now->plain);
	kstate_add_switcher("A;:1");
	kstate_add_switcher("A::2");
	kstate_set_life("L0");

	kstate_setcurr(1);
	kstate_add_xlat(xlat_now->accent);
	kstate_add_xlat(xlat_now->accent_xtra);
	kstate_add_switcher("A::3");
	kstate_set_life("L1");

	kstate_setcurr(2);
	kstate_add_xlat(xlat_now->umlaut);
	kstate_add_switcher("A;:3");
	kstate_set_life("L1");

	kstate_setcurr(3);
	kstate_add_xlat(xlat_now->acc_uml);
	kstate_set_life("L1");
}

/* INTPROTO */
void
kstate_end_all(void)
{
	int			 i;

	for (i = 0; i < MAX_STATES; i++) {
	kstate_setcurr(i);
	kstate_end();
	}
	kstate_setcurr(0);
}

/*
 * reset FSM
 */
/* INTPROTO */
void
kstate_reset(void)
{
	kstate_setcurr(0);
}

/* INTPROTO */
void
kstate_add_xlat(char *str)
{
	K_XLAT		 *xlat;
	u_int		  *pval_tmp;
	char		   *sval;
	int			 i;

	if (str == NULL)
		return;

	/* add a new xlat table in state */
	if (pStateNow->num_xlat == 0) {
		pStateNow->xlat = rxvt_malloc(sizeof(K_XLAT));
	}
	else			/* prefer contiguous data, realloc */
		pStateNow->xlat = rxvt_realloc (pStateNow->xlat, (pStateNow->num_xlat + 1) * sizeof(K_XLAT));

	xlat = &pStateNow->xlat[pStateNow->num_xlat];
	/* parse str and derive first, last, values */
	xlat->first = (u_int) atoi(strtok(str, "-"));
	xlat->last = (u_int) atoi(strtok(NULL, ":"));
	i = 0;
	pval_tmp = calloc(MAX_VAL, sizeof(K_XLAT));
	while ((sval = strtok(NULL, ",")) != NULL)
		pval_tmp[i++] = (u_int) (atoi(sval));
	xlat->pval = calloc(i, sizeof(K_XLAT));
	if (xlat->pval != NULL)
		MEMCPY(xlat->pval, pval_tmp, i * sizeof(u_int));
	free(pval_tmp);
	pStateNow->num_xlat++;
}

/*
 * Ascii only for this implementation
 */
/* INTPROTO */
void
kstate_add_switcher(char *str)
{
	K_SWITCH	   *switcher;

	if (str == NULL)
		return;
	if (pStateNow->num_switcher >= MAX_SWITCHER)
		return;
	switcher = &pStateNow->switcher[pStateNow->num_switcher];
	switch (switcher->type = str[0]) {
	case 'A':			/* ascii eg: A;:2 */
		switcher->code = str[1];
		switcher->nextstate = atoi(&str[3]);
		break;
	}
	switcher->on = 0;
	pStateNow->num_switcher++;
}

/* L1 or L0 */
/* INTPROTO */
void
kstate_set_life(char *str)
{
	pStateNow->life = atoi(&str[1]);
}

/* INTPROTO */
unsigned int
kstate_cxlat(unsigned int c)
{
	int			 i;

/* check for ascii switcher */
	for (i = 0; i < pStateNow->num_switcher; i++)
	if (pStateNow->switcher[i].type == 'A' &&	/* only ascii here */
		c == pStateNow->switcher[i].code) {
		kstate_setcurr(pStateNow->switcher[i].nextstate);
		pStateNow->switcher[i].on = 1;
		return ((unsigned int)-1);
	}
/* do translation */
	for (i = 0; i < pStateNow->num_xlat; i++)
	if (c >= pStateNow->xlat[i].first && c <= pStateNow->xlat[i].last) {
		c = pStateNow->xlat[i].pval[c - pStateNow->xlat[i].first];
		break;
	}
/* switch back to previous state if life of current is 1 */
	if (pStateNow->life == 1)
	kstate_setcurr(pStateNow->prev_state);
	return (c);
}

/* EXTPROTO */
void
greek_init(void)
{
	kstate_init_all(GreekMode);
}

/* EXTPROTO */
void
greek_end(void)
{
	kstate_end_all();
}

/* EXTPROTO */
void
greek_reset(void)
{
	kstate_reset();
}

/* EXTPROTO */
void
greek_setmode(int greek_mode)
{
	GreekMode = greek_mode;
}

/* EXTPROTO */
int
greek_getmode(void)
{
	return (GreekMode);
}

/*
 * xlate a given string in-place - return new string length
 */
/* EXTPROTO */
int
greek_xlat(char *s, int num_chars)
{
	int			 i, count;
	unsigned int	c;

	for (i = 0, count = 0; i < num_chars; i++) {
	c = kstate_cxlat((unsigned int)s[i]);
	if (c != -1)
		s[count++] = (char)c;
	}
	s[count] = '\0';
	return (count);

}

#ifdef TEST
int
main(void)
{
/*char text[] = "abcdef;aGDZXC"; */
	char			text[] = "abcdef;a:ibgdezhuiklmnjoprstyfxcv";

	kstate_init_all(GREEK_ELOT928);
	printf("text: %s\n", text);
	greek_xlat(text, STRLEN(text));
	printf("xlat'ed text: %s\n", text);
	kstate_end_all();
	return 0;
}
#endif

#endif				/* GREEK_SUPPORT */
/*----------------------- end-of-file (C source) -----------------------*/
