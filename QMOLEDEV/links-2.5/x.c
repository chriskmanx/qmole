/* x.c
 * (c) 2002 Petr 'Brain' Kulhavy
 * This file is a part of the Links program, released under GPL.
 */

/* Takovej mensi problemek se scrollovanim:
 * 
 * Mikulas a Xy zpusobili, ze scrollovani a prekreslovani je asynchronni. To znamena, ze
 * je v tom peknej bordylek. Kdyz BFU scrollne s oknem, tak se zavola funkce scroll. Ta
 * posle Xum XCopyArea (prekopiruje prislusny kus okna) a vygeneruje eventu
 * (GraphicsExpose) na postizenou (odkrytou) oblast. Funkce XCopyArea pripadne vygeneruje
 * dalsi GraphicsExpose eventu na postizenou oblast, ktera se muze objevit, kdyz je
 * linksove okno prekryto jinym oknem. 
 *
 * Funkce scroll skonci. V event handleru se nekdy v budoucnosti (treba za tyden)
 * zpracuji eventy od Xu, mezi nimi i GraphicsExpose - tedy prekreslovani postizenych
 * oblasti.
 *
 * Problem je v tom, ze v okamziku, kdy scroll skonci, neni obrazovka prekreslena do
 * konzistentniho stavu (misty je garbaz) a navic se muze volat dalsi scroll. Tedy
 * XCopyArea muze posunout garbaz nekam do cudu a az se dostane na radu prekreslovani
 * postizenych oblasti, garbaz se uz nikdy neprekresli.
 *
 * Ja jsem navrhoval udelat scrollovani synchronni, to znamena, ze v okamziku, kdy scroll
 * skonci, bude okno v konzistentnim stavu. To by znamenalo volat ze scrollu zpracovavani
 * eventu (alespon GraphicsExpose). To by ovsem nepomohlo, protoze prekreslovaci funkce
 * neprekresluje, ale registruje si bottom halfy a podobny ptakoviny a prekresluje az
 * nekdy v budoucnosti. Navic Mikulas rikal, ze prekreslovaci funkce muze generovat dalsi
 * prekreslovani (sice jsem nepochopil jak, ale hlavne, ze je vecirek), takze by to
 * neslo.
 *
 * Proto Mikulas vymyslel genialni tah - takzvany "genitah". Ve funkci scroll se projede
 * fronta eventu od Xserveru a vyberou se GraphicsExp(l)ose eventy a ulozi se do zvlastni
 * fronty. Ve funkci na zpracovani Xovych eventu se zpracuji eventy z teto fronty. Na
 * zacatku scrollovaci funkce se projedou vsechny eventy ve zvlastni fronte a updatuji se
 * jim souradnice podle prislusneho scrollu.
 * 
 * Na to jsem ja vymyslel uzasnou vymluvu: co kdyz 1. scroll vyrobi nejake postizene
 * oblasti a 2. scroll bude mit jinou clipovaci oblast, ktera bude tu postizenou oblast
 * zasahovat z casti. Tak to se bude jako ta postizena oblast stipat na casti a ty casti
 * se posunou podle toho, jestli jsou zasazene tim 2. scrollem? Tim jsem ho utrel, jak
 * zpoceny celo. 
 * 
 * Takze se to nakonec udela tak, ze ze scrollu vratim hromadu rectanglu, ktere se maji
 * prekreslit, a Mikulas si s tim udela, co bude chtit. Podobne jako ve svgalib, kde se
 * vrati 1 a Mikulas si prislusnou odkrytou oblast prekresli sam. Doufam jen, ze to je
 * posledni verze a ze nevzniknou dalsi problemy.
 *
 * Ve funkci scroll tedy pribude argument struct rect_set **.
 */


/* Data od XImage se alokujou pomoci malloc. get_links_icon musi alokovat taky
 * pomoci malloc.
 */

/* Pozor: po kazdem XSync nebo XFlush se musi dat
 * X_SCHEDULE_PROCESS_EVENTS
 * jinak to bude cekat na filedescriptoru, i kdyz to ma eventy uz ve fronte.
 *	-- mikulas
 */


#include "cfg.h"

#ifdef GRDRV_X

/* #define X_DEBUG */
/* #define SC_DEBUG */

#if defined(X_DEBUG) || defined(SC_DEBUG)
	#define MESSAGE(a) fprintf(stderr,"%s",a);
#endif

#include "links.h"

/* Mikulas je PRASE: definuje makro "format" a navrch to jeste nechce vopravit */
#ifdef format   
	#undef format
#endif

#include <stdlib.h>
#include <stdio.h>

#include <X11/Xlib.h>
#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#ifdef HAVE_X11_XLOCALE_H
#include <X11/Xlocale.h>
#endif


#ifndef XK_MISCELLANY
	#define XK_MISCELLANY
#endif

#ifndef XK_LATIN1
	#define XK_LATIN1
#endif
#include <X11/keysymdef.h>

#ifdef HAVE_LANGINFO_H
#include <langinfo.h>
#endif

#define X_BORDER_WIDTH 4
#define X_HASH_TABLE_SIZE 64


#define XPIXMAPP(a) ((struct x_pixmapa*)(a))

static int x_default_window_width;
static int x_default_window_height;

static long (*x_get_color_function)(int);

static void selection_request(XEvent *event);

static int x_fd;    /* x socket */
static Display *x_display;   /* display */
static int x_screen;   /* screen */
static int x_display_height,x_display_width;   /* screen dimensions */
static int x_black_pixel,x_white_pixel;  /* white and black pixel */
static int x_depth,x_bitmap_bpp;   /* bits per pixel and bytes per pixel */
static int x_bitmap_scanline_pad; /* bitmap scanline_padding in bytes */
static int x_colors;  /* colors in the palette (undefined when there's no palette) */
static int x_have_palette;
static int x_input_encoding;	/* locales encoding */
static int x_bitmap_bit_order;


static Window x_root_window, fake_window;
static GC x_normal_gc,x_copy_gc,x_drawbitmap_gc,x_scroll_gc;
static Colormap x_colormap;
static Atom x_delete_window_atom, x_wm_protocols_atom, x_sel_atom, x_targets_atom;
static Visual* x_default_visual;
static Pixmap x_icon;

extern struct graphics_driver x_driver;

static unsigned char *x_driver_param=NULL;
static int n_wins;	/* number of windows */


#define X_TYPE_PIXMAP 1
#define X_TYPE_IMAGE 2

struct x_pixmapa
{
	unsigned char type;
	union
	{
		XImage *image;
		Pixmap *pixmap;
	}data;
};


static struct
{
	unsigned char count;
	struct graphics_device **pointer;
}
x_hash_table[X_HASH_TABLE_SIZE];

/* string in clipboard is in x_input_encoding */
static unsigned char * x_my_clipboard=NULL;


/*----------------------------------------------------------------------*/

/* Tyhle opicarny tu jsou pro zvyseni rychlosti. Flush se nedela pri kazde operaci, ale
 * rekne se, ze je potreba udelat flush, a zaregistruje se bottom-half, ktery flush
 * provede. Takze jakmile se vrati rizeni do select smycky, tak se provede flush.
 */

static void x_wait_for_event(void)
{
	fd_set rfds;

	FD_ZERO(&rfds);
	FD_SET(x_fd, &rfds);
	select(x_fd+1, &rfds, NULL, NULL, NULL);
}

static void x_process_events(void *data);

static unsigned char flush_in_progress=0;
static unsigned char process_events_in_progress=0;

static inline void X_SCHEDULE_PROCESS_EVENTS(void)
{
	if (!process_events_in_progress)
	{
		register_bottom_half(x_process_events,NULL);
		process_events_in_progress = 1;
	}
}

static void x_do_flush(void *ignore)
{
	/* kdyz budu mit zaregistrovanej bottom-half na tuhle funkci a nekdo mi
	 * tipne Xy, tak se nic nedeje, maximalne se zavola XFlush na blbej
	 * display, ale Xy se nepodelaj */

	flush_in_progress=0;
	XFlush(x_display);
	X_SCHEDULE_PROCESS_EVENTS();
}

static inline void X_FLUSH(void)
{
	if (!flush_in_progress)
	{
		register_bottom_half(x_do_flush,NULL);
		flush_in_progress=1;
	}
}

static int (*old_error_handler)(Display *, XErrorEvent *) = NULL;
static int failure_happened;

static int failure_handler(Display *d, XErrorEvent *e)
{
	failure_happened = 1;
	return 0;
}

static void x_prepare_for_failure(void)
{
	if (old_error_handler)
		internal("x_prepare_for_failure: double call");
	failure_happened = 0;
	old_error_handler = XSetErrorHandler(failure_handler);
}

static int x_test_for_failure(void)
{
	XSync(x_display,False);
	X_SCHEDULE_PROCESS_EVENTS();
	XSetErrorHandler(old_error_handler);
	old_error_handler = NULL;
	return failure_happened;
}

/* suppose l<h */
static void x_clip_number(int *n,int l,int h)
{
	if ((*n)<l)*n=l;
	if ((*n)>h)*n=h;
}


static unsigned char * x_set_palette(void)
{
	XColor color;
	int a,r,g,b;
	int tbl0[4]={0,21845,43690,65535};
	int tbl1[8]={0,9362,18724,28086,37449,46811,56173,65535};

	x_colormap=XCreateColormap(x_display,x_root_window,x_default_visual,AllocAll);
	XInstallColormap(x_display,x_colormap);

	switch(x_depth)
	{
		case 4:
		for (a=0;a<16;a++)
		{
			color.red=(a&8)?65535:0;
			color.green=tbl0[(a>>1)&3];
			color.blue=(a&1)?65535:0;
			color.pixel=a;
			color.flags=DoRed|DoGreen|DoBlue;
			XStoreColor(x_display,x_colormap,&color);
		}
		break;

		case 8:
		for (a=0;a<256;a++)
		{
			color.red=tbl1[(a>>5)&7];
			color.green=tbl1[(a>>2)&7];
			color.blue=tbl0[a&3];
			color.pixel=a;
			color.flags=DoRed|DoGreen|DoBlue;
			XStoreColor(x_display,x_colormap,&color);
		}
		break;

		case 15:
                for (a=0;a<32768;a++){
                       	color.red=((a>>10)&31)*(65535/31);
                        color.green=((a>>5)&31)*(65535/31);
       	                color.blue=(a&31)*(65535/31);
			color.pixel=a;
			color.flags=DoRed|DoGreen|DoBlue;
			XStoreColor(x_display,x_colormap,&color);
                }
		break;
		case 16:
                for (a=0;a<65536;a++){
                       	color.red=((a>>11)&31)*(65535/31);
                        color.green=((a>>5)&63)*(65535/63);
       	                color.blue=(a&31)*(65535/31);
			color.pixel=a;
			color.flags=DoRed|DoGreen|DoBlue;
			XStoreColor(x_display,x_colormap,&color);
                }
		break;

		case 24:
		for (r=0;r<256;r++)
			for (g=0;g<256;g++)
				for (b=0;b<256;b++)
				{
                       			color.red=r<<8;
		                        color.green=g<<8;
       			                color.blue=b<<8;
					color.pixel=(r<<16)+(g<<8)+(b);
					color.flags=DoRed|DoGreen|DoBlue;
					XStoreColor(x_display,x_colormap,&color);
	       		         }
			
		break;
	}
	
	X_FLUSH();
	return NULL;
}


static inline int trans_key(unsigned char * str, int table)
{
	if (table==utf8_table){int a; GET_UTF_8(str,a);return a;}
	if (*str<128)return *str;
	return cp2u(*str,table);
}


/* translates X keys to links representation */
/* return value: 1=valid key, 0=nothing */
static int x_translate_key(XKeyEvent *e,int *key,int *flag)
{
	KeySym ks;
	static XComposeStatus comp = { NULL, 0 };
	static char str[16];
	int table=x_input_encoding<0?drv->codepage:x_input_encoding;
	int len;

	len=XLookupString(e,str,15,&ks,&comp);
	str[len>15?15:len]=0;
	if (!len) str[0]=ks, str[1]=0;
	*flag=0;
	*key=0;

	/* alt, control, shift ... */
	if (e->state&ShiftMask)*flag|=KBD_SHIFT;
	if (e->state&ControlMask)*flag|=KBD_CTRL;
	if (e->state&Mod1Mask)*flag|=KBD_ALT;

	/* alt-f4 */
	if (((*flag)&KBD_ALT)&&(ks==XK_F4)){*key=KBD_CTRL_C;*flag=0;return 1;}

	/* ctrl-c */
	if (((*flag)&KBD_CTRL)&&(ks==XK_c||ks==XK_C)){*key=KBD_CTRL_C;*flag=0;return 1;}
	
	switch (ks)
	{
		case NoSymbol:		return 0;
		case XK_Return:		*key=KBD_ENTER;break;
		case XK_BackSpace:	*key=KBD_BS;break;
#ifdef XK_KP_Tab
		case XK_KP_Tab:
#endif
		case XK_Tab:		*key=KBD_TAB;break;
		case XK_Escape:		*key=KBD_ESC;break;
#ifdef XK_KP_Left
		case XK_KP_Left:
#endif
		case XK_Left:		*key=KBD_LEFT;break;
#ifdef XK_KP_Right
		case XK_KP_Right:
#endif
		case XK_Right:		*key=KBD_RIGHT;break;
#ifdef XK_KP_Up
		case XK_KP_Up:
#endif
		case XK_Up:		*key=KBD_UP;break;
#ifdef XK_KP_Down
		case XK_KP_Down:
#endif
		case XK_Down:		*key=KBD_DOWN;break;
#ifdef XK_KP_Insert
		case XK_KP_Insert:
#endif
		case XK_Insert:		*key=KBD_INS;break;
#ifdef XK_KP_Delete
		case XK_KP_Delete:
#endif
		case XK_Delete:		*key=KBD_DEL;break;
#ifdef XK_KP_Home
		case XK_KP_Home:
#endif
		case XK_Home:		*key=KBD_HOME;break;
#ifdef XK_KP_End
		case XK_KP_End:
#endif
		case XK_End:		*key=KBD_END;break;
#ifdef XK_KP_Page_Up
		case XK_KP_Page_Up:
#endif
#ifdef XK_Page_Up
		case XK_Page_Up:
#endif
					*key=KBD_PAGE_UP;break;
#ifdef XK_KP_Page_Down
		case XK_KP_Page_Down:
#endif
#ifdef XK_Page_Down
		case XK_Page_Down:
#endif
					*key=KBD_PAGE_DOWN;break;
#ifdef XK_KP_F1
		case XK_KP_F1:
#endif
		case XK_F1:		*key=KBD_F1;break;
#ifdef XK_KP_F2
		case XK_KP_F2:
#endif
		case XK_F2:		*key=KBD_F2;break;
#ifdef XK_KP_F3
		case XK_KP_F3:
#endif
		case XK_F3:		*key=KBD_F3;break;
#ifdef XK_KP_F4
		case XK_KP_F4:
#endif
		case XK_F4:		*key=KBD_F4;break;
		case XK_F5:		*key=KBD_F5;break;
		case XK_F6:		*key=KBD_F6;break;
		case XK_F7:		*key=KBD_F7;break;
		case XK_F8:		*key=KBD_F8;break;
		case XK_F9:		*key=KBD_F9;break;
		case XK_F10:		*key=KBD_F10;break;
		case XK_F11:		*key=KBD_F11;break;
		case XK_F12:		*key=KBD_F12;break;
		case XK_KP_Subtract:	*key='-';break;
		case XK_KP_Decimal:	*key='.';break;
		case XK_KP_Divide:	*key='/';break;
		case XK_KP_Space:	*key=' ';break;
		case XK_KP_Enter:	*key=KBD_ENTER;break;
		case XK_KP_Equal:	*key='=';break;
		case XK_KP_Multiply:	*key='*';break;
		case XK_KP_Add:		*key='+';break;
		case XK_KP_0:		*key='0';break;
		case XK_KP_1:		*key='1';break;
		case XK_KP_2:		*key='2';break;
		case XK_KP_3:		*key='3';break;
		case XK_KP_4:		*key='4';break;
		case XK_KP_5:		*key='5';break;
		case XK_KP_6:		*key='6';break;
		case XK_KP_7:		*key='7';break;
		case XK_KP_8:		*key='8';break;
		case XK_KP_9:		*key='9';break;

		default:		
					if (ks&0x8000)return 0;
					*key=((*flag)&KBD_CTRL)?(int)ks&255:trans_key(str,table);
					break;
					/*
		default:		*key=((*flag)&KBD_CTRL)?(int)ks&255:trans_key(str,table);(*flag)&=~KBD_SHIFT;break;
		*/
	}
	return 1;
}

static void x_hash_table_init(void)
{
	int a;

	for (a=0;a<X_HASH_TABLE_SIZE;a++)
	{
		x_hash_table[a].count=0;
		x_hash_table[a].pointer=NULL;
	}
}


static void x_free_hash_table(void)
{
	int a,b;

	for (a=0;a<X_HASH_TABLE_SIZE;a++)
	{
		for (b=0;b<x_hash_table[a].count;b++)
			mem_free(x_hash_table[a].pointer[b]);
		if (x_hash_table[a].pointer)
			mem_free(x_hash_table[a].pointer);
	}
}



/* returns graphics device structure which belonging to the window */
static struct graphics_device * x_find_gd(Window *win)
{
	int a,b;

	a=(*win)&(X_HASH_TABLE_SIZE-1);
	if (!x_hash_table[a].count)return 0;
	for (b=0;b<x_hash_table[a].count;b++)
	{
		if ((*(Window*)((x_hash_table[a].pointer[b])->driver_data))==(*win))
		return x_hash_table[a].pointer[b];
	}
	return 0;
}

static void x_update_driver_param(int w, int h)
{
	int l=0;

	if (n_wins!=1)return;
	
	x_default_window_width=w;
	x_default_window_height=h;
	
	if (x_driver_param)mem_free(x_driver_param);
	x_driver_param=init_str();
	add_num_to_str(&x_driver_param,&l,x_default_window_width);
	add_to_str(&x_driver_param,&l,"x");
	add_num_to_str(&x_driver_param,&l,x_default_window_height);
}



/* adds graphics device to hash table */
static int x_add_to_table(struct graphics_device* gd)
{
	int a=(*((Window*)(gd->driver_data)))&(X_HASH_TABLE_SIZE-1);
	int c=x_hash_table[a].count;

	if (!c) {
		x_hash_table[a].pointer=mem_alloc(sizeof(struct graphics_device *));
	} else {
		if ((unsigned)c > MAXINT / sizeof(struct graphics_device *) - 1) overalloc();
		x_hash_table[a].pointer=mem_realloc(x_hash_table[a].pointer,(c+1)*sizeof(struct graphics_device *));
	}

	x_hash_table[a].pointer[c]=gd;
	x_hash_table[a].count++;
	return 0;
}


/* removes graphics device from table */
static void x_remove_from_table(Window *win)
{
	int a=(*win)&(X_HASH_TABLE_SIZE-1);
	int b;

	for (b=0;b<x_hash_table[a].count;b++)
		if ((*(Window*)((x_hash_table[a].pointer[b])->driver_data))==(*win))
		{
			memmove(x_hash_table[a].pointer+b,x_hash_table[a].pointer+b+1,(x_hash_table[a].count-b-1)*sizeof(struct graphics_device *));
			x_hash_table[a].count--;
			x_hash_table[a].pointer=mem_realloc(x_hash_table[a].pointer,x_hash_table[a].count*sizeof(struct graphics_device*));
		}
}


static void x_clear_clipboard(void)
{
    if(x_my_clipboard)
    {
        mem_free(x_my_clipboard);
        x_my_clipboard=NULL;
    }
}


static void x_process_events(void *data)
{
	XEvent event;
	XEvent last_event;
	struct graphics_device *gd;
	int last_was_mouse;
	int replay_event = 0;

	process_events_in_progress = 0;

#ifdef SC_DEBUG
	MESSAGE("x_process_event\n");
#endif
	memset(&last_event, 0, sizeof last_event);	/* against warning */
	last_was_mouse=0;
	while (XPending(x_display) || replay_event)
	{
		if (replay_event) replay_event = 0;
		else XNextEvent(x_display,&event);
		if (last_was_mouse&&(event.type==ButtonPress||event.type==ButtonRelease))  /* this is end of mouse move block --- call mouse handler */
		{
			int a,b;

			last_was_mouse=0;
#ifdef X_DEBUG
			MESSAGE("(MotionNotify event)\n");
			{
				unsigned char txt[256];
				sprintf(txt,"x=%d y=%d\n",last_event.xmotion.x,last_event.xmotion.y);
				MESSAGE(txt);
			}
#endif
			gd=x_find_gd(&(last_event.xmotion.window));
			if (!gd)break;
			a=B_LEFT;
			b=B_MOVE;
			if ((last_event.xmotion.state)&Button1Mask)
			{
				a=B_LEFT;
				b=B_DRAG;
#ifdef X_DEBUG
				MESSAGE("left button/drag\n");
#endif
			}
			if ((last_event.xmotion.state)&Button2Mask)
			{
				a=B_MIDDLE;
				b=B_DRAG;
#ifdef X_DEBUG	
				MESSAGE("middle button/drag\n");
#endif
			}
			if ((last_event.xmotion.state)&Button3Mask)
			{
				a=B_RIGHT;
				b=B_DRAG;
#ifdef X_DEBUG
				MESSAGE("right button/drag\n");
#endif
			}
			x_clip_number(&(last_event.xmotion.x),gd->size.x1,gd->size.x2);
			x_clip_number(&(last_event.xmotion.y),gd->size.y1,gd->size.y2);
			gd->mouse_handler(gd,last_event.xmotion.x,last_event.xmotion.y,a|b);
		}

		switch(event.type)
		{
			case GraphicsExpose:  /* redraw uncovered area during scroll */
			{
				struct rect r;

#ifdef X_DEBUG
				MESSAGE("(GraphicsExpose event)\n");
#endif
				gd=x_find_gd(&(event.xgraphicsexpose.drawable));
				if (!gd)break;
				r.x1=event.xgraphicsexpose.x;
				r.y1=event.xgraphicsexpose.y;
				r.x2=event.xgraphicsexpose.x+event.xgraphicsexpose.width;
				r.y2=event.xgraphicsexpose.y+event.xgraphicsexpose.height;
				gd->redraw_handler(gd,&r);
			}
			break;
			
			case Expose:   /* redraw part of the window */
			{
				struct rect r;

#ifdef X_DEBUG
				MESSAGE("(Expose event)\n");
#endif

				gd=x_find_gd(&(event.xexpose.window));
				if (!gd)break;
				r.x1=event.xexpose.x;
				r.y1=event.xexpose.y;
				r.x2=event.xexpose.x+event.xexpose.width;
				r.y2=event.xexpose.y+event.xexpose.height;
				gd->redraw_handler(gd,&r);
			}
			break;

			case ConfigureNotify:   /* resize window */
#ifdef X_DEBUG
			MESSAGE("(ConfigureNotify event)\n");
			{
				unsigned char txt[256];
				sprintf(txt,"width=%d height=%d\n",event.xconfigure.width,event.xconfigure.height);
				MESSAGE(txt);
			}
#endif
			gd=x_find_gd(&(event.xconfigure.window));
			if (!gd)break;
			/* when window only moved and size is the same, do nothing */
			if (gd->size.x2==event.xconfigure.width&&gd->size.y2==event.xconfigure.height)break;
			configure_notify_again:
			gd->size.x2=event.xconfigure.width;
			gd->size.y2=event.xconfigure.height;
			x_update_driver_param(event.xconfigure.width, event.xconfigure.height);
			while (XCheckWindowEvent(x_display,*((Window *)(gd->driver_data)),ExposureMask,&event)==True)
				;
			if (XCheckWindowEvent(x_display,*((Window *)(gd->driver_data)),StructureNotifyMask,&event)==True) {
				if (event.type==ConfigureNotify) goto configure_notify_again;
				replay_event=1;
			}
			gd->resize_handler(gd);
			break;

			case KeyPress:   /* a key was pressed */
			{
				int f,k;
#ifdef X_DEBUG
				MESSAGE("(KeyPress event)\n");
				{
					unsigned char txt[256];
					sprintf(txt,"keycode=%d state=%d\n",event.xkey.keycode,event.xkey.state);
					MESSAGE(txt);
				}
#endif
				gd=x_find_gd(&(event.xkey.window));
				if (!gd)break;
				if (x_translate_key((XKeyEvent*)(&event),&k,&f))
					gd->keyboard_handler(gd,k,f);
			}
			break;
	
			case ClientMessage:
			if (
				event.xclient.format!=32||
				event.xclient.message_type!=x_wm_protocols_atom||
				(Atom)event.xclient.data.l[0]!=x_delete_window_atom
			)break;
			
			/* This event is destroy window event from window manager */

			case DestroyNotify:
#ifdef X_DEBUG
			MESSAGE("(DestroyNotify event)\n");
#endif
			gd=x_find_gd(&(event.xkey.window));
			if (!gd)break;

			gd->keyboard_handler(gd,KBD_CLOSE,0);
			break;

			case ButtonRelease:    /* mouse button was released */
			{
				int a;
#ifdef X_DEBUG
				MESSAGE("(ButtonRelease event)\n");
				{
					unsigned char txt[256];
					sprintf(txt,"x=%d y=%d buttons=%d mask=%d\n",event.xbutton.x,event.xbutton.y,event.xbutton.button,event.xbutton.state);
					MESSAGE(txt);
				}
#endif
				gd=x_find_gd(&(event.xbutton.window));
				if (!gd)break;
				last_was_mouse=0;
				switch(event.xbutton.button)
				{
					case 1:
					a=B_LEFT;
					break;

					case 3:
					a=B_RIGHT;
					break;

					case 2:
					a=B_MIDDLE;
					break;

					case 8:
					a=B_FOURTH;
					break;

					case 9:
					a=B_FIFTH;
					break;

					default:
					goto r_xx;

				}
				x_clip_number(&(event.xmotion.x),gd->size.x1,gd->size.x2);
				x_clip_number(&(event.xmotion.y),gd->size.y1,gd->size.y2);
				gd->mouse_handler(gd,event.xbutton.x,event.xbutton.y,a|B_UP);
				r_xx:;
			}
			break;

			case ButtonPress:    /* mouse button was pressed */
			{
				int a;
#ifdef X_DEBUG
				MESSAGE("(ButtonPress event)\n");
				{
					unsigned char txt[256];
					sprintf(txt,"x=%d y=%d buttons=%d mask=%d\n",event.xbutton.x,event.xbutton.y,event.xbutton.button,event.xbutton.state);
					MESSAGE(txt);
				}
#endif
				gd=x_find_gd(&(event.xbutton.window));
				if (!gd)break;
				last_was_mouse=0;
				switch(event.xbutton.button)
				{
					case 1:
					a=B_LEFT;
					break;

					case 3:
					a=B_RIGHT;
					break;

					case 2:
					a=B_MIDDLE;
					break;

					case 4:
					a=B_WHEELUP;
					break;

					case 5:
					a=B_WHEELDOWN;
					break;

					case 6:
					a=B_WHEELLEFT;
					break;

					case 7:
					a=B_WHEELRIGHT;
					break;

					case 8:
					a=B_FOURTH;
					break;

					case 9:
					a=B_FIFTH;
					break;

					default:
					goto p_xx;
				}
				x_clip_number(&(event.xmotion.x),gd->size.x1,gd->size.x2);
				x_clip_number(&(event.xmotion.y),gd->size.y1,gd->size.y2);
				gd->mouse_handler(gd,event.xbutton.x,event.xbutton.y,a|(a != B_WHEELDOWN && a != B_WHEELUP && a != B_WHEELLEFT && a != B_WHEELRIGHT ? B_DOWN : B_MOVE));
				p_xx:;
			}
			break;

			case MotionNotify:   /* pointer moved */
			{
#ifdef X_DEBUG
				MESSAGE("(MotionNotify event)\n");
				{
					unsigned char txt[256];
					sprintf(txt,"x=%d y=%d\n",event.xmotion.x,event.xmotion.y);
					MESSAGE(txt);
				}
#endif
				/* just sign, that this was mouse event */
				last_was_mouse=1;
				last_event=event;
			}
			break;

			/* read clipboard */
			case SelectionNotify:
#ifdef X_DEBUG
                        MESSAGE("xselectionnotify\n");
#endif
			/* handled in x_get_clipboard_text */
			break;

/* This long code must be here in order to implement copying of stuff into the clipboard */
                        case SelectionRequest:
                        {
			    selection_request(&event);
                        }
                        break;
               		
			case MapNotify:
			XFlush (x_display);
			break;
  
			default:
#ifdef X_DEBUG
			{
				unsigned char txt[256];
				sprintf(txt,"event=%d\n",event.type);
				MESSAGE(txt);
			}
#endif
			break;
		}
	}

	if (last_was_mouse)  /* lthat was end of mouse move block --- call mouse handler */
	{
		int a,b;

		last_was_mouse=0;
#ifdef X_DEBUG
		MESSAGE("(MotionNotify event)\n");
		/*
		{
			unsigned char txt[256];
			sprintf(txt,"x=%d y=%d\n",last_event.xmotion.x,last_event.xmotion.y);
			MESSAGE(txt);
		}
		*/
#endif
		gd=x_find_gd(&(last_event.xmotion.window));
		if (!gd)goto ret;
		a=B_LEFT;
		b=B_MOVE;
		if ((last_event.xmotion.state)&Button1Mask)
		{
			a=B_LEFT;
			b=B_DRAG;
#ifdef X_DEBUG
			MESSAGE("left button/drag\n");
#endif
		}
		if ((last_event.xmotion.state)&Button2Mask)
		{
			a=B_MIDDLE;
			b=B_DRAG;
#ifdef X_DEBUG	
			MESSAGE("middle button/drag\n");
#endif
		}
		if ((last_event.xmotion.state)&Button3Mask)
		{
			a=B_RIGHT;
			b=B_DRAG;
#ifdef X_DEBUG
			MESSAGE("right button/drag\n");
#endif
		}
		x_clip_number(&(last_event.xmotion.x),gd->size.x1,gd->size.x2);
		x_clip_number(&(last_event.xmotion.y),gd->size.y1,gd->size.y2);
		gd->mouse_handler(gd,last_event.xmotion.x,last_event.xmotion.y,a|b);
	}
	ret:;
#ifdef SC_DEBUG
	MESSAGE("x_process_event end\n");
#endif
}


/* returns pointer to string with driver parameter or NULL */
static unsigned char * x_get_driver_param(void)
{
	return x_driver_param;
}


/* initiate connection with X server */
static unsigned char * x_init_driver(unsigned char *param, unsigned char *display)
{
	XGCValues gcv;
	XSetWindowAttributes win_attr;
	XVisualInfo vinfo;
	int misordered=-1;

	n_wins=0;

#if defined(HAVE_SETLOCALE) && defined(LC_CTYPE)
	setlocale(LC_CTYPE,"");
#endif
#ifdef X_DEBUG
	{
		unsigned char txt[256];
		sprintf(txt,"x_init_driver(%s, %s)\n",param, display);
		MESSAGE(txt);
	}
#endif
	x_input_encoding=-1;
#if defined(HAVE_NL_LANGINFO) && defined(HAVE_LANGINFO_H) && defined(CODESET)
	{
		unsigned char *cp;
		cp=nl_langinfo(CODESET);
		x_input_encoding=get_cp_index(cp);
	}
#endif
	if (x_input_encoding<0)x_driver.flags|=GD_NEED_CODEPAGE;

	if (!display||!(*display))display=0;

/*
	X documentation says on XOpenDisplay(display_name) :

	display_name
		Specifies the hardware display name, which determines the dis-
		play and communications domain to be used.  On a POSIX-confor-
		mant system, if the display_name is NULL, it defaults to the
		value of the DISPLAY environment variable.

	But OS/2 has problems when display_name is NULL ...

*/
	if (!display)display=getenv("DISPLAY");
#ifndef __linux__
	/* on Linux, do not assume XWINDOW present if $DISPLAY is not set
	   --- rather open links on svgalib or framebuffer console */
	if (!display)display=":0.0";	/* needed for MacOS X */
#endif

	x_display=XOpenDisplay(display);
	if (!x_display)
	{
		unsigned char *err=init_str();
		int l=0;

		add_to_str(&err,&l,"Can't open display \"");
		add_to_str(&err,&l,display?display:(unsigned char *)"(null)");
		add_to_str(&err,&l,"\"\n");
		return err;
	}

	x_hash_table_init();

	x_bitmap_bit_order=BitmapBitOrder(x_display);
	x_fd=XConnectionNumber(x_display);
	x_screen=DefaultScreen(x_display);
	x_display_height=DisplayHeight(x_display,x_screen);
	x_display_width=DisplayWidth(x_display,x_screen);
	x_root_window=RootWindow(x_display,x_screen);

	x_default_window_width=x_display_width-50;
	x_default_window_height=x_display_height-50;

	x_driver_param=NULL;

	if (param)
	{
		unsigned char *p, *e, *f;
		int w,h;
		
		x_driver_param=stracpy(param);
		
		for (p=x_driver_param;(*p)&&(*p)!='x'&&(*p)!='X';p++)
			;
		if (!(*p))goto done;
		*p=0;
		w=strtoul(x_driver_param,(char **)(void *)&e,10);
		h=strtoul(p+1,(char **)(void *)&f,10);
		if (!(*e)&&!(*f)&&w&&h){x_default_window_width=w;x_default_window_height=h;}
		*p='x';
		done:;
	}

	/* find best visual */
	{
#define DEPTHS 5
#define CLASSES 2
		int depths[DEPTHS]={24, 16, 15, 8, 4};
		int classes[CLASSES]={TrueColor, PseudoColor}; /* FIXME: dodelat DirectColor */
		int a,b;
		
		for (a=0;a<DEPTHS;a++)
			for (b=0;b<CLASSES;b++)
			{
				if (XMatchVisualInfo(x_display, x_screen,depths[a],classes[b], &vinfo))
				{
					x_default_visual=vinfo.visual;
					x_depth=vinfo.depth;

					/* determine bytes per pixel */
					{
						XPixmapFormatValues *pfm;
						int n,i;
						
						pfm=XListPixmapFormats(x_display,&n);
						for (i=0;i<n;i++)
							if (pfm[i].depth==x_depth)
							{
								x_bitmap_bpp=pfm[i].bits_per_pixel<8?1:((pfm[i].bits_per_pixel)>>3);
								x_bitmap_scanline_pad=(pfm[i].scanline_pad)>>3;
								XFree(pfm);
								goto bytes_per_pixel_found;
							}
						if(n) XFree(pfm);
						continue;
					}
bytes_per_pixel_found:

					/* test misordered flag */
					switch(x_depth)
					{
						case 4:
						case 8:
						if (x_bitmap_bpp!=1)break;
						if (vinfo.red_mask>=vinfo.green_mask&&vinfo.green_mask>=vinfo.blue_mask)
						{
							misordered=0;
							goto visual_found;
						}
						break;

						case 15:
						case 16:
						if (x_bitmap_bpp!=2)break;
						if (x_bitmap_bit_order==MSBFirst&&vinfo.red_mask>vinfo.green_mask&&vinfo.green_mask>vinfo.blue_mask)
						{
							misordered=256;
							goto visual_found;
						}
						if (x_bitmap_bit_order==MSBFirst)break;
						if (vinfo.red_mask>vinfo.green_mask&&vinfo.green_mask>vinfo.blue_mask)
						{
							misordered=0;
							goto visual_found;
						}
						break;

						case 24:
						if (x_bitmap_bpp!=3&&x_bitmap_bpp!=4) break;
						if (vinfo.red_mask<vinfo.green_mask&&vinfo.green_mask<vinfo.blue_mask)
						{
							misordered=256;
							goto visual_found;
						}
						if (x_bitmap_bit_order==MSBFirst&&vinfo.red_mask>vinfo.green_mask&&vinfo.green_mask>vinfo.blue_mask)
						{
							misordered=512;
							goto visual_found;
						}
						if (vinfo.red_mask>vinfo.green_mask&&vinfo.green_mask>vinfo.blue_mask)
						{
							misordered=0;
							goto visual_found;
						}
						break;
					}
				}
			}
			
		x_free_hash_table();
		if (x_driver_param) mem_free(x_driver_param);
		return stracpy("No supported color depth found.\n");
visual_found:;
	}

	x_driver.depth=0;
	x_driver.depth|=x_bitmap_bpp;
	x_driver.depth|=x_depth<<3;
	x_driver.depth|=misordered;

	/* check if depth is sane */
	switch (x_driver.depth)
	{
		case 33:
		case 65:
		case 122:
		case 130:
		case 451:
		case 195:
		case 196:
		case 378:
		case 386:
		case 452:
		case 708:
/* 			printf("depth=%d visualid=%x\n",x_driver.depth, vinfo.visualid); */
		break;

		default:
		{
			unsigned char nevidim_te_ani_te_neslysim_ale_smrdis_jako_lejno[MAX_STR_LEN];

			snprintf(nevidim_te_ani_te_neslysim_ale_smrdis_jako_lejno,MAX_STR_LEN,
			"Unsupported graphics mode: x_depth=%d, bits_per_pixel=%d, bytes_per_pixel=%d\n",x_driver.depth, x_depth, x_bitmap_bpp);
			x_free_hash_table();
			if (x_driver_param) mem_free(x_driver_param);
			return stracpy(nevidim_te_ani_te_neslysim_ale_smrdis_jako_lejno);
		}
		
	}
	
	x_get_color_function=get_color_fn(x_driver.depth);
	if (!x_get_color_function) internal("Unknown bit depth: %d", x_driver.depth);

#ifdef X_DEBUG
	{
		unsigned char txt[256];
		sprintf(txt,"x_driver.depth=%d\n",x_driver.depth);
		MESSAGE(txt);
	}
#endif
	
	x_colors=1<<x_depth;
	x_have_palette=0;
	if (vinfo.class==DirectColor||vinfo.class==PseudoColor)
	{
		unsigned char *t;

		x_have_palette=1;
		if((t=x_set_palette())){x_free_hash_table(); if (x_driver_param) mem_free(x_driver_param); return t;}
	}

	x_black_pixel=BlackPixel(x_display,x_screen);
	x_white_pixel=WhitePixel(x_display,x_screen);

	gcv.function=GXcopy;
	gcv.graphics_exposures=True;  /* we want to receive GraphicsExpose events when uninitialized area is discovered during scroll */
	gcv.fill_style=FillSolid;
	gcv.background=x_black_pixel;

	set_handlers(x_fd,x_process_events,0,0,0);

	x_delete_window_atom=XInternAtom(x_display,"WM_DELETE_WINDOW", False);
	x_wm_protocols_atom=XInternAtom(x_display,"WM_PROTOCOLS", False);
	x_sel_atom= XInternAtom(x_display, "SEL_PROP", False);
	x_targets_atom= XInternAtom(x_display, "TARGETS", False);

	if (x_have_palette) win_attr.colormap=x_colormap;
	else win_attr.colormap=XCreateColormap(x_display, x_root_window, x_default_visual, AllocNone);

	win_attr.border_pixel=x_black_pixel;

	fake_window=XCreateWindow(
		x_display, 
		x_root_window,
		0,
		0,
		10,
		10,
		0,
		x_depth,
		CopyFromParent,
		x_default_visual,
		CWColormap|CWBorderPixel,
		&win_attr
	);

	x_normal_gc=XCreateGC(x_display,fake_window,GCFillStyle|GCBackground,&gcv);
	if (!x_normal_gc) {x_free_hash_table(); if (x_driver_param) mem_free(x_driver_param); return stracpy("Cannot create graphic context.\n");}
	
	x_copy_gc=XCreateGC(x_display,fake_window,GCFunction,&gcv);
	if (!x_copy_gc) {x_free_hash_table(); if (x_driver_param) mem_free(x_driver_param); return stracpy("Cannot create graphic context.\n");}
	
	x_drawbitmap_gc=XCreateGC(x_display,fake_window,GCFunction,&gcv);
	if (!x_drawbitmap_gc) {x_free_hash_table(); if (x_driver_param) mem_free(x_driver_param); return stracpy("Cannot create graphic context.\n");}
	
	x_scroll_gc=XCreateGC(x_display,fake_window,GCGraphicsExposures|GCBackground,&gcv);
	if (!x_scroll_gc) {x_free_hash_table(); if (x_driver_param) mem_free(x_driver_param); return stracpy("Cannot create graphic context.\n");}

	XSetLineAttributes(x_display,x_normal_gc,1,LineSolid,CapRound,JoinRound);


	XSync(x_display,False);
	X_SCHEDULE_PROCESS_EVENTS();
	return NULL;
}


/* close connection with the X server */
static void x_shutdown_driver(void)
{
#ifdef X_DEBUG
	MESSAGE("x_shutdown_driver\n");
#endif
	unregister_bottom_half(x_process_events,NULL);
	unregister_bottom_half(x_do_flush,NULL);
	XDestroyWindow(x_display,fake_window);
	XFreeGC(x_display,x_normal_gc);
	XFreeGC(x_display,x_copy_gc);
	XFreeGC(x_display,x_drawbitmap_gc);
	XFreeGC(x_display,x_scroll_gc);
	XCloseDisplay(x_display);
	x_free_hash_table();
	if (x_driver_param)mem_free(x_driver_param);
	x_clear_clipboard();
}


/* create new window */
static struct graphics_device* x_init_device(void)
{
	struct graphics_device *gd;
	Window *win;
	XWMHints wm_hints;
	XClassHint class_hints;
	XTextProperty windowName;
	char *links_name="Links";
	XSetWindowAttributes win_attr;

#ifdef X_DEBUG
	MESSAGE("x_init_device\n");
#endif
	gd=mem_alloc(sizeof(struct graphics_device));

	win=mem_alloc(sizeof(Window));

	gd->size.x1=0;
	gd->size.y1=0;
	gd->size.x2=x_default_window_width;
	gd->size.y2=x_default_window_height;
	
	if (x_have_palette) win_attr.colormap=x_colormap;
	else win_attr.colormap=XCreateColormap(x_display, x_root_window, x_default_visual, AllocNone);
	win_attr.border_pixel=x_black_pixel;

	*win=XCreateWindow(
		x_display, 
		x_root_window,
		gd->size.x1,
		gd->size.y1,
		gd->size.x2,
		gd->size.y2,
		X_BORDER_WIDTH,
		x_depth,
		InputOutput,
		x_default_visual,
		CWColormap|CWBorderPixel,
		&win_attr
	);
	if (!x_icon)
	{
		XImage *img;
		unsigned char *data;
		int w,h;
		get_links_icon(&data,&w,&h,x_driver.depth);

		img=XCreateImage(x_display,x_default_visual,x_depth,ZPixmap,0,0,w,h,x_bitmap_scanline_pad<<3,w*((x_driver.depth)&7));
		if (!img){x_icon=0;goto nic_nebude_bobankove;}
		img->data=data;
		x_icon=XCreatePixmap(x_display,*win,w,h,x_depth);
		if (!x_icon){XDestroyImage(img);x_icon=0;goto nic_nebude_bobankove;}

		XPutImage(x_display,x_icon,x_copy_gc,img,0,0,0,0,w,h);
		XDestroyImage(img);
nic_nebude_bobankove:;
	}
	
	wm_hints.flags=InputHint;
	wm_hints.input=True;
	if (x_icon)
	{	
		wm_hints.flags=InputHint|IconPixmapHint;
		wm_hints.icon_pixmap=x_icon;
	}

	XSetWMHints(x_display, *win, &wm_hints);
	class_hints.res_name = links_name;
	class_hints.res_class = links_name;
	XSetClassHint(x_display, *win, &class_hints);
	XStringListToTextProperty(&links_name, 1, &windowName);
	XSetWMName(x_display, *win, &windowName);
	XStoreName(x_display,*win,links_name);
	XSetWMIconName(x_display, *win, &windowName);

	XMapWindow(x_display,*win);
	
	gd->clip.x1=gd->size.x1;
	gd->clip.y1=gd->size.y1;
	gd->clip.x2=gd->size.x2;
	gd->clip.y2=gd->size.y2;
	gd->driver_data=win;
	gd->user_data=0;

	XSetWindowBackgroundPixmap(x_display, *win, None);
	if (x_have_palette) XSetWindowColormap(x_display,*win,x_colormap);
	if (x_add_to_table(gd)){mem_free(win);mem_free(gd);return NULL;}
	
	XSetWMProtocols(x_display,*win,&x_delete_window_atom,1);
	
	XSelectInput(x_display,*win,
		ExposureMask|
		KeyPressMask|
		ButtonPressMask|
		ButtonReleaseMask|
		PointerMotionMask|
		ButtonMotionMask|
		StructureNotifyMask|
		0
	);

	XSync(x_display,False);
	X_SCHEDULE_PROCESS_EVENTS();
	n_wins++;
	return gd;
}


/* close window */
static void x_shutdown_device(struct graphics_device *gd)
{
#ifdef X_DEBUG
	MESSAGE("x_shutdown_device\n");
#endif
	if (!gd)return;

	n_wins--;
	XDestroyWindow(x_display,*(Window*)(gd->driver_data));
	XSync(x_display,False);
	X_SCHEDULE_PROCESS_EVENTS();
	
	x_remove_from_table((Window*)(gd->driver_data));
	mem_free(gd->driver_data);
	mem_free(gd);
}

#if 0
/* n is in bytes. dest must begin on pixel boundary. If n is not a whole number
 * of pixels, rounding is performed downwards.
 */
static inline void pixel_set(unsigned char *dest, int n,void * pattern)
{
	int a;

	internal("ma to v sobe FIXME, tak jsem to zablokoval, aby to nikdo nepouzival");
	/* Originally there was vga_bytes here but this function is not
	 * used in planar modes so that it's OK :-) */
	switch(x_bitmap_bpp)
	{
		case 1:
		memset(dest,*(char *)pattern,n);
		break;

		case 2:
		{
			short v=*(short *)pattern;	/* !!! FIXME: nezavislost !!! */
			int a;
			
			for (a=0;a<(n>>1);a++) ((short *)dest)[a]=v;
		}
		break;

		case 3:
		{
			unsigned char a,b,c;
			int i;
			
			a=*(char*)pattern;
			b=((char*)pattern)[1];
			c=((char*)pattern)[2];
			i=n/3;
			for (i=n/3;i;i--){
				dest[0]=a;
				dest[1]=b;
				dest[2]=c;
				dest+=3;
			}
		}
		break;

		case 4:
		{
			long v=*(long *)pattern;	/* !!! FIXME: nezavislost !!! */
			int a;
			
			for (a=0;a<(n>>2);a++) ((long *)dest)[a]=v;
		}
		break;

		default:
		for (a=0;a<n/x_bitmap_bpp;a++,dest+=x_bitmap_bpp) memcpy(dest,pattern,x_bitmap_bpp);
		break;
	}
	
}

static int x_get_filled_bitmap(struct bitmap *bmp, long color)
{
	struct x_pixmapa *p;
	XImage *image;
	Pixmap *pixmap;
	int pad;

#ifdef X_DEBUG
	MESSAGE("x_get_filled_bitmap\n");
#endif
	if (!bmp||!bmp->x||!bmp->y)internal("x_get_filled_bitmap called with strange arguments.\n");

	p=mem_alloc(sizeof(struct x_pixmapa));

	bmp->flags=p;
	pad=x_bitmap_scanline_pad-((bmp->x*x_bitmap_bpp)%x_bitmap_scanline_pad);
	if (pad==x_bitmap_scanline_pad)pad=0;
	bmp->skip=bmp->x*x_bitmap_bpp+pad;

	pixmap=mem_alloc(sizeof(Pixmap));
	(*pixmap)=XCreatePixmap(x_display,fake_window,bmp->x,bmp->y,x_depth);
	if (!(*pixmap))
	{
		int a;
		unsigned char *ptr;
		int PerM_si_odalokoval_vlastni_pytlik=(bmp->x*x_bitmap_bpp);

		mem_free(pixmap);
		p->type=X_TYPE_IMAGE;
		retry:
		bmp->data=malloc(bmp->skip*bmp->y);
		if (!bmp->data) {
			out_of_memory("x bitmap malloc", bmp->skip*bmp->y);
			goto retry;
		}
		retry2:
		image=XCreateImage(x_display,x_default_visual,x_depth,ZPixmap,0,0,bmp->x,bmp->y,x_bitmap_scanline_pad<<3,bmp->skip);
		if (!image) {
			out_of_memory("XCreateImage", bmp->y * bmp->skip);
			goto retry2;
		}
		image->data=bmp->data;
		for (a=0,ptr=image->data;a<bmp->y;a++,ptr+=bmp->skip)
			pixel_set(ptr,PerM_si_odalokoval_vlastni_pytlik,(void*)color);
		p->data.image=image;
		return 0;
	}
	else
	{
		XSetForeground(x_display,x_normal_gc,color);
		XFillRectangle(
			x_display,
			*pixmap,
			x_normal_gc,
			0,
			0,
			bmp->x,
			bmp->y
		);
		p->type=X_TYPE_PIXMAP;
		p->data.pixmap=pixmap;
		return 2;
	}
}
#endif

static int x_get_empty_bitmap(struct bitmap *bmp)
{
	int pad;
#ifdef X_DEBUG
	MESSAGE("x_get_empty_bitmap\n");
#endif
	pad=x_bitmap_scanline_pad-((bmp->x*x_bitmap_bpp)%x_bitmap_scanline_pad);
	if (pad==x_bitmap_scanline_pad)pad=0;
	bmp->skip=bmp->x*x_bitmap_bpp+pad;
	bmp->flags=NULL;
	retry:
	if (!(bmp->data=malloc(bmp->skip*bmp->y))) {
		if (out_of_memory(NULL, 0))
			goto retry;
		return -1;
	}
	/* on error bmp->data should point to NULL */
	return 0;
}

static void x_register_bitmap(struct bitmap *bmp)
{
	struct x_pixmapa *p;
	XImage *image;
	Pixmap *pixmap;
	int can_create_pixmap;
	
#ifdef X_DEBUG
	MESSAGE("x_register_bitmap\n");
#endif
	
	X_FLUSH();
	if (!bmp->data||!bmp->x||!bmp->y) goto cant_create;

	/* alloc struct x_bitmapa */
	p=mem_alloc(sizeof(struct x_pixmapa));

	/* alloc XImage in client's memory */
	retry:
	image=XCreateImage(x_display,x_default_visual,x_depth,ZPixmap,0,0,bmp->x,bmp->y,x_bitmap_scanline_pad<<3,bmp->skip);
	if (!image){
		if (out_of_memory(NULL, 0))
			goto retry;
		mem_free(p);
		goto cant_create;
	}
	image->data=bmp->data;
	

	/* try to alloc XPixmap in server's memory */
	can_create_pixmap=1;
	x_prepare_for_failure();
	pixmap=mem_alloc(sizeof(Pixmap));
	(*pixmap)=XCreatePixmap(x_display,fake_window,bmp->x,bmp->y,x_depth);
	if (x_test_for_failure()) {
		if (*pixmap) {
			x_prepare_for_failure();
			XFreePixmap(x_display,*pixmap);
			x_test_for_failure();
			*pixmap=0;
		}
	}
	if (!(*pixmap)){mem_free(pixmap);can_create_pixmap=0;}


	if (can_create_pixmap)
	{
#ifdef X_DEBUG
		MESSAGE("x_register_bitmap: creating pixmap\n");
#endif
		XPutImage(x_display,*pixmap,x_copy_gc,image,0,0,0,0,bmp->x,bmp->y);
		XDestroyImage(image);
		p->type=X_TYPE_PIXMAP;
		p->data.pixmap=pixmap;
	}
	else
	{
#ifdef X_DEBUG
		MESSAGE("x_register_bitmap: creating image\n");
#endif
		p->type=X_TYPE_IMAGE;
		p->data.image=image;
	}
	bmp->flags=p;
	bmp->data=NULL;
	return;

cant_create:
	if (bmp->data) free(bmp->data), bmp->data = NULL;
	bmp->flags=NULL;
	return;
}


static void x_unregister_bitmap(struct bitmap *bmp)
{
#ifdef X_DEBUG
	MESSAGE("x_unregister_bitmap\n");
#endif
	if (!bmp->flags) return;

	switch(XPIXMAPP(bmp->flags)->type)
	{
		case X_TYPE_PIXMAP:
		XFreePixmap(x_display,*(XPIXMAPP(bmp->flags)->data.pixmap));   /* free XPixmap from server's memory */
		mem_free(XPIXMAPP(bmp->flags)->data.pixmap);  /* XPixmap */
		break;

		case X_TYPE_IMAGE:
		XDestroyImage(XPIXMAPP(bmp->flags)->data.image);  /* free XImage from client's memory */
		break;
	}
	mem_free(bmp->flags);  /* struct x_pixmap */
}

/* prekonvertuje long z host do xserver byte order */
static inline long byte_order_host_to_server(long num)
{
	/* endianity test */
	int big=(htonl(0x12345678L)==0x12345678L);

	/* nedelam to makrama, protoze tohle chodi i pri cross-kompilaci */

	switch(x_bitmap_bit_order)
	{
		case LSBFirst:
		if (big)
		{
			/* je to sice VODPORNE POMALY, ale prenositelny */
			unsigned char bla[sizeof(long)];
			int a;
			for (a=0;a<(int)sizeof(long);a++) bla[a]=num&255,num>>=8;
			num=0;
			for (a=0;a<(int)sizeof(long);a++) num<<=8,num+=bla[a];
			return num;
			
		}
		return num;

		case MSBFirst:
		if (big)return num;
		return htonl(num);

		default: internal("Unknown endianity got from xlib.\n");
	}
	return 0;	/* aby GCC melo radost */
}

static long x_get_color(int rgb)
{
	long block;
	
#ifdef X_DEBUG
	MESSAGE("x_get_color\n");
#endif
	block=x_get_color_function(rgb);
	if (x_bitmap_bpp==1)return block;
	return byte_order_host_to_server(block);
}


static void x_fill_area(struct graphics_device *dev, int x1, int y1, int x2, int y2, long color)
{
	/*int a;*/
	
#ifdef X_DEBUG
	{
		unsigned char txt[256];
		sprintf(txt,"x_fill_area (x1=%d y1=%d x2=%d y2=%d)\n",x1,y1,x2,y2);
		MESSAGE(txt);
	}
#endif
	/* Mikulas: v takovem pripade radsi neplnit nic ... */
	/*
	if (x1>x2){a=x2;x2=x1;x1=a;}
	if (y1>y2){a=y2;y2=y1;y1=a;}
	*/
	if (x1 < dev->clip.x1) x1 = dev->clip.x1;
	if (x2 > dev->clip.x2) x2 = dev->clip.x2;
	if (y1 < dev->clip.y1) y1 = dev->clip.y1;
	if (y2 > dev->clip.y2) y2 = dev->clip.y2;
	if (x1>=x2) return;
	if (y1>=y2) return;
	
	XSetForeground(x_display,x_normal_gc,color);
	XFillRectangle(
		x_display,
		*((Window*)(dev->driver_data)),
		x_normal_gc,
		x1,
		y1,
		x2-x1,
		y2-y1
	);
	X_FLUSH();
}
   

static void x_draw_hline(struct graphics_device *dev, int left, int y, int right, long color)
{
#ifdef X_DEBUG
	MESSAGE("x_draw_hline\n");
#endif
	if (left>=right)return;
	if ((y>=dev->clip.y2)||(y<dev->clip.y1)) return;
	if (right<=dev->clip.x1||left>=dev->clip.x2)return;
	XSetForeground(x_display,x_normal_gc,color);
	XDrawLine(
		x_display,
		*((Window*)(dev->driver_data)),
		x_normal_gc,
		left,y,right-1,y
	);
	X_FLUSH();
}


static void x_draw_vline(struct graphics_device *dev, int x, int top, int bottom, long color)
{
#ifdef X_DEBUG
	MESSAGE("x_draw_vline\n");
#endif
	if (top>=bottom)return;
	if ((x>=dev->clip.x2)||(x<dev->clip.x1)) return;
	if (bottom<=dev->clip.y1||top>=dev->clip.y2)return;
	XSetForeground(x_display,x_normal_gc,color);
	XDrawLine(
		x_display,
		*((Window*)(dev->driver_data)),
		x_normal_gc,
		x,top,x,bottom-1
	);
	X_FLUSH();
}


static void x_set_clip_area(struct graphics_device *dev, struct rect *r)
{
	XRectangle xr;
	
#ifdef X_DEBUG
	{
		unsigned char txt[512];
		snprintf(txt,512,"x_set_clip_area(x1=%d, y1=%d, x2=%d, y2=%d\n",r->x1,r->y1,r->x2,r->y2);
		MESSAGE(txt);
	}
#endif
	dev->clip.x1=r->x1;
	dev->clip.x2=r->x2;
	dev->clip.y1=r->y1;
	dev->clip.y2=r->y2;
	
	xr.x=r->x1;
	xr.y=r->y1;
	if (r->x2<r->x1)xr.width=0;
	else xr.width=(r->x2)-(r->x1);
	if (r->y2<r->y1)xr.height=0;
	else xr.height=(r->y2)-(r->y1);

	XSetClipRectangles(x_display,x_normal_gc,0,0,&xr,1,Unsorted);
	XSetClipRectangles(x_display,x_scroll_gc,0,0,&xr,1,Unsorted);
	XSetClipRectangles(x_display,x_drawbitmap_gc,0,0,&xr,1,Unsorted);
	X_FLUSH();
}


static void x_draw_bitmap(struct graphics_device *dev, struct bitmap *bmp, int x, int y)
{
	int bmp_off_x, bmp_off_y, bmp_size_x, bmp_size_y;
#ifdef X_DEBUG
	MESSAGE("x_draw_bitmap\n");
#endif
	if (!(bmp->flags)||!bmp->x||!bmp->y) {
		return;
	}
	if ((x>=dev->clip.x2)||(y>=dev->clip.y2)) return;
	if ((x+(bmp->x)<=dev->clip.x1)||(y+(bmp->y)<=dev->clip.y1)) return;
	bmp_off_x = 0;
	bmp_off_y = 0;
	bmp_size_x = bmp->x;
	bmp_size_y = bmp->y;
	if (x < dev->clip.x1) {
		bmp_off_x = dev->clip.x1 - x;
		bmp_size_x -= dev->clip.x1 - x;
		x = dev->clip.x1;
	}
	if (x + bmp_size_x > dev->clip.x2) {
		bmp_size_x = dev->clip.x2 - x;
	}
	if (y < dev->clip.y1) {
		bmp_off_y = dev->clip.y1 - y;
		bmp_size_y -= dev->clip.y1 - y;
		y = dev->clip.y1;
	}
	if (y + bmp_size_y > dev->clip.y2) {
		bmp_size_y = dev->clip.y2 - y;
	}

	switch(XPIXMAPP(bmp->flags)->type)
	{
		case X_TYPE_PIXMAP:
		XCopyArea(x_display,*(XPIXMAPP(bmp->flags)->data.pixmap),*((Window*)(dev->driver_data)),x_drawbitmap_gc,bmp_off_x,bmp_off_y,bmp_size_x,bmp_size_y,x,y);
		break;

		case X_TYPE_IMAGE:
		XPutImage(x_display,*((Window *)(dev->driver_data)),x_drawbitmap_gc,XPIXMAPP(bmp->flags)->data.image,bmp_off_x,bmp_off_y,x,y,bmp_size_x,bmp_size_y);
		break;
	}
	X_FLUSH();
}


#if 0
static void x_draw_bitmaps(struct graphics_device *dev, struct bitmap **bmps, int n, int x, int y)
{
	int a;
#ifdef X_DEBUG
	MESSAGE("x_draw_bitmaps\n");
#endif

	if (!bmps)return;
	for (a=0;a<n;a++)
	{
		x_draw_bitmap(dev,bmps[a],x,y);
		x+=(bmps[a])->x;
	}
}
#endif



static int x_hscroll(struct graphics_device *dev, struct rect_set **set, int sc)
{
	XEvent ev;
	struct rect r;

	*set=NULL;
	if (!sc)return 0;
	*set=init_rect_set();
	if (!(*set))internal("Cannot allocate memory for rect set in scroll function.\n");

	XCopyArea(
		x_display,
		*((Window*)(dev->driver_data)),
		*((Window*)(dev->driver_data)),
		x_scroll_gc,
		dev->clip.x1,dev->clip.y1,
		dev->clip.x2-dev->clip.x1,dev->clip.y2-dev->clip.y1,
		dev->clip.x1+sc,dev->clip.y1
	);
	XSync(x_display,False);
	/* ten sync tady musi byt, protoze potrebuju zarucit, aby vsechny
	 * graphics-expose vyvolane timto scrollem byly vraceny v rect-set */

	/* take all graphics expose events for this window and put them into the rect set */
	while (XCheckWindowEvent(x_display,*((Window*)(dev->driver_data)),ExposureMask,&ev)==True)
	{
		switch(ev.type)
		{
			case GraphicsExpose:
			r.x1=ev.xgraphicsexpose.x;
			r.y1=ev.xgraphicsexpose.y;
			r.x2=ev.xgraphicsexpose.x+ev.xgraphicsexpose.width;
			r.y2=ev.xgraphicsexpose.y+ev.xgraphicsexpose.height;
			break;

			case Expose:
			r.x1=ev.xexpose.x;
			r.y1=ev.xexpose.y;
			r.x2=ev.xexpose.x+ev.xexpose.width;
			r.y2=ev.xexpose.y+ev.xexpose.height;
			break;

			default:
			continue;
		}
		if (r.x1 < dev->clip.x1 || r.x2 > dev->clip.x2 ||
		    r.y1 < dev->clip.y1 || r.y2 > dev->clip.y2) {
			switch(ev.type)
			{
				case GraphicsExpose:
				ev.xgraphicsexpose.x = 0;
				ev.xgraphicsexpose.y = 0;
				ev.xgraphicsexpose.width = dev->size.x2;
				ev.xgraphicsexpose.height = dev->size.y2;
				break;

				case Expose:
				ev.xexpose.x = 0;
				ev.xexpose.y = 0;
				ev.xexpose.width = dev->size.x2;
				ev.xexpose.height = dev->size.y2;
				break;
			}
			XPutBackEvent(x_display, &ev);
			mem_free(*set);
			*set = NULL;
			break;
		}
		add_to_rect_set(set,&r);
	}

	X_SCHEDULE_PROCESS_EVENTS();

#ifdef SC_DEBUG
	MESSAGE("hscroll\n");
#endif

	return 1;
}


static int x_vscroll(struct graphics_device *dev, struct rect_set **set, int sc)
{
	XEvent ev;
	struct rect r;

	*set=NULL;
	if (!sc)return 0;
	*set=init_rect_set();
	if (!(*set))internal("Cannot allocate memory for rect set in scroll function.\n");

	XCopyArea(
		x_display,
		*((Window*)(dev->driver_data)),
		*((Window*)(dev->driver_data)),
		x_scroll_gc,
		dev->clip.x1,dev->clip.y1,
		dev->clip.x2-dev->clip.x1,dev->clip.y2-dev->clip.y1,
		dev->clip.x1,dev->clip.y1+sc
	);
	XSync(x_display,False);
	/* ten sync tady musi byt, protoze potrebuju zarucit, aby vsechny
	 * graphics-expose vyvolane timto scrollem byly vraceny v rect-set */

	/* take all graphics expose events for this window and put them into the rect set */
	while (XCheckWindowEvent(x_display,*((Window*)(dev->driver_data)),ExposureMask,&ev)==True)
	{
		switch(ev.type)
		{
			case GraphicsExpose:
			r.x1=ev.xgraphicsexpose.x;
			r.y1=ev.xgraphicsexpose.y;
			r.x2=ev.xgraphicsexpose.x+ev.xgraphicsexpose.width;
			r.y2=ev.xgraphicsexpose.y+ev.xgraphicsexpose.height;
			break;

			case Expose:
			r.x1=ev.xexpose.x;
			r.y1=ev.xexpose.y;
			r.x2=ev.xexpose.x+ev.xexpose.width;
			r.y2=ev.xexpose.y+ev.xexpose.height;
			break;

			default:
			continue;
		}
		if (r.x1 < dev->clip.x1 || r.x2 > dev->clip.x2 ||
		    r.y1 < dev->clip.y1 || r.y2 > dev->clip.y2) {
			switch(ev.type)
			{
				case GraphicsExpose:
				ev.xgraphicsexpose.x = 0;
				ev.xgraphicsexpose.y = 0;
				ev.xgraphicsexpose.width = dev->size.x2;
				ev.xgraphicsexpose.height = dev->size.y2;
				break;

				case Expose:
				ev.xexpose.x = 0;
				ev.xexpose.y = 0;
				ev.xexpose.width = dev->size.x2;
				ev.xexpose.height = dev->size.y2;
				break;
			}
			XPutBackEvent(x_display, &ev);
			mem_free(*set);
			*set = NULL;
			break;
		}
		add_to_rect_set(set,&r);
	}

	X_SCHEDULE_PROCESS_EVENTS();

#ifdef SC_DEBUG
	MESSAGE("vscroll\n");
#endif

	return 1;
}


static void *x_prepare_strip(struct bitmap *bmp, int top, int lines)
{
	struct x_pixmapa *p=(struct x_pixmapa *)bmp->flags;
	XImage *image;
	void *x_data;

	if (!p) return NULL;

#ifdef DEBUG
	if (lines <= 0) internal("x_prepare_strip: %d lines",lines);
#endif

#ifdef X_DEBUG
	MESSAGE("x_prepare_strip\n");
#endif

	bmp->data = NULL;
	
	switch (p->type)
	{
		case X_TYPE_PIXMAP:

		retry:
		x_data=malloc(bmp->skip*lines);
		if (!x_data) {
			if (out_of_memory(NULL, 0))
				goto retry;
			return NULL;
		}

		retry2:
		image=XCreateImage(x_display,x_default_visual,x_depth,ZPixmap,0,0,bmp->x,lines,x_bitmap_scanline_pad<<3,bmp->skip);
		if (!image) {
			if (out_of_memory(NULL, 0))
				goto retry2;
			free(x_data);
			return NULL;
		}
		image->data = x_data;
		bmp->data=image;
		return image->data;

		case X_TYPE_IMAGE:
		return p->data.image->data+(bmp->skip*top);
	}
	internal("Unknown pixmap type found in x_prepare_strip. SOMETHING IS REALLY STRANGE!!!!\n");
	fatal_tty_exit();
	exit(RET_FATAL);	/* never called */
}


static void x_commit_strip(struct bitmap *bmp, int top, int lines)
{
	struct x_pixmapa *p=(struct x_pixmapa *)bmp->flags;

	if (!p) return;

#ifdef X_DEBUG
	MESSAGE("x_commit_strip\n");
#endif
	switch(p->type)
	{
		/* send image to pixmap in xserver */
		case X_TYPE_PIXMAP:
		if (!bmp->data) return;
		XPutImage(x_display,*(XPIXMAPP(bmp->flags)->data.pixmap),x_copy_gc,(XImage*)bmp->data,0,0,0,top,bmp->x,lines);
		XDestroyImage((XImage *)bmp->data);
		return;

		case X_TYPE_IMAGE:
		/* everything has been done by user */
		return;
	}
}


static void x_set_window_title(struct graphics_device *gd, unsigned char *title)
{
	struct conv_table *ct = get_translation_table(utf8_table,x_input_encoding >= 0?x_input_encoding:0);
	unsigned char *t;
	XTextProperty windowName;

	if (!gd)internal("x_set_window_title called with NULL graphics_device pointer.\n");
	t = convert_string(ct, title, strlen(title), NULL);
	clr_white(t);
	XStoreName(x_display,*(Window*)(gd->driver_data),t);

	XStringListToTextProperty((char**)(void *)(&t), 1, &windowName);
	XSetWMName(x_display, *(Window*)(gd->driver_data), &windowName);
	XSetWMIconName(x_display, *(Window*)(gd->driver_data), &windowName);
	XSync(x_display,False);
	X_SCHEDULE_PROCESS_EVENTS();
	mem_free(t);
}

/* gets string in UTF8 */
static void x_set_clipboard_text(struct graphics_device *gd, unsigned char * text)
{
	x_clear_clipboard();
	if(text && strlen(text))
	{
		struct conv_table *ct;
		
		ct=get_translation_table(utf8_table,x_input_encoding < 0 ? drv->codepage : x_input_encoding);
		x_my_clipboard=convert_string(ct, text, strlen(text), NULL);

		XSetSelectionOwner (x_display, XA_PRIMARY, *(Window*)(gd->driver_data), CurrentTime);
		XFlush (x_display);
		X_SCHEDULE_PROCESS_EVENTS();
	}
}

static void selection_request(XEvent *event)
{
	XSelectionRequestEvent *req;
	XSelectionEvent sel;

	req = &(event->xselectionrequest);
	sel.type = SelectionNotify;
	sel.requestor = req->requestor;
	sel.selection = XA_PRIMARY;
	sel.target = req->target;
	sel.property = req->property;
	sel.time = req->time;
	sel.display = req->display;
	#ifdef X_DEBUG
	{
	unsigned char txt[256];
	sprintf (txt,"xselectionrequest from %i\n",(int)event.xselection.requestor);
	MESSAGE(txt);
	sprintf (txt,"property:%i target:%i selection:%i\n", req->property,req->target, req->selection);
	MESSAGE(txt);
	}
	#endif
	if (req->target == XA_STRING) {
	XChangeProperty (x_display,
			 sel.requestor,
			 sel.property,
			 XA_STRING,
			 8,
			 PropModeReplace,
			 x_my_clipboard,
			 x_my_clipboard?strlen(x_my_clipboard):0
	);
	}
	else if (req->target == x_targets_atom) {
	unsigned tgt_atoms[2] /*= {x_targets_atom, XA_STRING }*/;
	tgt_atoms[0] = x_targets_atom;
	tgt_atoms[1] = XA_STRING;
	XChangeProperty (x_display,
			 sel.requestor,
			 sel.property,
			 XA_ATOM,
			 32,
			 PropModeReplace,
			 (char*)&tgt_atoms,
			 2
	);
	}
	else {
	#ifdef X_DEBUG
	{
	    unsigned char txt[256];
	    sprintf (txt,"Non-String wanted: %i\n",(int)req->target);
	    MESSAGE(txt);
	}
	#endif
	sel.property= None;
	}
	XSendEvent (x_display, sel.requestor,0,0,(XEvent*)&sel);
	XFlush (x_display);                    
	X_SCHEDULE_PROCESS_EVENTS();
}

static unsigned char *x_get_clipboard_text(void)
{
	struct conv_table *ct;
	XEvent event;

	XConvertSelection(x_display, XA_PRIMARY, XA_STRING, x_sel_atom, fake_window, CurrentTime);

	while (1) {
		XSync(x_display, False);
		if (XCheckTypedEvent(x_display,SelectionRequest, &event)) {
			selection_request(&event);
			continue;
		}
		if (XCheckTypedEvent(x_display,SelectionNotify, &event)) break;
		x_wait_for_event();
	}
	if (event.xselection.property)
	{
		unsigned char *buffer;
		unsigned long pty_size, pty_items;
		int pty_format, ret;
		Atom pty_type;

		if (event.xselection.target != XA_STRING) goto no_new_sel;
		if (event.xselection.property != x_sel_atom) goto no_new_sel;

		
		/* Get size and type of property */
		ret = XGetWindowProperty(
			x_display,
			fake_window,
			event.xselection.property,
			0,
			0,
			False,
			AnyPropertyType,
			&pty_type,
			&pty_format,
			&pty_items,
			&pty_size,
			&buffer);
		if (ret != Success) goto no_new_sel;
		XFree(buffer);

		ret = XGetWindowProperty(
			x_display,
			fake_window,
			event.xselection.property,
			0,
			(long)pty_size,
			True,
			AnyPropertyType,
			&pty_type,
			&pty_format,
			&pty_items,
			&pty_size,
			&buffer
		);
		if(ret != Success) goto no_new_sel;

		pty_size = (pty_format>>3) * pty_items;

		x_clear_clipboard();
		x_my_clipboard=stracpy(buffer);
		XFree(buffer);
	}

no_new_sel:
	X_SCHEDULE_PROCESS_EVENTS();
	if (!x_my_clipboard) return NULL;
	
	ct=get_translation_table(x_input_encoding < 0 ? drv->codepage : x_input_encoding,utf8_table);
	return convert_string(ct, x_my_clipboard, strlen(x_my_clipboard), NULL);
}


static int x_exec(unsigned char *command, int fg)
{
	unsigned char *run;
	int retval;

	if (!fg) return system(command);

	run=subst_file(*x_driver.shell?x_driver.shell:(unsigned char *)"xterm -e %",command, 0);
	retval=system(run);
	mem_free(run);
	return retval;
}

struct graphics_driver x_driver={
	"x",
	x_init_driver,
	x_init_device,
	x_shutdown_device,
	x_shutdown_driver,
	x_get_driver_param,
	x_get_empty_bitmap,
	/*x_get_filled_bitmap,*/
	x_register_bitmap,
	x_prepare_strip,
	x_commit_strip,
	x_unregister_bitmap,
	x_draw_bitmap,
	/*x_draw_bitmaps,*/
	x_get_color,
	x_fill_area,
	x_draw_hline,
	x_draw_vline,
	x_hscroll,
	x_vscroll,
	x_set_clip_area,
	dummy_block,
	dummy_unblock,
	x_set_window_title,
	x_exec,
	x_set_clipboard_text,
	x_get_clipboard_text,
	0,				/* depth (filled in x_init_driver function) */
	0, 0,				/* size (in X is empty) */
	0,				/* flags */
	0,				/* codepage */
	NULL,				/* shell */
};

#endif /* GRDRV_X */
