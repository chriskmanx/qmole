#include "links.h"

static void* block_new_item(void* ignore);
static void block_delete_item(void* data);
static void block_copy_item(void *in, void *out);
static unsigned char *block_type_item(struct terminal *term, void *data, int x);
static void block_edit_item(struct dialog_data *dlg, void *data, void (*ok_fn)(struct dialog_data *, void *, void *, struct list_description *), void *ok_arg, unsigned char dlg_title);
static void *block_find_item(void *start, unsigned char *str, int direction);

static struct history block_search_histroy = { 0, {&block_search_histroy.items, &block_search_histroy.items} };

struct list blocks = { &blocks, &blocks, 0, -1, NULL};
static struct list_description blocks_ld={
	0, /*flat*/
	&blocks,	/*list head*/
	block_new_item, /*ext_new_item,*/
	block_edit_item, /*ext_edit_item,*/
	assoc_default_value, /*assoc_default_value,*/
	block_delete_item, /*ext_delete_item,*/
	block_copy_item, /*ext_copy_item,*/
	block_type_item, /*ext_type_item,*/
	block_find_item, /*ext_find_item,*/
	&block_search_histroy, /*&ext_search_history,*/
	0,		/* this is set in init_assoc function */
	15,  /* # of items in main window */
	T_BLOCK_LIST, /*item title*/
	T_BLOCK_LIST_IN_USE, /*Already open message*/
	T_BLOCK_LIST_MANAGER, /*Window title*/
	T_BLOCK_DELETE,
	0,	/* no button */
	NULL,	/* no button */
	NULL,	/* no save */

	NULL,NULL,0,0,  /* internal vars */
	0, /* modified */
	NULL,
	NULL,
	0,
};


static void* block_new_item(void* ignore)
{
	/*Default constructor*/
	struct block *new; 

	if (!(new = mem_alloc(sizeof(struct block)))) return NULL;
	new->url = stracpy("");
	return new;
}

static void block_delete_item(void* data)
{
	/*Destructor */
	struct block *del=(struct block *)data;
	struct block *next=del->next;
	struct block *prev=del->prev;

	if (del->url)mem_free(del->url);

	if (next)next->prev=del->prev;
	if (prev)prev->next=del->next;
	mem_free(del);
}

static void block_copy_item(void *in, void *out)
{
	/*Copy construction */
	struct block *item_in=(struct block *)in;
	struct block *item_out=(struct block *)out;

	if (item_out->url)mem_free(item_out->url);
	item_out->url=stracpy(item_in->url);
}

/*This is used to display the items in the menu*/
static unsigned char *block_type_item(struct terminal *term, void *data, int x)
{
	unsigned char *txt, *txt1;
	struct conv_table *table;
	struct block* item=(struct block*)data;

	if ((struct list*)item==(&blocks)) return stracpy(_(TEXT_(T_BLOCK_LIST),term));
	txt=stracpy(item->url);
	

	/*I have no idea what this does, but it os copied from working code in types.c*/
	table=get_translation_table(blocks_ld.codepage,term->spec->charset);
	txt1=convert_string(table,txt,strlen(txt),NULL);
	mem_free(txt);
			
	return txt1;
}

struct assoc_ok_struct{
	void (*fn)(struct dialog_data *,void *,void *,struct list_description *);
	void *data;	
	struct dialog_data *dlg;
};

/* destroys an item, this function is called when edit window is aborted */
static void block_edit_abort(struct dialog_data *data)
{
	struct block *item=(struct block*)data->dlg->udata;
	struct dialog *dlg=data->dlg;

	mem_free(dlg->udata2);
	if (item)block_delete_item(item);
}

/* Puts url into the block list */
static void block_edit_done(void *data)
{
	/*Copied from types.c*/
	struct dialog *d=(struct dialog*)data;
	struct block *item=(struct block *)d->udata;
	struct assoc_ok_struct* s=(struct assoc_ok_struct*)d->udata2;
	unsigned char *txt;
	struct conv_table *table;
	unsigned char *url;

	/*See block_edit_item*/
	url=(unsigned char *)&d->items[4];
	
	table=get_translation_table(s->dlg->win->term->spec->charset,blocks_ld.codepage);
	txt=convert_string(table,url,strlen(url),NULL);
	mem_free(item->url); item->url=txt;

	s->fn(s->dlg,s->data,item,&blocks_ld);
	d->udata=0;  /* for abort function */
}

static void block_edit_item_fn(struct dialog_data *dlg)
{	
	/*Copied from input_field. I don't know how most of it works.*/
#define LL gf_val(1, G_BFU_FONT_SIZE)
	struct terminal *term = dlg->win->term;
	int max = 0, min = 0;
	int w, rw;
	int y = gf_val(-1, -G_BFU_FONT_SIZE);


	if (dlg->win->term->spec->braille) y += gf_val(1, G_BFU_FONT_SIZE);
	max_text_width(term, dlg->dlg->udata, &max, AL_LEFT);
	min_text_width(term, dlg->dlg->udata, &min, AL_LEFT);
	max_buttons_width(term, dlg->items + 1, 2, &max);
	min_buttons_width(term, dlg->items + 1, 2, &min);
	if (max < dlg->dlg->items->dlen) max = dlg->dlg->items->dlen;
	w = term->x * 9 / 10 - 2 * DIALOG_LB;
	if (w > max) w = max;
	if (w < min) w = min;
	rw = w;
	dlg_format_text_and_field(dlg, NULL, dlg->dlg->udata, dlg->items, 0, &y, w, &rw, COLOR_DIALOG_TEXT, AL_LEFT);
	y += LL;
	dlg_format_buttons(dlg, NULL, dlg->items + 1, 2, 0, &y, w, &rw, AL_CENTER);
	w = rw;
	dlg->xw = rw + 2 * DIALOG_LB;
	dlg->yw = y + 2 * DIALOG_TB;
	center_dlg(dlg);
	draw_dlg(dlg);
	y = dlg->y + DIALOG_TB;
	if (dlg->win->term->spec->braille) y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_text_and_field(dlg, term, dlg->dlg->udata, dlg->items, dlg->x + DIALOG_LB, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	y += LL;
	dlg_format_buttons(dlg, term, dlg->items + 1, 2, dlg->x + DIALOG_LB, &y, w, NULL, AL_CENTER);
}


static void block_edit_item(struct dialog_data *dlg, void *data, void (*ok_fn)(struct dialog_data *, void *, void *, struct list_description *), void *ok_arg, unsigned char dlg_title)
{
	/*Copied from types.c */
	/*Data is a new item generated by the "default" function*/
	struct block *new=(struct block*)data;

	struct terminal *term=dlg->win->term;
	struct dialog *d;
	struct assoc_ok_struct *s;
	unsigned char *url;


	/*Allocate space for dialog, 4 items followed by 1 string*/
	if (!(d = mem_alloc(sizeof(struct dialog) + 4 * sizeof(struct dialog_item) + 1 * MAX_STR_LEN))) return;
	memset(d, 0, sizeof(struct dialog) + 4 * sizeof(struct dialog_item) + 1 * MAX_STR_LEN);

	/*Set up this string */
	url=(unsigned char *)&d->items[4];
	if (new->url)safe_strncpy(url,new->url,MAX_STR_LEN);
	
	/* Create the dialog */
	if (!(s=mem_alloc(sizeof(struct assoc_ok_struct))))
	{
		mem_free(d);
		return;
	}

	s->fn=ok_fn;
	s->data=ok_arg;
	s->dlg=dlg;
		
	switch (dlg_title)
	{
		case TITLE_EDIT:
		d->title=TEXT_(T_BLOCK_EDIT);
		break;

		case TITLE_ADD:
		d->title=TEXT_(T_BLOCK_ADD);
		break;

		default:
		internal("Unsupported dialog title.\n");
	}

	d->udata=data;
	d->udata2=s;
	d->fn = block_edit_item_fn;
	d->abort=block_edit_abort;
	d->refresh=block_edit_done;
	d->refresh_data = d;
	d->items[0].type = D_FIELD;
	d->items[0].dlen = MAX_STR_LEN;
	d->items[0].data = url;
	d->items[0].fn = check_nonempty;
	d->items[1].type = D_BUTTON;
	d->items[1].gid = B_ENTER;
	d->items[1].fn = ok_dialog;
	d->items[1].text = TEXT_(T_OK);
	d->items[2].type = D_BUTTON;
	d->items[2].gid = B_ESC;
	d->items[2].text = TEXT_(T_CANCEL);
	d->items[2].fn = cancel_dialog;
	d->items[3].type = D_END;
	do_dialog(term, d, getml(d, NULL));
}

static void *block_find_item(void *start, unsigned char *str, int direction)
{
	struct block *e,*s=start;

	if (direction==1)
	{
		for (e=s->next; e!=s; e=e->next)
			if (e->depth>-1)
			{
				if (e->url && casestrstr(e->url,str)) return e;
			}
	}
	else
	{
		for (e=s->prev; e!=s; e=e->prev)
			if (e->depth>-1)
			{
				if (e->url && casestrstr(e->url,str)) return e;
			}
	}
	
	if (e==s&&e->depth>-1&&e->url && casestrstr(e->url,str)) return e;

	return NULL;
}


void block_manager(struct terminal *term,void *fcp,struct session *ses)
{
	create_list_window(&blocks_ld,&blocks,term,ses);
}


void* block_add_URL_fn(void* garbage, unsigned char* url)
{
	/*Callback from the dialog box created from the link menu*/
	struct block* new_b, *here;

	new_b = block_new_item(0);

	if(!new_b)
		return 0;
	if(new_b->url) 
		mem_free(new_b->url);
	
	new_b->url = stracpy(url);
	new_b->type = 0;
	
	here = (struct block*)next_in_tree(&blocks_ld, &blocks);

	add_at_pos(here, new_b);
	return 0;
}

void block_add_URL(struct terminal *term, void *xxx, struct session *ses)
{
	/*"Block Image" menu item calls this function*/
	unsigned char *u;
	struct f_data_c *fd = current_frame(ses);

	if (!fd) return;
	if (fd->vs->current_link == -1) return;
	if (!(u = fd->f_data->links[fd->vs->current_link].where_img)) return;


	input_field(ses->term, NULL, TEXT_(T_BLOCK_URL) , TEXT_(T_BLOCK_ADD), ses, 0, MAX_INPUT_URL_LEN, u, 0, 0, NULL, TEXT_(T_OK), (void (*)(void *, unsigned char *)) block_add_URL_fn, TEXT_(T_CANCEL), NULL, NULL);

}

static unsigned char *find_first_match(unsigned char *s, unsigned char *p, unsigned *ii)
{
	unsigned i;
	retry:
	for (i = 0; s[i] && p[i] && p[i] != '*'; i++) {
		if (s[i] != p[i] && p[i] != '?') {
			s++;
			goto retry;
		}
	}
	*ii = i;
	if (!p[i] || p[i] == '*') return s;
	return NULL;
}

static int simple_glob_match(unsigned char *s, unsigned char *p)
{
	unsigned i;
	if (find_first_match(s, p, &i) != s) return 0;
	if (!p[i]) return !s[i];
	while (1) {
		s += i;
		p += i + 1;
		if (!(s = find_first_match(s, p, &i))) return 0;
		if (!p[i]) {
			s += strlen(s) - i;
			return !!find_first_match(s, p, &i);
		}
	}
}


int is_url_blocked(unsigned char* url)
{
	struct block* b;	

	foreach(b, blocks)
	{
		if(simple_glob_match(url, b->url))
			return 1;
	}

	return 0;
}

void free_blocks(void)
{
	/*List destructor */
	struct block* b;

	foreach(b, blocks)
	{
		if(b->url)mem_free(b->url);
	}

	free_list(blocks);
	free_list(block_search_histroy.items);	
}




