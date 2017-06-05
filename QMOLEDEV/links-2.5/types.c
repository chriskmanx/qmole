/* types.c
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#include "links.h"


/*------------------------ ASSOCIATIONS -----------------------*/

/* DECLARATIONS */

static void assoc_edit_item(struct dialog_data *,void *,void (*)(struct dialog_data *,void *,void *,struct list_description *),void *, unsigned char);
static void assoc_copy_item(void *, void *);
static void *assoc_new_item(void *);
static void assoc_delete_item(void *);
static void *assoc_find_item(void *start, unsigned char *str, int direction);
static unsigned char *assoc_type_item(struct terminal *, void *, int);

static unsigned char *get_filename_from_header(unsigned char *head);


struct list assoc={&assoc,&assoc,0,-1,NULL};

static struct history assoc_search_history = { 0, { &assoc_search_history.items, &assoc_search_history.items } };

struct assoc_ok_struct{
	void (*fn)(struct dialog_data *,void *,void *,struct list_description *);
	void *data;	
	struct dialog_data *dlg;
};


static struct list_description assoc_ld={
	0,  /* 0= flat; 1=tree */
	&assoc,  /* list */
	assoc_new_item,
	assoc_edit_item,
	assoc_default_value,
	assoc_delete_item,
	assoc_copy_item,
	assoc_type_item,
	assoc_find_item,
	&assoc_search_history,
	0,		/* this is set in init_assoc function */
	15,  /* # of items in main window */
	T_ASSOCIATION,
	T_ASSOCIATIONS_ALREADY_IN_USE,
	T_ASSOCIATIONS_MANAGER,
	T_DELETE_ASSOCIATION,
	0,	/* no button */
	NULL,	/* no button */
	NULL,	/* no save*/

	NULL,NULL,0,0,  /* internal vars */
	0, /* modified */
	NULL,
	NULL,
	1,
};

void *assoc_default_value(struct session* ses, unsigned char type)
{
	return NULL;
}

static void *assoc_new_item(void *ignore)
{
	struct assoc *new;

	new = mem_calloc(sizeof(struct assoc));
	new->label = stracpy("");
	new->ct = stracpy("");
	new->prog = stracpy("");
	new->block = new->xwin = new->cons = 1;
	new->ask = 1;
	new->accept_http = 0;
	new->accept_ftp = 0;
	new->type = 0;
	new->system = SYSTEM_ID;
	return new;
}

static void assoc_delete_item(void *data)
{
	struct assoc *del=(struct assoc *)data;
	struct assoc *next=del->next;
	struct assoc *prev=del->prev;

	if (del->label)mem_free(del->label);
	if (del->ct)mem_free(del->ct);
	if (del->prog)mem_free(del->prog);
	if (next)next->prev=del->prev;
	if (prev)prev->next=del->next;
	mem_free(del);
}

static void assoc_copy_item(void *in, void *out)
{
	struct assoc *item_in=(struct assoc *)in;
	struct assoc *item_out=(struct assoc *)out;

	item_out->cons=item_in->cons;
	item_out->xwin=item_in->xwin;
	item_out->block=item_in->block;
	item_out->ask=item_in->ask;
	item_out->accept_http=item_in->accept_http;
	item_out->accept_ftp=item_in->accept_ftp;
	item_out->system=item_in->system;

	if (item_out->label)mem_free(item_out->label);
	if (item_out->ct)mem_free(item_out->ct);
	if (item_out->prog)mem_free(item_out->prog);

	item_out->label=stracpy(item_in->label);
	item_out->ct=stracpy(item_in->ct);
	item_out->prog=stracpy(item_in->prog);
}

/* allocate string and print association into it */
/* x: 0=type all, 1=type title only */
static unsigned char *assoc_type_item(struct terminal *term, void *data, int x)
{
	unsigned char *txt, *txt1;
	struct conv_table *table;
	struct assoc* item=(struct assoc*)data;

	if ((struct list*)item==(&assoc))return stracpy(_(TEXT_(T_ASSOCIATIONS),term));
	txt=stracpy("");
	if (item->system != SYSTEM_ID) add_to_strn(&txt, "XX ");
	add_to_strn(&txt, item->label);
	if (item->ct){add_to_strn(&txt,": ");add_to_strn(&txt,item->ct);}
	if (!x)
	{
		add_to_strn(&txt," -> ");
		if (item->prog)add_to_strn(&txt,item->prog);
	}
	table=get_translation_table(assoc_ld.codepage,term->spec->charset);
	txt1=convert_string(table,txt,strlen(txt),NULL);
	mem_free(txt);
			
	return txt1;
}

void menu_assoc_manager(struct terminal *term,void *fcp,struct session *ses)
{
	create_list_window(&assoc_ld,&assoc,term,ses);
}

static unsigned char *ct_msg[] = {
	TEXT_(T_LABEL),
	TEXT_(T_CONTENT_TYPES),
	TEXT_(T_PROGRAM__IS_REPLACED_WITH_FILE_NAME),
#ifdef ASSOC_BLOCK
	TEXT_(T_BLOCK_TERMINAL_WHILE_PROGRAM_RUNNING),
#endif
#ifdef ASSOC_CONS_XWIN
	TEXT_(T_RUN_ON_TERMINAL),
	TEXT_(T_RUN_IN_XWINDOW),
#endif
	TEXT_(T_ASK_BEFORE_OPENING),
	TEXT_(T_ACCEPT_HTTP),
	TEXT_(T_ACCEPT_FTP),
};

static void assoc_edit_item_fn(struct dialog_data *dlg)
{
	struct terminal *term = dlg->win->term;
	int max = 0, min = 0;
	int w, rw;
	int y = gf_val(-1, -G_BFU_FONT_SIZE);
	int p = 3;
	if (dlg->win->term->spec->braille) y += gf_val(1, G_BFU_FONT_SIZE);
#ifdef ASSOC_BLOCK
	p++;
#endif
#ifdef ASSOC_CONS_XWIN
	p += 2;
#endif
	max_text_width(term, ct_msg[0], &max, AL_LEFT);
	min_text_width(term, ct_msg[0], &min, AL_LEFT);
	max_text_width(term, ct_msg[1], &max, AL_LEFT);
	min_text_width(term, ct_msg[1], &min, AL_LEFT);
	max_text_width(term, ct_msg[2], &max, AL_LEFT);
	min_text_width(term, ct_msg[2], &min, AL_LEFT);
	max_group_width(term, ct_msg + 3, dlg->items + 3, p, &max);
	min_group_width(term, ct_msg + 3, dlg->items + 3, p, &min);
	max_buttons_width(term, dlg->items + 3 + p, 2, &max);
	min_buttons_width(term, dlg->items + 3 + p, 2, &min);
	w = term->x * 9 / 10 - 2 * DIALOG_LB;
	if (w > max) w = max;
	if (w < min) w = min;
	if (w > term->x - 2 * DIALOG_LB) w = term->x - 2 * DIALOG_LB;
	if (w < 1) w = 1;
	rw = 0;
	dlg_format_text_and_field(dlg, NULL, _(ct_msg[0], term), &dlg->items[0], 0, &y, w, &rw, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE * 1);
	dlg_format_text_and_field(dlg, NULL, _(ct_msg[1], term), &dlg->items[1], 0, &y, w, &rw, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE * 1);
	dlg_format_text_and_field(dlg, NULL, _(ct_msg[2], term), &dlg->items[2], 0, &y, w, &rw, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE * 1);
	dlg_format_group(dlg, NULL, ct_msg + 3, dlg->items + 3, p, 0, &y, w, &rw);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_buttons(dlg, NULL, dlg->items + 3 + p, 2, 0, &y, w, &rw, AL_CENTER);
	w = rw;
	dlg->xw = w + 2 * DIALOG_LB;
	dlg->yw = y + 2 * DIALOG_TB;
	center_dlg(dlg);
	draw_dlg(dlg);
	y = dlg->y + DIALOG_TB;
	if (dlg->win->term->spec->braille) y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_text_and_field(dlg, term, ct_msg[0], &dlg->items[0], dlg->x + DIALOG_LB, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_text_and_field(dlg, term, ct_msg[1], &dlg->items[1], dlg->x + DIALOG_LB, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_text_and_field(dlg, term, ct_msg[2], &dlg->items[2], dlg->x + DIALOG_LB, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_group(dlg, term, ct_msg + 3, &dlg->items[3], p, dlg->x + DIALOG_LB, &y, w, NULL);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_buttons(dlg, term, &dlg->items[3 + p], 2, dlg->x + DIALOG_LB, &y, w, NULL, AL_CENTER);
}

/* Puts url and title into the bookmark item */
static void assoc_edit_done(void *data)
{
	struct dialog *d=(struct dialog*)data;
	struct assoc *item=(struct assoc *)d->udata;
	struct assoc_ok_struct* s=(struct assoc_ok_struct*)d->udata2;
	unsigned char *txt;
	struct conv_table *table;
	unsigned char *label, *ct, *prog;

	label=(unsigned char *)&d->items[12];
	ct=label+MAX_STR_LEN;
	prog=ct+MAX_STR_LEN;

	table=get_translation_table(s->dlg->win->term->spec->charset,assoc_ld.codepage);
	txt=convert_string(table,label,strlen(label),NULL);
	mem_free(item->label); item->label=txt;

	txt=convert_string(table,ct,strlen(ct),NULL);
	mem_free(item->ct); item->ct=txt;

	txt=convert_string(table,prog,strlen(prog),NULL);
	mem_free(item->prog); item->prog=txt;

	s->fn(s->dlg,s->data,item,&assoc_ld);
	d->udata=0;  /* for abort function */
}

/* destroys an item, this function is called when edit window is aborted */
static void assoc_edit_abort(struct dialog_data *data)
{
	struct assoc *item=(struct assoc*)data->dlg->udata;
	struct dialog *dlg=data->dlg;

	mem_free(dlg->udata2);
	if (item)assoc_delete_item(item);
}

static void assoc_edit_item(struct dialog_data *dlg, void *data, void (*ok_fn)(struct dialog_data *, void *, void *, struct list_description *), void *ok_arg, unsigned char dlg_title)
{
	int p;
	struct assoc *new=(struct assoc*)data;
	struct terminal *term=dlg->win->term;
	struct dialog *d;
	struct assoc_ok_struct *s;
	unsigned char *ct, *prog, *label;

	d = mem_calloc(sizeof(struct dialog) + 12 * sizeof(struct dialog_item) + 3 * MAX_STR_LEN);

	label=(unsigned char *)&d->items[12];
	ct=label+MAX_STR_LEN;
	prog=ct+MAX_STR_LEN;

	if (new->label)safe_strncpy(label,new->label,MAX_STR_LEN);
	if (new->ct)safe_strncpy(ct,new->ct,MAX_STR_LEN);
	if (new->prog)safe_strncpy(prog,new->prog,MAX_STR_LEN);
	
	/* Create the dialog */
	s=mem_alloc(sizeof(struct assoc_ok_struct));
	s->fn=ok_fn;
	s->data=ok_arg;
	s->dlg=dlg;
		
	switch (dlg_title)
	{
		case TITLE_EDIT:
		d->title=TEXT_(T_EDIT_ASSOCIATION);
		break;

		case TITLE_ADD:
		d->title=TEXT_(T_ADD_ASSOCIATION);
		break;

		default:
		internal("Unsupported dialog title.\n");
	}

	d->udata=data;
	d->udata2=s;
	d->fn = assoc_edit_item_fn;
	d->abort=assoc_edit_abort;
	d->refresh=assoc_edit_done;
	d->refresh_data = d;
	d->items[0].type = D_FIELD;
	d->items[0].dlen = MAX_STR_LEN;
	d->items[0].data = label;
	d->items[0].fn = check_nonempty;
	d->items[1].type = D_FIELD;
	d->items[1].dlen = MAX_STR_LEN;
	d->items[1].data = ct;
	d->items[1].fn = check_nonempty;
	d->items[2].type = D_FIELD;
	d->items[2].dlen = MAX_STR_LEN;
	d->items[2].data = prog;
	d->items[2].fn = check_nonempty;
	p = 3;
#ifdef ASSOC_BLOCK
	d->items[p].type = D_CHECKBOX;
	d->items[p].data = (unsigned char *)&new->block;
	d->items[p++].dlen = sizeof(int);
#endif
#ifdef ASSOC_CONS_XWIN
	d->items[p].type = D_CHECKBOX;
	d->items[p].data = (unsigned char *)&new->cons;
	d->items[p++].dlen = sizeof(int);
	d->items[p].type = D_CHECKBOX;
	d->items[p].data = (unsigned char *)&new->xwin;
	d->items[p++].dlen = sizeof(int);
#endif
	d->items[p].type = D_CHECKBOX;
	d->items[p].data = (unsigned char *)&new->ask;
	d->items[p++].dlen = sizeof(int);
	d->items[p].type = D_CHECKBOX;
	d->items[p].data = (unsigned char *)&new->accept_http;
	d->items[p++].dlen = sizeof(int);
	d->items[p].type = D_CHECKBOX;
	d->items[p].data = (unsigned char *)&new->accept_ftp;
	d->items[p++].dlen = sizeof(int);
	d->items[p].type = D_BUTTON;
	d->items[p].gid = B_ENTER;
	d->items[p].fn = ok_dialog;
	d->items[p++].text = TEXT_(T_OK);
	d->items[p].type = D_BUTTON;
	d->items[p].gid = B_ESC;
	d->items[p].text = TEXT_(T_CANCEL);
	d->items[p++].fn = cancel_dialog;
	d->items[p++].type = D_END;
	do_dialog(term, d, getml(d, NULL));
}

static void *assoc_find_item(void *start, unsigned char *str, int direction)
{
	struct assoc *a,*s=(struct assoc *)start;
	
	if (direction==1)
	{
		for (a=s->next; a!=s; a=a->next)
			if (a->depth>-1)
			{
				if (a->label && casestrstr(a->label,str)) return a;
				if (a->ct && casestrstr(a->ct,str)) return a;
			}
	}
	else
	{
		for (a=s->prev; a!=s; a=a->prev)
			if (a->depth>-1)
			{
				if (a->label && casestrstr(a->label,str)) return a;
				if (a->ct && casestrstr(a->ct,str)) return a;
			}
	}
	if (a==s&&a->depth>-1&&a->label && casestrstr(a->label,str)) return a;
	if (a==s&&a->depth>-1&&a->ct && casestrstr(a->ct,str)) return a;

	return NULL;
}


void update_assoc(struct assoc *new)
{
	struct assoc *repl;
	if (!new->label[0] || !new->ct[0] || !new->prog[0]) return;
	foreach(repl, assoc) if (!strcmp(repl->label, new->label)
			      && !strcmp(repl->ct, new->ct)
			      && !strcmp(repl->prog, new->prog)
			      && repl->block == new->block
			      && repl->cons == new->cons
			      && repl->xwin == new->xwin
			      && repl->ask == new->ask
			      && repl->accept_http == new->accept_http
			      && repl->accept_ftp == new->accept_ftp
			      && repl->system == new->system) {
		del_from_list(repl);
		add_to_list(assoc, repl);
		return;
	}
	repl = mem_calloc(sizeof(struct assoc));
	add_to_list(assoc, repl);
	repl->label = stracpy(new->label);
	repl->ct = stracpy(new->ct);
	repl->prog = stracpy(new->prog);
	repl->block = new->block;
	repl->cons = new->cons;
	repl->xwin = new->xwin;
	repl->ask = new->ask;
	repl->accept_http = new->accept_http;
	repl->accept_ftp = new->accept_ftp;
	repl->system = new->system;
	repl->type = 0;
	/*new->system = new->system; co to je? */
}

/*------------------------ EXTENSIONS -----------------------*/

/* DECLARATIONS */
static void ext_edit_item(struct dialog_data *,void *,void (*)(struct dialog_data *,void *,void *,struct list_description *),void *, unsigned char);
static void ext_copy_item(void *, void *);
static void *ext_new_item(void *);
static void ext_delete_item(void *);
static void *ext_find_item(void *start, unsigned char *str, int direction);
static unsigned char *ext_type_item(struct terminal *, void *, int);

struct list extensions = { &extensions, &extensions, 0, -1, NULL };

static struct history ext_search_history = { 0, { &ext_search_history.items, &ext_search_history.items } };


static struct list_description ext_ld={
	0,  /* 0= flat; 1=tree */
	&extensions,  /* list */
	ext_new_item,
	ext_edit_item,
	assoc_default_value,
	ext_delete_item,
	ext_copy_item,
	ext_type_item,
	ext_find_item,
	&ext_search_history,
	0,		/* this is set in init_assoc function */
	15,  /* # of items in main window */
	T_eXTENSION,
	T_EXTENSIONS_ALREADY_IN_USE,
	T_EXTENSIONS_MANAGER,
	T_DELETE_EXTENSION,
	0,	/* no button */
	NULL,	/* no button */
	NULL,	/* no save*/

	NULL,NULL,0,0,  /* internal vars */
	0, /* modified */
	NULL,
	NULL,
	0,
};





static void *ext_new_item(void *ignore)
{
	struct extension *new;

	new = mem_calloc(sizeof(struct extension));
	new->ext = stracpy("");
	new->ct = stracpy("");
	new->type=0;
	return new;
}


static void ext_delete_item(void *data)
{
	struct extension *del=(struct extension *)data;
	struct extension *next=del->next;
	struct extension *prev=del->prev;

	if (del->ext)mem_free(del->ext);
	if (del->ct)mem_free(del->ct);
	if (next)next->prev=del->prev;
	if (prev)prev->next=del->next;
	mem_free(del);
}


static void ext_copy_item(void *in, void *out)
{
	struct extension *item_in=(struct extension *)in;
	struct extension *item_out=(struct extension *)out;

	if (item_out->ext)mem_free(item_out->ext);
	if (item_out->ct)mem_free(item_out->ct);

	item_out->ext=stracpy(item_in->ext);
	item_out->ct=stracpy(item_in->ct);
}


/* allocate string and print extension into it */
/* x: 0=type all, 1=type title only */
static unsigned char *ext_type_item(struct terminal *term, void *data, int x)
{
	unsigned char *txt, *txt1;
	struct conv_table *table;
	struct extension* item=(struct extension*)data;

	if ((struct list*)item==(&extensions)) return stracpy(_(TEXT_(T_FILE_EXTENSIONS),term));
	txt=stracpy(item->ext);
	if (item->ct){add_to_strn(&txt,": ");add_to_strn(&txt,item->ct);}
	table=get_translation_table(assoc_ld.codepage,term->spec->charset);
	txt1=convert_string(table,txt,strlen(txt),NULL);
	mem_free(txt);
			
	return txt1;
}


void menu_ext_manager(struct terminal *term,void *fcp,struct session *ses)
{
	create_list_window(&ext_ld,&extensions,term,ses);
}

static unsigned char *ext_msg[] = {
	TEXT_(T_EXTENSION_S),
	TEXT_(T_CONTENT_TYPE),
};

static void ext_edit_item_fn(struct dialog_data *dlg)
{
	struct terminal *term = dlg->win->term;
	int max = 0, min = 0;
	int w, rw;
	int y = gf_val(-1, -G_BFU_FONT_SIZE);
	if (dlg->win->term->spec->braille) y += gf_val(1, G_BFU_FONT_SIZE);
	max_text_width(term, ext_msg[0], &max, AL_LEFT);
	min_text_width(term, ext_msg[0], &min, AL_LEFT);
	max_text_width(term, ext_msg[1], &max, AL_LEFT);
	min_text_width(term, ext_msg[1], &min, AL_LEFT);
	max_buttons_width(term, dlg->items + 2, 2, &max);
	min_buttons_width(term, dlg->items + 2, 2, &min);
	w = term->x * 9 / 10 - 2 * DIALOG_LB;
	if (w > max) w = max;
	if (w < min) w = min;
	if (w > term->x - 2 * DIALOG_LB) w = term->x - 2 * DIALOG_LB;
	if (w < 1) w = 1;
	rw = 0;
	dlg_format_text_and_field(dlg, NULL, ext_msg[0], &dlg->items[0], 0, &y, w, &rw, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE * 1);
	dlg_format_text_and_field(dlg, NULL, ext_msg[1], &dlg->items[1], 0, &y, w, &rw, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE * 1);
	dlg_format_buttons(dlg, NULL, dlg->items + 2, 2, 0, &y, w, &rw, AL_CENTER);
	w = rw;
	dlg->xw = w + 2 * DIALOG_LB;
	dlg->yw = y + 2 * DIALOG_TB;
	center_dlg(dlg);
	draw_dlg(dlg);
	y = dlg->y + DIALOG_TB;
	if (dlg->win->term->spec->braille) y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_text_and_field(dlg, term, ext_msg[0], &dlg->items[0], dlg->x + DIALOG_LB, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_text_and_field(dlg, term, ext_msg[1], &dlg->items[1], dlg->x + DIALOG_LB, &y, w, NULL, COLOR_DIALOG_TEXT, AL_LEFT);
	y += gf_val(1, G_BFU_FONT_SIZE);
	dlg_format_buttons(dlg, term, &dlg->items[2], 2, dlg->x + DIALOG_LB, &y, w, NULL, AL_CENTER);
}


/* Puts url and title into the bookmark item */
static void ext_edit_done(void *data)
{
	struct dialog *d=(struct dialog*)data;
	struct extension *item=(struct extension *)d->udata;
	struct assoc_ok_struct* s=(struct assoc_ok_struct*)d->udata2;
	unsigned char *txt;
	struct conv_table *table;
	unsigned char *ext, *ct;

	ext=(unsigned char *)&d->items[5];
	ct=ext+MAX_STR_LEN;

	table=get_translation_table(s->dlg->win->term->spec->charset,ext_ld.codepage);
	txt=convert_string(table,ext,strlen(ext),NULL);
	mem_free(item->ext); item->ext=txt;

	txt=convert_string(table,ct,strlen(ct),NULL);
	mem_free(item->ct); item->ct=txt;

	s->fn(s->dlg,s->data,item,&ext_ld);
	d->udata=0;  /* for abort function */
}


/* destroys an item, this function is called when edit window is aborted */
static void ext_edit_abort(struct dialog_data *data)
{
	struct extension *item=(struct extension*)data->dlg->udata;
	struct dialog *dlg=data->dlg;

	mem_free(dlg->udata2);
	if (item)ext_delete_item(item);
}


static void ext_edit_item(struct dialog_data *dlg, void *data, void (*ok_fn)(struct dialog_data *, void *, void *, struct list_description *), void *ok_arg, unsigned char dlg_title)
{
	struct extension *new=(struct extension*)data;
	struct terminal *term=dlg->win->term;
	struct dialog *d;
	struct assoc_ok_struct *s;
	unsigned char *ext;
	unsigned char *ct;

	d = mem_calloc(sizeof(struct dialog) + 5 * sizeof(struct dialog_item) + 2 * MAX_STR_LEN);

	ext=(unsigned char *)&d->items[5];
	ct = ext + MAX_STR_LEN;
	if (new->ext)safe_strncpy(ext, new->ext, MAX_STR_LEN);
	if (new->ct)safe_strncpy(ct, new->ct, MAX_STR_LEN);

	/* Create the dialog */
	s=mem_alloc(sizeof(struct assoc_ok_struct));
	s->fn=ok_fn;
	s->data=ok_arg;
	s->dlg=dlg;
		
	switch (dlg_title)
	{
		case TITLE_EDIT:
		d->title=TEXT_(T_EDIT_EXTENSION);
		break;

		case TITLE_ADD:
		d->title=TEXT_(T_ADD_EXTENSION);
		break;

		default:
		internal("Unsupported dialog title.\n");
	}

	d->udata=data;
	d->udata2=s;
	d->abort=ext_edit_abort;
	d->refresh=ext_edit_done;
	d->refresh_data = d;
	d->title = TEXT_(T_EXTENSION);
	d->fn = ext_edit_item_fn;
	d->items[0].type = D_FIELD;
	d->items[0].dlen = MAX_STR_LEN;
	d->items[0].data = ext;
	d->items[0].fn = check_nonempty;
	d->items[1].type = D_FIELD;
	d->items[1].dlen = MAX_STR_LEN;
	d->items[1].data = ct;
	d->items[1].fn = check_nonempty;
	d->items[2].type = D_BUTTON;
	d->items[2].gid = B_ENTER;
	d->items[2].fn = ok_dialog;
	d->items[2].text = TEXT_(T_OK);
	d->items[3].type = D_BUTTON;
	d->items[3].gid = B_ESC;
	d->items[3].text = TEXT_(T_CANCEL);
	d->items[3].fn = cancel_dialog;
	d->items[4].type = D_END;
	do_dialog(term, d, getml(d, NULL));
}


static void *ext_find_item(void *start, unsigned char *str, int direction)
{
	struct extension *e,*s=(struct extension *)start;
	
	
	if (direction==1)
	{
		for (e=s->next; e!=s; e=e->next)
			if (e->depth>-1)
			{
				if (e->ext && casestrstr(e->ext,str)) return e;
				if (e->ct && casestrstr(e->ct,str)) return e;
			}
	}
	else
	{
		for (e=s->prev; e!=s; e=e->prev)
			if (e->depth>-1)
			{
				if (e->ext && casestrstr(e->ext,str)) return e;
				if (e->ct && casestrstr(e->ct,str)) return e;
			}
	}
	if (e==s&&e->depth>-1&&e->ext && casestrstr(e->ext,str)) return e;
	if (e==s&&e->depth>-1&&e->ct && casestrstr(e->ct,str)) return e;

	return NULL;
}


void update_ext(struct extension *new)
{
	struct extension *repl;
	if (!new->ext[0] || !new->ct[0]) return;
	foreach(repl, extensions) if (!strcmp(repl->ext, new->ext) && !strcmp(repl->ct, new->ct)) {
		del_from_list(repl);
		add_to_list(extensions, repl);
		return;
	}
	repl = mem_calloc(sizeof(struct extension));
	add_to_list(extensions, repl);
	repl->ext = stracpy(new->ext);
	repl->ct = stracpy(new->ct);
	repl->type=0;
}


/* creates default extensions if extension list is empty */
void create_initial_extensions(void)
{
	struct extension ext;

	if (!list_empty(extensions))return;

	/* here you can add any default extension you want */
	ext.ext="aif,aiff,aifc",ext.ct="audio/x-aiff",update_ext(&ext);
	ext.ext="au,snd",ext.ct="audio/basic",update_ext(&ext);
	ext.ext="avi",ext.ct="video/x-msvideo",update_ext(&ext);
	ext.ext="deb",ext.ct="application/x-debian-package",update_ext(&ext);
	ext.ext="dl",ext.ct="video/dl",update_ext(&ext);
	ext.ext="dxf",ext.ct="application/dxf",update_ext(&ext);
	ext.ext="dvi",ext.ct="application/x-dvi",update_ext(&ext);
	ext.ext="fli",ext.ct="video/fli",update_ext(&ext);
	ext.ext="g",ext.ct="application/brlcad",update_ext(&ext);
	ext.ext="gbr",ext.ct="application/gerber",update_ext(&ext);
	ext.ext="gif",ext.ct="image/gif",update_ext(&ext);
	ext.ext="gl",ext.ct="video/gl",update_ext(&ext);
	ext.ext="grb",ext.ct="application/gerber",update_ext(&ext);
	ext.ext="jpg,jpeg,jpe",ext.ct="image/jpeg",update_ext(&ext);
	ext.ext="mid,midi",ext.ct="audio/midi",update_ext(&ext);
	ext.ext="mpeg,mpg,mpe",ext.ct="video/mpeg",update_ext(&ext);
	ext.ext="pbm",ext.ct="image/x-portable-bitmap",update_ext(&ext);
	ext.ext="pcb",ext.ct="application/pcb",update_ext(&ext);
	ext.ext="pdf",ext.ct="application/pdf",update_ext(&ext);
	ext.ext="pgm",ext.ct="image/x-portable-graymap",update_ext(&ext);
	ext.ext="pgp",ext.ct="application/pgp-signature",update_ext(&ext);
	ext.ext="png",ext.ct="image/png",update_ext(&ext);
	ext.ext="pnm",ext.ct="image/x-portable-anymap",update_ext(&ext);
	ext.ext="ppm",ext.ct="image/x-portable-pixmap",update_ext(&ext);
	ext.ext="ppt",ext.ct="application/powerpoint",update_ext(&ext);
	ext.ext="ps,eps,ai",ext.ct="application/postscript",update_ext(&ext);
	ext.ext="qt,mov",ext.ct="video/quicktime",update_ext(&ext);
	ext.ext="ra,rm,ram",ext.ct="audio/x-pn-realaudio",update_ext(&ext);
	ext.ext="rtf",ext.ct="application/rtf",update_ext(&ext);
	ext.ext="sch",ext.ct="application/gschem",update_ext(&ext);
	ext.ext="svg",ext.ct="image/svg",update_ext(&ext);
	ext.ext="swf",ext.ct="application/x-shockwave-flash",update_ext(&ext);
	ext.ext="sxw",ext.ct="application/x-openoffice",update_ext(&ext);
	ext.ext="tga",ext.ct="image/targa",update_ext(&ext);
	ext.ext="tiff,tif",ext.ct="image/tiff",update_ext(&ext);
	ext.ext="wav",ext.ct="audio/x-wav",update_ext(&ext);
	ext.ext="xbm",ext.ct="image/x-xbitmap",update_ext(&ext);
	ext.ext="xls",ext.ct="application/excel",update_ext(&ext);
	ext.ext="xpm",ext.ct="image/x-xpixmap",update_ext(&ext);
}

/* --------------------------- PROG -----------------------------*/


struct list_head mailto_prog = { &mailto_prog, &mailto_prog };
struct list_head telnet_prog = { &telnet_prog, &telnet_prog };
struct list_head tn3270_prog = { &tn3270_prog, &tn3270_prog };
struct list_head mms_prog = { &mms_prog, &mms_prog };
struct list_head magnet_prog = { &magnet_prog, &magnet_prog };


static int is_in_list(unsigned char *list, unsigned char *str, int l)
{
	unsigned char *l2, *l3;
	if (!l) return 0;
	rep:
	while (*list && *list <= ' ') list++;
	if (!*list) return 0;
	for (l2 = list; *l2 && *l2 != ','; l2++)
		;
	for (l3 = l2 - 1; l3 >= list && *l3 <= ' '; l3--)
		;
	l3++;
	if (l3 - list == l && !casecmp(str, list, l)) return 1;
	list = l2;
	if (*list == ',') list++;
	goto rep;
}

unsigned char *get_compress_by_extension(unsigned char *ext, unsigned char *ext_end)
{
	size_t len = ext_end - ext;
	if (len == 1 && !casecmp(ext, "z", 1)) return "compress";
	if (len == 2 && !casecmp(ext, "gz", 2)) return "gzip";
	if (len == 3 && !casecmp(ext, "tgz", 2)) return "gzip";
	if (len == 3 && !casecmp(ext, "bz2", 3)) return "bzip2";
	if (len == 4 && !casecmp(ext, "lzma", 4)) return "lzma";
	if (len == 2 && !casecmp(ext, "xz", 2)) return "lzma2";
	return NULL;
}

static unsigned char *get_content_type_by_extension(unsigned char *url)
{
	struct extension *e;
	struct assoc *a;
	unsigned char *ct, *eod, *ext, *exxt;
	int extl, el;
	ext = NULL, extl = 0;
	if (!(ct = get_url_data(url))) ct = url;
	for (eod = ct; *eod && !end_of_dir(url, *eod); eod++)
		;
	for (; ct < eod; ct++)
		if (*ct == '.') {
			if (ext) {
				if (get_compress_by_extension(ct + 1, eod))
					break;
			}
			ext = ct + 1;
		} else if (dir_sep(*ct)) {
			ext = NULL;
		}
	if (ext) while (ext[extl] && ext[extl] != '.' && !dir_sep(ext[extl]) && !end_of_dir(url, ext[extl])) extl++;
	if ((extl == 3 && !casecmp(ext, "htm", 3)) ||
	    (extl == 4 && !casecmp(ext, "html", 4))) return stracpy("text/html");
	foreach(e, extensions) {
		unsigned char *fname = NULL;
		if (!(ct = get_url_data(url))) ct = url;
		for (; *ct && !end_of_dir(url, *ct); ct++)
			if (dir_sep(*ct)) fname = ct + 1;
		if (!fname) {
			if (is_in_list(e->ext, ext, extl)) return stracpy(e->ct);
		} else {
			int fnlen = 0;
			int x;
			while (fname[fnlen] && !end_of_dir(url, fname[fnlen])) fnlen++;
			for (x = 0; x < fnlen; x++) if (fname[x] == '.') if (is_in_list(e->ext, fname + x + 1, fnlen - x - 1)) return stracpy(e->ct);
		}
	}

	if ((extl == 3 && !casecmp(ext, "jpg", 3)) ||
	    (extl == 4 && !casecmp(ext, "pjpg", 4))||
	    (extl == 4 && !casecmp(ext, "jpeg", 4))||
	    (extl == 5 && !casecmp(ext, "pjpeg", 5))) return stracpy("image/jpeg");
	if ((extl == 3 && !casecmp(ext, "png", 3))) return stracpy("image/png");
	if ((extl == 3 && !casecmp(ext, "gif", 3))) return stracpy("image/gif");
	if ((extl == 3 && !casecmp(ext, "xbm", 3))) return stracpy("image/x-xbitmap");
	if ((extl == 3 && !casecmp(ext, "tif", 3)) ||
	    (extl == 4 && !casecmp(ext, "tiff", 4))) return stracpy("image/tiff");
	exxt = init_str(); el = 0;
	add_to_str(&exxt, &el, "application/x-");
	add_bytes_to_str(&exxt, &el, ext, extl);
	foreach(a, assoc) if (is_in_list(a->ct, exxt, el)) return exxt;
	mem_free(exxt);
	return NULL;
}

static unsigned char *get_content_type_by_header_and_extension(unsigned char *head, unsigned char *url)
{
	unsigned char *ct, *file;
	ct = get_content_type_by_extension(url);
	if (ct) return ct;
	file = get_filename_from_header(head);
	if (file) {
		ct = get_content_type_by_extension(file);
		mem_free(file);
		if (ct) return ct;
	}
	return NULL;
}

static unsigned char *get_extension_by_content_type(unsigned char *ct)
{
	struct extension *e;
	unsigned char *x, *y;
	if (is_html_type(ct)) return stracpy("html");
	foreach(e, extensions) {
		if (!strcasecmp(e->ct, ct)) {
			x = stracpy(e->ext);
			if ((y = strchr(x, ','))) *y = 0;
			return x;
		}
	}
	if (!strcasecmp(ct, "image/jpeg") ||
	    !strcasecmp(ct, "image/jpg") ||
	    !strcasecmp(ct, "image/jpe") ||
	    !strcasecmp(ct, "image/pjpe") ||
	    !strcasecmp(ct, "image/pjpeg") ||
	    !strcasecmp(ct, "image/pjpg"))
		return stracpy("jpg");
	if (!strcasecmp(ct, "image/png") ||
	    !strcasecmp(ct, "image/x-png"))
		return stracpy("png");
	if (!strcasecmp(ct, "image/gif"))
		return stracpy("gif");
	if (!strcasecmp(ct, "image/x-bitmap"))
		return stracpy("xbm");
	if (!strcasecmp(ct, "image/tiff") ||
	    !strcasecmp(ct, "image/tif"))
		return stracpy("tiff");
	if (!cmpbeg(ct, "application/x-")) {
		x = ct + strlen("application/x-");
		if (strcasecmp(x, "z") &&
		    strcasecmp(x, "gz") &&
		    strcasecmp(x, "gzip") &&
		    strcasecmp(x, "bz2") &&
		    strcasecmp(x, "bzip2") &&
		    strcasecmp(x, "lzma") &&
		    strcasecmp(x, "lzma2") &&
		    strcasecmp(x, "xz") &&
		    !strchr(x, '-') &&
		    strlen(x) <= 4) {
			return stracpy(x);
		}
	}
	return NULL;
}

static unsigned char *get_content_encoding_from_content_type(unsigned char *ct)
{
	if (!strcasecmp(ct, "application/x-gzip") ||
	    !strcasecmp(ct, "application/x-tgz")) return "gzip";
	if (!strcasecmp(ct, "application/x-bzip2") ||
	    !strcasecmp(ct, "application/x-bzip")) return "bzip2";
	if (!strcasecmp(ct, "application/x-lzma")) return "lzma";
	if (!strcasecmp(ct, "application/x-lzma2") ||
	    !strcasecmp(ct, "application/x-xz")) return "lzma2";
	return NULL;
}

unsigned char *get_content_type(unsigned char *head, unsigned char *url)
{
	unsigned char *ct;
	if ((ct = parse_http_header(head, "Content-Type", NULL))) {
		unsigned char *s;
		if ((s = strchr(ct, ';'))) *s = 0;
		while (*ct && ct[strlen(ct) - 1] <= ' ') ct[strlen(ct) - 1] = 0;
		if (*ct == '"' && ct[1] && ct[strlen(ct) - 1] == '"') {
			memmove(ct, ct + 1, strlen(ct));
			ct[strlen(ct) - 1] = 0;
		}
		if (!strcasecmp(ct, "text/plain") ||
		    !strcasecmp(ct, "application/octet-stream") ||
		    !strcasecmp(ct, "application/octetstream") ||
		    !strcasecmp(ct, "application/octet_stream") ||
		    get_content_encoding_from_content_type(ct)) {
			unsigned char *ctt;
			int code;
			if (!get_http_code(head, &code, NULL) && code >= 300)
				goto no_code_by_extension;
			ctt = get_content_type_by_header_and_extension(head, url);
			if (ctt) {
				mem_free(ct);
				return ctt;
			}
		}
no_code_by_extension:
		if (!*ct) mem_free(ct);
		else return ct;
	}
	ct = get_content_type_by_header_and_extension(head, url);
	if (ct) return ct;
	return !force_html ? stracpy("text/plain") : stracpy("text/html");
}

unsigned char *get_content_encoding(unsigned char *head, unsigned char *url)
{
	unsigned char *ce, *ct, *ext, *extd;
	unsigned char *u;
	int code;
	if ((ce = parse_http_header(head, "Content-Encoding", NULL))) return ce;
	if ((ct = parse_http_header(head, "Content-Type", NULL))) {
		unsigned char *s;
		if ((s = strchr(ct, ';'))) *s = 0;
		ce = get_content_encoding_from_content_type(ct);
		if (ce) {
			mem_free(ct);
			return stracpy(ce);
		}
		if (is_html_type(ct)) {
			mem_free(ct);
			return NULL;
		}
		mem_free(ct);
	}
	if (!get_http_code(head, &code, NULL) && code >= 300) return NULL;
	if (!(ext = get_url_data(url))) ext = url;
	for (u = ext; *u; u++) if (end_of_dir(url, *u)) goto skip_ext;
	extd = strrchr(ext, '.');
	if (extd) {
		ce = get_compress_by_extension(extd + 1, strchr(extd + 1, 0));
		if (ce) return stracpy(ce);
	}
	skip_ext:
	if ((ext = get_filename_from_header(head))) {
		extd = strrchr(ext, '.');
		if (extd) {
			ce = get_compress_by_extension(extd + 1, strchr(extd + 1, 0));
			if (ce) {
				mem_free(ext);
				return stracpy(ce);
			}
		}
		mem_free(ext);
	}
	return NULL;
}

unsigned char *encoding_2_extension(unsigned char *encoding)
{
	if (!strcasecmp(encoding, "gzip") || !strcasecmp(encoding, "x-gzip")) return "gz";
	if (!strcasecmp(encoding, "compress") || !strcasecmp(encoding, "x-compress")) return "Z";
	if (!strcasecmp(encoding, "bzip2")) return "bz2";
	if (!strcasecmp(encoding, "lzma")) return "lzma";
	if (!strcasecmp(encoding, "lzma2")) return "xz";
	return NULL;
}

/* returns field with associations */
struct assoc *get_type_assoc(struct terminal *term, unsigned char *type, int *n)
{
	struct assoc *vecirek;
	struct assoc *a;
	int count=0;
	foreach(a, assoc) 
		if (a->system == SYSTEM_ID && (term->environment & ENV_XWIN ? a->xwin : a->cons) && is_in_list(a->ct, type, strlen(type))) {
			if (count == MAXINT) overalloc();
			count++;
		}
	*n=count;
	if (!count)return NULL;
	if ((unsigned)count > MAXINT / sizeof(struct assoc)) overalloc();
	vecirek=mem_alloc(count*sizeof(struct assoc));
	count=0;
	foreach(a, assoc) 
		if (a->system == SYSTEM_ID && (term->environment & ENV_XWIN ? a->xwin : a->cons) && is_in_list(a->ct, type, strlen(type))) 
			vecirek[count++]=*a;
	return vecirek;
}

void free_types(void)
{
	struct assoc *a;
	struct extension *e;
	struct protocol_program *p;
	foreach(a, assoc) {
		mem_free(a->ct);
		mem_free(a->prog);
		mem_free(a->label);
	}
	free_list(assoc);
	foreach(e, extensions) {
		mem_free(e->ext);
		mem_free(e->ct);
	}
	free_list(extensions);
	foreach(p, mailto_prog) mem_free(p->prog);
	free_list(mailto_prog);
	foreach(p, telnet_prog) mem_free(p->prog);
	free_list(telnet_prog);
	foreach(p, tn3270_prog) mem_free(p->prog);
	free_list(tn3270_prog);
	foreach(p, mms_prog) mem_free(p->prog);
	free_list(mms_prog);
	foreach(p, magnet_prog) mem_free(p->prog);
	free_list(magnet_prog);

	free_list(ext_search_history.items);
	free_list(assoc_search_history.items);
}

void update_prog(struct list_head *l, unsigned char *p, int s)
{
	struct protocol_program *repl;
	foreach(repl, *l) if (repl->system == s) {
		mem_free(repl->prog);
		goto ss;
	}
	repl = mem_alloc(sizeof(struct protocol_program));
	add_to_list(*l, repl);
	repl->system = s;
	ss:
	repl->prog = mem_alloc(MAX_STR_LEN);
	safe_strncpy(repl->prog, p, MAX_STR_LEN);
}

unsigned char *get_prog(struct list_head *l)
{
	struct protocol_program *repl;
	foreach(repl, *l) if (repl->system == SYSTEM_ID) return repl->prog;
	update_prog(l, "", SYSTEM_ID);
	foreach(repl, *l) if (repl->system == SYSTEM_ID) return repl->prog;
	return NULL;
}

int is_html_type(unsigned char *ct)
{
	return	!strcasecmp(ct, "text/html") ||
		!strcasecmp(ct, "text/x-server-parsed-html") ||
		!casecmp(ct, "application/xhtml", strlen("application/xhtml"));
}

static unsigned char *get_filename_from_header(unsigned char *head)
{
	unsigned char *ct, *x, *y;
	if ((ct = parse_http_header(head, "Content-Disposition", NULL))) {
		x = parse_header_param(ct, "filename", 1);
		mem_free(ct);
		if (x) {
			if (*x) goto ret_x;
			mem_free(x);
		}
	}
	if ((ct = parse_http_header(head, "Content-Type", NULL))) {
		x = parse_header_param(ct, "name", 0);
		mem_free(ct);
		if (x) {
			if (*x) goto ret_x;
			mem_free(x);
		}
	}
	return NULL;
	ret_x:
	for (y = x; *y; y++) if (dir_sep(*y)
#if defined(DOS_FS) || defined(SPAD)
		|| *y == ':'
#endif
		) *y = '-';
	return x;
}

unsigned char *get_filename_from_url(unsigned char *url, unsigned char *head, int tmp)
{
	int ll = 0;
	unsigned char *u, *s, *e, *f, *x;
	unsigned char *ct, *want_ext;
	want_ext = stracpy("");
	f = get_filename_from_header(head);
	if (f) goto no_ct;
	if (!(u = get_url_data(url))) u = url;
	for (e = s = u; *e && !end_of_dir(url, *e); e++) {
		if (dir_sep(*e)) s = e + 1;
	}
	ll = 0;
	f = init_str();
	add_conv_str(&f, &ll, s, e - s, -2);
	if (!(ct = parse_http_header(head, "Content-Type", NULL))) goto no_ct;
	mem_free(ct);
	ct = get_content_type(head, url);
	if (ct) {
		x = get_extension_by_content_type(ct);
		if (x) {
			add_to_strn(&want_ext, ".");
			add_to_strn(&want_ext, x);
			mem_free(x);
		}
		mem_free(ct);
	}
	no_ct:
	ct = get_content_encoding(head, url);
	if (ct) {
		x = encoding_2_extension(ct);
		if (!tmp) {
			if (x) {
				if (!strcasecmp(want_ext, ".tgz") && !strcasecmp(x, "gz"))
					goto skip_tgz_1;
				add_to_strn(&want_ext, ".");
				add_to_strn(&want_ext, x);
				skip_tgz_1:;
			}
		} else {
			if (x) {
				if (strlen(x) + 1 < strlen(f) && f[strlen(f) - strlen(x) - 1] == '.' && !strcasecmp(f + strlen(f) - strlen(x), x)) {
					f[strlen(f) - strlen(x) - 1] = 0;
				}
			}
		}
		mem_free(ct);
	}
	if (strlen(want_ext) > strlen(f) || strcasecmp(want_ext, f + strlen(f) - strlen(want_ext))) {
		x = strrchr(f, '.');
		if (x && !strcasecmp(x, ".tgz") && !strcasecmp(want_ext, ".gz"))
			goto skip_tgz_2;
		if (x) *x = 0;
		add_to_strn(&f, want_ext);
		skip_tgz_2:;
	}
	mem_free(want_ext);
	return f;
}
