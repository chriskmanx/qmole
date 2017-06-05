/* main.c
 * main()
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#include "links.h"

int retval = RET_OK;

static void unhandle_basic_signals(struct terminal *);
static void poll_fg(void *);

#ifdef WIN32
static void sig_terminate(struct terminal *t)
{
	unhandle_basic_signals(t);
	terminate_loop = 1;
	retval = RET_SIGNAL;
}
#endif

static void sig_intr(struct terminal *t)
{
	if (!t) {
		unhandle_basic_signals(t);
		terminate_loop = 1;
	} else {
		unhandle_basic_signals(t);
		exit_prog(t, NULL, NULL);
	}
}

static void sig_ctrl_c(struct terminal *t)
{
	if (!is_blocked()) kbd_ctrl_c();
}

#ifdef SIGTTOU
static void sig_ign(void *x)
{
}
#endif

static int fg_poll_timer = -1;

void sig_tstp(struct terminal *t)
{
#if defined(SIGSTOP) && !defined(NO_CTRL_Z)
#if defined(SIGCONT) && defined(SIGTTOU) && defined(HAVE_GETPID)
	pid_t pid = getpid();
	pid_t newpid;
#endif
	if (!F) {
		block_itrm(1);
	}
#ifdef G
	else {
		drv->block(NULL);
	}
#endif
#if defined(SIGCONT) && defined(SIGTTOU) && defined(HAVE_GETPID)
	if (!(newpid = fork())) {
		while (1) {
			sleep(1);
			kill(pid, SIGCONT);
		}
	}
#endif
	raise(SIGSTOP);
#if defined(SIGCONT) && defined(SIGTTOU) && defined(HAVE_GETPID)
	if (newpid != -1) kill(newpid, SIGKILL);
#endif
#endif
	if (fg_poll_timer != -1) kill_timer(fg_poll_timer);
	fg_poll_timer = install_timer(FG_POLL_TIME, poll_fg, t);
}

static void poll_fg(void *t)
{
	int r;
	fg_poll_timer = -1;
	if (!F) {
		r = unblock_itrm(1);
#ifdef G
	} else {
		r = drv->unblock(NULL);
#endif
	}
	if (r == -1) {
		fg_poll_timer = install_timer(FG_POLL_TIME, poll_fg, t);
	}
	if (r == -2) {
		/* This will unblock externally spawned viewer, if it exists */
#ifdef SIGCONT
		kill(0, SIGCONT);
#endif
	}
}

void sig_cont(struct terminal *t)
{
	if (!F) {
		unblock_itrm(1);
#ifdef G
	} else {
		drv->unblock(NULL);
#endif
	}
	/*else register_bottom_half(raise, SIGSTOP);*/
}

static void handle_basic_signals(struct terminal *term)
{
	install_signal_handler(SIGHUP, (void (*)(void *))sig_intr, term, 0);
	if (!F) install_signal_handler(SIGINT, (void (*)(void *))sig_ctrl_c, term, 0);
	/*install_signal_handler(SIGTERM, (void (*)(void *))sig_terminate, term, 0);*/
#ifdef WIN32
	install_signal_handler(SIGQUIT, (void (*)(void *))sig_terminate, term, 0);
#endif
#ifdef SIGTSTP
	if (!F) install_signal_handler(SIGTSTP, (void (*)(void *))sig_tstp, term, 0);
#endif
#ifdef SIGTTIN
	if (!F) install_signal_handler(SIGTTIN, (void (*)(void *))sig_tstp, term, 0);
#endif
#ifdef SIGTTOU
	install_signal_handler(SIGTTOU, (void (*)(void *))sig_ign, term, 0);
#endif
#ifdef SIGCONT
	if (!F) install_signal_handler(SIGCONT, (void (*)(void *))sig_cont, term, 0);
#endif
}

void unhandle_terminal_signals(struct terminal *term)
{
	install_signal_handler(SIGHUP, NULL, NULL, 0);
	if (!F) install_signal_handler(SIGINT, NULL, NULL, 0);
#ifdef SIGTSTP
	install_signal_handler(SIGTSTP, NULL, NULL, 0);
#endif
#ifdef SIGTTIN
	install_signal_handler(SIGTTIN, NULL, NULL, 0);
#endif
#ifdef SIGTTOU
	install_signal_handler(SIGTTOU, NULL, NULL, 0);
#endif
#ifdef SIGCONT
	install_signal_handler(SIGCONT, NULL, NULL, 0);
#endif
	if (fg_poll_timer != -1) kill_timer(fg_poll_timer), fg_poll_timer = -1;
}

void unhandle_basic_signals(struct terminal *term)
{
	install_signal_handler(SIGHUP, NULL, NULL, 0);
	if (!F) install_signal_handler(SIGINT, NULL, NULL, 0);
	/*install_signal_handler(SIGTERM, NULL, NULL, 0);*/
#ifdef SIGTSTP
	install_signal_handler(SIGTSTP, NULL, NULL, 0);
#endif
#ifdef SIGTTIN
	install_signal_handler(SIGTTIN, NULL, NULL, 0);
#endif
#ifdef SIGTTOU
	install_signal_handler(SIGTTOU, NULL, NULL, 0);
#endif
#ifdef SIGCONT
	install_signal_handler(SIGCONT, NULL, NULL, 0);
#endif
	if (fg_poll_timer != -1) kill_timer(fg_poll_timer), fg_poll_timer = -1;
}

static int terminal_pipe[2];

int attach_terminal(int in, int out, int ctl, void *info, int len)
{
	struct terminal *term;
	fcntl(terminal_pipe[0], F_SETFL, O_NONBLOCK);
	fcntl(terminal_pipe[1], F_SETFL, O_NONBLOCK);
	handle_trm(in, out, out, terminal_pipe[1], ctl, info, len);
	mem_free(info);
	if ((term = init_term(terminal_pipe[0], out, win_func))) {
		handle_basic_signals(term);	/* OK, this is race condition, but it must be so; GPM installs it's own buggy TSTP handler */
		return terminal_pipe[1];
	}
	close(terminal_pipe[0]);
	close(terminal_pipe[1]);
	return -1;
}

#ifdef G

int attach_g_terminal(void *info, int len)
{
	struct terminal *term;
	term = init_gfx_term(win_func, info, len);
	mem_free(info);
	return term ? 0 : -1;
}

#endif

static struct object_request *dump_obj;
static off_t dump_pos;

static void end_dump(struct object_request *r, void *p)
{
	struct cache_entry *ce;
	int oh;
	if (!r->state || (r->state == 1 && dmp != D_SOURCE)) return;
	if ((oh = get_output_handle()) == -1) return;
	ce = r->ce;
	if (dmp == D_SOURCE) {
		if (ce) {
			struct fragment *frag;
			nextfrag:
			foreach(frag, ce->frag) if (frag->offset <= dump_pos && frag->offset + frag->length > dump_pos) {
				int l = frag->length - (dump_pos - frag->offset);
				int w = hard_write(oh, frag->data + dump_pos - frag->offset, l);
				if (w != l) {
					detach_object_connection(r, dump_pos);
					if (w < 0) fprintf(stderr, "Error writing to stdout: %s.\n", strerror(errno));
					else fprintf(stderr, "Can't write to stdout.\n");
					retval = RET_ERROR;
					goto terminate;
				}
				dump_pos += w;
				detach_object_connection(r, dump_pos);
				goto nextfrag;
			}
		}
		if (r->state >= 0) return;
	} else if (ce) {
		struct document_options o;
		struct f_data_c *fd;
		if (!(fd = create_f_data_c(NULL, NULL))) goto terminate;
		memset(&o, 0, sizeof(struct document_options));
		o.xp = 0;
		o.yp = 1;
		o.xw = screen_width;
		o.yw = 25;
		o.col = 0;
		o.cp = dump_codepage == -1 ? 0 : dump_codepage;
		ds2do(&dds, &o);
		o.plain = 0;
		o.frames = 0;
		o.js_enable = 0;
		memcpy(&o.default_fg, &default_fg, sizeof(struct rgb));
		memcpy(&o.default_bg, &default_bg, sizeof(struct rgb));
		memcpy(&o.default_link, &default_link, sizeof(struct rgb));
		memcpy(&o.default_vlink, &default_vlink, sizeof(struct rgb));
		o.framename = "";
		if (!(fd->f_data = cached_format_html(fd, r, r->url, &o, NULL))) goto term_1;
		dump_to_file(fd->f_data, oh);
		term_1:
		reinit_f_data_c(fd);
		mem_free(fd);
	}
	if (r->state != O_OK) {
		unsigned char *m = get_err_msg(r->stat.state);
		fprintf(stderr, "%s\n", get_english_translation(m));
		retval = RET_ERROR;
		goto terminate;
	}
	terminate:
	terminate_loop = 1;
}

int g_argc;
unsigned char **g_argv;

unsigned char *path_to_exe;

static unsigned char init_b = 0;

static void initialize_all_subsystems(void);
static void initialize_all_subsystems_2(void);

static void init(void)
{
	int uh;
	void *info;
	int len;
	unsigned char *u;
	initialize_all_subsystems();

	utf8_table=get_cp_index("UTF-8");

/* OS/2 has some stupid bug and the pipe must be created before socket :-/ */
	if (c_pipe(terminal_pipe)) {
		error("ERROR: can't create pipe for internal communication");
		retval = RET_FATAL;
		goto ttt;
	}
	if (!(u = parse_options(g_argc - 1, g_argv + 1))) {
		retval = RET_SYNTAX;
		goto ttt;
	}
	if (ggr_drv[0] || ggr_mode[0]) ggr = 1;
	if (dmp) ggr = 0;
	if (!dmp && !ggr) {
		init_os_terminal();
	}
	if (!ggr && !no_connect && (uh = bind_to_af_unix()) != -1) {
		close(terminal_pipe[0]);
		close(terminal_pipe[1]);
		if (!(info = create_session_info(base_session, u, default_target, &len))) {
			close(uh);
			retval = RET_FATAL;
			goto ttt;
		}
		initialize_all_subsystems_2();
		handle_trm(get_input_handle(), get_output_handle(), uh, uh, get_ctl_handle(), info, len);
		handle_basic_signals(NULL);	/* OK, this is race condition, but it must be so; GPM installs it's own buggy TSTP handler */
		mem_free(info);
		return;
	}
	if ((dds.assume_cp = get_cp_index("ISO-8859-1")) == -1) dds.assume_cp = 0;
	load_config();
	init_b = 1;
	init_bookmarks();
	create_initial_extensions();
	load_url_history();
	init_cookies();
	u = parse_options(g_argc - 1, g_argv + 1);
	if (!u) {
		ttt:
		initialize_all_subsystems_2();
		tttt:
		terminate_loop = 1;
		return;
	}
	if (!dmp) {
		if (ggr) {
#ifdef G
			unsigned char *r;
			if ((r = init_graphics(ggr_drv, ggr_mode, ggr_display))) {
				fprintf(stderr, "%s", r);
				mem_free(r);
				retval = RET_SYNTAX;
				goto ttt;
			}
			handle_basic_signals(NULL);
			init_dither(drv->depth);
			F = 1;
#else
			fprintf(stderr, "Graphics not enabled when compiling\n");
			retval = RET_SYNTAX;
			goto ttt;
#endif
		}
		initialize_all_subsystems_2();
		if (!((info = create_session_info(base_session, u, default_target, &len)) && gf_val(attach_terminal(get_input_handle(), get_output_handle(), get_ctl_handle(), info, len), attach_g_terminal(info, len)) != -1)) {
			retval = RET_FATAL;
			terminate_loop = 1;
		}
	} else {
		unsigned char *uu, *wd;
		initialize_all_subsystems_2();
		close(terminal_pipe[0]);
		close(terminal_pipe[1]);
		if (!*u) {
			fprintf(stderr, "URL expected after %s\n.", dmp == D_DUMP ? "-dump" : "-source");
			retval = RET_SYNTAX;
			goto tttt;
		}
		if (!(uu = translate_url(u, wd = get_cwd()))) uu = stracpy(u);
		request_object(NULL, uu, NULL, PRI_MAIN, NC_RELOAD, end_dump, NULL, &dump_obj);
		mem_free(uu);
		if (wd) mem_free(wd);
	}
}

/* Is called before gaphics driver init */
static void initialize_all_subsystems(void)
{
	init_trans();
	set_sigcld();
	init_home();
	init_dns();
	init_cache();
	iinit_bfu();
	memset(&dd_opt, 0, sizeof dd_opt);
}

/* Is called sometimes after and sometimes before graphics driver init */
static void initialize_all_subsystems_2(void)
{
	GF(init_dip());
	init_bfu();
	GF(init_imgcache());
	init_fcache();
	GF(init_grview());
}

static void terminate_all_subsystems(void)
{
	if (!F) af_unix_close();
	check_bottom_halves();
	abort_all_downloads();
#ifdef HAVE_SSL
	ssl_finish();
#endif
	check_bottom_halves();
	destroy_all_terminals();
	check_bottom_halves();
	shutdown_bfu();
	if (!F) free_all_itrms();
	release_object(&dump_obj);
	abort_all_connections();

	free_all_caches();
	if (init_b) save_url_history();
	free_history_lists();
	free_term_specs();
	free_types();
	free_blocks();
	if (init_b) finalize_bookmarks();
	free_conv_table();
	free_blacklist();
	if (init_b) cleanup_cookies();
	cleanup_auth();
	check_bottom_halves();
	end_config();
	free_strerror_buf();
	shutdown_trans();
	GF(shutdown_graphics());
	terminate_osdep();
	if (clipboard) mem_free(clipboard);
	if (fg_poll_timer != -1) kill_timer(fg_poll_timer), fg_poll_timer = -1;
}

int main(int argc, char *argv[])
{
	g_argc = argc;
	g_argv = (unsigned char **)argv;

	init_os();

	get_path_to_exe();

	select_loop(init);
	terminate_all_subsystems();

	check_memory_leaks();
	return retval;
}

