/* error.c
 * (c) 2002 Mikulas Patocka
 * This file is a part of the Links program, released under GPL.
 */

#include "links.h"

#if DEBUGLEVEL >= 2
#define RED_ZONE	'R'
#define FREE_FILL	0xfe
#define REALLOC_FILL	0xfd
#define ALLOC_FILL	0xfc
#endif

#if DEBUGLEVEL < 0
#define FREE_FILL	0xfe
#endif

#if DEBUGLEVEL < 0
#define NO_IE
#endif

#ifdef RED_ZONE
#define RED_ZONE_INC	1
#else
#define RED_ZONE_INC	0
#endif

volatile char dummy_val;
volatile char * volatile dummy_ptr = &dummy_val;
volatile char * volatile x_ptr;

void *do_not_optimize_here(void *p)
{
	/* break ANSI aliasing */
	x_ptr = p;
	*dummy_ptr = 0;
	return p;
}

#ifdef LEAK_DEBUG

long mem_amount = 0;
#define ALLOC_MAGIC		0xa110c
#define ALLOC_FREE_MAGIC	0xf4ee
#define ALLOC_REALLOC_MAGIC	0x4ea110c

#ifndef LEAK_DEBUG_LIST
struct alloc_header {
	int magic;
	size_t size;
};
#else
struct alloc_header {
	struct alloc_header *next;
	struct alloc_header *prev;
	size_t size;
	int magic;
	int line;
	unsigned char *file;
	unsigned char *comment;
};
static struct list_head memory_list = { &memory_list, &memory_list };
#endif

#define L_D_S ((sizeof(struct alloc_header) + 15) & ~15)

#endif

static inline void force_dump(void)
{
	fprintf(stderr, "\n\033[1m%s\033[0m\n", "Forcing core dump");
	fflush(stdout);
	fflush(stderr);
	fatal_tty_exit();
	raise(SIGSEGV);
}

void check_memory_leaks(void)
{
#if defined(NO_IE)
	return;
#else
#ifdef LEAK_DEBUG
	if (mem_amount) {
		fprintf(stderr, "\n\033[1mMemory leak by %ld bytes\033[0m\n", mem_amount);
#ifdef LEAK_DEBUG_LIST
		fprintf(stderr, "\nList of blocks: ");
		{
			int r = 0;
			struct alloc_header *ah;
			foreach (ah, memory_list) {
				fprintf(stderr, "%s%p:%lu @ %s:%d", r ? ", ": "", (unsigned char *)ah + L_D_S, (unsigned long)ah->size, ah->file, ah->line), r = 1;
				if (ah->comment) fprintf(stderr, ":\"%s\"", ah->comment);
			}
			fprintf(stderr, "\n");
		}
#endif
		force_dump();
	}
#endif
#endif
}

static void er(int b, unsigned char *m, va_list l)
{
	if (b) fprintf(stderr, "%c", (unsigned char)7);
#ifdef HAVE_VPRINTF
	vfprintf(stderr, m, l);
#else
	fprintf(stderr, "%s", m);
#endif
	fprintf(stderr, "\n");
	sleep(1);
}

void error(unsigned char *m, ...)
{
	va_list l;
	va_start(l, m);
	er(1, m, l);
	va_end(l);
}

int errline;
unsigned char *errfile;

static unsigned char errbuf[4096];

void int_error(unsigned char *m, ...)
{
#ifdef NO_IE
	return;
#else
	va_list l;
	va_start(l, m);
	sprintf(errbuf, "\033[1mINTERNAL ERROR\033[0m at %s:%d: ", errfile, errline);
	strcat(errbuf, m);
	er(1, errbuf, l);
	force_dump();
	va_end(l);
#endif
}

void debug_msg(unsigned char *m, ...)
{
	va_list l;
	va_start(l, m);
	sprintf(errbuf, "DEBUG MESSAGE at %s:%d: ", errfile, errline);
	strcat(errbuf, m);
	er(0, errbuf, l);
	va_end(l);
}

#ifdef LEAK_DEBUG

void *debug_mem_alloc(unsigned char *file, int line, size_t size)
{
	void *p;
#ifdef LEAK_DEBUG
	struct alloc_header *ah;
#endif
	debug_test_free(file, line);
	if (!size) return DUMMY;
	if (size > MAXINT) overalloc();
#ifdef LEAK_DEBUG
	mem_amount += size;
	size += L_D_S;
#endif
	retry:
	if (!(p = malloc(size + RED_ZONE_INC))) {
		out_of_memory("malloc", size + RED_ZONE_INC);
		goto retry;
	}
#ifdef RED_ZONE
	*((unsigned char *)p + size + RED_ZONE_INC - 1) = RED_ZONE;
#endif
#ifdef LEAK_DEBUG
	ah = p;
	p = (unsigned char *)p + L_D_S;
	ah->size = size - L_D_S;
	ah->magic = ALLOC_MAGIC;
#ifdef LEAK_DEBUG_LIST
	ah->file = file;
	ah->line = line;
	ah->comment = NULL;
	add_to_list(memory_list, ah);
#endif
#endif
#ifdef ALLOC_FILL
	memset(p, ALLOC_FILL, size - L_D_S);
#endif
	return p;
}

void *debug_mem_calloc(unsigned char *file, int line, size_t size)
{
	void *p;
#ifdef LEAK_DEBUG
	struct alloc_header *ah;
#endif
	debug_test_free(file, line);
	if (!size) return DUMMY;
	if (size > MAXINT) overalloc();
#ifdef LEAK_DEBUG
	mem_amount += size;
	size += L_D_S;
#endif
	retry:
	if (!(p = x_calloc(size + RED_ZONE_INC))) {
		out_of_memory("calloc", size + RED_ZONE_INC);
		goto retry;
	}
#ifdef RED_ZONE
	*((unsigned char *)p + size + RED_ZONE_INC - 1) = RED_ZONE;
#endif
#ifdef LEAK_DEBUG
	ah = p;
	p = (unsigned char *)p + L_D_S;
	ah->size = size - L_D_S;
	ah->magic = ALLOC_MAGIC;
#ifdef LEAK_DEBUG_LIST
	ah->file = file;
	ah->line = line;
	ah->comment = NULL;
	add_to_list(memory_list, ah);
#endif
#endif
	return p;
}

void debug_mem_free(unsigned char *file, int line, void *p)
{
#ifdef LEAK_DEBUG
	struct alloc_header *ah;
#endif
	if (p == DUMMY) return;
	if (!p) {
		errfile = file, errline = line, int_error("mem_free(NULL)");
		return;
	}
#ifdef LEAK_DEBUG
	p = (unsigned char *)p - L_D_S;
	ah = p;
	if (ah->magic != ALLOC_MAGIC) {
		errfile = file, errline = line, int_error("mem_free: magic doesn't match: %08x", ah->magic);
		return;
	}
#ifdef FREE_FILL
	memset((unsigned char *)p + L_D_S, FREE_FILL, ah->size);
#endif
	ah->magic = ALLOC_FREE_MAGIC;
#ifdef LEAK_DEBUG_LIST
	del_from_list(ah);
	if (ah->comment) free(ah->comment);
#endif
	mem_amount -= ah->size;
#endif
#ifdef RED_ZONE
	if (*((unsigned char *)p + L_D_S + ah->size + RED_ZONE_INC - 1) != RED_ZONE) {
		errfile = file, errline = line, int_error("mem_free: red zone damaged: %02x (block allocated at %s:%d:%s)", *((unsigned char *)p + L_D_S + ah->size + RED_ZONE_INC - 1),
#ifdef LEAK_DEBUG_LIST
		ah->file, ah->line, ah->comment);
#else
		"-", 0, "-");
#endif
		return;
	}
#endif
	free(p);
}

void *debug_mem_realloc(unsigned char *file, int line, void *p, size_t size)
{
#ifdef LEAK_DEBUG
	struct alloc_header *ah;
#endif
	void *np;
	if (p == DUMMY) return debug_mem_alloc(file, line, size);
	debug_test_free(file, line);
	if (!p) {
		errfile = file, errline = line, int_error("mem_realloc(NULL, %d)", size);
		return NULL;
	}
	if (!size) {
		debug_mem_free(file, line, p);
		return DUMMY;
	}
	if (size > MAXINT) overalloc();
#ifdef LEAK_DEBUG
	p = (unsigned char *)p - L_D_S;
	ah = p;
	if (ah->magic != ALLOC_MAGIC) {
		errfile = file, errline = line, int_error("mem_realloc: magic doesn't match: %08x", ah->magic);
		return NULL;
	}
	ah->magic = ALLOC_REALLOC_MAGIC;
#ifdef REALLOC_FILL
	if (size < (size_t)ah->size) memset((unsigned char *)p + L_D_S + size, REALLOC_FILL, ah->size - size);
#endif
#endif
#ifdef RED_ZONE
	if (*((unsigned char *)p + L_D_S + ah->size + RED_ZONE_INC - 1) != RED_ZONE) {
		errfile = file, errline = line, int_error("mem_realloc: red zone damaged: %02x (block allocated at %s:%d:%s)", *((unsigned char *)p + L_D_S + ah->size + RED_ZONE_INC - 1),
#ifdef LEAK_DEBUG_LIST
		ah->file, ah->line, ah->comment);
#else
		"-", 0, "-");
#endif
		return (unsigned char *)p + L_D_S;
	}
#endif
	retry:
	if (!(np = realloc(p, size + L_D_S + RED_ZONE_INC))) {
		out_of_memory("realloc", size + L_D_S + RED_ZONE_INC);
		goto retry;
	}
	p = np;
#ifdef RED_ZONE
	*((unsigned char *)p + size + L_D_S + RED_ZONE_INC - 1) = RED_ZONE;
#endif
#ifdef LEAK_DEBUG
	ah = p;
	mem_amount += size - ah->size;
	ah->size = size;
	ah->magic = ALLOC_MAGIC;
#ifdef LEAK_DEBUG_LIST
	ah->prev->next = ah;
	ah->next->prev = ah;
#endif
#endif
	return (unsigned char *)p + L_D_S;
}

void set_mem_comment(void *p, unsigned char *c, int l)
{
#ifdef LEAK_DEBUG_LIST
	struct alloc_header *ah = (struct alloc_header *)((unsigned char *)p - L_D_S);
	if (ah->comment) free(ah->comment);
	if ((ah->comment = malloc(l + 1))) memcpy(ah->comment, c, l), ah->comment[l] = 0;
#endif
}

#ifdef JS
unsigned char *get_mem_comment(void *p)
{
#ifdef LEAK_DEBUG_LIST
	/* perm je prase: return ((struct alloc_header*)((unsigned char*)((void*)((unsigned char*)p-sizeof(int))) - L_D_S))->comment;*/
	struct alloc_header *ah = (struct alloc_header *)((unsigned char *)p - L_D_S);
	if (!ah->comment) return "";
	else return ah->comment;
#else
	return "";
#endif
}
#endif

#else

void *mem_alloc(size_t size)
{
	void *p;
	debug_test_free(NULL, 0);
	if (!size) return DUMMY;
	if (size > MAXINT) overalloc();
	retry:
	if (!(p = malloc(size))) {
		out_of_memory("malloc", size);
		goto retry;
	}
	return p;
}

void *mem_calloc(size_t size)
{
	void *p;
	debug_test_free(NULL, 0);
	if (!size) return DUMMY;
	if (size > MAXINT) overalloc();
	retry:
	if (!(p = x_calloc(size))) {
		out_of_memory("calloc", size);
		goto retry;
	}
	return p;
}

void mem_free(void *p)
{
	if (p == DUMMY) return;
	if (!p) {
		internal((unsigned char *)"mem_free(NULL)");
		return;
	}
	free(p);
}

void *mem_realloc(void *p, size_t size)
{
	void *np;
	if (p == DUMMY) return mem_alloc(size);
	debug_test_free(NULL, 0);
	if (!p) {
		internal((unsigned char *)"mem_realloc(NULL, %d)", size);
		return NULL;
	}
	if (!size) {
		mem_free(p);
		return DUMMY;
	}
	if (size > MAXINT) overalloc();
	retry:
	if (!(np = realloc(p, size))) {
		out_of_memory("realloc", size);
		goto retry;
	}
	return np;
}

#endif

#ifdef OOPS

struct prot {
	struct prot *next;
	struct prot *prev;
	sigjmp_buf buf;
};

static struct list_head prot = {&prot, &prot };

static void fault(void *dummy)
{
	struct prot *p;
	/*fprintf(stderr, "FAULT: %d !\n", (int)(unsigned long)dummy);*/
	if (list_empty(prot)) {
		fatal_tty_exit();
		exit(0);
	}
	p = prot.next;
	del_from_list(p);
	longjmp(p->buf, 1);
}

sigjmp_buf *new_stack_frame(void)
{
	static int handled = 0;
	struct prot *new;
	if (!handled) {
		install_signal_handler(SIGSEGV, fault, (void *)SIGSEGV, 1);
		install_signal_handler(SIGBUS, fault, (void *)SIGBUS, 1);
		install_signal_handler(SIGFPE, fault, (void *)SIGFPE, 1);
		install_signal_handler(SIGILL, fault, (void *)SIGILL, 1);
		install_signal_handler(SIGABRT, fault, (void *)SIGABRT, 1);
		handled = 1;
	}
	if (!(new = mem_alloc(sizeof(struct prot)))) return NULL;
	add_to_list(prot, new);
	return &new->buf;
}

void xpr(void)
{
	if (!list_empty(prot)) {
		struct prot *next = prot.next;
		del_from_list(next);
		mem_free(next);
	}
}

void nopr(void)
{
	free_list(prot);
}

#endif

#if !(defined(LEAK_DEBUG) && defined(LEAK_DEBUG_LIST))

unsigned char *memacpy(const unsigned char *src, size_t len)
{
	unsigned char *m;
	m = (unsigned char *)mem_alloc(len + 1);
	memcpy(m, src, len);
	m[len] = 0;
	return m;
}

unsigned char *stracpy(const unsigned char *src)
{
	return src ? memacpy(src, src != DUMMY ? strlen(src) : 0) : NULL;
}

#else

unsigned char *debug_memacpy(unsigned char *f, int l, unsigned char *src, size_t len)
{
	unsigned char *m;
	m = (unsigned char *)debug_mem_alloc(f, l, len + 1);
	memcpy(m, src, len);
	m[len] = 0;
	return m;
}

unsigned char *debug_stracpy(unsigned char *f, int l, unsigned char *src)
{
	return src ? (unsigned char *)debug_memacpy(f, l, src, src != DUMMY ? strlen(src) : 0L) : NULL;
}

#endif

int snprint(unsigned char *s, int n, off_t num)
{
	off_t q = 1;
	while (q <= num / 10) q *= 10;
	while (n-- > 1 && q) *(s++) = num / q + '0', num %= q, q /= 10;
	*s = 0;
	return !!q;
}

int snzprint(unsigned char *s, int n, off_t num)
{
	if (n > 1 && num < 0) *(s++) = '-', num = -num, n--;
	return snprint(s, n, num);
}

void add_to_strn(unsigned char **s, unsigned char *a)
{
	unsigned char *p;
	size_t l1 = strlen(*s), l2 = strlen(a);
	if (((l1 | l2) | (l1 + l2 + 1)) > MAXINT) overalloc();
	p = (unsigned char *)mem_realloc(*s, l1 + l2 + 1);
	strcat(p, a);
	*s = p;
}

void extend_str(unsigned char **s, int n)
{
	size_t l = strlen(*s);
	if (((l | n) | (l + n + 1)) > MAXINT) overalloc();
	*s = (unsigned char *)mem_realloc(*s, l + n + 1);
}

void add_to_str(unsigned char **s, int *l, unsigned char *a)
{
	unsigned char *p=*s;
	unsigned old_length;
	size_t new_length;
	unsigned x;

	old_length=*l;
	new_length=strlen(a);
	if (new_length + old_length >= MAXINT / 2 || new_length + old_length < new_length) overalloc();
	new_length+=old_length;
	*l=new_length;
	x=old_length^new_length;
	if (x>=old_length){
		/* Need to realloc */
		new_length|=(new_length>>1);
		new_length|=(new_length>>2);
		new_length|=(new_length>>4);
		new_length|=(new_length>>8);
		new_length|=(new_length>>16);
		p=(unsigned char *)mem_realloc(p,new_length+1L);
	}
	*s=p;
	strcpy((p+old_length),a);
}

void add_bytes_to_str(unsigned char **s, int *l, unsigned char *a, size_t ll)
{
	unsigned char *p=*s;
	unsigned long old_length;
	unsigned long new_length;
	unsigned long x;

	old_length=*l;
	if (ll + old_length >= (unsigned)MAXINT / 2 || ll + old_length < (unsigned)ll) overalloc();
	new_length=old_length+ll;
	*l=new_length;
	x=old_length^new_length;
	if (x>=old_length){
		/* Need to realloc */
		new_length|=(new_length>>1);
		new_length|=(new_length>>2);
		new_length|=(new_length>>4);
		new_length|=(new_length>>8);
		new_length|=(new_length>>16);
		p=(unsigned char *)mem_realloc(p,new_length+1);
	}
	*s=p;
	memcpy(p+old_length,a,ll);
	p[*l]=0;
}

void add_chr_to_str(unsigned char **s, int *l, unsigned char a)
{
	unsigned char *p=*s;
	unsigned long old_length;
	unsigned long new_length;
	unsigned long x;

	old_length=*l;
	if (1 + old_length >= MAXINT / 2 || 1 + old_length < 1) overalloc();
	new_length=old_length+1;
	*l=new_length;
	x=old_length^new_length;
	if (x>=old_length){
		p=(unsigned char *)mem_realloc(p,new_length<<1);
	}
	*s=p;
	p[old_length]=a;
	p[new_length]=0;
}

void add_num_to_str(unsigned char **s, int *l, off_t n)
{
	unsigned char a[64];
	/*sprintf(a, "%d", n);*/
	snzprint(a, 64, n);
	add_to_str(s, l, a);
}

void add_knum_to_str(unsigned char **s, int *l, off_t n)
{
	unsigned char a[13];
	if (n && n / (1024 * 1024) * (1024 * 1024) == n) snzprint(a, 12, n / (1024 * 1024)), a[strlen(a) + 1] = 0, a[strlen(a)] = 'M';
	else if (n && n / 1024 * 1024 == n) snzprint(a, 12, n / 1024), a[strlen(a) + 1] = 0, a[strlen(a)] = 'k';
	else snzprint(a, 13, n);
	add_to_str(s, l, a);
}

long strtolx(unsigned char *c, unsigned char **end)
{
	long l;
	if (c[0] == '0' && upcase(c[1]) == 'X' && c[2]) l = strtol(c + 2, (char **)end, 16);
	else l = strtol(c, (char **)end, 10);
	if (!*end) return l;
	if (upcase(**end) == 'K') {
		(*end)++;
		if (l < -MAXINT / 1024) return -MAXINT;
		if (l > MAXINT / 1024) return MAXINT;
		return l * 1024;
	}
	if (upcase(**end) == 'M') {
		(*end)++;
		if (l < -MAXINT / (1024 * 1024)) return -MAXINT;
		if (l > MAXINT / (1024 * 1024)) return MAXINT;
		return l * (1024 * 1024);
	}
	return l;
}

/* Copies at most dst_size chars into dst. Ensures null termination of dst. */
unsigned char *safe_strncpy(unsigned char *dst, const unsigned char *src, size_t dst_size)
{
	size_t to_copy;
	if (!dst_size) return dst;
	to_copy = strlen(src);
	if (to_copy >= dst_size) to_copy = dst_size - 1;
	memcpy(dst, src, to_copy);
	memset(dst + to_copy, 0, dst_size - to_copy);
	return dst;
}

#ifdef JS
/* deletes all nonprintable characters from string */
void skip_nonprintable(unsigned char *txt)
{
	unsigned char *txt1=txt;

	if (!txt||!*txt)return;
	for (;*txt;txt++)
		switch(*txt)
		{
			case 1:
			case 2:
			case 3:
			case 4:
			case 5:
			case 6:
			case 7:
			case 8:
			case 11:
			case 12:
			case 13:
			case 14:
			case 15:
			case 16:
			case 17:
			case 18:
			case 19:
			case 20:
			case 21:
			case 22:
			case 23:
			case 24:
			case 25:
			case 26:
			case 27:
			case 28:
			case 29:
			case 30:
			case 31:
			break;

			case 9:
			*txt1=' ';
			txt1++;
			break;

			default:
			*txt1=*txt;
			txt1++;
			break;
		}
	*txt1=0;
}
#endif

/* case insensitive compare of 2 strings */
/* comparison ends after len (or less) characters */
/* return value: 1=strings differ, 0=strings are same */
int casecmp(unsigned char *c1, unsigned char *c2, size_t len)
{
	size_t i;
	for (i = 0; i < len; i++) if (upcase(c1[i]) != upcase(c2[i])) return 1;
	return 0;
}

int casestrstr(unsigned char *h, unsigned char *n)
{
	unsigned char *p;

	for (p=h;*p;p++)
	{
		if (!srch_cmp(*p,*n))  /* same */
		{
			unsigned char *q, *r;
			for (q=n, r=p;*r&&*q;)
			{
				if (!srch_cmp(*q,*r)) r++,q++;    /* same */
				else break;
			}
			if (!*q) return 1;
		}
	}

	return 0;
}

