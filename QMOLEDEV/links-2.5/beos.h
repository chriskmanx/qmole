int be_socket(int, int, int);
int be_connect(int, struct sockaddr *, int);
int be_getpeername(int, struct sockaddr *, int *);
int be_getsockname(int, struct sockaddr *, int *);
int be_listen(int, int);
int be_accept(int, struct sockaddr *, int *);
int be_bind(int, struct sockaddr *, int);
int be_pipe(int *);
int be_read(int, void *, int);
int be_write(int, void *, int);
int be_close(int);
int be_select(int, struct fd_set *, struct fd_set *, struct fd_set *, struct timeval *);
int be_getsockopt(int, int, int, void *, int *);

int start_thr(void (*fn)(void *), void *data, unsigned char *name);

int get_input_handle(void);
void block_stdin(void);
void unblock_stdin(void);

void error(unsigned char *, ...);
void fatal_tty_exit(void);
