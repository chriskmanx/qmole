/* protable signal() like */

#include <signal.h>

/* Portable signal() from R.Stevens,
 * modified to reset the handler */
void (*Signal(int signo, void (*func)(int)))(int)
{
	struct sigaction act, oact;

	act.sa_handler = func;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0; /* So if set SA_RESETHAND is cleared */
	if (signo == SIGALRM)
	{
#ifdef SA_INTERRUPT
		act.sa_flags |= SA_INTERRUPT;   /* SunOS 4.x */
#endif
	}
	else
	{
#ifdef SA_RESTART
		act.sa_flags |= SA_RESTART;     /* SVR4, 4.4BSD, Linux */
#endif
	}
	if (sigaction(signo, &act, &oact) == -1)
		return SIG_ERR;
	return (oact.sa_handler);
}
