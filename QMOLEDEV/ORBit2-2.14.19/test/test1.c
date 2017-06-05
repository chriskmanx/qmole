#include "test1.h"

int main(int argc, char *argv[])
{
	CORBA_ORB orb;
	CORBA_Environment ev;
	CORBA_exception_init(&ev);
	orb = CORBA_ORB_init(&argc, argv, "orbit-local-orb", &ev);
	return 0;
}
