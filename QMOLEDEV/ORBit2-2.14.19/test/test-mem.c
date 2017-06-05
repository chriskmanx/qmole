#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#if  defined(_POSIX_SOURCE) 
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#endif 

#include <orbit/orbit.h>

#include "test1.h"

#define ORBIT_EX(ev) ((ev)->_major != CORBA_NO_EXCEPTION && (g_warning("%s", CORBA_exception_id(ev)),TRUE))
#define ORBIT_EX_IS_A(ev,id) (((ev)->_major != CORBA_NO_EXCEPTION  && strcmp(id, CORBA_exception_id(ev)) == 0) || (g_warning("%s", CORBA_exception_id(ev)),FALSE))

/*** App-specific servant structures ***/

typedef struct
{
   POA_Test servant;

   /* ------ add private attributes here ------ */
   CORBA_octet buf[1024]; /* big enough to see mem-leaks fast*/ 
   /* ------ ---------- end ------------ ------ */
} impl_POA_Test;

/*** Implementation stub prototypes ***/
static void impl_Test__destroy(impl_POA_Test * servant,
                               CORBA_Environment * ev);

static CORBA_string
impl_Test_op(impl_POA_Test * servant,
	     const CORBA_char * astr, CORBA_Environment * ev);

/*** epv structures ***/

static PortableServer_ServantBase__epv impl_Test_base_epv = {
   NULL,			/* _private data */
   (gpointer) & impl_Test__destroy,     /* finalize routine */
   NULL,			/* default_POA routine */
};
static POA_Test__epv impl_Test_epv = {
   NULL,			/* _private */
   (gpointer) & impl_Test_op,

};

/*** vepv structures ***/

static POA_Test__vepv impl_Test_vepv = {
   &impl_Test_base_epv,
   &impl_Test_epv
};

/*** Stub implementations ***/
static void
impl_Test__destroy(impl_POA_Test * servant, CORBA_Environment * ev)
{
   POA_Test__fini((PortableServer_Servant) servant, ev);
   g_free(servant);
}
 


static CORBA_string
impl_Test_op(impl_POA_Test * servant,
	     const CORBA_char * astr, CORBA_Environment * ev)
{
   CORBA_string retval;

   /* ------   insert method code here   ------ */
   retval = CORBA_string_dup ("blahblah");
   /* ------ ---------- end ------------ ------ */

   return retval;
}

static void
test_ORBit_alloc (void)
{
	gpointer p;
	int      i;

	p = ORBit_alloc_string (100);
	g_assert ((gulong)p & 0x1);
	for (i = 0; i < 100; i++)
		((guchar *)p) [i] = i;
	CORBA_free (p);

	p = CORBA_string_dup ("Foo");
	g_assert (((gulong)p & 0x1));
	CORBA_free (p);

	p = ORBit_alloc_simple (100);
	g_assert (!((gulong)p & 0x1));
	for (i = 0; i < 100; i++)
		((guchar *)p) [i] = i;
	CORBA_free (p);

	p = ORBit_alloc_tcval (TC_CORBA_sequence_CORBA_octet, 1);
	g_assert (!((gulong)p & 0x1));
	CORBA_free (p);

	p = ORBit_alloc_tcval (TC_ORBit_IInterface, 8);
	g_assert (!((gulong)p & 0x1));
	CORBA_free (p);
}


static void
test_ORBit_sequence (void) {
	gpointer seq 
		= ORBit_sequence_alloc (TC_CORBA_sequence_CORBA_octet,
					1000);
	CORBA_free (seq); 
}

static void
test_activate_deactivate (PortableServer_POA poa, CORBA_Environment * ev)
{
   impl_POA_Test *newservant;
   PortableServer_ObjectId *objid;

   newservant = g_new0(impl_POA_Test, 1);
   newservant->servant.vepv = &impl_Test_vepv;

   POA_Test__init((PortableServer_Servant) newservant, ev);

   objid = PortableServer_POA_activate_object(poa, newservant, ev);
   g_assert (!ORBIT_EX (ev));

   PortableServer_POA_deactivate_object (poa, objid, ev);
   g_assert (!ORBIT_EX (ev));

   CORBA_free(objid);
}

static CORBA_ORB  global_orb = CORBA_OBJECT_NIL; /* global orb */

static void 
server_shutdown (int sig) {
        CORBA_Environment  local_ev[1];
        CORBA_exception_init(local_ev);
	
        if (global_orb != CORBA_OBJECT_NIL) {
                CORBA_ORB_shutdown (global_orb, FALSE, local_ev);
		g_assert (!ORBIT_EX (local_ev));
        }
}

static long 
get_procmem (void) {
#if  defined(_POSIX_SOURCE)
	long ret=0;
	char cmd[255];	
	FILE *handle = NULL;
	
 	sprintf (cmd, "ps --no-headers -o \"rss\" -p %d 2>/dev/null", getpid());
	handle = popen (cmd, "r");
	if (!handle)
		return 0;
	
	switch (fscanf (handle, "%ld", &ret)) {
	case EOF:
		g_warning ("EOF, executing \"%s\" failed", cmd);
		break;
	case 0:
		g_warning ("parse error, executing \"%s\" failed", cmd);
		break;
	default:
		break;
	}
	pclose (handle);	
	return ret;
#else
	return 0;
#endif
}

#define LEAK_DETECT_WITH_TOLERANCE(N,CALL,TOLERANCE) do {                  \
		long mem_usage_start = 0;         \
		long mem_usage_end   = 0;         \
		long i = 0;                       \
		mem_usage_start = get_procmem (); \
		for (i = 0; i < (N); ++i) {       \
		        (CALL);                   \
		}                                 \
		mem_usage_end = get_procmem ();   \
		g_print ("mem usage prev/post: "  \
			 "%5ldKB / %5ldKB -- %5dx " #CALL "\n", \
			 mem_usage_start,         \
			 mem_usage_end,           \
                         N);                      \
                g_assert ( mem_usage_end - mem_usage_start < TOLERANCE); \
} while(FALSE)                              

static
void
main_func(int argc, char *argv[]) {
        PortableServer_POA         poa         = CORBA_OBJECT_NIL;
        PortableServer_POAManager  poa_manager = CORBA_OBJECT_NIL;
        CORBA_Environment  ev[1];
        CORBA_exception_init(ev);
	
	/* init signal handling */
	
        signal(SIGINT,  server_shutdown);
        signal(SIGTERM, server_shutdown);
	
        /* create Object Request Broker (ORB) */
	
        global_orb = CORBA_ORB_init(&argc, 
				    argv, 
				    "orbit-local-treaded-orb", 
				    ev);
        g_assert (!ORBIT_EX (ev));
	
        /* get Portable Object Adaptor (POA) */
	
        poa = (PortableServer_POA) 
          CORBA_ORB_resolve_initial_references(global_orb,
					       "RootPOA",
					       ev);
        g_assert (!ORBIT_EX (ev));

        /* activate POA Manager */
	
	poa_manager = PortableServer_POA__get_the_POAManager(poa, ev);
        g_assert (!ORBIT_EX (ev));
	
        PortableServer_POAManager_activate(poa_manager, ev);
        g_assert (!ORBIT_EX (ev));
	
 	LEAK_DETECT_WITH_TOLERANCE (1000, test_ORBit_alloc (), 50); 
 	LEAK_DETECT_WITH_TOLERANCE (1000, test_ORBit_sequence (), 50); 
	LEAK_DETECT_WITH_TOLERANCE (1000, test_activate_deactivate (poa, ev), 50);

        /* tear down the ORB */
	CORBA_Object_release ((CORBA_Object) poa_manager, ev);
	g_assert (!ORBIT_EX (ev));

	CORBA_Object_release ((CORBA_Object) poa, ev);
	g_assert (!ORBIT_EX (ev));

        if (global_orb != CORBA_OBJECT_NIL) {
                /* going to destroy orb.. */
                CORBA_ORB_destroy(global_orb, ev);
		g_assert (!ORBIT_EX (ev));
        }
}

int 
main(int argc, char *argv[]) {
	/* ORBit2-2.7.6 leaks on shutdown with about 700KB */
	/* LEAK_DETECT_WITH_TOLERANCE (1, main_func (argc,argv), 20); */
	g_thread_init (NULL);
	main_func (argc, argv);
	exit (0);
}

