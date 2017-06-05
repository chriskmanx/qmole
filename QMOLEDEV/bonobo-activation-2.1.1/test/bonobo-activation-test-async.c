/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include <bonobo-activation/bonobo-activation.h>

#define DEBUG_TIMEOUT 2
#define DEBUG_TIME    1

typedef struct {
        gboolean callback_called;
        gboolean succeeded;
} callback_data_t;


static void
test_callback (CORBA_Object   activated_object, 
               const char    *error_reason, 
               gpointer       user_data)
{
        callback_data_t *data;

        data = (callback_data_t *) user_data;

        if (activated_object == CORBA_OBJECT_NIL) {
                data->succeeded = FALSE;
        } else {
                CORBA_Environment ev;

                CORBA_exception_init (&ev);
                CORBA_Object_release (activated_object, &ev);
                CORBA_exception_free (&ev);

                data->succeeded = TRUE;
        }
                
        data->callback_called = TRUE;
}


/* returns TRUE in case of success. FALSE otherwise. 
   -1 if answer timeouted.... */
static int
test_activate (char *requirements)
{
        CORBA_Environment ev;
        callback_data_t data;
#if DEBUG_TIME        
        time_t beg_time;
#endif

        CORBA_exception_init (&ev);

        data.callback_called = FALSE;
        bonobo_activation_activate_async (requirements, NULL, 0, test_callback, &data, &ev);

#if DEBUG_TIME
        beg_time = time (NULL);
#endif

        while (data.callback_called == FALSE) {
                g_main_context_iteration (NULL, FALSE);
#if DEBUG_TIME
                if (time (NULL) > (beg_time + DEBUG_TIMEOUT)) {
                        return -1;
                }
#endif
        }

        
        if (data.succeeded == TRUE) {
                return TRUE;
        } else {
                return FALSE;
        }
}

/* returns TRUE in case of success. FALSE otherwise. 
   -1 if answer timeouted.... */
static int
test_activate_from_id (char *aid)
{
        CORBA_Environment ev;
        callback_data_t data;
#if DEBUG_TIME        
        time_t beg_time;
#endif

        CORBA_exception_init (&ev);

        data.callback_called = FALSE;
        bonobo_activation_activate_from_id_async (aid, 0, test_callback, &data, &ev);

#if DEBUG_TIME
        beg_time = time (NULL);
#endif

        while (data.callback_called == FALSE) {
                g_main_context_iteration (NULL, FALSE);
#if DEBUG_TIME
                if (time (NULL) > (beg_time + DEBUG_TIMEOUT)) {
                        return -1;
                }
#endif
        }

        
        if (data.succeeded == TRUE) {
                return TRUE;
        } else {
                return FALSE;
        }
}


#define TOTAL_TESTS 4
int
main (int argc, char *argv[])
{
        int test_status;
        int test_passed;

        test_passed = 0;

	bonobo_activation_init (argc, argv);
        printf ("testing async interfaces\n");

        printf ("testing activate_async... ");
        /* this should fail */
        test_status = test_activate ("");
        if (test_status == FALSE) {
                test_passed++;
                printf (" passed\n");
        } else if (test_status == TRUE
                   || test_status == -1) {
                printf (" failed\n");
        }

        printf ("testing activate_async... ");
        test_status = test_activate ("has (repo_ids, 'IDL:Empty:1.0')");
        if (test_status == TRUE) {
                test_passed++;
                printf (" passed\n");
        } else if (test_status == FALSE
                   || test_status == -1) {
                printf (" failed\n");
        }

        printf ("testing activate_from_id_async... ");
        test_status = test_activate_from_id ("");
        if (test_status == FALSE) {
                test_passed++;
                printf (" passed\n");
        } else if (test_status == TRUE
                   || test_status == -1) {
                printf (" failed\n");
        }

        printf ("testing activate_from_id_async... ");
        test_status = test_activate_from_id ("OAFIID:Empty:19991025");
        if (test_status == TRUE) {
                test_passed++;
                printf (" passed\n");
        } else if (test_status == FALSE
                   || test_status == -1) {
                printf (" failed\n");
        }

        printf ("Async Test Results: %d passed upon %d \n", 
                test_passed, TOTAL_TESTS);

        if (test_passed != TOTAL_TESTS) {
                return 1;
        }

        if (bonobo_activation_debug_shutdown ()) {
                return 0;
        } else {
                return 1;
        }
}
