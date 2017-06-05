/* -*- Mode: C; indent-tabs-mode:nil; c-basic-offset:8 -*- */
/**
 *This test example shows how to use the SAC api.
 *
 *This features a simple parser that prints the selector list
 *and the properties list of each CSS ruleset found.
 *It also prints the number of properties found in each CSS ruleset.
 *
 *At the end of the parsing, it a prints the number of rulesets found
 *in the CSS document.
 *
 *To to this, several handler callbacks have been registered.
 *We have also created a data structure that we call a parsing context.
 *This parsing context stores the data necessary to achieve the calculation
 *done during the parsing (count the number of rulesets and the number
 *of properties per ruleset)
 *The parsing context itself is stored in the "app_data" field of the sac handler.
 *This field is there to provide applications with a
 *place to store the custom data they want to be able to get/set during the parsing.
 *
 *To compile this file, type:
 *
 *gcc -g  -Wall -o sac-example-2 `croco-0.6-config --cflags`  `croco-0.6-config --libs` sac-example-2.c
 *
 *Make sure you have compiled and installed libcroco prior to trying to
 *compile this file :)
 *
 *Once you have compiled it, type:
 *
 *./sac-example-2 <a-path-to-a-css-file>
 *
 *to try it on a CSS file of your choice.
 *
 *Initial Author: Dodji Seketeli <Dodji 47 seketeli dot org>
 */
#include <string.h>
#include <stdlib.h>
#include <libcroco/libcroco.h>

/**
 *A Context that will hold the
 *variables necessary to count the number
 *of rulesets and the number of properties
 *per ruleset.
 */
struct MyFooContext
{
        /**the total number of rulesets in the stylesheet.*/
        int nb_rulesets ;
        
        /**the number of property per ruleset.*/
        int nb_props_per_ruleset ;
} ;

/**
 *This callback is called only once, at the begining of
 *the CSS Document.
 *So here, we will allocate a custom parsing context
 *where we will store data necessary for us to
 *count the number of rulesets and properties found in
 *the CSS document.
 *@param a_handler the sac handler.
 */
static void
start_document_cb (CRDocHandler *a_handler)
{
        struct MyFooContext * parsing_context = NULL ;

        /*
         *Allocate the parsing context.
         *Our custom parsing context.
         */
        parsing_context = (struct MyFooContext*) malloc 
                (sizeof (struct MyFooContext)) ;
        if (!parsing_context)
        {
                /*
                 *the system ran out of memory. Advertise it and
                 *stop the program.
                 */
                fprintf (stderr, "progam ran out of memory") ;
                exit (-1) ;
        }

        /*
         *Initialize the newly allocated custom parsing context
         *to zero.
         */
        memset (parsing_context, 0, sizeof (struct MyFooContext)) ;

        /*
         *Store the parsing context in the document handler.
         *The app_data field CRDocHandler is especially there
         *for that: give applications a place to store some data 
         *during the parsing.
         */
        a_handler->app_data = parsing_context ;
}

/**
 *This callback function will
 *be called at the begining of each css ruleset.
 *@param a_handler a pointer to the current sac
 *document handler
 *@param a_selector a pointer to the selector.
 *of the current ruleset.
 */
static void
start_selector_cb (CRDocHandler *a_handler,
		   CRSelector *a_selector)
{
	struct MyFooContext *context = NULL ;
        
        context = (struct MyFooContext*) a_handler->app_data ;
        if (!context)
                return ;
        context->nb_props_per_ruleset = 0 ;
        
        cr_selector_dump (a_selector, stdout) ;
        printf (" {\n") ;
                
}

/**
 *This callback function is called when
 *the parser encounters a CSS property.
 *@param a_handler the SAC handler.
 *@param a_name string that contains the
 *name of the property.
 *@param a_value the value of the property.
 */
static void
property_cb (CRDocHandler *a_handler,
             CRString *a_name,
             CRTerm *a_value,
             gboolean a_important)
{
        struct MyFooContext *context = NULL ;
        
        context = (struct MyFooContext *)a_handler->app_data ;

        if (!context || !a_name)
                return ;
        context->nb_props_per_ruleset ++ ;
        
        printf ("%s : ", cr_string_peek_raw_str (a_name)) ;
        cr_term_dump (a_value, stdout) ;
        printf ("\n") ;
}


/**
 *This is a callback function that will be called at the end
 *of the each css ruleset.
 */
static void
end_selector_cb (CRDocHandler *a_handler,
		 CRSelector *a_selector)
{
        struct MyFooContext *context = NULL ;

	context = (struct MyFooContext*)a_handler->app_data ;
        if (!context)
                return ;
        context->nb_rulesets ++ ;
        printf ("\n}\n") ;
        printf ("**Number of properties in this ruleset: %d\n\n\n",
                context->nb_props_per_ruleset) ;
}

/**
 *This callback is called only once at the end
 *of the CSS document.
 *@param a_handler the SAC handler.
 */
static void
end_document_cb (CRDocHandler *a_handler)
{
        struct MyFooContext *context = NULL ;

        context = (struct MyFooContext*) a_handler->app_data ;
        if (!context)
                return ;

        printf ("\nTotal number of rulesets: %d\n", 
                context->nb_rulesets) ;
        
        free (context) ;
        a_handler->app_data = NULL ;
}

/**
 *Displays some information about how to use this program.
 *@param a_prog_name the name of the current program.
 */
void
display_usage (unsigned char *a_prog_name)
{
	unsigned char *prog_name = a_prog_name ;

	if (!prog_name)
	{
		prog_name = (unsigned char*) "sac-example-1" ;
	}

	printf ("usage: %s [--help] | <css file name>\n", prog_name) ;	
}

int
main (int argc, char **argv)
{	
	unsigned short i = 0 ;
	unsigned char * file_path = NULL ;
	CRParser * parser = NULL ;
	CRDocHandler *sac_handler = NULL ;

	if (argc <= 1)
	{
		display_usage ((unsigned char*)argv[0]) ;
		return -1 ;
	}

	/*
	 *Let's parse the
	 *command line arguments of this 
	 *program in this loop.
	 */
	for (i=1 ; i < argc ;i++)
	{
		if (*argv[i] != '-')
			break ;

		if (!strcmp (argv[i], "--help")
		    || !strcmp (argv[i], "-h"))
		{
			display_usage ((unsigned char*)argv[0]) ;
			return -1;
		}
		else
		{
			/*
			 *no other option is
			 *available now, so this is
			 *a bit redundant...
			 */
			/*display_usage ((unsigned char*)argv[0]) ;*/
		}
	}

	if (i > argc)
	{
		/*
		 *no file name has been given
		 *in parameter, go out.
		 */
		return -1;
	}
	
	/****************************************
	 *Now, the real libcroco related stuffs...
	 ****************************************/

	file_path = (unsigned char*)argv[i] ;
	
	/*
	 *Instanciate the libcroco parser.
	 */
	parser = cr_parser_new_from_file (file_path,
					  CR_ASCII) ;
	if (!parser)
	{
		/*
		 *Damned, something bad happened ...
		 */
		return -1 ;
	}

	/*
	 *Instanciates the SAC document handler.
	 */
	sac_handler = cr_doc_handler_new () ;
	if (!sac_handler)
	{
		/*
		 *Argh, something bad happened here :-\
		 *Let's release the resources we allocated 
		 *and let's get out.
		 */

		cr_parser_destroy (parser) ;
		return -1 ;
	}

	/****************************************
	 *Set some of the sac document handlers.
	 ****************************************/

        /*
         *This callback function will be called by the parser
         *only once, at the beginning of the CSS Document.
         */
        sac_handler->start_document = start_document_cb ;

        /*
         *This callback function will be called by the parser
         *only once, at the end of the CSS Document.
         */
        sac_handler->end_document =end_document_cb ;

	/*
	 *This callback function will be called by the parser
	 *each time it encounters the beginning of a ruleset.
	 */
	sac_handler->start_selector = start_selector_cb ;

        /*
	 *This callback function will be called by the parser
	 *each time it encounters the beginning of a ruleset.
	 */
        sac_handler->property = property_cb ;

	/*
	 *This sac handler callback function will be called by the parser
	 *each time it encounters the end of a ruleset.
	 */
	sac_handler->end_selector = end_selector_cb ;

	/*
	 *Let's register our sac handler into the parser.
	 */
	cr_parser_set_sac_handler (parser, sac_handler) ;

	/*
	 *Now, let's do the parsing !!!
	 */
	cr_parser_parse (parser) ;
	
	/*******************************************************
	 *End of the parsing. A Couple of CSS rulesets must have
         *been printed on the screen...
	 ***********************************************/

	/*
	 *Time to free the resources we allocated.
	 */
	cr_parser_destroy (parser) ;

	return 0 ;
}
