/* -*- Mode: C; indent-tabs-mode:nil; c-basic-offset:8 -*- */

/**
 *This test example shows how to use the SAC api.
 *
 *This features a simple parser that says "hey" when
 *it encounters the beginning of a CSS ruleset, and "woohoo"
 *at the end of a CSS ruleset.
 *
 *To compile this file, type:
 *
 *gcc -g -Wall -o sac-example-1 `croco-0.6-config --cflags`  `croco-0.6-config --libs` sac-example-1.c
 *
 *Make sure you have compiled and installed libcroco prior to trying to
 *compile this file :)
 *
 *Once you have compiled it, type:
 *
 *./sac-example-1 <a-path-to-a-css-file>
 *
 *to try it on a CSS file of your choice.
 *
 *Initial Author: Dodji Seketeli <Dodji 47 seketeli dot org>
 */

#include <string.h>
#include <libcroco/libcroco.h>

/**
 *This is a callback function that will
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
	printf ("==========================================\n") ;
	printf ("Hey, this is the begining of a ruleset\n") ;
}

/**
 *This is a callback function that will be called at the end
 *of the each css ruleset.
 */
static void
end_selector_cb (CRDocHandler *a_handler,
		 CRSelector *a_selector)
{
	printf ("Woohoo, this is the end of a ruleset\n") ;
	printf ("======================================\n\n") ;
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
		prog_name = (unsigned char*)"sac-example-1" ;
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
			display_usage ((unsigned char*)argv[0]) ;
		}
	}

	if (i > argc)
	{
		/*
		 *no file name has been given
		 *in parameter, go out.
		 */
		return -1 ;
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
		return -1;
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
		return -1;
	}

	/******************
	 *Sets some of the sac document handlers.
	 ****************/

	/*
	 *This sac handler callback function will get called by the parser
	 *each time it encounters the beginning of a ruleset.
	 */
	sac_handler->start_selector = start_selector_cb ;
	
	/*
	 *This sac handler callback function will get called by the parser
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
	 *End of the parsing. A lot of sentences begining with "Hey"
	 *may have been printed on the screen...
	 ***********************************************/

	/*
	 *Time to free the resources we allocated.
	 */
	cr_parser_destroy (parser) ;

	cr_doc_handler_unref (sac_handler) ;

	return 0 ;
}
