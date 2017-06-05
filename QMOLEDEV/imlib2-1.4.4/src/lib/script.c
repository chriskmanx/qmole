#include "common.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#ifdef BUILD_X11
# include <X11/Xlib.h>
#else
# define X_DISPLAY_MISSING
#endif
#include "image.h"
#include "file.h"
#include "dynamic_filters.h"
#include "script.h"
#include "loaderpath.h"

/*
#define FDEBUG 1
*/
#ifdef FDEBUG
# define D( str ) printf( "DEBUG: %s\n", str )
#else
#define  D( str )
#endif

IVariable          *vars, *current_var, *curtail;

static int
__imlib_find_string(const char *haystack, const char *needle)
{
   if (strstr(haystack, needle) != NULL)
      return (strstr(haystack, needle) - haystack);
   return 0;
}

static char        *
__imlib_stripwhitespace(char *str)
{
   int                 i, strt = 0, in_quote = 0, str_len;
   char               *tmpstr = NULL;

   str_len = strlen(str);
   tmpstr = calloc(str_len + 1, sizeof(char));
   for (i = 0; i < str_len; i++)
     {
        if (str[i] == '\"')
           in_quote = (in_quote == 0 ? 1 : 0);
        if (in_quote || !isspace(*(str + i)))
           tmpstr[strt++] = str[i];
     }
   strcpy(str, tmpstr);
   free(tmpstr);
   return str;
}

static char        *
__imlib_copystr(char *str, int start, int end)
{
   int                 i = 0;
   char               *rstr = calloc(1024, sizeof(char));

   if (start <= end && end < (int)strlen(str))
     {
        for (i = start; i <= end; i++)
           rstr[i - start] = str[i];
        return rstr;
     }
   return NULL;
}

static void
__imlib_script_tidyup_params(IFunctionParam * param)
{
   if (param->next)
     {
        __imlib_script_tidyup_params(param->next);
     }
   free(param->key);
   if (param->type == VAR_CHAR)
      free(param->data);
   free(param);
}

static void
__imlib_script_delete_variable(IVariable * var)
{
   if (var->next != NULL)
      __imlib_script_delete_variable(var->next);
   free(var);
}

void
__imlib_script_tidyup(void)
{
   __imlib_script_delete_variable(vars);
}

void               *
__imlib_script_get_next_var(void)
{
   if (current_var != NULL)
      current_var = current_var->next;
   if (current_var != NULL)
      return current_var->ptr;
   else
      return NULL;
}

void
__imlib_script_add_var(void *ptr)
{
   curtail->next = malloc(sizeof(IVariable));
   curtail = curtail->next;
   curtail->ptr = ptr;
   curtail->next = NULL;
}

IFunctionParam     *
__imlib_script_parse_parameters(Imlib_Image im, char *parameters)
{
   int                 i = 0, in_quote = 0, depth = 0, start = 0, value_start =
      0;
   int                 param_len;
   char               *value = NULL;
   IFunctionParam     *rootptr, *ptr;

   D("(--) ===> Entering __imlib_script_parse_parameters()");

   rootptr = malloc(sizeof(IFunctionParam));
   rootptr->key = strdup("NO-KEY");
   rootptr->type = VAR_CHAR;
   rootptr->data = strdup("NO-VALUE");
   rootptr->next = NULL;
   ptr = rootptr;

   param_len = strlen(parameters);
   for (i = 0; i <= param_len; i++)
     {
        if (parameters[i] == '\"')
           in_quote = (in_quote == 0 ? 1 : 0);
        if (!in_quote && parameters[i] == '(')
           depth++;
        if (!in_quote && parameters[i] == ')')
           depth--;
        if (!in_quote && parameters[i] == '=' && depth == 0)
           value_start = i + 1;
        if (!in_quote && (parameters[i] == ',' || i == param_len) && depth == 0)
          {
             ptr->next = malloc(sizeof(IFunctionParam));
             ptr = ptr->next;
             ptr->key = __imlib_copystr(parameters, start, value_start - 2);
             value = __imlib_copystr(parameters, value_start, i - 1);
#ifdef FDEBUG
             printf("DEBUG: (--)    --> Variable \"%s\" = \"%s\"\n", ptr->key,
                    value);
#endif
             if (__imlib_find_string(value, "(") <
                 __imlib_find_string(value, "\""))
               {
                  D("(--)   Found a function");
                  ptr->data = __imlib_script_parse_function(im, value);
                  ptr->type = VAR_PTR;
                  free(value);
               }
             else
               {
                  if (strcmp(value, "[]") == 0)
                    {
                       ptr->data = __imlib_script_get_next_var();
                       if (ptr->data == NULL)
                         {
                            D("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEK");
                         }
                       /*     printf( "Using pointer variable %p\n", ptr->data ); */
                       ptr->type = VAR_PTR;
                       free(value);
                    }
                  else
                    {
                       ptr->data = value;
                       ptr->type = VAR_CHAR;
                    }
               }
             ptr->next = NULL;
             start = i + 1;
          }
     }
   D("(--) <=== Leaving __imlib_script_parse_parameters()");
   return rootptr;
}

Imlib_Image
__imlib_script_parse_function(Imlib_Image im, char *function)
{
   char               *funcname, *funcparams;
   IFunctionParam     *params;
   ImlibExternalFilter *filter = NULL;
   Imlib_Image         retval;

   D("(--) ===> Entering __imlib_script_parse_function()");
   funcname =
      __imlib_copystr(function, 0, __imlib_find_string(function, "(") - 1);
   funcparams =
      __imlib_copystr(function, __imlib_find_string(function, "(") + 1,
                      strlen(function) - 2);
#ifdef FDEBUG
   printf("DEBUG: (?\?)   = function <%s>( \"%s\" )\n", funcname, funcparams);
#endif
   params = __imlib_script_parse_parameters(im, funcparams);
   /* excute the filter */
   filter = __imlib_get_dynamic_filter(funcname);
   if (filter != NULL)
     {
#ifdef FDEBUG
        printf("DEBUG: (--)   Executing Filter \"%s\".\n", funcname);
#endif
        retval = filter->exec_filter(funcname, im, params);
     }
   else
     {
#ifdef FDEBUG
        printf
           ("DEBUG: (!!)   Can't find filter \"%s\", returning given image.\n",
            funcname);
#endif
        retval = im;
     }
   D("Get Here");
   /* clean up params */
   free(funcname);
   free(funcparams);
   __imlib_script_tidyup_params(params);
   D("(--) <=== Leaving __imlib_script_parse_function()");
   return retval;
}

Imlib_Image
__imlib_script_parse(Imlib_Image im, char *script, va_list param_list)
{
   int                 i = 0, in_quote = 0, start = 0, depth = 0;
   int                 script_len;
   char               *scriptbuf = NULL, *function;

   D("(--) Script Parser Start.");
   if (script != NULL && script[0] != 0)
     {
        vars = malloc(sizeof(IVariable));
        vars->ptr = NULL;
        vars->next = NULL;
        curtail = vars;
        current_var = vars;
        /* gather up variable from the command line */
        D("(--) String Whitespace from script.");
        scriptbuf = __imlib_stripwhitespace(strdup(script));

        i = __imlib_find_string(scriptbuf + start, "=[]") - 1;
        while (i > 0)
          {
             __imlib_script_add_var(va_arg(param_list, void *));

             start = start + i + 2;
             i = __imlib_find_string(scriptbuf + start, "=[]") - 1;
             i = (i == 0 ? 0 : i);
             D("(?\?)   Found pointer variable");
          }

        start = 0;
        i = 0;
        script_len = strlen(scriptbuf);
        for (i = 0; i < script_len; i++)
          {
             if (script[i] == '\"')
                in_quote = (in_quote == 0 ? 1 : 0);
             if (!in_quote && script[i] == '(')
                depth++;
             if (!in_quote && script[i] == ')')
                depth--;
             if (!in_quote && (script[i] == ';') && depth == 0)
               {
                  function = __imlib_copystr(scriptbuf, start, i - 1);
                  im = __imlib_script_parse_function(im, function);
                  imlib_context_set_image(im);
                  start = i + 1;
                  free(function);
               }
          }
        D("(--) Cleaning up parameter list");
        __imlib_script_tidyup();
        D("(--) Script Parser Successful.");
        free(scriptbuf);
        return im;
     }
   else
     {
        D("(!!) Script Parser Failed.");
        return NULL;
     }
}
