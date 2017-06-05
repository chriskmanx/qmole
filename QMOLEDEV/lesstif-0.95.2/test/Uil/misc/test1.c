/* $Id: test1.c,v 1.3 2000/10/24 21:09:29 amai Exp $ */

#include <stdlib.h>
#include <stdio.h>

#include <uil/UilDBDef.h>
#include <uil/UilSymGl.h>


static const char *classes[] = {
    "skip",
    "tkn_k_class_argument",
    "tkn_k_class_charset",
    "tkn_k_class_color",
    "tkn_k_class_enumval",
    "tkn_k_class_font",
    "tkn_k_class_identifier",
    "tkn_k_class_keyword",
    "tkn_k_class_literal",
    "tkn_k_class_reason",
    "tkn_k_class_reserved",
    "tkn_k_class_special",
    "tkn_k_class_unused",
    "tkn_k_class_class",
    "tkn_k_class_child",
    NULL
};


const char
*token_class(int class)
{
    if (class < 1 || class > tkn_k_class_child)
    {
	fprintf(stderr, "CLASS OUT OF RANGE: %d\n", class);
	exit(0);
    }

    return classes[class];
}


void
dump_keytable(key_keytable_entry_type *tab)
{
    int i;

    for (i = 0; i < key_k_keyword_count; i++)
    {
	printf("    {\n\t%s,\t%5d,\t%2d,\t%3d,\t\"%s\"\n    },\n",
		token_class(tab[i].b_class),
		tab[i].b_subclass,
		tab[i].b_length,
		tab[i].b_token,
		tab[i].at_name);
    }
}


void
dump_aa_table(void)
{
    printf("unsigned char **allow_argument_table = NULL;\n");
}


void
dump_aat_table(void)
{
    int i;

    for (i = 0; argument_type_table[i]; i++)
	printf("%02x\n", argument_type_table[i]);
}


void
dump_charsets(void)
{
    int i;

    printf("char **charset_xmstring_names_table = NULL;\n\n");

    printf("char *priv_charset_lang_names_table[] = {\n");
    for (i = 0; i < charset_lang_table_max; i++)
    {
	printf("    \"%s\",\n", charset_lang_names_table[i]);
    }
    printf("};\n\n");
    printf("char **charset_lang_names_table = priv_charset_lang_names_table;\n\n");

    printf("unsigned short int priv_charset_lang_codes_table[] = {\n");
    for (i = 0; i < charset_lang_table_max; i++)
    {
	printf("    %d,\n", charset_lang_codes_table[i]);
    }
    printf("};\n\n");
    printf("unsigned short int *charset_lang_codes_table = priv_charset_lang_codes_table;\n\n");

    printf("unsigned short int charset_lang_table_max = %d;\n\n",
	   charset_lang_table_max);
}


void
dump_objs(void)
{
    int i;

    printf("int uil_max_object = %d;\n\n", uil_max_object);
    printf("char *priv_uil_widget_names[] = {\n");
    for (i = 0; i < uil_max_object; i++)
    {
	printf("    \"%s\",\n", uil_widget_names[i]);
    }
    printf("};\n\n");
    printf("char **uil_widget_names = priv_uil_widget_names;\n\n");
}


void
dump_args(void)
{
    int i;

    printf("int uil_max_arg = %d;\n\n", uil_max_arg);
    printf("char *priv_uil_argument_names[] = {\n");
    for (i = 0; i < uil_max_arg; i++)
    {
	printf("    \"%s\",\n", uil_argument_names[i]);
    }
    printf("};\n\n");
    printf("char **uil_argument_names = priv_uil_argument_names;\n\n");
}


void
dump_children(void)
{
    int i;

    printf("int uil_max_child = %d;\n\n", uil_max_child);
    printf("char *priv_uil_child_names[] = {\n");
    for (i = 0; i < uil_max_child; i++)
    {
	printf("    \"%s\",\n", uil_child_names[i]);
    }
    printf("};\n\n");
    printf("char **uil_child_names = priv_uil_child_names;\n\n");
}


void
dump_reasons(void)
{
    int i;

    printf("int uil_max_reason = %d;\n\n", uil_max_reason);
    printf("char *priv_uil_reason_names[] = {\n");
    for (i = 0; i < uil_max_reason; i++)
    {
	printf("    \"%s\",\n", uil_reason_names[i]);
    }
    printf("};\n\n");
    printf("char **uil_reason_names = priv_uil_reason_names;\n\n");
}


void
dump_enums(void)
{
    int i;

    printf("int uil_max_enumset = %d;\n\n", uil_max_enumset);
    printf("int uil_max_enumval = %d;\n\n", uil_max_enumval);
    printf("char *priv_uil_enumval_names[] = {\n");
    for (i = 0; i < uil_max_enumval; i++)
    {
	printf("    \"%s\",\n", uil_enumval_names[i]);
    }
    printf("};\n\n");
    printf("char **uil_enumval_names = priv_uil_enumval_names;\n\n");
}


void
dump_csets(void)
{
    int i;

    printf("int uil_max_charset = %d;\n\n", uil_max_charset);
    printf("char *priv_uil_charset_names[] = {\n");
    for (i = 0; i < uil_max_charset; i++)
    {
	printf("    \"%s\",\n", uil_charset_names[i]);
    }
    printf("};\n\n");
    printf("char **uil_charset_names = priv_uil_charset_names;\n");
}


void
dump_toks(void)
{
    int i;

    printf("int tok_num_tokens = %d;\n\n", tok_num_tokens);
    printf("char *priv_tok_token_name_table[] = {\n");
    for (i = 0; i < tok_num_tokens; i++)
    {
	printf("    \"%s\",\n", tok_token_name_table[i]);
    }
    printf("};\n\n");
    printf("char **tok_token_name_table = priv_tok_token_name_table;\n\n");
}


void
dump_funcs(void)
{
    int i;

    printf("char *priv_uil_widget_funcs[] = {\n");
    for (i = 0; i < uil_max_object; i++)
    {
	printf("    \"%s\",\n", uil_widget_funcs[i]);
    }
    printf("};\n\n");
    printf("char **uil_widget_funcs = priv_uil_widget_funcs;\n\n");
}


void
dump_misc(void)
{
    int i;

    printf("char *priv_uil_argument_toolkit_names[] = {\n");
    for (i = 0; i < uil_max_arg; i++)
    {
	printf("    \"%s\",\n", uil_argument_toolkit_names[i]);
    }
    printf("};\n\n");
    printf("char **uil_argument_toolkit_names = priv_uil_argument_toolkit_names;\n\n");

    printf("/********************************************************/\n\n");

    printf("char *priv_uil_reason_toolkit_names[] = {\n");
    for (i = 0; i < uil_max_reason; i++)
    {
	printf("    \"%s\",\n", uil_reason_toolkit_names[i]);
    }
    printf("};\n\n");
    printf("char **uil_reason_toolkit_names = priv_uil_reason_toolkit_names;\n\n");

    printf("/********************************************************/\n\n");

    printf("int uil_max_value = %d;\n\n", uil_max_value);
    printf("char *uil_datatype_names[] = {\n");
    for (i = 0; i < uil_max_value; i++)
    {
	printf("    \"%s\",\n", uil_datatype_names[i]);
	fflush(stdout);
    }
    printf("};\n\n");
}


int
main(int argc, char *argv[])
{
    printf("key_keytable_entry_type priv_key_table[] = {\n");
    dump_keytable(key_table);
    printf("};\n\n");
    printf("key_keytable_entry_type *key_table = priv_key_table;\n");

    printf("/* %d */\n", key_k_keyword_count);
    printf("int key_k_keyword_count = sizeof(priv_key_table) / sizeof(key_keytable_entry_type);\n\n");

    printf("int key_k_keyword_max_length = %d;\n\n", key_k_keyword_max_length);

    printf("key_keytable_entry_type priv_key_table_case_ins[] = {\n");
    dump_keytable(key_table_case_ins);
    printf("};\n\n");
    printf("key_keytable_entry_type *key_table_case_ins = priv_key_table_case_ins;\n\n");

    dump_aa_table();

    printf("/********************************************************/\n\n");

    dump_charsets();

    printf("/********************************************************/\n\n");

    dump_objs();

    printf("/********************************************************/\n\n");

    dump_args();

    printf("/********************************************************/\n\n");

    dump_children();

    printf("/********************************************************/\n\n");

    dump_reasons();

    printf("/********************************************************/\n\n");

    dump_enums();

    printf("/********************************************************/\n\n");

    dump_csets();

    printf("/********************************************************/\n\n");

    dump_toks();

    printf("/********************************************************/\n\n");

    dump_funcs();

    printf("/********************************************************/\n\n");

    dump_misc();

    exit(0);
}
