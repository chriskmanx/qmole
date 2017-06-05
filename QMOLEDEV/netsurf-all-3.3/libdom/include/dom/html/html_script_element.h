/*
 * This file is part of libdom.
 * Licensed under the MIT License,
 *                http://www.opensource.org/licenses/mit-license.php
 * Copyright 2009 Bo Yang <struggleyb.nku@gmail.com>
 * Copyright 2014 Rupinder Singh Khokhar <rsk1coder99@gmail.com>
 */
#ifndef dom_html_script_element_h_
#define dom_html_script_element_h_

#include <stdbool.h>
#include <dom/core/exceptions.h>
#include <dom/core/string.h>

typedef struct dom_html_script_element dom_html_script_element;

dom_exception dom_html_script_element_get_defer(
	dom_html_script_element *ele, bool *defer);

dom_exception dom_html_script_element_set_defer(
	dom_html_script_element *ele, bool defer);

dom_exception dom_html_script_element_get_text(
	dom_html_script_element *element, dom_string **text);

dom_exception dom_html_script_element_set_text(
	dom_html_script_element *element, dom_string *text);

dom_exception dom_html_script_element_get_html_for(
	dom_html_script_element *element, dom_string **html_for);

dom_exception dom_html_script_element_set_html_for(
	dom_html_script_element *element, dom_string *html_for);

dom_exception dom_html_script_element_get_event(
	dom_html_script_element *element, dom_string **event);

dom_exception dom_html_script_element_set_event(
	dom_html_script_element *ele, dom_string *event);

dom_exception dom_html_script_element_get_charset(
	dom_html_script_element *ele, dom_string **charset);

dom_exception dom_html_script_element_set_charset(
	dom_html_script_element *ele, dom_string *charset);

dom_exception dom_html_script_element_get_src(
	dom_html_script_element *ele, dom_string **src);

dom_exception dom_html_script_element_set_src(
	dom_html_script_element *ele, dom_string *src);

dom_exception dom_html_script_element_get_type(
	dom_html_script_element *ele, dom_string **type);

dom_exception dom_html_script_element_set_type(
	dom_html_script_element *ele, dom_string *type);

#endif
