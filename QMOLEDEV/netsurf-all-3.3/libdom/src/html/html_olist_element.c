/*
 * This file is part of libdom.
 * Licensed under the MIT License,
 *                http://www.opensource.org/licenses/mit-license.php
 * Copyright 2009 Bo Yang <struggleyb.nku@gmail.com>
 * Copyright 2014 Rupinder Singh Khokhar<rsk1coder99@gmail.com>
 */
#include <assert.h>
#include <stdlib.h>

#include <dom/html/html_olist_element.h>

#include "html/html_document.h"
#include "html/html_olist_element.h"

#include "core/node.h"
#include "core/attr.h"
#include "utils/utils.h"

static struct dom_element_protected_vtable _protect_vtable = {
	{
		DOM_NODE_PROTECT_VTABLE_HTML_O_LIST_ELEMENT
	},
	DOM_HTML_O_LIST_ELEMENT_PROTECT_VTABLE
};

/**
 * Create a dom_html_o_list_element object
 *
 * \param doc  The document object
 * \param ele  The returned element object
 * \return DOM_NO_ERR on success, appropriate dom_exception on failure.
 */
dom_exception _dom_html_o_list_element_create(struct dom_html_document *doc,
		dom_string *namespace, dom_string *o_listfix,
		struct dom_html_o_list_element **ele)
{
	struct dom_node_internal *node;

	*ele = malloc(sizeof(dom_html_o_list_element));
	if (*ele == NULL)
		return DOM_NO_MEM_ERR;

	/* Set up vtables */
	node = (struct dom_node_internal *) *ele;
	node->base.vtable = &_dom_html_element_vtable;
	node->vtable = &_protect_vtable;

	return _dom_html_o_list_element_initialise(doc, namespace, o_listfix, *ele);
}

/**
 * Initialise a dom_html_o_list_element object
 *
 * \param doc  The document object
 * \param ele  The dom_html_o_list_element object
 * \return DOM_NO_ERR on success, appropriate dom_exception on failure.
 */
dom_exception _dom_html_o_list_element_initialise(struct dom_html_document *doc,
		dom_string *namespace, dom_string *o_listfix,
		struct dom_html_o_list_element *ele)
{
	return _dom_html_element_initialise(doc, &ele->base,
					    doc->memoised[hds_OL],
					    namespace, o_listfix);
}

/**
 * Finalise a dom_html_o_list_element object
 *
 * \param ele  The dom_html_o_list_element object
 */
void _dom_html_o_list_element_finalise(struct dom_html_o_list_element *ele)
{
	_dom_html_element_finalise(&ele->base);
}

/**
 * Destroy a dom_html_o_list_element object
 *
 * \param ele  The dom_html_o_list_element object
 */
void _dom_html_o_list_element_destroy(struct dom_html_o_list_element *ele)
{
	_dom_html_o_list_element_finalise(ele);
	free(ele);
}

/**
 * Get the compact Property
 *
 * \param o_list	The dom_html_o_list_element object
 */
dom_exception dom_html_o_list_element_get_compact(
		        dom_html_o_list_element *o_list, bool *compact)
{
	return dom_html_element_get_bool_property(&o_list->base, "compact",
			SLEN("compact"), compact);
}

/**
 * Set the compact Property
 *
 * \param o_list	The dom_html_o_list_element object
 */
dom_exception dom_html_o_list_element_set_compact(
		        dom_html_o_list_element *o_list, bool compact)
{
	return dom_html_element_set_bool_property(&o_list->base, "compact",
			SLEN("compact"), compact);
}

/**
 * Get the start Property
 *
 * \param o_list	The dom_html_o_list_element object
 */
dom_exception dom_html_o_list_element_get_start(
		        dom_html_o_list_element *o_list, int32_t *start)
{
	return dom_html_element_get_int32_t_property(&o_list->base, "start",
			SLEN("start"), start);
}

/**
 * Set the start Property
 *
 * \param o_list	The dom_html_o_list_element object
 */
dom_exception dom_html_o_list_element_set_start(
		        dom_html_o_list_element *o_list, uint32_t start)
{
	return dom_html_element_set_int32_t_property(&o_list->base, "start",
			SLEN("start"), start);
}

/*------------------------------------------------------------------------*/
/* The protected virtual functions */

/* The virtual function used to parse attribute value, see src/core/element.c
 * for detail */
dom_exception _dom_html_o_list_element_parse_attribute(dom_element *ele,
		dom_string *name, dom_string *value,
		dom_string **parsed)
{
	UNUSED(ele);
	UNUSED(name);

	dom_string_ref(value);
	*parsed = value;

	return DOM_NO_ERR;
}

/* The virtual destroy function, see src/core/node.c for detail */
void _dom_virtual_html_o_list_element_destroy(dom_node_internal *node)
{
	_dom_html_o_list_element_destroy((struct dom_html_o_list_element *) node);
}

/* The virtual copy function, see src/core/node.c for detail */
dom_exception _dom_html_o_list_element_copy(dom_node_internal *old,
		                dom_node_internal **copy)
{
	        return _dom_html_element_copy(old, copy);
}

/*-----------------------------------------------------------------------*/
/* API functions */

#define SIMPLE_GET(attr)						\
		dom_exception dom_html_o_list_element_get_##attr(		\
						dom_html_o_list_element *element,			\
						dom_string **attr)					\
	{								\
				dom_exception ret;					\
				dom_string *_memo_##attr;				\
											\
				_memo_##attr =						\
					((struct dom_html_document *)			\
					 			 ((struct dom_node_internal *)element)->owner)->\
					memoised[hds_##attr];				\
											\
				ret = dom_element_get_attribute(element, _memo_##attr, attr); \
											\
				return ret;						\
			}
#define SIMPLE_SET(attr)						\
	dom_exception dom_html_o_list_element_set_##attr(			\
					dom_html_o_list_element *element,			\
					dom_string *attr)					\
	{								\
				dom_exception ret;					\
				dom_string *_memo_##attr;				\
											\
				_memo_##attr =						\
					((struct dom_html_document *)			\
					 			 ((struct dom_node_internal *)element)->owner)->\
					memoised[hds_##attr];				\
											\
				ret = dom_element_set_attribute(element, _memo_##attr, attr); \
											\
				return ret;						\
			}

#define SIMPLE_GET_SET(attr) SIMPLE_GET(attr) SIMPLE_SET(attr)

SIMPLE_GET_SET(type);
