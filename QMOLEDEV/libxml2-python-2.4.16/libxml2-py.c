/* Generated */

#include <Python.h>
#include <libxml/tree.h>
#include "libxml_wrap.h"
#include "libxml2-py.h"

PyObject *
libxml_xmlLoadCatalogs(PyObject *self, PyObject *args) {
    char * paths;

    if (!PyArg_ParseTuple(args, "z:xmlLoadCatalogs", &paths))
        return(NULL);

    xmlLoadCatalogs(paths);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlGetDocEntity(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlEntityPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlGetDocEntity", &pyobj_doc, &name))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlGetDocEntity(doc, name);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeGetBase(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlNodeGetBase", &pyobj_doc, &pyobj_cur))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlNodeGetBase(doc, cur);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlRegisterHTTPPostCallbacks(PyObject *self, PyObject *args) {

    xmlRegisterHTTPPostCallbacks();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNextAncestorOrSelf(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextAncestorOrSelf", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextAncestorOrSelf(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStrstr(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xmlChar * str;
    xmlChar * val;

    if (!PyArg_ParseTuple(args, "zz:xmlStrstr", &str, &val))
        return(NULL);

    c_retval = xmlStrstr(str, val);
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetCompressMode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;

    c_retval = xmlGetCompressMode();
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseChunk(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    char * chunk;
    int size;
    int terminate;

    if (!PyArg_ParseTuple(args, "Ozii:xmlParseChunk", &pyobj_ctxt, &chunk, &size, &terminate))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseChunk(ctxt, chunk, size, terminate);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlNewDocNoDtD(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    htmlDocPtr c_retval;
    xmlChar * URI;
    xmlChar * ExternalID;

    if (!PyArg_ParseTuple(args, "zz:htmlNewDocNoDtD", &URI, &ExternalID))
        return(NULL);

    c_retval = htmlNewDocNoDtD(URI, ExternalID);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNewString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    xmlChar * val;

    if (!PyArg_ParseTuple(args, "z:xmlXPathNewString", &val))
        return(NULL);

    c_retval = xmlXPathNewString(val);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlAddSibling(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlNodePtr elem;
    PyObject *pyobj_elem;

    if (!PyArg_ParseTuple(args, "OO:xmlAddSibling", &pyobj_cur, &pyobj_elem))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);
    elem = (xmlNodePtr) PyxmlNode_Get(pyobj_elem);

    c_retval = xmlAddSibling(cur, elem);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlScanName(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlScanName", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlScanName(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseElementDecl(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseElementDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseElementDecl(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlRegisterDefaultInputCallbacks(PyObject *self, PyObject *args) {

    xmlRegisterDefaultInputCallbacks();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNextDescendantOrSelf(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextDescendantOrSelf", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextDescendantOrSelf(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNsLookup(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * prefix;

    if (!PyArg_ParseTuple(args, "Oz:xmlXPathNsLookup", &pyobj_ctxt, &prefix))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = xmlXPathNsLookup(ctxt, prefix);
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDebugDumpEntities(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "OO:xmlDebugDumpEntities", &pyobj_output, &pyobj_doc))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    xmlDebugDumpEntities(output, doc);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURISetQuery(PyObject *self, PyObject *args) {
    xmlURIPtr URI;
    PyObject *pyobj_URI;
    char * query;

    if (!PyArg_ParseTuple(args, "Oz:xmlURISetQuery", &pyobj_URI, &query))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    if (URI->query != NULL) xmlFree(URI->query);
    URI->query = xmlStrdup(query);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURISetPort(PyObject *self, PyObject *args) {
    xmlURIPtr URI;
    PyObject *pyobj_URI;
    int port;

    if (!PyArg_ParseTuple(args, "Oi:xmlURISetPort", &pyobj_URI, &port))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    URI->port = port;
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlDebugDumpNodeList(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlNodePtr node;
    PyObject *pyobj_node;
    int depth;

    if (!PyArg_ParseTuple(args, "OOi:xmlDebugDumpNodeList", &pyobj_output, &pyobj_node, &depth))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    xmlDebugDumpNodeList(output, node, depth);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNextAncestor(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextAncestor", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextAncestor(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlUTF8Strlen(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * utf;

    if (!PyArg_ParseTuple(args, "z:xmlUTF8Strlen", &utf))
        return(NULL);

    c_retval = xmlUTF8Strlen(utf);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathAddValues(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathAddValues", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathAddValues(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParserHandleReference(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParserHandleReference", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParserHandleReference(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNewTextLen(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlChar * content;
    int len;

    if (!PyArg_ParseTuple(args, "zi:xmlNewTextLen", &content, &len))
        return(NULL);

    c_retval = xmlNewTextLen(content, len);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNanoFTPCleanup(PyObject *self, PyObject *args) {

    xmlNanoFTPCleanup();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlRecoverMemory(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    char * buffer;
    int size;

    if (!PyArg_ParseTuple(args, "zi:xmlRecoverMemory", &buffer, &size))
        return(NULL);

    c_retval = xmlRecoverMemory(buffer, size);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathValueFlipSign(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathValueFlipSign", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathValueFlipSign(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_htmlSaveFile(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * filename;
    xmlDocPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "zO:htmlSaveFile", &filename, &pyobj_cur))
        return(NULL);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = htmlSaveFile(filename, cur);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetID(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * ID;

    if (!PyArg_ParseTuple(args, "Oz:xmlGetID", &pyobj_doc, &ID))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlGetID(doc, ID);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParserGetWellFormed(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParserGetWellFormed", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->wellFormed;
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlEncodeSpecialChars(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * input;

    if (!PyArg_ParseTuple(args, "Oz:xmlEncodeSpecialChars", &pyobj_doc, &input))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlEncodeSpecialChars(doc, input);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlFreePropList(PyObject *self, PyObject *args) {
    xmlAttrPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlFreePropList", &pyobj_cur))
        return(NULL);
    cur = (xmlAttrPtr) PyxmlNode_Get(pyobj_cur);

    xmlFreePropList(cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlStrsub(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * str;
    int start;
    int len;

    if (!PyArg_ParseTuple(args, "zii:xmlStrsub", &str, &start, &len))
        return(NULL);

    c_retval = xmlStrsub(str, start, len);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathStringFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathStringFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathStringFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlAddNextSibling(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlNodePtr elem;
    PyObject *pyobj_elem;

    if (!PyArg_ParseTuple(args, "OO:xmlAddNextSibling", &pyobj_cur, &pyobj_elem))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);
    elem = (xmlNodePtr) PyxmlNode_Get(pyobj_elem);

    c_retval = xmlAddNextSibling(cur, elem);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathInit(PyObject *self, PyObject *args) {

    xmlXPathInit();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlInitParserCtxt(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlInitParserCtxt", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlInitParserCtxt(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCheckLanguageID(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * lang;

    if (!PyArg_ParseTuple(args, "z:xmlCheckLanguageID", &lang))
        return(NULL);

    c_retval = xmlCheckLanguageID(lang);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlFreeNs(PyObject *self, PyObject *args) {
    xmlNsPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlFreeNs", &pyobj_cur))
        return(NULL);
    cur = (xmlNsPtr) PyxmlNode_Get(pyobj_cur);

    xmlFreeNs(cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNextChar(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlNextChar", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlNextChar(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathFalseFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathFalseFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathFalseFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURIGetPath(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    xmlURIPtr URI;
    PyObject *pyobj_URI;

    if (!PyArg_ParseTuple(args, "O:xmlURIGetPath", &pyobj_URI))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    c_retval = URI->path;
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * name;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "Ozz:xmlNewProp", &pyobj_node, &name, &value))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlNewProp(node, name, value);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParserGetDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParserGetDoc", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->myDoc;
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStringLenGetNodeList(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * value;
    int len;

    if (!PyArg_ParseTuple(args, "Ozi:xmlStringLenGetNodeList", &pyobj_doc, &value, &len))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlStringLenGetNodeList(doc, value, len);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathRoundFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathRoundFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathRoundFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNodeAddContentLen(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlChar * content;
    int len;

    if (!PyArg_ParseTuple(args, "Ozi:xmlNodeAddContentLen", &pyobj_cur, &content, &len))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlNodeAddContentLen(cur, content, len);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlRegisterDefaultOutputCallbacks(PyObject *self, PyObject *args) {

    xmlRegisterDefaultOutputCallbacks();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCleanupCharEncodingHandlers(PyObject *self, PyObject *args) {

    xmlCleanupCharEncodingHandlers();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParserHandlePEReference(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParserHandlePEReference", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParserHandlePEReference(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_htmlNodeDumpFile(PyObject *self, PyObject *args) {
    FILE * out;
    PyObject *pyobj_out;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OOO:htmlNodeDumpFile", &pyobj_out, &pyobj_doc, &pyobj_cur))
        return(NULL);
    out = (FILE *) PyFile_Get(pyobj_out);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    htmlNodeDumpFile(out, doc, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathGetContextNode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathGetContextNode", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = ctxt->node;
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParserSetPedantic(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    int pedantic;

    if (!PyArg_ParseTuple(args, "Oi:xmlParserSetPedantic", &pyobj_ctxt, &pedantic))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    ctxt->pedantic = pedantic;
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseEntityRef(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlEntityPtr c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseEntityRef", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseEntityRef(ctxt);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCatalogIsEmpty(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;

    if (!PyArg_ParseTuple(args, "O:xmlCatalogIsEmpty", &pyobj_catal))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);

    c_retval = xmlCatalogIsEmpty(catal);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlIsPubidChar(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int c;

    if (!PyArg_ParseTuple(args, "i:xmlIsPubidChar", &c))
        return(NULL);

    c_retval = xmlIsPubidChar(c);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseEncodingDecl(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseEncodingDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseEncodingDecl(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlInitAutoClose(PyObject *self, PyObject *args) {

    htmlInitAutoClose();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlLoadSGMLSuperCatalog(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlCatalogPtr c_retval;
    char * filename;

    if (!PyArg_ParseTuple(args, "z:xmlLoadSGMLSuperCatalog", &filename))
        return(NULL);

    c_retval = xmlLoadSGMLSuperCatalog(filename);
    py_retval = libxml_xmlCatalogPtrWrap((xmlCatalogPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetNsProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * name;
    xmlChar * nameSpace;

    if (!PyArg_ParseTuple(args, "Ozz:xmlGetNsProp", &pyobj_node, &name, &nameSpace))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlGetNsProp(node, name, nameSpace);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewDocProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "Ozz:xmlNewDocProp", &pyobj_doc, &name, &value))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewDocProp(doc, name, value);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathSetContextDoc(PyObject *self, PyObject *args) {
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathSetContextDoc", &pyobj_ctxt, &pyobj_doc))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    ctxt->doc = doc;
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlDebugDumpString(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlChar * str;

    if (!PyArg_ParseTuple(args, "Oz:xmlDebugDumpString", &pyobj_output, &str))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);

    xmlDebugDumpString(output, str);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlLoadACatalog(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlCatalogPtr c_retval;
    char * filename;

    if (!PyArg_ParseTuple(args, "z:xmlLoadACatalog", &filename))
        return(NULL);

    c_retval = xmlLoadACatalog(filename);
    py_retval = libxml_xmlCatalogPtrWrap((xmlCatalogPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCopyChar(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int len;
    xmlChar * out;
    int val;

    if (!PyArg_ParseTuple(args, "izi:xmlCopyChar", &len, &out, &val))
        return(NULL);

    c_retval = xmlCopyChar(len, out, val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCatalogGetSystem(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xmlChar * sysID;

    if (!PyArg_ParseTuple(args, "z:xmlCatalogGetSystem", &sysID))
        return(NULL);

    c_retval = xmlCatalogGetSystem(sysID);
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlEncodeEntities(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * input;

    if (!PyArg_ParseTuple(args, "Oz:xmlEncodeEntities", &pyobj_doc, &input))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlEncodeEntities(doc, input);
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetDtdQElementDesc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlElementPtr c_retval;
    xmlDtdPtr dtd;
    PyObject *pyobj_dtd;
    xmlChar * name;
    xmlChar * prefix;

    if (!PyArg_ParseTuple(args, "Ozz:xmlGetDtdQElementDesc", &pyobj_dtd, &name, &prefix))
        return(NULL);
    dtd = (xmlDtdPtr) PyxmlNode_Get(pyobj_dtd);

    c_retval = xmlGetDtdQElementDesc(dtd, name, prefix);
    py_retval = libxml_xmlElementPtrWrap((xmlElementPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlURIEscape(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * str;

    if (!PyArg_ParseTuple(args, "z:xmlURIEscape", &str))
        return(NULL);

    c_retval = xmlURIEscape(str);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStrncasecmp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * str1;
    xmlChar * str2;
    int len;

    if (!PyArg_ParseTuple(args, "zzi:xmlStrncasecmp", &str1, &str2, &len))
        return(NULL);

    c_retval = xmlStrncasecmp(str1, str2, len);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathLocalNameFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathLocalNameFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathLocalNameFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlHasProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlHasProp", &pyobj_node, &name))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlHasProp(node, name);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNextPrecedingSibling(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextPrecedingSibling", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextPrecedingSibling(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCatalogCleanup(PyObject *self, PyObject *args) {

    xmlCatalogCleanup();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlGetDtdElementDesc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlElementPtr c_retval;
    xmlDtdPtr dtd;
    PyObject *pyobj_dtd;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlGetDtdElementDesc", &pyobj_dtd, &name))
        return(NULL);
    dtd = (xmlDtdPtr) PyxmlNode_Get(pyobj_dtd);

    c_retval = xmlGetDtdElementDesc(dtd, name);
    py_retval = libxml_xmlElementPtrWrap((xmlElementPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCastNodeToString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "O:xmlXPathCastNodeToString", &pyobj_node))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlXPathCastNodeToString(node);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNanoHTTPInit(PyObject *self, PyObject *args) {

    xmlNanoHTTPInit();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCopyNamespaceList(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xmlNsPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlCopyNamespaceList", &pyobj_cur))
        return(NULL);
    cur = (xmlNsPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlCopyNamespaceList(cur);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlIsID(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr elem;
    PyObject *pyobj_elem;
    xmlAttrPtr attr;
    PyObject *pyobj_attr;

    if (!PyArg_ParseTuple(args, "OOO:xmlIsID", &pyobj_doc, &pyobj_elem, &pyobj_attr))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    elem = (xmlNodePtr) PyxmlNode_Get(pyobj_elem);
    attr = (xmlAttrPtr) PyxmlNode_Get(pyobj_attr);

    c_retval = xmlIsID(doc, elem, attr);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetDtdQAttrDesc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttributePtr c_retval;
    xmlDtdPtr dtd;
    PyObject *pyobj_dtd;
    xmlChar * elem;
    xmlChar * name;
    xmlChar * prefix;

    if (!PyArg_ParseTuple(args, "Ozzz:xmlGetDtdQAttrDesc", &pyobj_dtd, &elem, &name, &prefix))
        return(NULL);
    dtd = (xmlDtdPtr) PyxmlNode_Get(pyobj_dtd);

    c_retval = xmlGetDtdQAttrDesc(dtd, elem, name, prefix);
    py_retval = libxml_xmlAttributePtrWrap((xmlAttributePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseCDSect(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseCDSect", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseCDSect(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlSetNs(PyObject *self, PyObject *args) {
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlNsPtr ns;
    PyObject *pyobj_ns;

    if (!PyArg_ParseTuple(args, "OO:xmlSetNs", &pyobj_node, &pyobj_ns))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    ns = (xmlNsPtr) PyxmlNode_Get(pyobj_ns);

    xmlSetNs(node, ns);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNamespaceParseNCName(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlNamespaceParseNCName", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlNamespaceParseNCName(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathContainsFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathContainsFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathContainsFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseExtParsedEnt(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseExtParsedEnt", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseExtParsedEnt(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCopyDtd(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDtdPtr c_retval;
    xmlDtdPtr dtd;
    PyObject *pyobj_dtd;

    if (!PyArg_ParseTuple(args, "O:xmlCopyDtd", &pyobj_dtd))
        return(NULL);
    dtd = (xmlDtdPtr) PyxmlNode_Get(pyobj_dtd);

    c_retval = xmlCopyDtd(dtd);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetDtdEntity(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlEntityPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlGetDtdEntity", &pyobj_doc, &name))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlGetDtdEntity(doc, name);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeListGetRawString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr list;
    PyObject *pyobj_list;
    int inLine;

    if (!PyArg_ParseTuple(args, "OOi:xmlNodeListGetRawString", &pyobj_doc, &pyobj_list, &inLine))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    list = (xmlNodePtr) PyxmlNode_Get(pyobj_list);

    c_retval = xmlNodeListGetRawString(doc, list, inLine);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewDocNode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNsPtr ns;
    PyObject *pyobj_ns;
    xmlChar * name;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "OOzz:xmlNewDocNode", &pyobj_doc, &pyobj_ns, &name, &content))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    ns = (xmlNsPtr) PyxmlNode_Get(pyobj_ns);

    c_retval = xmlNewDocNode(doc, ns, name, content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlIsDigit(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int c;

    if (!PyArg_ParseTuple(args, "i:xmlIsDigit", &c))
        return(NULL);

    c_retval = xmlIsDigit(c);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlParseDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    htmlDocPtr c_retval;
    xmlChar * cur;
    char * encoding;

    if (!PyArg_ParseTuple(args, "zz:htmlParseDoc", &cur, &encoding))
        return(NULL);

    c_retval = htmlParseDoc(cur, encoding);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetParameterEntity(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlEntityPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlGetParameterEntity", &pyobj_doc, &name))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlGetParameterEntity(doc, name);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathFreeParserContext(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathFreeParserContext", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathFreeParserContext(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathRegisteredNsCleanup(PyObject *self, PyObject *args) {
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathRegisteredNsCleanup", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    xmlXPathRegisteredNsCleanup(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNewDocTextLen(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * content;
    int len;

    if (!PyArg_ParseTuple(args, "Ozi:xmlNewDocTextLen", &pyobj_doc, &content, &len))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewDocTextLen(doc, content, len);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlFreeNsList(PyObject *self, PyObject *args) {
    xmlNsPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlFreeNsList", &pyobj_cur))
        return(NULL);
    cur = (xmlNsPtr) PyxmlNode_Get(pyobj_cur);

    xmlFreeNsList(cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseEntityDecl(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseEntityDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseEntityDecl(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseAttValue(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseAttValue", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseAttValue(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDocCopyNode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    int recursive;

    if (!PyArg_ParseTuple(args, "OOi:xmlDocCopyNode", &pyobj_node, &pyobj_doc, &recursive))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlDocCopyNode(node, doc, recursive);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_nodePop(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:nodePop", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = nodePop(ctxt);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCreateURI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlURIPtr c_retval;

    c_retval = xmlCreateURI();
    py_retval = libxml_xmlURIPtrWrap((xmlURIPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlGetProp", &pyobj_node, &name))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlGetProp(node, name);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNextDescendant(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextDescendant", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextDescendant(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStrcat(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * cur;
    xmlChar * add;

    if (!PyArg_ParseTuple(args, "zz:xmlStrcat", &cur, &add))
        return(NULL);

    c_retval = xmlStrcat(cur, add);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlParseFile(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    htmlDocPtr c_retval;
    char * filename;
    char * encoding;

    if (!PyArg_ParseTuple(args, "zz:htmlParseFile", &filename, &encoding))
        return(NULL);

    c_retval = htmlParseFile(filename, encoding);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlACatalogResolveURI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;
    xmlChar * URI;

    if (!PyArg_ParseTuple(args, "Oz:xmlACatalogResolveURI", &pyobj_catal, &URI))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);

    c_retval = xmlACatalogResolveURI(catal, URI);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlURIGetAuthority(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    xmlURIPtr URI;
    PyObject *pyobj_URI;

    if (!PyArg_ParseTuple(args, "O:xmlURIGetAuthority", &pyobj_URI))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    c_retval = URI->authority;
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlInitializeCatalog(PyObject *self, PyObject *args) {

    xmlInitializeCatalog();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlLoadCatalog(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * filename;

    if (!PyArg_ParseTuple(args, "z:xmlLoadCatalog", &filename))
        return(NULL);

    c_retval = xmlLoadCatalog(filename);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParserSetLoadSubset(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    int loadsubset;

    if (!PyArg_ParseTuple(args, "Oi:xmlParserSetLoadSubset", &pyobj_ctxt, &loadsubset))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    ctxt->loadsubset = loadsubset;
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlDocGetRootElement(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "O:xmlDocGetRootElement", &pyobj_doc))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlDocGetRootElement(doc);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStringGetNodeList(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "Oz:xmlStringGetNodeList", &pyobj_doc, &value))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlStringGetNodeList(doc, value);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlHandleOmittedElem(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int val;

    if (!PyArg_ParseTuple(args, "i:htmlHandleOmittedElem", &val))
        return(NULL);

    c_retval = htmlHandleOmittedElem(val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseMarkupDecl(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseMarkupDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseMarkupDecl(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathPopString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathPopString", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = xmlXPathPopString(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlCreateFileParserCtxt(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    htmlParserCtxtPtr c_retval;
    char * filename;
    char * encoding;

    if (!PyArg_ParseTuple(args, "zz:htmlCreateFileParserCtxt", &filename, &encoding))
        return(NULL);

    c_retval = htmlCreateFileParserCtxt(filename, encoding);
    py_retval = libxml_xmlParserCtxtPtrWrap((xmlParserCtxtPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlAddEncodingAlias(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * name;
    char * alias;

    if (!PyArg_ParseTuple(args, "zz:xmlAddEncodingAlias", &name, &alias))
        return(NULL);

    c_retval = xmlAddEncodingAlias(name, alias);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlHasNsProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * name;
    xmlChar * nameSpace;

    if (!PyArg_ParseTuple(args, "Ozz:xmlHasNsProp", &pyobj_node, &name, &nameSpace))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlHasNsProp(node, name, nameSpace);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlAddPrevSibling(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlNodePtr elem;
    PyObject *pyobj_elem;

    if (!PyArg_ParseTuple(args, "OO:xmlAddPrevSibling", &pyobj_cur, &pyobj_elem))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);
    elem = (xmlNodePtr) PyxmlNode_Get(pyobj_elem);

    c_retval = xmlAddPrevSibling(cur, elem);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewPI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlChar * name;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "zz:xmlNewPI", &name, &content))
        return(NULL);

    c_retval = xmlNewPI(name, content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetDtdAttrDesc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttributePtr c_retval;
    xmlDtdPtr dtd;
    PyObject *pyobj_dtd;
    xmlChar * elem;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Ozz:xmlGetDtdAttrDesc", &pyobj_dtd, &elem, &name))
        return(NULL);
    dtd = (xmlDtdPtr) PyxmlNode_Get(pyobj_dtd);

    c_retval = xmlGetDtdAttrDesc(dtd, elem, name);
    py_retval = libxml_xmlAttributePtrWrap((xmlAttributePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCastNumberToBoolean(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    double val;

    if (!PyArg_ParseTuple(args, "d:xmlXPathCastNumberToBoolean", &val))
        return(NULL);

    c_retval = xmlXPathCastNumberToBoolean(val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlNodeDumpFileFormat(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    FILE * out;
    PyObject *pyobj_out;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    char * encoding;
    int format;

    if (!PyArg_ParseTuple(args, "OOOzi:htmlNodeDumpFileFormat", &pyobj_out, &pyobj_doc, &pyobj_cur, &encoding, &format))
        return(NULL);
    out = (FILE *) PyFile_Get(pyobj_out);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = htmlNodeDumpFileFormat(out, doc, cur, encoding, format);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlUnsetNsProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlNsPtr ns;
    PyObject *pyobj_ns;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "OOz:xmlUnsetNsProp", &pyobj_node, &pyobj_ns, &name))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    ns = (xmlNsPtr) PyxmlNode_Get(pyobj_ns);

    c_retval = xmlUnsetNsProp(node, ns, name);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlElemDump(PyObject *self, PyObject *args) {
    FILE * f;
    PyObject *pyobj_f;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OOO:xmlElemDump", &pyobj_f, &pyobj_doc, &pyobj_cur))
        return(NULL);
    f = (FILE *) PyFile_Get(pyobj_f);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlElemDump(f, doc, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathTrueFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathTrueFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathTrueFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathConcatFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathConcatFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathConcatFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathGetContextPosition(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathGetContextPosition", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = ctxt->proximityPosition;
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetIntSubset(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDtdPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "O:xmlGetIntSubset", &pyobj_doc))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlGetIntSubset(doc);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlShellPrintXPathError(PyObject *self, PyObject *args) {
    int errorType;
    char * arg;

    if (!PyArg_ParseTuple(args, "iz:xmlShellPrintXPathError", &errorType, &arg))
        return(NULL);

    xmlShellPrintXPathError(errorType, arg);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_htmlSaveFileFormat(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * filename;
    xmlDocPtr cur;
    PyObject *pyobj_cur;
    char * encoding;
    int format;

    if (!PyArg_ParseTuple(args, "zOzi:htmlSaveFileFormat", &filename, &pyobj_cur, &encoding, &format))
        return(NULL);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = htmlSaveFileFormat(filename, cur, encoding, format);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCatalogResolve(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * pubID;
    xmlChar * sysID;

    if (!PyArg_ParseTuple(args, "zz:xmlCatalogResolve", &pubID, &sysID))
        return(NULL);

    c_retval = xmlCatalogResolve(pubID, sysID);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCleanupInputCallbacks(PyObject *self, PyObject *args) {

    xmlCleanupInputCallbacks();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlTextConcat(PyObject *self, PyObject *args) {
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * content;
    int len;

    if (!PyArg_ParseTuple(args, "Ozi:xmlTextConcat", &pyobj_node, &content, &len))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    xmlTextConcat(node, content, len);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNormalizeURIPath(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * path;

    if (!PyArg_ParseTuple(args, "z:xmlNormalizeURIPath", &path))
        return(NULL);

    c_retval = xmlNormalizeURIPath(path);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeSetContent(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "Oz:xmlNodeSetContent", &pyobj_cur, &content))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlNodeSetContent(cur, content);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseCharRef(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseCharRef", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseCharRef(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDebugDumpAttrList(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlAttrPtr attr;
    PyObject *pyobj_attr;
    int depth;

    if (!PyArg_ParseTuple(args, "OOi:xmlDebugDumpAttrList", &pyobj_output, &pyobj_attr, &depth))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    attr = (xmlAttrPtr) PyxmlNode_Get(pyobj_attr);

    xmlDebugDumpAttrList(output, attr, depth);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCopyCharMultiByte(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * out;
    int val;

    if (!PyArg_ParseTuple(args, "zi:xmlCopyCharMultiByte", &out, &val))
        return(NULL);

    c_retval = xmlCopyCharMultiByte(out, val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseVersionNum(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseVersionNum", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseVersionNum(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseCharData(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    int cdata;

    if (!PyArg_ParseTuple(args, "Oi:xmlParseCharData", &pyobj_ctxt, &cdata))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseCharData(ctxt, cdata);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathRegisteredVariablesCleanup(PyObject *self, PyObject *args) {
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathRegisteredVariablesCleanup", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    xmlXPathRegisteredVariablesCleanup(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlSetCompressMode(PyObject *self, PyObject *args) {
    int mode;

    if (!PyArg_ParseTuple(args, "i:xmlSetCompressMode", &mode))
        return(NULL);

    xmlSetCompressMode(mode);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCatalogResolveSystem(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * sysID;

    if (!PyArg_ParseTuple(args, "z:xmlCatalogResolveSystem", &sysID))
        return(NULL);

    c_retval = xmlCatalogResolveSystem(sysID);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeAddContent(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "Oz:xmlNodeAddContent", &pyobj_cur, &content))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlNodeAddContent(cur, content);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURIGetQuery(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    xmlURIPtr URI;
    PyObject *pyobj_URI;

    if (!PyArg_ParseTuple(args, "O:xmlURIGetQuery", &pyobj_URI))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    c_retval = URI->query;
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStrlen(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * str;

    if (!PyArg_ParseTuple(args, "z:xmlStrlen", &str))
        return(NULL);

    c_retval = xmlStrlen(str);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathSubstringFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathSubstringFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathSubstringFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlIsBlankNode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "O:xmlIsBlankNode", &pyobj_node))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlIsBlankNode(node);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDebugDumpNode(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlNodePtr node;
    PyObject *pyobj_node;
    int depth;

    if (!PyArg_ParseTuple(args, "OOi:xmlDebugDumpNode", &pyobj_output, &pyobj_node, &depth))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    xmlDebugDumpNode(output, node, depth);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCopyDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    int recursive;

    if (!PyArg_ParseTuple(args, "Oi:xmlCopyDoc", &pyobj_doc, &recursive))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlCopyDoc(doc, recursive);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathEqualValues(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathEqualValues", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = xmlXPathEqualValues(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewNsProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlNsPtr ns;
    PyObject *pyobj_ns;
    xmlChar * name;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "OOzz:xmlNewNsProp", &pyobj_node, &pyobj_ns, &name, &value))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    ns = (xmlNsPtr) PyxmlNode_Get(pyobj_ns);

    c_retval = xmlNewNsProp(node, ns, name, value);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlFreeProp(PyObject *self, PyObject *args) {
    xmlAttrPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlFreeProp", &pyobj_cur))
        return(NULL);
    cur = (xmlAttrPtr) PyxmlNode_Get(pyobj_cur);

    xmlFreeProp(cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParserSetReplaceEntities(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    int replaceEntities;

    if (!PyArg_ParseTuple(args, "Oi:xmlParserSetReplaceEntities", &pyobj_ctxt, &replaceEntities))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    ctxt->replaceEntities = replaceEntities;
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlStrcasecmp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * str1;
    xmlChar * str2;

    if (!PyArg_ParseTuple(args, "zz:xmlStrcasecmp", &str1, &str2))
        return(NULL);

    c_retval = xmlStrcasecmp(str1, str2);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathSumFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathSumFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathSumFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseSystemLiteral(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseSystemLiteral", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseSystemLiteral(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNanoFTPProxy(PyObject *self, PyObject *args) {
    char * host;
    int port;
    char * user;
    char * passwd;
    int type;

    if (!PyArg_ParseTuple(args, "zizzi:xmlNanoFTPProxy", &host, &port, &user, &passwd, &type))
        return(NULL);

    xmlNanoFTPProxy(host, port, user, passwd, type);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNodeSetContentLen(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlChar * content;
    int len;

    if (!PyArg_ParseTuple(args, "Ozi:xmlNodeSetContentLen", &pyobj_cur, &content, &len))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlNodeSetContentLen(cur, content, len);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlStrcmp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * str1;
    xmlChar * str2;

    if (!PyArg_ParseTuple(args, "zz:xmlStrcmp", &str1, &str2))
        return(NULL);

    c_retval = xmlStrcmp(str1, str2);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCreateMemoryParserCtxt(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlParserCtxtPtr c_retval;
    char * buffer;
    int size;

    if (!PyArg_ParseTuple(args, "zi:xmlCreateMemoryParserCtxt", &buffer, &size))
        return(NULL);

    c_retval = xmlCreateMemoryParserCtxt(buffer, size);
    py_retval = libxml_xmlParserCtxtPtrWrap((xmlParserCtxtPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseAttributeListDecl(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseAttributeListDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseAttributeListDecl(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCheckVersion(PyObject *self, PyObject *args) {
    int version;

    if (!PyArg_ParseTuple(args, "i:xmlCheckVersion", &version))
        return(NULL);

    xmlCheckVersion(version);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseName(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseName", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseName(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_nodePush(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr value;
    PyObject *pyobj_value;

    if (!PyArg_ParseTuple(args, "OO:nodePush", &pyobj_ctxt, &pyobj_value))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);
    value = (xmlNodePtr) PyxmlNode_Get(pyobj_value);

    c_retval = nodePush(ctxt, value);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlURIUnescapeString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    char * c_retval;
    char * str;
    int len;
    char * target;

    if (!PyArg_ParseTuple(args, "ziz:xmlURIUnescapeString", &str, &len, &target))
        return(NULL);

    c_retval = xmlURIUnescapeString(str, len, target);
    py_retval = libxml_charPtrWrap((char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCatalogAdd(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * type;
    xmlChar * orig;
    xmlChar * replace;

    if (!PyArg_ParseTuple(args, "zzz:xmlCatalogAdd", &type, &orig, &replace))
        return(NULL);

    c_retval = xmlCatalogAdd(type, orig, replace);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlNewDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    htmlDocPtr c_retval;
    xmlChar * URI;
    xmlChar * ExternalID;

    if (!PyArg_ParseTuple(args, "zz:htmlNewDoc", &URI, &ExternalID))
        return(NULL);

    c_retval = htmlNewDoc(URI, ExternalID);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathIsInf(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    double val;

    if (!PyArg_ParseTuple(args, "d:xmlXPathIsInf", &val))
        return(NULL);

    c_retval = xmlXPathIsInf(val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCopyNode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    int recursive;

    if (!PyArg_ParseTuple(args, "Oi:xmlCopyNode", &pyobj_node, &recursive))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlCopyNode(node, recursive);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCastStringToBoolean(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * val;

    if (!PyArg_ParseTuple(args, "z:xmlXPathCastStringToBoolean", &val))
        return(NULL);

    c_retval = xmlXPathCastStringToBoolean(val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlURISetUser(PyObject *self, PyObject *args) {
    xmlURIPtr URI;
    PyObject *pyobj_URI;
    char * user;

    if (!PyArg_ParseTuple(args, "Oz:xmlURISetUser", &pyobj_URI, &user))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    if (URI->user != NULL) xmlFree(URI->user);
    URI->user = xmlStrdup(user);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlInitializePredefinedEntities(PyObject *self, PyObject *args) {

    xmlInitializePredefinedEntities();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNextAttribute(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextAttribute", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextAttribute(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlIsAutoClosed(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    htmlDocPtr doc;
    PyObject *pyobj_doc;
    htmlNodePtr elem;
    PyObject *pyobj_elem;

    if (!PyArg_ParseTuple(args, "OO:htmlIsAutoClosed", &pyobj_doc, &pyobj_elem))
        return(NULL);
    doc = (htmlDocPtr) PyxmlNode_Get(pyobj_doc);
    elem = (htmlNodePtr) PyxmlNode_Get(pyobj_elem);

    c_retval = htmlIsAutoClosed(doc, elem);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSearchNs(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * nameSpace;

    if (!PyArg_ParseTuple(args, "OOz:xmlSearchNs", &pyobj_doc, &pyobj_node, &nameSpace))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlSearchNs(doc, node, nameSpace);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathVariableLookupNS(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * name;
    xmlChar * ns_uri;

    if (!PyArg_ParseTuple(args, "Ozz:xmlXPathVariableLookupNS", &pyobj_ctxt, &name, &ns_uri))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = xmlXPathVariableLookupNS(ctxt, name, ns_uri);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlReconciliateNs(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr tree;
    PyObject *pyobj_tree;

    if (!PyArg_ParseTuple(args, "OO:xmlReconciliateNs", &pyobj_doc, &pyobj_tree))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    tree = (xmlNodePtr) PyxmlNode_Get(pyobj_tree);

    c_retval = xmlReconciliateNs(doc, tree);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathLangFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathLangFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathLangFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathEvalExpression(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    xmlChar * str;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "zO:xmlXPathEvalExpression", &str, &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = xmlXPathEvalExpression(str, ctxt);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlShellPrintNode(PyObject *self, PyObject *args) {
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "O:xmlShellPrintNode", &pyobj_node))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    xmlShellPrintNode(node);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlConvertSGMLCatalog(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;

    if (!PyArg_ParseTuple(args, "O:xmlConvertSGMLCatalog", &pyobj_catal))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);

    c_retval = xmlConvertSGMLCatalog(catal);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlUTF8Strsub(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * utf;
    int start;
    int len;

    if (!PyArg_ParseTuple(args, "zii:xmlUTF8Strsub", &utf, &start, &len))
        return(NULL);

    c_retval = xmlUTF8Strsub(utf, start, len);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlValidNormalizeAttributeValue(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr elem;
    PyObject *pyobj_elem;
    xmlChar * name;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "OOzz:xmlValidNormalizeAttributeValue", &pyobj_doc, &pyobj_elem, &name, &value))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    elem = (xmlNodePtr) PyxmlNode_Get(pyobj_elem);

    c_retval = xmlValidNormalizeAttributeValue(doc, elem, name, value);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeSetSpacePreserve(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    int val;

    if (!PyArg_ParseTuple(args, "Oi:xmlNodeSetSpacePreserve", &pyobj_cur, &val))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlNodeSetSpacePreserve(cur, val);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNewContext(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathContextPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "O:xmlXPathNewContext", &pyobj_doc))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlXPathNewContext(doc);
    py_retval = libxml_xmlXPathContextPtrWrap((xmlXPathContextPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCreateIntSubset(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDtdPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;
    xmlChar * ExternalID;
    xmlChar * SystemID;

    if (!PyArg_ParseTuple(args, "Ozzz:xmlCreateIntSubset", &pyobj_doc, &name, &ExternalID, &SystemID))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlCreateIntSubset(doc, name, ExternalID, SystemID);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCatalogSetDebug(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int level;

    if (!PyArg_ParseTuple(args, "i:xmlCatalogSetDebug", &level))
        return(NULL);

    c_retval = xmlCatalogSetDebug(level);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathSubValues(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathSubValues", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathSubValues(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNanoHTTPCleanup(PyObject *self, PyObject *args) {

    xmlNanoHTTPCleanup();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNodeSetBase(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlChar * uri;

    if (!PyArg_ParseTuple(args, "Oz:xmlNodeSetBase", &pyobj_cur, &uri))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlNodeSetBase(cur, uri);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParserGetDirectory(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    char * c_retval;
    char * filename;

    if (!PyArg_ParseTuple(args, "z:xmlParserGetDirectory", &filename))
        return(NULL);

    c_retval = xmlParserGetDirectory(filename);
    py_retval = libxml_charPtrWrap((char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseQuotedString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseQuotedString", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseQuotedString(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCastNodeToNumber(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    double c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "O:xmlXPathCastNodeToNumber", &pyobj_node))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlXPathCastNodeToNumber(node);
    py_retval = libxml_doubleWrap((double) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetPredefinedEntity(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlEntityPtr c_retval;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "z:xmlGetPredefinedEntity", &name))
        return(NULL);

    c_retval = xmlGetPredefinedEntity(name);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewText(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "z:xmlNewText", &content))
        return(NULL);

    c_retval = xmlNewText(content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSaveFile(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * filename;
    xmlDocPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "zO:xmlSaveFile", &filename, &pyobj_cur))
        return(NULL);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlSaveFile(filename, cur);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlACatalogResolvePublic(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;
    xmlChar * pubID;

    if (!PyArg_ParseTuple(args, "Oz:xmlACatalogResolvePublic", &pyobj_catal, &pubID))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);

    c_retval = xmlACatalogResolvePublic(catal, pubID);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNextNamespace(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextNamespace", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextNamespace(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStopParser(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlStopParser", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlStopParser(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNewDocText(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "Oz:xmlNewDocText", &pyobj_doc, &content))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewDocText(doc, content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlACatalogResolveSystem(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;
    xmlChar * sysID;

    if (!PyArg_ParseTuple(args, "Oz:xmlACatalogResolveSystem", &pyobj_catal, &sysID))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);

    c_retval = xmlACatalogResolveSystem(catal, sysID);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNewFloat(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    double val;

    if (!PyArg_ParseTuple(args, "d:xmlXPathNewFloat", &val))
        return(NULL);

    c_retval = xmlXPathNewFloat(val);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlFreeCatalog(PyObject *self, PyObject *args) {
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;

    if (!PyArg_ParseTuple(args, "O:xmlFreeCatalog", &pyobj_catal))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);

    xmlFreeCatalog(catal);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCreateDocParserCtxt(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlParserCtxtPtr c_retval;
    xmlChar * cur;

    if (!PyArg_ParseTuple(args, "z:xmlCreateDocParserCtxt", &cur))
        return(NULL);

    c_retval = xmlCreateDocParserCtxt(cur);
    py_retval = libxml_xmlParserCtxtPtrWrap((xmlParserCtxtPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlDocDump(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    FILE * f;
    PyObject *pyobj_f;
    xmlDocPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:htmlDocDump", &pyobj_f, &pyobj_cur))
        return(NULL);
    f = (FILE *) PyFile_Get(pyobj_f);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = htmlDocDump(f, cur);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNewValueTree(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    xmlNodePtr val;
    PyObject *pyobj_val;

    if (!PyArg_ParseTuple(args, "O:xmlXPathNewValueTree", &pyobj_val))
        return(NULL);
    val = (xmlNodePtr) PyxmlNode_Get(pyobj_val);

    c_retval = xmlXPathNewValueTree(val);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathParseName(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathParseName", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = xmlXPathParseName(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlFreeDtd(PyObject *self, PyObject *args) {
    xmlDtdPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlFreeDtd", &pyobj_cur))
        return(NULL);
    cur = (xmlDtdPtr) PyxmlNode_Get(pyobj_cur);

    xmlFreeDtd(cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlFreeNodeList(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlFreeNodeList", &pyobj_cur))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlFreeNodeList(cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathDivValues(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathDivValues", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathDivValues(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathPositionFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathPositionFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathPositionFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlLsCountNode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "O:xmlLsCountNode", &pyobj_node))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlLsCountNode(node);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseCatalogFile(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    char * filename;

    if (!PyArg_ParseTuple(args, "z:xmlParseCatalogFile", &filename))
        return(NULL);

    c_retval = xmlParseCatalogFile(filename);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSetListDoc(PyObject *self, PyObject *args) {
    xmlNodePtr list;
    PyObject *pyobj_list;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "OO:xmlSetListDoc", &pyobj_list, &pyobj_doc))
        return(NULL);
    list = (xmlNodePtr) PyxmlNode_Get(pyobj_list);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    xmlSetListDoc(list, doc);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURISetPath(PyObject *self, PyObject *args) {
    xmlURIPtr URI;
    PyObject *pyobj_URI;
    char * path;

    if (!PyArg_ParseTuple(args, "Oz:xmlURISetPath", &pyobj_URI, &path))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    if (URI->path != NULL) xmlFree(URI->path);
    URI->path = xmlStrdup(path);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathGetFunctionURI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathGetFunctionURI", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = ctxt->functionURI;
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNanoHTTPScanProxy(PyObject *self, PyObject *args) {
    char * URL;

    if (!PyArg_ParseTuple(args, "z:xmlNanoHTTPScanProxy", &URL))
        return(NULL);

    xmlNanoHTTPScanProxy(URL);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNewChild(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr parent;
    PyObject *pyobj_parent;
    xmlNsPtr ns;
    PyObject *pyobj_ns;
    xmlChar * name;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "OOzz:xmlNewChild", &pyobj_parent, &pyobj_ns, &name, &content))
        return(NULL);
    parent = (xmlNodePtr) PyxmlNode_Get(pyobj_parent);
    ns = (xmlNsPtr) PyxmlNode_Get(pyobj_ns);

    c_retval = xmlNewChild(parent, ns, name, content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathLastFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathLastFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathLastFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNodeListGetString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr list;
    PyObject *pyobj_list;
    int inLine;

    if (!PyArg_ParseTuple(args, "OOi:xmlNodeListGetString", &pyobj_doc, &pyobj_list, &inLine))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    list = (xmlNodePtr) PyxmlNode_Get(pyobj_list);

    c_retval = xmlNodeListGetString(doc, list, inLine);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetLastChild(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr parent;
    PyObject *pyobj_parent;

    if (!PyArg_ParseTuple(args, "O:xmlGetLastChild", &pyobj_parent))
        return(NULL);
    parent = (xmlNodePtr) PyxmlNode_Get(pyobj_parent);

    c_retval = xmlGetLastChild(parent);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetEncodingAlias(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    char * alias;

    if (!PyArg_ParseTuple(args, "z:xmlGetEncodingAlias", &alias))
        return(NULL);

    c_retval = xmlGetEncodingAlias(alias);
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlACatalogAdd(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;
    xmlChar * type;
    xmlChar * orig;
    xmlChar * replace;

    if (!PyArg_ParseTuple(args, "Ozzz:xmlACatalogAdd", &pyobj_catal, &type, &orig, &replace))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);

    c_retval = xmlACatalogAdd(catal, type, orig, replace);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlAddDtdEntity(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlEntityPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;
    int type;
    xmlChar * ExternalID;
    xmlChar * SystemID;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "Ozizzz:xmlAddDtdEntity", &pyobj_doc, &name, &type, &ExternalID, &SystemID, &content))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlAddDtdEntity(doc, name, type, ExternalID, SystemID, content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseNotationDecl(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseNotationDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseNotationDecl(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNewCString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    char * val;

    if (!PyArg_ParseTuple(args, "z:xmlXPathNewCString", &val))
        return(NULL);

    c_retval = xmlXPathNewCString(val);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseMisc(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseMisc", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseMisc(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXIncludeProcess(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "O:xmlXIncludeProcess", &pyobj_doc))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlXIncludeProcess(doc);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewDocFragment(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "O:xmlNewDocFragment", &pyobj_doc))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewDocFragment(doc);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathFreeContext(PyObject *self, PyObject *args) {
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathFreeContext", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    xmlXPathFreeContext(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlStrdup(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * cur;

    if (!PyArg_ParseTuple(args, "z:xmlStrdup", &cur))
        return(NULL);

    c_retval = xmlStrdup(cur);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNamespaceURIFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathNamespaceURIFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathNamespaceURIFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlRemoveRef(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlAttrPtr attr;
    PyObject *pyobj_attr;

    if (!PyArg_ParseTuple(args, "OO:xmlRemoveRef", &pyobj_doc, &pyobj_attr))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    attr = (xmlAttrPtr) PyxmlNode_Get(pyobj_attr);

    c_retval = xmlRemoveRef(doc, attr);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSubstituteEntitiesDefault(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int val;

    if (!PyArg_ParseTuple(args, "i:xmlSubstituteEntitiesDefault", &val))
        return(NULL);

    c_retval = xmlSubstituteEntitiesDefault(val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStrncat(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * cur;
    xmlChar * add;
    int len;

    if (!PyArg_ParseTuple(args, "zzi:xmlStrncat", &cur, &add, &len))
        return(NULL);

    c_retval = xmlStrncat(cur, add, len);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlSaveFileEnc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * filename;
    xmlDocPtr cur;
    PyObject *pyobj_cur;
    char * encoding;

    if (!PyArg_ParseTuple(args, "zOz:htmlSaveFileEnc", &filename, &pyobj_cur, &encoding))
        return(NULL);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = htmlSaveFileEnc(filename, cur, encoding);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParsePubidLiteral(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParsePubidLiteral", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParsePubidLiteral(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_namePop(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:namePop", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = namePop(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlInitCharEncodingHandlers(PyObject *self, PyObject *args) {

    xmlInitCharEncodingHandlers();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlACatalogResolve(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;
    xmlChar * pubID;
    xmlChar * sysID;

    if (!PyArg_ParseTuple(args, "Ozz:xmlACatalogResolve", &pyobj_catal, &pubID, &sysID))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);

    c_retval = xmlACatalogResolve(catal, pubID, sysID);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseContent(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseContent", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseContent(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPatherror(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    char * file;
    int line;
    int no;

    if (!PyArg_ParseTuple(args, "Ozii:xmlXPatherror", &pyobj_ctxt, &file, &line, &no))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPatherror(ctxt, file, line, no);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseMemory(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    char * buffer;
    int size;

    if (!PyArg_ParseTuple(args, "zi:xmlParseMemory", &buffer, &size))
        return(NULL);

    c_retval = xmlParseMemory(buffer, size);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStrcasestr(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xmlChar * str;
    xmlChar * val;

    if (!PyArg_ParseTuple(args, "zz:xmlStrcasestr", &str, &val))
        return(NULL);

    c_retval = xmlStrcasestr(str, val);
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCleanupEncodingAliases(PyObject *self, PyObject *args) {

    xmlCleanupEncodingAliases();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlSetNsProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlNsPtr ns;
    PyObject *pyobj_ns;
    xmlChar * name;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "OOzz:xmlSetNsProp", &pyobj_node, &pyobj_ns, &name, &value))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    ns = (xmlNsPtr) PyxmlNode_Get(pyobj_ns);

    c_retval = xmlSetNsProp(node, ns, name, value);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCeilingFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathCeilingFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathCeilingFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURISetScheme(PyObject *self, PyObject *args) {
    xmlURIPtr URI;
    PyObject *pyobj_URI;
    char * scheme;

    if (!PyArg_ParseTuple(args, "Oz:xmlURISetScheme", &pyobj_URI, &scheme))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    if (URI->scheme != NULL) xmlFree(URI->scheme);
    URI->scheme = xmlStrdup(scheme);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlInitParser(PyObject *self, PyObject *args) {

    xmlInitParser();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathStartsWithFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathStartsWithFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathStartsWithFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCreateFileParserCtxt(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlParserCtxtPtr c_retval;
    char * filename;

    if (!PyArg_ParseTuple(args, "z:xmlCreateFileParserCtxt", &filename))
        return(NULL);

    c_retval = xmlCreateFileParserCtxt(filename);
    py_retval = libxml_xmlParserCtxtPtrWrap((xmlParserCtxtPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCopyNamespace(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xmlNsPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlCopyNamespace", &pyobj_cur))
        return(NULL);
    cur = (xmlNsPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlCopyNamespace(cur);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlIsScriptAttribute(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "z:htmlIsScriptAttribute", &name))
        return(NULL);

    c_retval = htmlIsScriptAttribute(name);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseTextDecl(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseTextDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseTextDecl(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNextPreceding(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextPreceding", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextPreceding(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathRegisteredFuncsCleanup(PyObject *self, PyObject *args) {
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathRegisteredFuncsCleanup", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    xmlXPathRegisteredFuncsCleanup(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathRegisterAllFunctions(PyObject *self, PyObject *args) {
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathRegisterAllFunctions", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    xmlXPathRegisterAllFunctions(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathFloorFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathFloorFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathFloorFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURISetFragment(PyObject *self, PyObject *args) {
    xmlURIPtr URI;
    PyObject *pyobj_URI;
    char * fragment;

    if (!PyArg_ParseTuple(args, "Oz:xmlURISetFragment", &pyobj_URI, &fragment))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    if (URI->fragment != NULL) xmlFree(URI->fragment);
    URI->fragment = xmlStrdup(fragment);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlHandleEntity(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlEntityPtr entity;
    PyObject *pyobj_entity;

    if (!PyArg_ParseTuple(args, "OO:xmlHandleEntity", &pyobj_ctxt, &pyobj_entity))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);
    entity = (xmlEntityPtr) PyxmlNode_Get(pyobj_entity);

    xmlHandleEntity(ctxt, entity);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlSkipBlankChars(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlSkipBlankChars", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlSkipBlankChars(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlAddChildList(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr parent;
    PyObject *pyobj_parent;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlAddChildList", &pyobj_parent, &pyobj_cur))
        return(NULL);
    parent = (xmlNodePtr) PyxmlNode_Get(pyobj_parent);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlAddChildList(parent, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetNodePath(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "O:xmlGetNodePath", &pyobj_node))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlGetNodePath(node);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSearchNsByHref(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * href;

    if (!PyArg_ParseTuple(args, "OOz:xmlSearchNsByHref", &pyobj_doc, &pyobj_node, &href))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlSearchNsByHref(doc, node, href);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlFreeParserCtxt(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlFreeParserCtxt", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlFreeParserCtxt(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_htmlParseChunk(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    htmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    char * chunk;
    int size;
    int terminate;

    if (!PyArg_ParseTuple(args, "Ozii:htmlParseChunk", &pyobj_ctxt, &chunk, &size, &terminate))
        return(NULL);
    ctxt = (htmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = htmlParseChunk(ctxt, chunk, size, terminate);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseDTD(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDtdPtr c_retval;
    xmlChar * ExternalID;
    xmlChar * SystemID;

    if (!PyArg_ParseTuple(args, "zz:xmlParseDTD", &ExternalID, &SystemID))
        return(NULL);

    c_retval = xmlParseDTD(ExternalID, SystemID);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewGlobalNs(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * href;
    xmlChar * prefix;

    if (!PyArg_ParseTuple(args, "Ozz:xmlNewGlobalNs", &pyobj_doc, &href, &prefix))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewGlobalNs(doc, href, prefix);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathIdFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathIdFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathIdFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlIsLetter(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int c;

    if (!PyArg_ParseTuple(args, "i:xmlIsLetter", &c))
        return(NULL);

    c_retval = xmlIsLetter(c);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlTextMerge(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr first;
    PyObject *pyobj_first;
    xmlNodePtr second;
    PyObject *pyobj_second;

    if (!PyArg_ParseTuple(args, "OO:xmlTextMerge", &pyobj_first, &pyobj_second))
        return(NULL);
    first = (xmlNodePtr) PyxmlNode_Get(pyobj_first);
    second = (xmlNodePtr) PyxmlNode_Get(pyobj_second);

    c_retval = xmlTextMerge(first, second);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathStringLengthFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathStringLengthFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathStringLengthFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlIsChar(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int c;

    if (!PyArg_ParseTuple(args, "i:xmlIsChar", &c))
        return(NULL);

    c_retval = xmlIsChar(c);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDocDump(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    FILE * f;
    PyObject *pyobj_f;
    xmlDocPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlDocDump", &pyobj_f, &pyobj_cur))
        return(NULL);
    f = (FILE *) PyFile_Get(pyobj_f);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlDocDump(f, cur);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlIsIdeographic(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int c;

    if (!PyArg_ParseTuple(args, "i:xmlIsIdeographic", &c))
        return(NULL);

    c_retval = xmlIsIdeographic(c);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseEntity(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    char * filename;

    if (!PyArg_ParseTuple(args, "z:xmlParseEntity", &filename))
        return(NULL);

    c_retval = xmlParseEntity(filename);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewCatalog(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlCatalogPtr c_retval;
    int sgml;

    if (!PyArg_ParseTuple(args, "i:xmlNewCatalog", &sgml))
        return(NULL);

    c_retval = xmlNewCatalog(sgml);
    py_retval = libxml_xmlCatalogPtrWrap((xmlCatalogPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlPedanticParserDefault(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int val;

    if (!PyArg_ParseTuple(args, "i:xmlPedanticParserDefault", &val))
        return(NULL);

    c_retval = xmlPedanticParserDefault(val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseEndTag(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseEndTag", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseEndTag(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNewReference(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlNewReference", &pyobj_doc, &name))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewReference(doc, name);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    xmlChar * cur;

    if (!PyArg_ParseTuple(args, "z:xmlParseDoc", &cur))
        return(NULL);

    c_retval = xmlParseDoc(cur);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseURI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlURIPtr c_retval;
    char * str;

    if (!PyArg_ParseTuple(args, "z:xmlParseURI", &str))
        return(NULL);

    c_retval = xmlParseURI(str);
    py_retval = libxml_xmlURIPtrWrap((xmlURIPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathParseNCName(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathParseNCName", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = xmlXPathParseNCName(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCopyProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlNodePtr target;
    PyObject *pyobj_target;
    xmlAttrPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlCopyProp", &pyobj_target, &pyobj_cur))
        return(NULL);
    target = (xmlNodePtr) PyxmlNode_Get(pyobj_target);
    cur = (xmlAttrPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlCopyProp(target, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSaveUri(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlURIPtr uri;
    PyObject *pyobj_uri;

    if (!PyArg_ParseTuple(args, "O:xmlSaveUri", &pyobj_uri))
        return(NULL);
    uri = (xmlURIPtr) PyURI_Get(pyobj_uri);

    c_retval = xmlSaveUri(uri);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewCharRef(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlNewCharRef", &pyobj_doc, &name))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewCharRef(doc, name);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNewBoolean(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    int val;

    if (!PyArg_ParseTuple(args, "i:xmlXPathNewBoolean", &val))
        return(NULL);

    c_retval = xmlXPathNewBoolean(val);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathEvalExpr(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathEvalExpr", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathEvalExpr(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlLineNumbersDefault(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int val;

    if (!PyArg_ParseTuple(args, "i:xmlLineNumbersDefault", &val))
        return(NULL);

    c_retval = xmlLineNumbersDefault(val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSetDocCompressMode(PyObject *self, PyObject *args) {
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    int mode;

    if (!PyArg_ParseTuple(args, "Oi:xmlSetDocCompressMode", &pyobj_doc, &mode))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    xmlSetDocCompressMode(doc, mode);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlEncodeEntitiesReentrant(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * input;

    if (!PyArg_ParseTuple(args, "Oz:xmlEncodeEntitiesReentrant", &pyobj_doc, &input))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlEncodeEntitiesReentrant(doc, input);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathMultValues(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathMultValues", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathMultValues(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlRemoveProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlAttrPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlRemoveProp", &pyobj_cur))
        return(NULL);
    cur = (xmlAttrPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlRemoveProp(cur);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlACatalogDump(PyObject *self, PyObject *args) {
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;
    FILE * out;
    PyObject *pyobj_out;

    if (!PyArg_ParseTuple(args, "OO:xmlACatalogDump", &pyobj_catal, &pyobj_out))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);
    out = (FILE *) PyFile_Get(pyobj_out);

    xmlACatalogDump(catal, out);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURISetAuthority(PyObject *self, PyObject *args) {
    xmlURIPtr URI;
    PyObject *pyobj_URI;
    char * authority;

    if (!PyArg_ParseTuple(args, "Oz:xmlURISetAuthority", &pyobj_URI, &authority))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    if (URI->authority != NULL) xmlFree(URI->authority);
    URI->authority = xmlStrdup(authority);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURIGetPort(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlURIPtr URI;
    PyObject *pyobj_URI;

    if (!PyArg_ParseTuple(args, "O:xmlURIGetPort", &pyobj_URI))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    c_retval = URI->port;
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNewParserContext(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathParserContextPtr c_retval;
    xmlChar * str;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "zO:xmlXPathNewParserContext", &str, &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = xmlXPathNewParserContext(str, ctxt);
    py_retval = libxml_xmlXPathParserContextPtrWrap((xmlXPathParserContextPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseFile(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    char * filename;

    if (!PyArg_ParseTuple(args, "z:xmlParseFile", &filename))
        return(NULL);

    c_retval = xmlParseFile(filename);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseDocument(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseDocument", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseDocument(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStrncmp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * str1;
    xmlChar * str2;
    int len;

    if (!PyArg_ParseTuple(args, "zzi:xmlStrncmp", &str1, &str2, &len))
        return(NULL);

    c_retval = xmlStrncmp(str1, str2, len);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlFreeNode(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlFreeNode", &pyobj_cur))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlFreeNode(cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCatalogGetPublic(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xmlChar * pubID;

    if (!PyArg_ParseTuple(args, "z:xmlCatalogGetPublic", &pubID))
        return(NULL);

    c_retval = xmlCatalogGetPublic(pubID);
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathTranslateFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathTranslateFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathTranslateFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURIGetFragment(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    xmlURIPtr URI;
    PyObject *pyobj_URI;

    if (!PyArg_ParseTuple(args, "O:xmlURIGetFragment", &pyobj_URI))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    c_retval = URI->fragment;
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSaveFormatFile(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * filename;
    xmlDocPtr cur;
    PyObject *pyobj_cur;
    int format;

    if (!PyArg_ParseTuple(args, "zOi:xmlSaveFormatFile", &filename, &pyobj_cur, &format))
        return(NULL);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlSaveFormatFile(filename, cur, format);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathEval(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    xmlChar * str;
    xmlXPathContextPtr ctx;
    PyObject *pyobj_ctx;

    if (!PyArg_ParseTuple(args, "zO:xmlXPathEval", &str, &pyobj_ctx))
        return(NULL);
    ctx = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctx);

    c_retval = xmlXPathEval(str, ctx);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNextSelf(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextSelf", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextSelf(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlUTF8Strpos(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * utf;
    int pos;

    if (!PyArg_ParseTuple(args, "zi:xmlUTF8Strpos", &utf, &pos))
        return(NULL);

    c_retval = xmlUTF8Strpos(utf, pos);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParsePITarget(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParsePITarget", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParsePITarget(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlURISetOpaque(PyObject *self, PyObject *args) {
    xmlURIPtr URI;
    PyObject *pyobj_URI;
    char * opaque;

    if (!PyArg_ParseTuple(args, "Oz:xmlURISetOpaque", &pyobj_URI, &opaque))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    if (URI->opaque != NULL) xmlFree(URI->opaque);
    URI->opaque = xmlStrdup(opaque);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlStrEqual(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * str1;
    xmlChar * str2;

    if (!PyArg_ParseTuple(args, "zz:xmlStrEqual", &str1, &str2))
        return(NULL);

    c_retval = xmlStrEqual(str1, str2);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParserSetLineNumbers(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    int linenumbers;

    if (!PyArg_ParseTuple(args, "Oi:xmlParserSetLineNumbers", &pyobj_ctxt, &linenumbers))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    ctxt->linenumbers = linenumbers;
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParsePEReference(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParsePEReference", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParsePEReference(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNormalizeFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathNormalizeFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathNormalizeFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlDelEncodingAlias(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * alias;

    if (!PyArg_ParseTuple(args, "z:xmlDelEncodingAlias", &alias))
        return(NULL);

    c_retval = xmlDelEncodingAlias(alias);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseXMLDecl(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseXMLDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseXMLDecl(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlIsCombining(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int c;

    if (!PyArg_ParseTuple(args, "i:xmlIsCombining", &c))
        return(NULL);

    c_retval = xmlIsCombining(c);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewComment(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "z:xmlNewComment", &content))
        return(NULL);

    c_retval = xmlNewComment(content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlUnsetProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlUnsetProp", &pyobj_node, &name))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlUnsetProp(node, name);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCastStringToNumber(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    double c_retval;
    xmlChar * val;

    if (!PyArg_ParseTuple(args, "z:xmlXPathCastStringToNumber", &val))
        return(NULL);

    c_retval = xmlXPathCastStringToNumber(val);
    py_retval = libxml_doubleWrap((double) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCatalogResolvePublic(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * pubID;

    if (!PyArg_ParseTuple(args, "z:xmlCatalogResolvePublic", &pubID))
        return(NULL);

    c_retval = xmlCatalogResolvePublic(pubID);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewCDataBlock(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * content;
    int len;

    if (!PyArg_ParseTuple(args, "Ozi:xmlNewCDataBlock", &pyobj_doc, &content, &len))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewCDataBlock(doc, content, len);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseURIReference(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlURIPtr uri;
    PyObject *pyobj_uri;
    char * str;

    if (!PyArg_ParseTuple(args, "Oz:xmlParseURIReference", &pyobj_uri, &str))
        return(NULL);
    uri = (xmlURIPtr) PyURI_Get(pyobj_uri);

    c_retval = xmlParseURIReference(uri, str);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCharStrdup(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    char * cur;

    if (!PyArg_ParseTuple(args, "z:xmlCharStrdup", &cur))
        return(NULL);

    c_retval = xmlCharStrdup(cur);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlURIGetServer(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    xmlURIPtr URI;
    PyObject *pyobj_URI;

    if (!PyArg_ParseTuple(args, "O:xmlURIGetServer", &pyobj_URI))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    c_retval = URI->server;
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlIsRef(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr elem;
    PyObject *pyobj_elem;
    xmlAttrPtr attr;
    PyObject *pyobj_attr;

    if (!PyArg_ParseTuple(args, "OOO:xmlIsRef", &pyobj_doc, &pyobj_elem, &pyobj_attr))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    elem = (xmlNodePtr) PyxmlNode_Get(pyobj_elem);
    attr = (xmlAttrPtr) PyxmlNode_Get(pyobj_attr);

    c_retval = xmlIsRef(doc, elem, attr);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_namePush(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "Oz:namePush", &pyobj_ctxt, &value))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = namePush(ctxt, value);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathRegisterNs(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * prefix;
    xmlChar * ns_uri;

    if (!PyArg_ParseTuple(args, "Ozz:xmlXPathRegisterNs", &pyobj_ctxt, &prefix, &ns_uri))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = xmlXPathRegisterNs(ctxt, prefix, ns_uri);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeIsText(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "O:xmlNodeIsText", &pyobj_node))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlNodeIsText(node);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseElement(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseElement", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseElement(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathVariableLookup(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlXPathVariableLookup", &pyobj_ctxt, &name))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = xmlXPathVariableLookup(ctxt, name);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathStringEvalNumber(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    double c_retval;
    xmlChar * str;

    if (!PyArg_ParseTuple(args, "z:xmlXPathStringEvalNumber", &str))
        return(NULL);

    c_retval = xmlXPathStringEvalNumber(str);
    py_retval = libxml_doubleWrap((double) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCmpNodes(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlNodePtr node1;
    PyObject *pyobj_node1;
    xmlNodePtr node2;
    PyObject *pyobj_node2;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathCmpNodes", &pyobj_node1, &pyobj_node2))
        return(NULL);
    node1 = (xmlNodePtr) PyxmlNode_Get(pyobj_node1);
    node2 = (xmlNodePtr) PyxmlNode_Get(pyobj_node2);

    c_retval = xmlXPathCmpNodes(node1, node2);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDebugDumpAttr(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlAttrPtr attr;
    PyObject *pyobj_attr;
    int depth;

    if (!PyArg_ParseTuple(args, "OOi:xmlDebugDumpAttr", &pyobj_output, &pyobj_attr, &depth))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    attr = (xmlAttrPtr) PyxmlNode_Get(pyobj_attr);

    xmlDebugDumpAttr(output, attr, depth);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlUTF8Strsize(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * utf;
    int len;

    if (!PyArg_ParseTuple(args, "zi:xmlUTF8Strsize", &utf, &len))
        return(NULL);

    c_retval = xmlUTF8Strsize(utf, len);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCleanupOutputCallbacks(PyObject *self, PyObject *args) {

    xmlCleanupOutputCallbacks();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathSetContextNode(PyObject *self, PyObject *args) {
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathSetContextNode", &pyobj_ctxt, &pyobj_node))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    ctxt->node = node;
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlSaveFileEnc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * filename;
    xmlDocPtr cur;
    PyObject *pyobj_cur;
    char * encoding;

    if (!PyArg_ParseTuple(args, "zOz:xmlSaveFileEnc", &filename, &pyobj_cur, &encoding))
        return(NULL);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlSaveFileEnc(filename, cur, encoding);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlFreeParserCtxt(PyObject *self, PyObject *args) {
    htmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:htmlFreeParserCtxt", &pyobj_ctxt))
        return(NULL);
    ctxt = (htmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    htmlFreeParserCtxt(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathGetFunction(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathGetFunction", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = ctxt->function;
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlIsMixedElement(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlIsMixedElement", &pyobj_doc, &name))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlIsMixedElement(doc, name);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDebugDumpOneNode(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlNodePtr node;
    PyObject *pyobj_node;
    int depth;

    if (!PyArg_ParseTuple(args, "OOi:xmlDebugDumpOneNode", &pyobj_output, &pyobj_node, &depth))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    xmlDebugDumpOneNode(output, node, depth);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlUTF8Strloc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * utf;
    xmlChar * utfchar;

    if (!PyArg_ParseTuple(args, "zz:xmlUTF8Strloc", &utf, &utfchar))
        return(NULL);

    c_retval = xmlUTF8Strloc(utf, utfchar);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseStartTag(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseStartTag", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseStartTag(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewNs(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * href;
    xmlChar * prefix;

    if (!PyArg_ParseTuple(args, "Ozz:xmlNewNs", &pyobj_node, &href, &prefix))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlNewNs(node, href, prefix);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNumberFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathNumberFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathNumberFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlLsOneNode(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "OO:xmlLsOneNode", &pyobj_output, &pyobj_node))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    xmlLsOneNode(output, node);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseExternalSubset(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * ExternalID;
    xmlChar * SystemID;

    if (!PyArg_ParseTuple(args, "Ozz:xmlParseExternalSubset", &pyobj_ctxt, &ExternalID, &SystemID))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseExternalSubset(ctxt, ExternalID, SystemID);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_htmlGetMetaEncoding(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    htmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "O:htmlGetMetaEncoding", &pyobj_doc))
        return(NULL);
    doc = (htmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = htmlGetMetaEncoding(doc);
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlURIGetOpaque(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    xmlURIPtr URI;
    PyObject *pyobj_URI;

    if (!PyArg_ParseTuple(args, "O:xmlURIGetOpaque", &pyobj_URI))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    c_retval = URI->opaque;
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathPopBoolean(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathPopBoolean", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = xmlXPathPopBoolean(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStrndup(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * cur;
    int len;

    if (!PyArg_ParseTuple(args, "zi:xmlStrndup", &cur, &len))
        return(NULL);

    c_retval = xmlStrndup(cur, len);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathParserGetContext(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathContextPtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathParserGetContext", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = ctxt->context;
    py_retval = libxml_xmlXPathContextPtrWrap((xmlXPathContextPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlRecoverFile(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    char * filename;

    if (!PyArg_ParseTuple(args, "z:xmlRecoverFile", &filename))
        return(NULL);

    c_retval = xmlRecoverFile(filename);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlURIEscapeStr(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * str;
    xmlChar * list;

    if (!PyArg_ParseTuple(args, "zz:xmlURIEscapeStr", &str, &list))
        return(NULL);

    c_retval = xmlURIEscapeStr(str, list);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNextFollowingSibling(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextFollowingSibling", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextFollowingSibling(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlIsExtender(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int c;

    if (!PyArg_ParseTuple(args, "i:xmlIsExtender", &c))
        return(NULL);

    c_retval = xmlIsExtender(c);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlAddDocEntity(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlEntityPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;
    int type;
    xmlChar * ExternalID;
    xmlChar * SystemID;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "Ozizzz:xmlAddDocEntity", &pyobj_doc, &name, &type, &ExternalID, &SystemID, &content))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlAddDocEntity(doc, name, type, ExternalID, SystemID, content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathRoot(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathRoot", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathRoot(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathCompareValues(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int inf;
    int strict;

    if (!PyArg_ParseTuple(args, "Oii:xmlXPathCompareValues", &pyobj_ctxt, &inf, &strict))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = xmlXPathCompareValues(ctxt, inf, strict);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCountFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathCountFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathCountFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlPrintURI(PyObject *self, PyObject *args) {
    FILE * stream;
    PyObject *pyobj_stream;
    xmlURIPtr uri;
    PyObject *pyobj_uri;

    if (!PyArg_ParseTuple(args, "OO:xmlPrintURI", &pyobj_stream, &pyobj_uri))
        return(NULL);
    stream = (FILE *) PyFile_Get(pyobj_stream);
    uri = (xmlURIPtr) PyURI_Get(pyobj_uri);

    xmlPrintURI(stream, uri);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathIsNodeType(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "z:xmlXPathIsNodeType", &name))
        return(NULL);

    c_retval = xmlXPathIsNodeType(name);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlUTF8Strndup(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * utf;
    int len;

    if (!PyArg_ParseTuple(args, "zi:xmlUTF8Strndup", &utf, &len))
        return(NULL);

    c_retval = xmlUTF8Strndup(utf, len);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlBuildURI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * URI;
    xmlChar * base;

    if (!PyArg_ParseTuple(args, "zz:xmlBuildURI", &URI, &base))
        return(NULL);

    c_retval = xmlBuildURI(URI, base);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCastBooleanToString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    int val;

    if (!PyArg_ParseTuple(args, "i:xmlXPathCastBooleanToString", &val))
        return(NULL);

    c_retval = xmlXPathCastBooleanToString(val);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathSubstringBeforeFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathSubstringBeforeFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathSubstringBeforeFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURISetServer(PyObject *self, PyObject *args) {
    xmlURIPtr URI;
    PyObject *pyobj_URI;
    char * server;

    if (!PyArg_ParseTuple(args, "Oz:xmlURISetServer", &pyobj_URI, &server))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    if (URI->server != NULL) xmlFree(URI->server);
    URI->server = xmlStrdup(server);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNextFollowing(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextFollowing", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextFollowing(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_valuePop(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:valuePop", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = valuePop(ctxt);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCleanupPredefinedEntities(PyObject *self, PyObject *args) {

    xmlCleanupPredefinedEntities();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlReplaceNode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr old;
    PyObject *pyobj_old;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlReplaceNode", &pyobj_old, &pyobj_cur))
        return(NULL);
    old = (xmlNodePtr) PyxmlNode_Get(pyobj_old);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlReplaceNode(old, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNewNodeSet(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    xmlNodePtr val;
    PyObject *pyobj_val;

    if (!PyArg_ParseTuple(args, "O:xmlXPathNewNodeSet", &pyobj_val))
        return(NULL);
    val = (xmlNodePtr) PyxmlNode_Get(pyobj_val);

    c_retval = xmlXPathNewNodeSet(val);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewTextChild(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr parent;
    PyObject *pyobj_parent;
    xmlNsPtr ns;
    PyObject *pyobj_ns;
    xmlChar * name;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "OOzz:xmlNewTextChild", &pyobj_parent, &pyobj_ns, &name, &content))
        return(NULL);
    parent = (xmlNodePtr) PyxmlNode_Get(pyobj_parent);
    ns = (xmlNsPtr) PyxmlNode_Get(pyobj_ns);

    c_retval = xmlNewTextChild(parent, ns, name, content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStringDecodeEntities(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * str;
    int what;
    xmlChar end;
    xmlChar end2;
    xmlChar end3;

    if (!PyArg_ParseTuple(args, "Oziccc:xmlStringDecodeEntities", &pyobj_ctxt, &str, &what, &end, &end2, &end3))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlStringDecodeEntities(ctxt, str, what, end, end2, end3);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCatalogConvert(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;

    c_retval = xmlCatalogConvert();
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCharStrndup(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    char * cur;
    int len;

    if (!PyArg_ParseTuple(args, "zi:xmlCharStrndup", &cur, &len))
        return(NULL);

    c_retval = xmlCharStrndup(cur, len);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlAddChild(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr parent;
    PyObject *pyobj_parent;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlAddChild", &pyobj_parent, &pyobj_cur))
        return(NULL);
    parent = (xmlNodePtr) PyxmlNode_Get(pyobj_parent);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlAddChild(parent, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeSetName(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlChar * name;

    if (!PyArg_ParseTuple(args, "Oz:xmlNodeSetName", &pyobj_cur, &name))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlNodeSetName(cur, name);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNewDocRawNode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNsPtr ns;
    PyObject *pyobj_ns;
    xmlChar * name;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "OOzz:xmlNewDocRawNode", &pyobj_doc, &pyobj_ns, &name, &content))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    ns = (xmlNsPtr) PyxmlNode_Get(pyobj_ns);

    c_retval = xmlNewDocRawNode(doc, ns, name, content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewDocComment(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * content;

    if (!PyArg_ParseTuple(args, "Oz:xmlNewDocComment", &pyobj_doc, &content))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewDocComment(doc, content);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseVersionInfo(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseVersionInfo", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseVersionInfo(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlFreeURI(PyObject *self, PyObject *args) {
    xmlURIPtr uri;
    PyObject *pyobj_uri;

    if (!PyArg_ParseTuple(args, "O:xmlFreeURI", &pyobj_uri))
        return(NULL);
    uri = (xmlURIPtr) PyURI_Get(pyobj_uri);

    xmlFreeURI(uri);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlSetTreeDoc(PyObject *self, PyObject *args) {
    xmlNodePtr tree;
    PyObject *pyobj_tree;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "OO:xmlSetTreeDoc", &pyobj_tree, &pyobj_doc))
        return(NULL);
    tree = (xmlNodePtr) PyxmlNode_Get(pyobj_tree);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    xmlSetTreeDoc(tree, doc);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCopyNodeList(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "O:xmlCopyNodeList", &pyobj_node))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlCopyNodeList(node);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNextParent(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextParent", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextParent(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlStrchr(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xmlChar * str;
    xmlChar val;

    if (!PyArg_ParseTuple(args, "zc:xmlStrchr", &str, &val))
        return(NULL);

    c_retval = xmlStrchr(str, val);
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseSDDecl(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseSDDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseSDDecl(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathGetContextSize(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathGetContextSize", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = ctxt->contextSize;
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSetProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * name;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "Ozz:xmlSetProp", &pyobj_node, &name, &value))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlSetProp(node, name, value);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeGetContent(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlNodeGetContent", &pyobj_cur))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlNodeGetContent(cur);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlRemoveID(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlAttrPtr attr;
    PyObject *pyobj_attr;

    if (!PyArg_ParseTuple(args, "OO:xmlRemoveID", &pyobj_doc, &pyobj_attr))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    attr = (xmlAttrPtr) PyxmlNode_Get(pyobj_attr);

    c_retval = xmlRemoveID(doc, attr);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCheckUTF8(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    unsigned char * utf;

    if (!PyArg_ParseTuple(args, "z:xmlCheckUTF8", &utf))
        return(NULL);

    c_retval = xmlCheckUTF8(utf);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathNotFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathNotFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathNotFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathNextChild(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlXPathNextChild", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlXPathNextChild(ctxt, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeSetLang(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlChar * lang;

    if (!PyArg_ParseTuple(args, "Oz:xmlNodeSetLang", &pyobj_cur, &lang))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlNodeSetLang(cur, lang);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNodeGetSpacePreserve(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlNodeGetSpacePreserve", &pyobj_cur))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlNodeGetSpacePreserve(cur);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCatalogResolveURI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlChar * URI;

    if (!PyArg_ParseTuple(args, "z:xmlCatalogResolveURI", &URI))
        return(NULL);

    c_retval = xmlCatalogResolveURI(URI);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathPopNumber(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    double c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathPopNumber", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = xmlXPathPopNumber(ctxt);
    py_retval = libxml_doubleWrap((double) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathModValues(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathModValues", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathModValues(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlIsBlank(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int c;

    if (!PyArg_ParseTuple(args, "i:xmlIsBlank", &c))
        return(NULL);

    c_retval = xmlIsBlank(c);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathIsNaN(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    double val;

    if (!PyArg_ParseTuple(args, "d:xmlXPathIsNaN", &val))
        return(NULL);

    c_retval = xmlXPathIsNaN(val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlURIGetScheme(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    xmlURIPtr URI;
    PyObject *pyobj_URI;

    if (!PyArg_ParseTuple(args, "O:xmlURIGetScheme", &pyobj_URI))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    c_retval = URI->scheme;
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseEncName(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseEncName", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseEncName(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNanoFTPInit(PyObject *self, PyObject *args) {

    xmlNanoFTPInit();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathCastBooleanToNumber(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    double c_retval;
    int val;

    if (!PyArg_ParseTuple(args, "i:xmlXPathCastBooleanToNumber", &val))
        return(NULL);

    c_retval = xmlXPathCastBooleanToNumber(val);
    py_retval = libxml_doubleWrap((double) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlAutoCloseTag(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    htmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;
    htmlNodePtr elem;
    PyObject *pyobj_elem;

    if (!PyArg_ParseTuple(args, "OzO:htmlAutoCloseTag", &pyobj_doc, &name, &pyobj_elem))
        return(NULL);
    doc = (htmlDocPtr) PyxmlNode_Get(pyobj_doc);
    elem = (htmlNodePtr) PyxmlNode_Get(pyobj_elem);

    c_retval = htmlAutoCloseTag(doc, name, elem);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDocSetRootElement(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlNodePtr root;
    PyObject *pyobj_root;

    if (!PyArg_ParseTuple(args, "OO:xmlDocSetRootElement", &pyobj_doc, &pyobj_root))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);
    root = (xmlNodePtr) PyxmlNode_Get(pyobj_root);

    c_retval = xmlDocSetRootElement(doc, root);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetLineNo(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    long c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "O:xmlGetLineNo", &pyobj_node))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xmlGetLineNo(node);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNodeGetLang(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlNodeGetLang", &pyobj_cur))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlNodeGetLang(cur);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlGetDocCompressMode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "O:xmlGetDocCompressMode", &pyobj_doc))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlGetDocCompressMode(doc);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNewParserCtxt(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlParserCtxtPtr c_retval;

    c_retval = xmlNewParserCtxt();
    py_retval = libxml_xmlParserCtxtPtrWrap((xmlParserCtxtPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDebugDumpDocumentHead(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "OO:xmlDebugDumpDocumentHead", &pyobj_output, &pyobj_doc))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    xmlDebugDumpDocumentHead(output, doc);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNanoFTPScanProxy(PyObject *self, PyObject *args) {
    char * URL;

    if (!PyArg_ParseTuple(args, "z:xmlNanoFTPScanProxy", &URL))
        return(NULL);

    xmlNanoFTPScanProxy(URL);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlUnlinkNode(PyObject *self, PyObject *args) {
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlUnlinkNode", &pyobj_cur))
        return(NULL);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xmlUnlinkNode(cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCreateEntityParserCtxt(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlParserCtxtPtr c_retval;
    xmlChar * URL;
    xmlChar * ID;
    xmlChar * base;

    if (!PyArg_ParseTuple(args, "zzz:xmlCreateEntityParserCtxt", &URL, &ID, &base))
        return(NULL);

    c_retval = xmlCreateEntityParserCtxt(URL, ID, base);
    py_retval = libxml_xmlParserCtxtPtrWrap((xmlParserCtxtPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlParseElement(PyObject *self, PyObject *args) {
    htmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:htmlParseElement", &pyobj_ctxt))
        return(NULL);
    ctxt = (htmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    htmlParseElement(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCopyPropList(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xmlNodePtr target;
    PyObject *pyobj_target;
    xmlAttrPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xmlCopyPropList", &pyobj_target, &pyobj_cur))
        return(NULL);
    target = (xmlNodePtr) PyxmlNode_Get(pyobj_target);
    cur = (xmlAttrPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlCopyPropList(target, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDebugDumpDocument(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "OO:xmlDebugDumpDocument", &pyobj_output, &pyobj_doc))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    xmlDebugDumpDocument(output, doc);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlKeepBlanksDefault(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int val;

    if (!PyArg_ParseTuple(args, "i:xmlKeepBlanksDefault", &val))
        return(NULL);

    c_retval = xmlKeepBlanksDefault(val);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlDecodeEntities(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    int len;
    int what;
    xmlChar end;
    xmlChar end2;
    xmlChar end3;

    if (!PyArg_ParseTuple(args, "Oiiccc:xmlDecodeEntities", &pyobj_ctxt, &len, &what, &end, &end2, &end3))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlDecodeEntities(ctxt, len, what, end, end2, end3);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlPopInput(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlPopInput", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlPopInput(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathGetContextDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlXPathGetContextDoc", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = ctxt->doc;
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathBooleanFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathBooleanFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathBooleanFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNewDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    xmlChar * version;

    if (!PyArg_ParseTuple(args, "z:xmlNewDoc", &version))
        return(NULL);

    c_retval = xmlNewDoc(version);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlACatalogRemove(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlCatalogPtr catal;
    PyObject *pyobj_catal;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "Oz:xmlACatalogRemove", &pyobj_catal, &value))
        return(NULL);
    catal = (xmlCatalogPtr) Pycatalog_Get(pyobj_catal);

    c_retval = xmlACatalogRemove(catal, value);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlNamespaceParseNSDef(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlNamespaceParseNSDef", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlNamespaceParseNSDef(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlXPathCastNumberToString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    double val;

    if (!PyArg_ParseTuple(args, "d:xmlXPathCastNumberToString", &val))
        return(NULL);

    c_retval = xmlXPathCastNumberToString(val);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlSetMetaEncoding(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    htmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * encoding;

    if (!PyArg_ParseTuple(args, "Oz:htmlSetMetaEncoding", &pyobj_doc, &encoding))
        return(NULL);
    doc = (htmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = htmlSetMetaEncoding(doc, encoding);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParserSetValidate(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;
    int validate;

    if (!PyArg_ParseTuple(args, "Oi:xmlParserSetValidate", &pyobj_ctxt, &validate))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    ctxt->validate = validate;
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseComment(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseComment", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseComment(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlXPathSubstringAfterFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xmlXPathSubstringAfterFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xmlXPathSubstringAfterFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCatalogRemove(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "z:xmlCatalogRemove", &value))
        return(NULL);

    c_retval = xmlCatalogRemove(value);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlSaveFormatFileEnc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * filename;
    xmlDocPtr cur;
    PyObject *pyobj_cur;
    char * encoding;
    int format;

    if (!PyArg_ParseTuple(args, "zOzi:xmlSaveFormatFileEnc", &filename, &pyobj_cur, &encoding, &format))
        return(NULL);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xmlSaveFormatFileEnc(filename, cur, encoding, format);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_htmlParseCharRef(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    htmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:htmlParseCharRef", &pyobj_ctxt))
        return(NULL);
    ctxt = (htmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = htmlParseCharRef(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseNmtoken(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseNmtoken", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = xmlParseNmtoken(ctxt);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParserGetIsValid(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParserGetIsValid", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->valid;
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseReference(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseReference", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseReference(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlDebugDumpDTD(PyObject *self, PyObject *args) {
    FILE * output;
    PyObject *pyobj_output;
    xmlDtdPtr dtd;
    PyObject *pyobj_dtd;

    if (!PyArg_ParseTuple(args, "OO:xmlDebugDumpDTD", &pyobj_output, &pyobj_dtd))
        return(NULL);
    output = (FILE *) PyFile_Get(pyobj_output);
    dtd = (xmlDtdPtr) PyxmlNode_Get(pyobj_dtd);

    xmlDebugDumpDTD(output, dtd);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlRecoverDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    xmlChar * cur;

    if (!PyArg_ParseTuple(args, "z:xmlRecoverDoc", &cur))
        return(NULL);

    c_retval = xmlRecoverDoc(cur);
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlCleanupParser(PyObject *self, PyObject *args) {

    xmlCleanupParser();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlNewDtd(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDtdPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;
    xmlChar * name;
    xmlChar * ExternalID;
    xmlChar * SystemID;

    if (!PyArg_ParseTuple(args, "Ozzz:xmlNewDtd", &pyobj_doc, &name, &ExternalID, &SystemID))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xmlNewDtd(doc, name, ExternalID, SystemID);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParseNamespace(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseNamespace", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseNamespace(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlParseDocTypeDecl(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParseDocTypeDecl", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParseDocTypeDecl(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlFreeDoc(PyObject *self, PyObject *args) {
    xmlDocPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "O:xmlFreeDoc", &pyobj_cur))
        return(NULL);
    cur = (xmlDocPtr) PyxmlNode_Get(pyobj_cur);

    xmlFreeDoc(cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlClearParserCtxt(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlClearParserCtxt", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlClearParserCtxt(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlURIGetUser(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    xmlURIPtr URI;
    PyObject *pyobj_URI;

    if (!PyArg_ParseTuple(args, "O:xmlURIGetUser", &pyobj_URI))
        return(NULL);
    URI = (xmlURIPtr) PyURI_Get(pyobj_URI);

    c_retval = URI->user;
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxml_xmlParsePI(PyObject *self, PyObject *args) {
    xmlParserCtxtPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xmlParsePI", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlParserCtxtPtr) PyparserCtxt_Get(pyobj_ctxt);

    xmlParsePI(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlCatalogDump(PyObject *self, PyObject *args) {
    FILE * out;
    PyObject *pyobj_out;

    if (!PyArg_ParseTuple(args, "O:xmlCatalogDump", &pyobj_out))
        return(NULL);
    out = (FILE *) PyFile_Get(pyobj_out);

    xmlCatalogDump(out);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxml_xmlIsBaseChar(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int c;

    if (!PyArg_ParseTuple(args, "i:xmlIsBaseChar", &c))
        return(NULL);

    c_retval = xmlIsBaseChar(c);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

