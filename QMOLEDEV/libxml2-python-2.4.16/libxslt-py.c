/* Generated */

#include "libxslt_wrap.h"
#include "libxslt-py.h"

PyObject *
libxslt_xsltTransformGetInstruction(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltTransformGetInstruction", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->inst;
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltTransformGetModeURI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltTransformGetModeURI", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->modeURI;
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltGenerateIdFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xsltGenerateIdFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xsltGenerateIdFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltApplyAttributeSet(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlNodePtr inst;
    PyObject *pyobj_inst;
    xmlChar * attributes;

    if (!PyArg_ParseTuple(args, "OOOz:xsltApplyAttributeSet", &pyobj_ctxt, &pyobj_node, &pyobj_inst, &attributes))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    inst = (xmlNodePtr) PyxmlNode_Get(pyobj_inst);

    xsltApplyAttributeSet(ctxt, node, inst, attributes);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltTransformGetContext(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathContextPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltTransformGetContext", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->xpathCtxt;
    py_retval = libxml_xmlXPathContextPtrWrap((xmlXPathContextPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltAttrListTemplateProcess(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr target;
    PyObject *pyobj_target;
    xmlAttrPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OOO:xsltAttrListTemplateProcess", &pyobj_ctxt, &pyobj_target, &pyobj_cur))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    target = (xmlNodePtr) PyxmlNode_Get(pyobj_target);
    cur = (xmlAttrPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xsltAttrListTemplateProcess(ctxt, target, cur);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltParseStylesheetAttributeSet(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xsltParseStylesheetAttributeSet", &pyobj_style, &pyobj_cur))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xsltParseStylesheetAttributeSet(style, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltMessage(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlNodePtr inst;
    PyObject *pyobj_inst;

    if (!PyArg_ParseTuple(args, "OOO:xsltMessage", &pyobj_ctxt, &pyobj_node, &pyobj_inst))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    inst = (xmlNodePtr) PyxmlNode_Get(pyobj_inst);

    xsltMessage(ctxt, node, inst);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltParseStylesheetProcess(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltStylesheetPtr c_retval;
    xsltStylesheetPtr ret;
    PyObject *pyobj_ret;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "OO:xsltParseStylesheetProcess", &pyobj_ret, &pyobj_doc))
        return(NULL);
    ret = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_ret);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xsltParseStylesheetProcess(ret, doc);
    py_retval = libxslt_xsltStylesheetPtrWrap((xsltStylesheetPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltUnparsedEntityURIFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xsltUnparsedEntityURIFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xsltUnparsedEntityURIFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltXPathGetTransformContext(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltTransformContextPtr c_retval;
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltXPathGetTransformContext", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    c_retval = ctxt->extra;
    py_retval = libxslt_xsltTransformContextPtrWrap((xsltTransformContextPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltStylesheetGetImports(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltStylesheetPtr c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetImports", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->imports;
    py_retval = libxslt_xsltStylesheetPtrWrap((xsltStylesheetPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltNextImport(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltStylesheetPtr c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltNextImport", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = xsltNextImport(style);
    py_retval = libxslt_xsltStylesheetPtrWrap((xsltStylesheetPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltFreeCtxtExts(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltFreeCtxtExts", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    xsltFreeCtxtExts(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltNamespaceAlias(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "OO:xsltNamespaceAlias", &pyobj_style, &pyobj_node))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    xsltNamespaceAlias(style, node);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltTransformGetInsertNode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltTransformGetInsertNode", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->insert;
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltFreeDocuments(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltFreeDocuments", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    xsltFreeDocuments(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltStylePreCompute(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr inst;
    PyObject *pyobj_inst;

    if (!PyArg_ParseTuple(args, "OO:xsltStylePreCompute", &pyobj_style, &pyobj_inst))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    inst = (xmlNodePtr) PyxmlNode_Get(pyobj_inst);

    xsltStylePreCompute(style, inst);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltDocumentFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xsltDocumentFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xsltDocumentFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltNewStylesheet(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltStylesheetPtr c_retval;

    c_retval = xsltNewStylesheet();
    py_retval = libxslt_xsltStylesheetPtrWrap((xsltStylesheetPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltStylesheetGetNext(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltStylesheetPtr c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetNext", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->next;
    py_retval = libxslt_xsltStylesheetPtrWrap((xsltStylesheetPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltCopyNamespaceList(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlNsPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OOO:xsltCopyNamespaceList", &pyobj_ctxt, &pyobj_node, &pyobj_cur))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    cur = (xmlNsPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xsltCopyNamespaceList(ctxt, node, cur);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltCleanupGlobals(PyObject *self, PyObject *args) {

    xsltCleanupGlobals();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltParseTemplateContent(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr templ;
    PyObject *pyobj_templ;

    if (!PyArg_ParseTuple(args, "OO:xsltParseTemplateContent", &pyobj_style, &pyobj_templ))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    templ = (xmlNodePtr) PyxmlNode_Get(pyobj_templ);

    xsltParseTemplateContent(style, templ);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltEvalOneUserParam(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * name;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "Ozz:xsltEvalOneUserParam", &pyobj_ctxt, &name, &value))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = xsltEvalOneUserParam(ctxt, name, value);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltUnregisterExtModuleFunction(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * name;
    xmlChar * URI;

    if (!PyArg_ParseTuple(args, "zz:xsltUnregisterExtModuleFunction", &name, &URI))
        return(NULL);

    c_retval = xsltUnregisterExtModuleFunction(name, URI);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltFreeGlobalVariables(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltFreeGlobalVariables", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    xsltFreeGlobalVariables(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltSaveResultToFile(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    FILE * file;
    PyObject *pyobj_file;
    xmlDocPtr result;
    PyObject *pyobj_result;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "OOO:xsltSaveResultToFile", &pyobj_file, &pyobj_result, &pyobj_style))
        return(NULL);
    file = (FILE *) PyFile_Get(pyobj_file);
    result = (xmlDocPtr) PyxmlNode_Get(pyobj_result);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = xsltSaveResultToFile(file, result, style);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltParseGlobalVariable(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xsltParseGlobalVariable", &pyobj_style, &pyobj_cur))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xsltParseGlobalVariable(style, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltShutdownExts(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltShutdownExts", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    xsltShutdownExts(style);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFindElemSpaceHandling(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "OO:xsltFindElemSpaceHandling", &pyobj_ctxt, &pyobj_node))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xsltFindElemSpaceHandling(ctxt, node);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltStylesheetGetParent(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltStylesheetPtr c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetParent", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->parent;
    py_retval = libxslt_xsltStylesheetPtrWrap((xsltStylesheetPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltSaveResultToFd(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    int fd;
    xmlDocPtr result;
    PyObject *pyobj_result;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "iOO:xsltSaveResultToFd", &fd, &pyobj_result, &pyobj_style))
        return(NULL);
    result = (xmlDocPtr) PyxmlNode_Get(pyobj_result);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = xsltSaveResultToFd(fd, result, style);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltCopyNamespace(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlNsPtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OOO:xsltCopyNamespace", &pyobj_ctxt, &pyobj_node, &pyobj_cur))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    cur = (xmlNsPtr) PyxmlNode_Get(pyobj_cur);

    c_retval = xsltCopyNamespace(ctxt, node, cur);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltCalibrateAdjust(PyObject *self, PyObject *args) {
    long delta;

    if (!PyArg_ParseTuple(args, "i:xsltCalibrateAdjust", &delta))
        return(NULL);

    xsltCalibrateAdjust(delta);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltSaveProfiling(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    FILE * output;
    PyObject *pyobj_output;

    if (!PyArg_ParseTuple(args, "OO:xsltSaveProfiling", &pyobj_ctxt, &pyobj_output))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    output = (FILE *) PyFile_Get(pyobj_output);

    xsltSaveProfiling(ctxt, output);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFunctionNodeSet(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xsltFunctionNodeSet", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xsltFunctionNodeSet(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltCleanupTemplates(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltCleanupTemplates", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    xsltCleanupTemplates(style);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltCheckExtPrefix(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlChar * prefix;

    if (!PyArg_ParseTuple(args, "Oz:xsltCheckExtPrefix", &pyobj_style, &prefix))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = xsltCheckExtPrefix(style, prefix);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltElementAvailableFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xsltElementAvailableFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xsltElementAvailableFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltTimestamp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    long c_retval;

    c_retval = xsltTimestamp();
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltRegisterExtras(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltRegisterExtras", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    xsltRegisterExtras(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFunctionAvailableFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xsltFunctionAvailableFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xsltFunctionAvailableFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltSetXIncludeDefault(PyObject *self, PyObject *args) {
    int xinclude;

    if (!PyArg_ParseTuple(args, "i:xsltSetXIncludeDefault", &xinclude))
        return(NULL);

    xsltSetXIncludeDefault(xinclude);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFormatNumberFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xsltFormatNumberFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xsltFormatNumberFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltEvalTemplateString(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlNodePtr parent;
    PyObject *pyobj_parent;

    if (!PyArg_ParseTuple(args, "OOO:xsltEvalTemplateString", &pyobj_ctxt, &pyobj_node, &pyobj_parent))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);
    parent = (xmlNodePtr) PyxmlNode_Get(pyobj_parent);

    c_retval = xsltEvalTemplateString(ctxt, node, parent);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltGetNsProp(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * name;
    xmlChar * nameSpace;

    if (!PyArg_ParseTuple(args, "Ozz:xsltGetNsProp", &pyobj_node, &name, &nameSpace))
        return(NULL);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xsltGetNsProp(node, name, nameSpace);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltAddKey(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlChar * name;
    xmlChar * nameURI;
    xmlChar * match;
    xmlChar * use;
    xmlNodePtr inst;
    PyObject *pyobj_inst;

    if (!PyArg_ParseTuple(args, "OzzzzO:xsltAddKey", &pyobj_style, &name, &nameURI, &match, &use, &pyobj_inst))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    inst = (xmlNodePtr) PyxmlNode_Get(pyobj_inst);

    c_retval = xsltAddKey(style, name, nameURI, match, use, inst);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltNewTransformContext(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltTransformContextPtr c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "OO:xsltNewTransformContext", &pyobj_style, &pyobj_doc))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xsltNewTransformContext(style, doc);
    py_retval = libxslt_xsltTransformContextPtrWrap((xsltTransformContextPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltRegisterAllFunctions(PyObject *self, PyObject *args) {
    xmlXPathContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltRegisterAllFunctions", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathContextPtr) PyxmlXPathContext_Get(pyobj_ctxt);

    xsltRegisterAllFunctions(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltApplyStripSpaces(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "OO:xsltApplyStripSpaces", &pyobj_ctxt, &pyobj_node))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    xsltApplyStripSpaces(ctxt, node);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltUnregisterExtModuleTopLevel(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * name;
    xmlChar * URI;

    if (!PyArg_ParseTuple(args, "zz:xsltUnregisterExtModuleTopLevel", &name, &URI))
        return(NULL);

    c_retval = xsltUnregisterExtModuleTopLevel(name, URI);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltParseStylesheetImport(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xsltParseStylesheetImport", &pyobj_style, &pyobj_cur))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xsltParseStylesheetImport(style, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltXPathParserGetContext(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathContextPtr c_retval;
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltXPathParserGetContext", &pyobj_ctxt))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    c_retval = ctxt->context;
    py_retval = libxml_xmlXPathContextPtrWrap((xmlXPathContextPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltAllocateExtraCtxt(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltAllocateExtraCtxt", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = xsltAllocateExtraCtxt(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltStylesheetGetMethod(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetMethod", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->method;
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltRegisterTestModule(PyObject *self, PyObject *args) {

    xsltRegisterTestModule();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFreeKeys(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltFreeKeys", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    xsltFreeKeys(style);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltParseStylesheetVariable(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xsltParseStylesheetVariable", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xsltParseStylesheetVariable(ctxt, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltGetSpecialNamespace(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlChar * URI;
    xmlChar * prefix;
    xmlNodePtr out;
    PyObject *pyobj_out;

    if (!PyArg_ParseTuple(args, "OOzzO:xsltGetSpecialNamespace", &pyobj_ctxt, &pyobj_cur, &URI, &prefix, &pyobj_out))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);
    out = (xmlNodePtr) PyxmlNode_Get(pyobj_out);

    c_retval = xsltGetSpecialNamespace(ctxt, cur, URI, prefix, out);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltLoadStylesheetPI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltStylesheetPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "O:xsltLoadStylesheetPI", &pyobj_doc))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xsltLoadStylesheetPI(doc);
    py_retval = libxslt_xsltStylesheetPtrWrap((xsltStylesheetPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltStylesheetGetDoctypePublic(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetDoctypePublic", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->doctypePublic;
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltAllocateExtra(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltAllocateExtra", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = xsltAllocateExtra(style);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltAttrTemplateProcess(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlAttrPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr target;
    PyObject *pyobj_target;
    xmlAttrPtr attr;
    PyObject *pyobj_attr;

    if (!PyArg_ParseTuple(args, "OOO:xsltAttrTemplateProcess", &pyobj_ctxt, &pyobj_target, &pyobj_attr))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    target = (xmlNodePtr) PyxmlNode_Get(pyobj_target);
    attr = (xmlAttrPtr) PyxmlNode_Get(pyobj_attr);

    c_retval = xsltAttrTemplateProcess(ctxt, target, attr);
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltNeedElemSpaceHandling(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltNeedElemSpaceHandling", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = xsltNeedElemSpaceHandling(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltTransformGetOutputDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltTransformGetOutputDoc", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->output;
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltFreeTemplateHashes(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltFreeTemplateHashes", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    xsltFreeTemplateHashes(style);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltInitCtxtExts(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltInitCtxtExts", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = xsltInitCtxtExts(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltStylesheetGetEncoding(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetEncoding", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->encoding;
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltTransformGetStyle(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltStylesheetPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltTransformGetStyle", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->style;
    py_retval = libxslt_xsltStylesheetPtrWrap((xsltStylesheetPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltTransformGetMode(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const xmlChar * c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltTransformGetMode", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->mode;
    py_retval = libxml_xmlCharPtrConstWrap((const xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltFreeTransformContext(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltFreeTransformContext", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    xsltFreeTransformContext(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFreeExts(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltFreeExts", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    xsltFreeExts(style);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltParseGlobalParam(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xsltParseGlobalParam", &pyobj_style, &pyobj_cur))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xsltParseGlobalParam(style, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltParseStylesheetParam(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xsltParseStylesheetParam", &pyobj_ctxt, &pyobj_cur))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xsltParseStylesheetParam(ctxt, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltParseStylesheetOutput(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xsltParseStylesheetOutput", &pyobj_style, &pyobj_cur))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xsltParseStylesheetOutput(style, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFreeStylePreComps(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltFreeStylePreComps", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    xsltFreeStylePreComps(style);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltGetNamespace(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNsPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr cur;
    PyObject *pyobj_cur;
    xmlNsPtr ns;
    PyObject *pyobj_ns;
    xmlNodePtr out;
    PyObject *pyobj_out;

    if (!PyArg_ParseTuple(args, "OOOO:xsltGetNamespace", &pyobj_ctxt, &pyobj_cur, &pyobj_ns, &pyobj_out))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);
    ns = (xmlNsPtr) PyxmlNode_Get(pyobj_ns);
    out = (xmlNodePtr) PyxmlNode_Get(pyobj_out);

    c_retval = xsltGetNamespace(ctxt, cur, ns, out);
    py_retval = libxml_xmlNsPtrWrap((xmlNsPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltShutdownCtxtExts(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltShutdownCtxtExts", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    xsltShutdownCtxtExts(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltRegisterAllElement(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltRegisterAllElement", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    xsltRegisterAllElement(ctxt);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltQuoteOneUserParam(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * name;
    xmlChar * value;

    if (!PyArg_ParseTuple(args, "Ozz:xsltQuoteOneUserParam", &pyobj_ctxt, &name, &value))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = xsltQuoteOneUserParam(ctxt, name, value);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltUnregisterExtModule(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * URI;

    if (!PyArg_ParseTuple(args, "z:xsltUnregisterExtModule", &URI))
        return(NULL);

    c_retval = xsltUnregisterExtModule(URI);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltStylesheetGetVersion(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetVersion", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->version;
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltUnregisterExtModuleElement(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * name;
    xmlChar * URI;

    if (!PyArg_ParseTuple(args, "zz:xsltUnregisterExtModuleElement", &name, &URI))
        return(NULL);

    c_retval = xsltUnregisterExtModuleElement(name, URI);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltParseStylesheetInclude(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr cur;
    PyObject *pyobj_cur;

    if (!PyArg_ParseTuple(args, "OO:xsltParseStylesheetInclude", &pyobj_style, &pyobj_cur))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    cur = (xmlNodePtr) PyxmlNode_Get(pyobj_cur);

    xsltParseStylesheetInclude(style, cur);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltSaveResultToFilename(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    char * URI;
    xmlDocPtr result;
    PyObject *pyobj_result;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    int compression;

    if (!PyArg_ParseTuple(args, "zOOi:xsltSaveResultToFilename", &URI, &pyobj_result, &pyobj_style, &compression))
        return(NULL);
    result = (xmlDocPtr) PyxmlNode_Get(pyobj_result);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = xsltSaveResultToFilename(URI, result, style, compression);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltSystemPropertyFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xsltSystemPropertyFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xsltSystemPropertyFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFreeStyleDocuments(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltFreeStyleDocuments", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    xsltFreeStyleDocuments(style);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFreeNamespaceAliasHashes(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltFreeNamespaceAliasHashes", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    xsltFreeNamespaceAliasHashes(style);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltTransformGetOutputURI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    const char * c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltTransformGetOutputURI", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->outputFile;
    py_retval = libxml_charPtrConstWrap((const char *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltEvalAttrValueTemplate(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlNodePtr node;
    PyObject *pyobj_node;
    xmlChar * name;
    xmlChar * ns;

    if (!PyArg_ParseTuple(args, "OOzz:xsltEvalAttrValueTemplate", &pyobj_ctxt, &pyobj_node, &name, &ns))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    c_retval = xsltEvalAttrValueTemplate(ctxt, node, name, ns);
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltTransformGetCurrent(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlNodePtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltTransformGetCurrent", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = ctxt->node;
    py_retval = libxml_xmlNodePtrWrap((xmlNodePtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltPrintErrorContext(PyObject *self, PyObject *args) {
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlNodePtr node;
    PyObject *pyobj_node;

    if (!PyArg_ParseTuple(args, "OOO:xsltPrintErrorContext", &pyobj_ctxt, &pyobj_style, &pyobj_node))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);
    node = (xmlNodePtr) PyxmlNode_Get(pyobj_node);

    xsltPrintErrorContext(ctxt, style, node);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltStylesheetGetDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlDocPtr c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetDoc", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->doc;
    py_retval = libxml_xmlDocPtrWrap((xmlDocPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltStylesheetGetMethodURI(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetMethodURI", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->methodURI;
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltFreeStylesheet(PyObject *self, PyObject *args) {
    xsltStylesheetPtr sheet;
    PyObject *pyobj_sheet;

    if (!PyArg_ParseTuple(args, "O:xsltFreeStylesheet", &pyobj_sheet))
        return(NULL);
    sheet = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_sheet);

    xsltFreeStylesheet(sheet);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltFreeAttributeSetsHashes(PyObject *self, PyObject *args) {
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltFreeAttributeSetsHashes", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    xsltFreeAttributeSetsHashes(style);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltEvalGlobalVariables(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;

    if (!PyArg_ParseTuple(args, "O:xsltEvalGlobalVariables", &pyobj_ctxt))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = xsltEvalGlobalVariables(ctxt);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltRegisterAllExtras(PyObject *self, PyObject *args) {

    xsltRegisterAllExtras();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltGetXIncludeDefault(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;

    c_retval = xsltGetXIncludeDefault();
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltIsBlank(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xmlChar * str;

    if (!PyArg_ParseTuple(args, "z:xsltIsBlank", &str))
        return(NULL);

    c_retval = xsltIsBlank(str);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltStylesheetGetDoctypeSystem(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlChar * c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;

    if (!PyArg_ParseTuple(args, "O:xsltStylesheetGetDoctypeSystem", &pyobj_style))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = style->doctypeSystem;
    py_retval = libxml_xmlCharPtrWrap((xmlChar *) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltParseStylesheetDoc(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xsltStylesheetPtr c_retval;
    xmlDocPtr doc;
    PyObject *pyobj_doc;

    if (!PyArg_ParseTuple(args, "O:xsltParseStylesheetDoc", &pyobj_doc))
        return(NULL);
    doc = (xmlDocPtr) PyxmlNode_Get(pyobj_doc);

    c_retval = xsltParseStylesheetDoc(doc);
    py_retval = libxslt_xsltStylesheetPtrWrap((xsltStylesheetPtr) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xsltKeyFunction(PyObject *self, PyObject *args) {
    xmlXPathParserContextPtr ctxt;
    PyObject *pyobj_ctxt;
    int nargs;

    if (!PyArg_ParseTuple(args, "Oi:xsltKeyFunction", &pyobj_ctxt, &nargs))
        return(NULL);
    ctxt = (xmlXPathParserContextPtr) PyxmlXPathParserContext_Get(pyobj_ctxt);

    xsltKeyFunction(ctxt, nargs);
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltRegisterExtPrefix(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    int c_retval;
    xsltStylesheetPtr style;
    PyObject *pyobj_style;
    xmlChar * prefix;
    xmlChar * URI;

    if (!PyArg_ParseTuple(args, "Ozz:xsltRegisterExtPrefix", &pyobj_style, &prefix, &URI))
        return(NULL);
    style = (xsltStylesheetPtr) Pystylesheet_Get(pyobj_style);

    c_retval = xsltRegisterExtPrefix(style, prefix, URI);
    py_retval = libxml_intWrap((int) c_retval);
    return(py_retval);
}

PyObject *
libxslt_xslDropCall(PyObject *self, PyObject *args) {

    xslDropCall();
    Py_INCREF(Py_None);
    return(Py_None);
}

PyObject *
libxslt_xsltVariableLookup(PyObject *self, PyObject *args) {
    PyObject *py_retval;
    xmlXPathObjectPtr c_retval;
    xsltTransformContextPtr ctxt;
    PyObject *pyobj_ctxt;
    xmlChar * name;
    xmlChar * ns_uri;

    if (!PyArg_ParseTuple(args, "Ozz:xsltVariableLookup", &pyobj_ctxt, &name, &ns_uri))
        return(NULL);
    ctxt = (xsltTransformContextPtr) PytransformCtxt_Get(pyobj_ctxt);

    c_retval = xsltVariableLookup(ctxt, name, ns_uri);
    py_retval = libxml_xmlXPathObjectPtrWrap((xmlXPathObjectPtr) c_retval);
    return(py_retval);
}

