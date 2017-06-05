#
# Both libxml2mod and libxsltmod have a dependancy on libxml2.so
# and they should share the same module, try to convince the python
# loader to work in that mode if feasible
#
import sys
try:
    from dl import RTLD_GLOBAL, RTLD_NOW
except ImportError:
    RTLD_GLOBAL = -1
    RTLD_NOW = -1
    try:
        import os
	osname = os.uname()[0]
	if osname == 'Linux':
	    RTLD_GLOBAL = 0x00100
	    RTLD_NOW = 0x00002
	#
	# is there a better method ?
	#
	else:
	    print "libxslt could not guess RTLD_GLOBAL and RTLD_NOW " + \
	          "on this platform: %s" % (osname)
    except:
	print "libxslt could not guess RTLD_GLOBAL and RTLD_NOW " + \
	      "on this platform: %s" % (osname)

if RTLD_GLOBAL != -1 and RTLD_NOW != -1:
    try:
	flags = sys.getdlopenflags() 
	sys.setdlopenflags(RTLD_GLOBAL | RTLD_NOW)
	try:
	    import libxml2mod
	    import libxsltmod
	    import libxml2
	finally:
	    sys.setdlopenflags(flags)
    except:
	import libxml2mod
	import libxsltmod
	import libxml2
else:
    import libxml2mod
    import libxsltmod
    import libxml2

#
# Everything below this point is automatically generated
#

#
# Functions from module extensions
#

def registerTestModule():
    """Registers the test module"""
    libxsltmod.xsltRegisterTestModule()

def unregisterExtModule(URI):
    """Unregister an XSLT extension module from the library."""
    ret = libxsltmod.xsltUnregisterExtModule(URI)
    return ret

def unregisterExtModuleElement(name, URI):
    """Unregisters an extension module element"""
    ret = libxsltmod.xsltUnregisterExtModuleElement(name, URI)
    return ret

def unregisterExtModuleFunction(name, URI):
    """Unregisters an extension module function"""
    ret = libxsltmod.xsltUnregisterExtModuleFunction(name, URI)
    return ret

def unregisterExtModuleTopLevel(name, URI):
    """Unregisters an extension module top-level element"""
    ret = libxsltmod.xsltUnregisterExtModuleTopLevel(name, URI)
    return ret

#
# Functions from module extra
#

def registerAllExtras():
    """Registers the built-in extensions"""
    libxsltmod.xsltRegisterAllExtras()

#
# Functions from module python
#

def cleanup():
    """Cleanup all libxslt and libxml2 memory allocated"""
    libxsltmod.xsltCleanup()

def registerExtModuleFunction(name, URI, f):
    """Register a Python written function to the XSLT engine"""
    ret = libxsltmod.xsltRegisterExtModuleFunction(name, URI, f)
    return ret

#
# Functions from module transform
#

def setXIncludeDefault(xinclude):
    """Set whether XInclude should be processed on document being
       loaded by default"""
    libxsltmod.xsltSetXIncludeDefault(xinclude)

def xincludeDefault():
    """return the default state for XInclude processing"""
    ret = libxsltmod.xsltGetXIncludeDefault()
    return ret

#
# Functions from module xslt
#

def cleanupGlobals():
    """Unregister all global variables set up by the XSLT library"""
    libxsltmod.xsltCleanupGlobals()

#
# Functions from module xsltInternals
#

def isBlank(str):
    """Check if a string is ignorable"""
    ret = libxsltmod.xsltIsBlank(str)
    return ret

def loadStylesheetPI(doc):
    """This function tries to locate the stylesheet PI in the
       given document If found, and if contained within the
       document, it will extract that subtree to build the
       stylesheet to process doc (doc itself will be modified).
       If found but referencing an external document it will
       attempt to load it and generate a stylesheet from it. In
       both cases, the resulting stylesheet and the document need
       to be freed once the transformation is done."""
    if doc == None: doc__o = None
    else: doc__o = doc._o
    ret = libxsltmod.xsltLoadStylesheetPI(doc__o)
    if ret == None: return None
    return stylesheet(_obj=ret)

def newStylesheet():
    """Create a new XSLT Stylesheet"""
    ret = libxsltmod.xsltNewStylesheet()
    if ret == None: return None
    return stylesheet(_obj=ret)

def parseStylesheetDoc(doc):
    """parse an XSLT stylesheet building the associated structures"""
    if doc == None: doc__o = None
    else: doc__o = doc._o
    ret = libxsltmod.xsltParseStylesheetDoc(doc__o)
    if ret == None: return None
    return stylesheet(_obj=ret)

#
# Functions from module xsltutils
#

def calibrateAdjust(delta):
    """Used for to correct the calibration for xsltTimestamp()"""
    libxsltmod.xsltCalibrateAdjust(delta)

def nsProp(node, name, nameSpace):
    """Similar to xmlGetNsProp() but with a slightly different
       semantic """
    if node == None: node__o = None
    else: node__o = node._o
    ret = libxsltmod.xsltGetNsProp(node__o, name, nameSpace)
    return ret

def timestamp():
    """Used for gathering profiling data"""
    ret = libxsltmod.xsltTimestamp()
    return ret

def xslDropCall():
    """Drop the topmost item off the call stack"""
    libxsltmod.xslDropCall()

class xpathParserContext(libxml2.xpathParserContext):
    def __init__(self, _obj=None):
        self._o = None
        libxml2.xpathParserContext.__init__(self, _obj=_obj)

    # accessors for xpathParserContext
    def context(self):
        """Get the xpathContext from an xpathParserContext"""
        ret = libxsltmod.xsltXPathParserGetContext(self._o)
        if ret == None: return None
        return xpathContext(_obj=ret)

    #
    # xpathParserContext functions from module extra
    #

    def functionNodeSet(self, nargs):
        """Implement the node-set() XSLT function node-set
           node-set(result-tree) """
        libxsltmod.xsltFunctionNodeSet(self._o, nargs)

    #
    # xpathParserContext functions from module functions
    #

    def documentFunction(self, nargs):
        """Implement the document() XSLT function node-set
           document(object, node-set?)"""
        libxsltmod.xsltDocumentFunction(self._o, nargs)

    def elementAvailableFunction(self, nargs):
        """Implement the element-available() XSLT function boolean
           element-available(string)"""
        libxsltmod.xsltElementAvailableFunction(self._o, nargs)

    def formatNumberFunction(self, nargs):
        """Implement the format-number() XSLT function string
           format-number(number, string, string?)"""
        libxsltmod.xsltFormatNumberFunction(self._o, nargs)

    def functionAvailableFunction(self, nargs):
        """Implement the function-available() XSLT function boolean
           function-available(string)"""
        libxsltmod.xsltFunctionAvailableFunction(self._o, nargs)

    def generateIdFunction(self, nargs):
        """Implement the generate-id() XSLT function string
           generate-id(node-set?)"""
        libxsltmod.xsltGenerateIdFunction(self._o, nargs)

    def keyFunction(self, nargs):
        """Implement the key() XSLT function node-set key(string,
           object)"""
        libxsltmod.xsltKeyFunction(self._o, nargs)

    def systemPropertyFunction(self, nargs):
        """Implement the system-property() XSLT function object
           system-property(string)"""
        libxsltmod.xsltSystemPropertyFunction(self._o, nargs)

    def unparsedEntityURIFunction(self, nargs):
        """Implement the unparsed-entity-uri() XSLT function string
           unparsed-entity-uri(string)"""
        libxsltmod.xsltUnparsedEntityURIFunction(self._o, nargs)

class xpathContext(libxml2.xpathContext):
    def __init__(self, _obj=None):
        self._o = None
        libxml2.xpathContext.__init__(self, _obj=_obj)

    def __del__(self):
        pass
    # accessors for xpathContext
    def transformContext(self):
        """Get the transformation context from an xpathContext"""
        ret = libxsltmod.xsltXPathGetTransformContext(self._o)
        if ret == None: return None
        return transformCtxt(_obj=ret)

    #
    # xpathContext functions from module functions
    #

    def registerAllFunctions(self):
        """Registers all default XSLT functions in this context"""
        libxsltmod.xsltRegisterAllFunctions(self._o)

class transformCtxt:
    def __init__(self, _obj=None):
        if _obj != None:self._o = _obj;return
        self._o = None

    # accessors for transformCtxt
    def context(self):
        """Get the XPath context of a transformation"""
        ret = libxsltmod.xsltTransformGetContext(self._o)
        if ret == None: return None
        return xpathContext(_obj=ret)

    def current(self):
        """Get the current() node of a transformation"""
        ret = libxsltmod.xsltTransformGetCurrent(self._o)
        if ret == None: return None
        return libxml2.xmlNode(_obj=ret)

    def insertNode(self):
        """Get the insertion node in the output document"""
        ret = libxsltmod.xsltTransformGetInsertNode(self._o)
        if ret == None: return None
        return libxml2.xmlNode(_obj=ret)

    def instruction(self):
        """Get the instruction node in the stylesheet"""
        ret = libxsltmod.xsltTransformGetInstruction(self._o)
        if ret == None: return None
        return libxml2.xmlNode(_obj=ret)

    def mode(self):
        """Get the mode of a transformation"""
        ret = libxsltmod.xsltTransformGetMode(self._o)
        return ret

    def modeURI(self):
        """Get the mode URI of a transformation"""
        ret = libxsltmod.xsltTransformGetModeURI(self._o)
        return ret

    def outputDoc(self):
        """Get the output document of a transformation"""
        ret = libxsltmod.xsltTransformGetOutputDoc(self._o)
        if ret == None: return None
        return libxml2.xmlDoc(_obj=ret)

    def outputURI(self):
        """Get the output URI of a transformation if known"""
        ret = libxsltmod.xsltTransformGetOutputURI(self._o)
        return ret

    def style(self):
        """Get the stylesheet from a transformation"""
        ret = libxsltmod.xsltTransformGetStyle(self._o)
        if ret == None: return None
        return stylesheet(_obj=ret)

    #
    # transformCtxt functions from module attributes
    #

    def applyAttributeSet(self, node, inst, attributes):
        """Apply the xsl:use-attribute-sets"""
        if node == None: node__o = None
        else: node__o = node._o
        if inst == None: inst__o = None
        else: inst__o = inst._o
        libxsltmod.xsltApplyAttributeSet(self._o, node__o, inst__o, attributes)

    #
    # transformCtxt functions from module documents
    #

    def freeDocuments(self):
        """Free up all the space used by the loaded documents"""
        libxsltmod.xsltFreeDocuments(self._o)

    #
    # transformCtxt functions from module extensions
    #

    def freeCtxtExts(self):
        """Free the XSLT extension data"""
        libxsltmod.xsltFreeCtxtExts(self._o)

    def initCtxtExts(self):
        """Initialize the set of modules with registered stylesheet
           data"""
        ret = libxsltmod.xsltInitCtxtExts(self._o)
        return ret

    def shutdownCtxtExts(self):
        """Shutdown the set of modules loaded"""
        libxsltmod.xsltShutdownCtxtExts(self._o)

    #
    # transformCtxt functions from module extra
    #

    def registerExtras(self):
        """Registers the built-in extensions. This function is
           deprecated, use xsltRegisterAllExtras instead."""
        libxsltmod.xsltRegisterExtras(self._o)

    #
    # transformCtxt functions from module imports
    #

    def findElemSpaceHandling(self, node):
        """Find strip-space or preserve-space informations for an
           element respect the import precedence or the wildcards"""
        if node == None: node__o = None
        else: node__o = node._o
        ret = libxsltmod.xsltFindElemSpaceHandling(self._o, node__o)
        return ret

    def needElemSpaceHandling(self):
        """Returns whether that stylesheet requires white-space
           stripping"""
        ret = libxsltmod.xsltNeedElemSpaceHandling(self._o)
        return ret

    #
    # transformCtxt functions from module namespaces
    #

    def copyNamespace(self, node, cur):
        """Do a copy of an namespace node. If node is non-None the new
           namespaces are added automatically. This handles
           namespaces aliases"""
        if node == None: node__o = None
        else: node__o = node._o
        if cur == None: cur__o = None
        else: cur__o = cur._o
        ret = libxsltmod.xsltCopyNamespace(self._o, node__o, cur__o)
        if ret == None: return None
        return libxml2.xmlNs(_obj=ret)

    def copyNamespaceList(self, node, cur):
        """Do a copy of an namespace list. If node is non-None the new
           namespaces are added automatically. This handles
           namespaces aliases"""
        if node == None: node__o = None
        else: node__o = node._o
        if cur == None: cur__o = None
        else: cur__o = cur._o
        ret = libxsltmod.xsltCopyNamespaceList(self._o, node__o, cur__o)
        if ret == None: return None
        return libxml2.xmlNs(_obj=ret)

    def namespace(self, cur, ns, out):
        """Find the right namespace value for this prefix, if needed
           create and add a new namespace decalaration on the node
           Handle namespace aliases"""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        if ns == None: ns__o = None
        else: ns__o = ns._o
        if out == None: out__o = None
        else: out__o = out._o
        ret = libxsltmod.xsltGetNamespace(self._o, cur__o, ns__o, out__o)
        if ret == None: return None
        return libxml2.xmlNs(_obj=ret)

    def specialNamespace(self, cur, URI, prefix, out):
        """Find the right namespace value for this URI, if needed
           create and add a new namespace decalaration on the node"""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        if out == None: out__o = None
        else: out__o = out._o
        ret = libxsltmod.xsltGetSpecialNamespace(self._o, cur__o, URI, prefix, out__o)
        if ret == None: return None
        return libxml2.xmlNs(_obj=ret)

    #
    # transformCtxt functions from module templates
    #

    def attrListTemplateProcess(self, target, cur):
        """Do a copy of an attribute list with attribute template
           processing"""
        if target == None: target__o = None
        else: target__o = target._o
        if cur == None: cur__o = None
        else: cur__o = cur._o
        ret = libxsltmod.xsltAttrListTemplateProcess(self._o, target__o, cur__o)
        if ret == None: return None
        return libxml2.xmlAttr(_obj=ret)

    def attrTemplateProcess(self, target, attr):
        """Process the given attribute and return the new processed
           copy."""
        if target == None: target__o = None
        else: target__o = target._o
        if attr == None: attr__o = None
        else: attr__o = attr._o
        ret = libxsltmod.xsltAttrTemplateProcess(self._o, target__o, attr__o)
        if ret == None: return None
        return libxml2.xmlAttr(_obj=ret)

    def evalAttrValueTemplate(self, node, name, ns):
        """Evaluate a attribute value template, i.e. the attribute
           value can contain expressions contained in curly braces
           ({}) and those are substituted by they computed value."""
        if node == None: node__o = None
        else: node__o = node._o
        ret = libxsltmod.xsltEvalAttrValueTemplate(self._o, node__o, name, ns)
        return ret

    def evalTemplateString(self, node, parent):
        """Evaluate a template string value, i.e. the parent list is
           interpreter as template content and the resulting tree
           string value is returned This is needed for example by
           xsl:comment and xsl:processing-instruction"""
        if node == None: node__o = None
        else: node__o = node._o
        if parent == None: parent__o = None
        else: parent__o = parent._o
        ret = libxsltmod.xsltEvalTemplateString(self._o, node__o, parent__o)
        return ret

    #
    # transformCtxt functions from module transform
    #

    def applyStripSpaces(self, node):
        """Strip the unwanted ignorable spaces from the input tree"""
        if node == None: node__o = None
        else: node__o = node._o
        libxsltmod.xsltApplyStripSpaces(self._o, node__o)

    def freeTransformContext(self):
        """Free up the memory allocated by ctxt"""
        libxsltmod.xsltFreeTransformContext(self._o)

    def registerAllElement(self):
        """Registers all default XSLT elements in this context"""
        libxsltmod.xsltRegisterAllElement(self._o)

    #
    # transformCtxt functions from module variables
    #

    def evalGlobalVariables(self):
        """Evaluate the global variables of a stylesheet. This need to
           be done on parsed stylesheets before starting to apply
           transformations"""
        ret = libxsltmod.xsltEvalGlobalVariables(self._o)
        return ret

    def evalOneUserParam(self, name, value):
        """ctxt: the XSLT transformation context name: a null
           terminated string giving the name of the parameter value a
           null terminated string giving the XPath expression to be
           evaluated """
        ret = libxsltmod.xsltEvalOneUserParam(self._o, name, value)
        return ret

    def freeGlobalVariables(self):
        """Free up the data associated to the global variables its
           value."""
        libxsltmod.xsltFreeGlobalVariables(self._o)

    def parseStylesheetParam(self, cur):
        """parse an XSLT transformation param declaration and record
           its value."""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        libxsltmod.xsltParseStylesheetParam(self._o, cur__o)

    def parseStylesheetVariable(self, cur):
        """parse an XSLT transformation variable declaration and
           record its value."""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        libxsltmod.xsltParseStylesheetVariable(self._o, cur__o)

    def quoteOneUserParam(self, name, value):
        """ctxt: the XSLT transformation context name: a null
           terminated string giving the name of the parameter value a
           null terminated string giving the parameter value """
        ret = libxsltmod.xsltQuoteOneUserParam(self._o, name, value)
        return ret

    def variableLookup(self, name, ns_uri):
        """Search in the Variable array of the context for the given
           variable value."""
        ret = libxsltmod.xsltVariableLookup(self._o, name, ns_uri)
        if ret == None: return None
        return xpathObjectRet(ret)

    #
    # transformCtxt functions from module xsltInternals
    #

    def allocateExtraCtxt(self):
        ret = libxsltmod.xsltAllocateExtraCtxt(self._o)
        return ret

    #
    # transformCtxt functions from module xsltutils
    #

    def message(self, node, inst):
        """Process and xsl:message construct"""
        if node == None: node__o = None
        else: node__o = node._o
        if inst == None: inst__o = None
        else: inst__o = inst._o
        libxsltmod.xsltMessage(self._o, node__o, inst__o)

    def printErrorContext(self, style, node):
        """Display the context of an error."""
        if style == None: style__o = None
        else: style__o = style._o
        if node == None: node__o = None
        else: node__o = node._o
        libxsltmod.xsltPrintErrorContext(self._o, style__o, node__o)

    def saveProfiling(self, output):
        """Save the profiling informations on output"""
        libxsltmod.xsltSaveProfiling(self._o, output)

class stylesheet:
    def __init__(self, _obj=None):
        if _obj != None:self._o = _obj;return
        self._o = None

    # accessors for stylesheet
    def doc(self):
        """Get the document of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetDoc(self._o)
        if ret == None: return None
        return libxml2.xmlDoc(_obj=ret)

    def doctypePublic(self):
        """Get the output PUBLIC of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetDoctypePublic(self._o)
        return ret

    def doctypeSystem(self):
        """Get the output SYSTEM of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetDoctypeSystem(self._o)
        return ret

    def encoding(self):
        """Get the output encoding of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetEncoding(self._o)
        return ret

    def imports(self):
        """Get the imports of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetImports(self._o)
        if ret == None: return None
        return stylesheet(_obj=ret)

    def method(self):
        """Get the output method of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetMethod(self._o)
        return ret

    def methodURI(self):
        """Get the output method URI of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetMethodURI(self._o)
        return ret

    def next(self):
        """Get the next sibling of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetNext(self._o)
        if ret == None: return None
        return stylesheet(_obj=ret)

    def parent(self):
        """Get the parent of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetParent(self._o)
        if ret == None: return None
        return stylesheet(_obj=ret)

    def version(self):
        """Get the output version of a stylesheet"""
        ret = libxsltmod.xsltStylesheetGetVersion(self._o)
        return ret

    #
    # stylesheet functions from module attributes
    #

    def freeAttributeSetsHashes(self):
        """Free up the memory used by attribute sets"""
        libxsltmod.xsltFreeAttributeSetsHashes(self._o)

    def parseStylesheetAttributeSet(self, cur):
        """parse an XSLT stylesheet preserve-space element and record
           elements needing preserving"""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        libxsltmod.xsltParseStylesheetAttributeSet(self._o, cur__o)

    #
    # stylesheet functions from module documents
    #

    def freeStyleDocuments(self):
        """Free up all the space used by the loaded documents"""
        libxsltmod.xsltFreeStyleDocuments(self._o)

    #
    # stylesheet functions from module extensions
    #

    def checkExtPrefix(self, prefix):
        """Check if the given prefix is one of the declared extensions"""
        ret = libxsltmod.xsltCheckExtPrefix(self._o, prefix)
        return ret

    def freeExts(self):
        """Free up the memory used by XSLT extensions in a stylesheet"""
        libxsltmod.xsltFreeExts(self._o)

    def registerExtPrefix(self, prefix, URI):
        """Registers an extension namespace"""
        ret = libxsltmod.xsltRegisterExtPrefix(self._o, prefix, URI)
        return ret

    def shutdownExts(self):
        """Shutdown the set of modules loaded"""
        libxsltmod.xsltShutdownExts(self._o)

    #
    # stylesheet functions from module imports
    #

    def nextImport(self):
        """Find the next stylesheet in import precedence."""
        ret = libxsltmod.xsltNextImport(self._o)
        if ret == None: return None
        return stylesheet(_obj=ret)

    def parseStylesheetImport(self, cur):
        """parse an XSLT stylesheet strip-space element and record
           elements needing stripping"""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        libxsltmod.xsltParseStylesheetImport(self._o, cur__o)

    def parseStylesheetInclude(self, cur):
        """parse an XSLT stylesheet strip-space element and record
           elements needing stripping"""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        libxsltmod.xsltParseStylesheetInclude(self._o, cur__o)

    #
    # stylesheet functions from module keys
    #

    def addKey(self, name, nameURI, match, use, inst):
        """add a key definition to a stylesheet"""
        if inst == None: inst__o = None
        else: inst__o = inst._o
        ret = libxsltmod.xsltAddKey(self._o, name, nameURI, match, use, inst__o)
        return ret

    def freeKeys(self):
        """Free up the memory used by XSLT keys in a stylesheet"""
        libxsltmod.xsltFreeKeys(self._o)

    #
    # stylesheet functions from module namespaces
    #

    def freeNamespaceAliasHashes(self):
        """Free up the memory used by namespaces aliases"""
        libxsltmod.xsltFreeNamespaceAliasHashes(self._o)

    def namespaceAlias(self, node):
        """Read the stylesheet-prefix and result-prefix attributes,
           register them as well as the corresponding namespace."""
        if node == None: node__o = None
        else: node__o = node._o
        libxsltmod.xsltNamespaceAlias(self._o, node__o)

    #
    # stylesheet functions from module pattern
    #

    def cleanupTemplates(self):
        """Cleanup the state of the templates used by the stylesheet
           and the ones it imports."""
        libxsltmod.xsltCleanupTemplates(self._o)

    def freeTemplateHashes(self):
        """Free up the memory used by xsltAddTemplate/xsltGetTemplate
           mechanism"""
        libxsltmod.xsltFreeTemplateHashes(self._o)

    #
    # stylesheet functions from module preproc
    #

    def freeStylePreComps(self):
        """Free up the memory allocated by all precomputed blocks"""
        libxsltmod.xsltFreeStylePreComps(self._o)

    def stylePreCompute(self, inst):
        """Precompute an XSLT stylesheet element"""
        if inst == None: inst__o = None
        else: inst__o = inst._o
        libxsltmod.xsltStylePreCompute(self._o, inst__o)

    #
    # stylesheet functions from module python
    #

    def applyStylesheet(self, doc, params):
        """Apply the stylesheet to the document"""
        if doc == None: doc__o = None
        else: doc__o = doc._o
        ret = libxsltmod.xsltApplyStylesheet(self._o, doc__o, params)
        if ret == None: return None
        return libxml2.xmlDoc(_obj=ret)

    #
    # stylesheet functions from module transform
    #

    def newTransformContext(self, doc):
        """Create a new XSLT TransformContext"""
        if doc == None: doc__o = None
        else: doc__o = doc._o
        ret = libxsltmod.xsltNewTransformContext(self._o, doc__o)
        if ret == None: return None
        return transformCtxt(_obj=ret)

    #
    # stylesheet functions from module variables
    #

    def parseGlobalParam(self, cur):
        """parse an XSLT transformation param declaration and record
           its value."""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        libxsltmod.xsltParseGlobalParam(self._o, cur__o)

    def parseGlobalVariable(self, cur):
        """parse an XSLT transformation variable declaration and
           record its value."""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        libxsltmod.xsltParseGlobalVariable(self._o, cur__o)

    #
    # stylesheet functions from module xsltInternals
    #

    def allocateExtra(self):
        ret = libxsltmod.xsltAllocateExtra(self._o)
        return ret

    def freeStylesheet(self):
        """Free up the memory allocated by sheet"""
        libxsltmod.xsltFreeStylesheet(self._o)

    def parseStylesheetOutput(self, cur):
        """parse an XSLT stylesheet output element and record
           information related to the stylesheet output"""
        if cur == None: cur__o = None
        else: cur__o = cur._o
        libxsltmod.xsltParseStylesheetOutput(self._o, cur__o)

    def parseStylesheetProcess(self, doc):
        """parse an XSLT stylesheet adding the associated structures"""
        if doc == None: doc__o = None
        else: doc__o = doc._o
        ret = libxsltmod.xsltParseStylesheetProcess(self._o, doc__o)
        if ret == None: return None
        return stylesheet(_obj=ret)

    def parseTemplateContent(self, templ):
        """parse a template content-model Clean-up the template
           content from unwanted ignorable blank nodes and process
           xslt:text"""
        if templ == None: templ__o = None
        else: templ__o = templ._o
        libxsltmod.xsltParseTemplateContent(self._o, templ__o)

    #
    # stylesheet functions from module xsltutils
    #

    def saveResultToFd(self, fd, result):
        """Save the result result obtained by applying the style
           stylesheet to an open file descriptor This does not close
           the descriptor."""
        if result == None: result__o = None
        else: result__o = result._o
        ret = libxsltmod.xsltSaveResultToFd(fd, result__o, self._o)
        return ret

    def saveResultToFile(self, file, result):
        """Save the result result obtained by applying the style
           stylesheet to an open FILE * I/O. This does not close the
           FILE file"""
        if result == None: result__o = None
        else: result__o = result._o
        ret = libxsltmod.xsltSaveResultToFile(file, result__o, self._o)
        return ret

    def saveResultToFilename(self, URI, result, compression):
        """Save the result result obtained by applying the style
           stylesheet to a file or URL"""
        if result == None: result__o = None
        else: result__o = result._o
        ret = libxsltmod.xsltSaveResultToFilename(URI, result__o, self._o, compression)
        return ret

