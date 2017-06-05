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

