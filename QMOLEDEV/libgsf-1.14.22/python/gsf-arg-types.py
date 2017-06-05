import argtypes

arg = argtypes.StringArg()

argtypes.matcher.register('guint8*', arg)
argtypes.matcher.register('const-guint8*', arg)
argtypes.matcher.register('guint8-const*', arg)
 
arg = argtypes.IntArg()

argtypes.matcher.register('GsfZipCompressionMethod', arg)
