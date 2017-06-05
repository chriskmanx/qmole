Type types[][120] = {
// abstractMethodError
{ Type_object, Type_object, Type_object, Type_none },
// addendum
{ Type_object, Type_object, Type_object, Type_none },
// arithmeticException
{ Type_object, Type_object, Type_object, Type_none },
// array
{ Type_uintptr_t, Type_array, Type_object, Type_none },
// arrayIndexOutOfBoundsException
{ Type_object, Type_object, Type_object, Type_none },
// arrayStoreException
{ Type_object, Type_object, Type_object, Type_none },
// boolean
{ Type_uint8_t, Type_none },
// booleanArray
{ Type_uintptr_t, Type_array, Type_uint8_t, Type_none },
// byte
{ Type_uint8_t, Type_none },
// byteArray
{ Type_uintptr_t, Type_array, Type_int8_t, Type_none },
// callNode
{ Type_intptr_t, Type_object, Type_uintptr_t, Type_object, Type_none },
// char
{ Type_uint16_t, Type_none },
// charArray
{ Type_uintptr_t, Type_array, Type_uint16_t, Type_none },
// class
{ Type_uint16_t, Type_uint16_t, Type_uint16_t, Type_uint8_t, Type_uint8_t, Type_object, Type_uint32_t, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_uintptr_t, Type_array, Type_word, Type_none },
// classAddendum
{ Type_object, Type_object, Type_object, Type_object, Type_object, Type_uint32_t, Type_object, Type_object, Type_none },
// classCastException
{ Type_object, Type_object, Type_object, Type_none },
// classLoader
{ Type_object, Type_object, Type_object, Type_none },
// classNotFoundException
{ Type_object, Type_object, Type_object, Type_object, Type_none },
// classRuntimeData
{ Type_object, Type_object, Type_object, Type_object, Type_none },
// cleaner
{ Type_object, Type_none },
// cloneNotSupportedException
{ Type_object, Type_object, Type_object, Type_none },
// cloneable
{ Type_none },
// code
{ Type_object, Type_object, Type_object, Type_object, Type_intptr_t, Type_uint32_t, Type_uint16_t, Type_uint16_t, Type_uintptr_t, Type_array, Type_uint8_t, Type_none },
// compileRoots
{ Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_none },
// constantPool
{ Type_none },
// continuation
{ Type_object, Type_object, Type_object, Type_word, Type_uintptr_t, Type_uintptr_t, Type_uintptr_t, Type_array, Type_uintptr_t, Type_none },
// continuationContext
{ Type_object, Type_object, Type_object, Type_object, Type_object, Type_none },
// double
{ Type_uint64_t, Type_none },
// doubleArray
{ Type_uintptr_t, Type_array, Type_uint64_t, Type_none },
// error
{ Type_object, Type_object, Type_object, Type_none },
// exception
{ Type_object, Type_object, Type_object, Type_none },
// exceptionHandlerTable
{ Type_uintptr_t, Type_array, Type_uint64_t, Type_none },
// exceptionInInitializerError
{ Type_object, Type_object, Type_object, Type_object, Type_none },
// field
{ Type_uint8_t, Type_uint8_t, Type_uint16_t, Type_uint16_t, Type_uint32_t, Type_object, Type_object, Type_object, Type_object, Type_none },
// fieldAddendum
{ Type_object, Type_object, Type_object, Type_none },
// fileNotFoundException
{ Type_object, Type_object, Type_object, Type_none },
// finalizer
{ Type_object_nogc, Type_word, Type_object_nogc, Type_object, Type_object, Type_none },
// finder
{ Type_word, Type_object, Type_object, Type_none },
// float
{ Type_uint32_t, Type_none },
// floatArray
{ Type_uintptr_t, Type_array, Type_uint32_t, Type_none },
// hashMap
{ Type_uint32_t, Type_object, Type_none },
// illegalArgumentException
{ Type_object, Type_object, Type_object, Type_none },
// illegalMonitorStateException
{ Type_object, Type_object, Type_object, Type_none },
// illegalStateException
{ Type_object, Type_object, Type_object, Type_none },
// incompatibleClassChangeError
{ Type_object, Type_object, Type_object, Type_none },
// incompatibleContinuationException
{ Type_object, Type_object, Type_object, Type_none },
// indexOutOfBoundsException
{ Type_object, Type_object, Type_object, Type_none },
// innerClassReference
{ Type_object, Type_object, Type_object, Type_uint16_t, Type_none },
// int
{ Type_uint32_t, Type_none },
// intArray
{ Type_uintptr_t, Type_array, Type_int32_t, Type_none },
// interruptedException
{ Type_object, Type_object, Type_object, Type_none },
// invocation
{ Type_uint16_t, Type_int32_t, Type_object, Type_object, Type_object, Type_object, Type_none },
// invocationTargetException
{ Type_object, Type_object, Type_object, Type_object, Type_none },
// ioException
{ Type_object, Type_object, Type_object, Type_none },
// jaccessibleObject
{ Type_none },
// jboolean
{ Type_none },
// jbyte
{ Type_none },
// jchar
{ Type_none },
// jclass
{ Type_object, Type_none },
// jconstructor
{ Type_object, Type_none },
// jdouble
{ Type_none },
// jfield
{ Type_object, Type_uint8_t, Type_none },
// jfloat
{ Type_none },
// jint
{ Type_none },
// jlong
{ Type_none },
// jmethod
{ Type_object, Type_uint8_t, Type_none },
// jobject
{ Type_none },
// jreference
{ Type_object_nogc, Type_object_nogc, Type_object_nogc, Type_object, Type_none },
// jshort
{ Type_none },
// jvoid
{ Type_none },
// lineNumberTable
{ Type_uintptr_t, Type_array, Type_uint64_t, Type_none },
// linkageError
{ Type_object, Type_object, Type_object, Type_none },
// list
{ Type_uint32_t, Type_object, Type_object, Type_none },
// long
{ Type_uint64_t, Type_none },
// longArray
{ Type_uintptr_t, Type_array, Type_int64_t, Type_none },
// method
{ Type_uint8_t, Type_uint8_t, Type_uint8_t, Type_uint8_t, Type_uint16_t, Type_uint16_t, Type_uint32_t, Type_uint32_t, Type_object, Type_object, Type_object, Type_object, Type_object, Type_none },
// methodAddendum
{ Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_none },
// methodRuntimeData
{ Type_object, Type_none },
// monitor
{ Type_word, Type_word, Type_word, Type_object, Type_object, Type_uint32_t, Type_none },
// monitorNode
{ Type_word, Type_object, Type_none },
// native
{ Type_word, Type_uint8_t, Type_none },
// nativeIntercept
{ Type_word, Type_uint8_t, Type_object, Type_none },
// negativeArraySizeException
{ Type_object, Type_object, Type_object, Type_none },
// noClassDefFoundError
{ Type_object, Type_object, Type_object, Type_none },
// noSuchFieldError
{ Type_object, Type_object, Type_object, Type_none },
// noSuchMethodError
{ Type_object, Type_object, Type_object, Type_none },
// nullPointerException
{ Type_object, Type_object, Type_object, Type_none },
// number
{ Type_none },
// outOfMemoryError
{ Type_object, Type_object, Type_object, Type_none },
// pair
{ Type_object, Type_object, Type_none },
// phantomReference
{ Type_object_nogc, Type_object_nogc, Type_object_nogc, Type_object, Type_none },
// pointer
{ Type_word, Type_none },
// reference
{ Type_uint8_t, Type_object, Type_object, Type_object, Type_none },
// referenceQueue
{ Type_object, Type_object, Type_none },
// reflectiveOperationException
{ Type_object, Type_object, Type_object, Type_none },
// region
{ Type_word, Type_uint32_t, Type_none },
// roots
{ Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_none },
// runtimeException
{ Type_object, Type_object, Type_object, Type_none },
// serializable
{ Type_none },
// short
{ Type_uint16_t, Type_none },
// shortArray
{ Type_uintptr_t, Type_array, Type_int16_t, Type_none },
// singleton
{ Type_uintptr_t, Type_array, Type_uintptr_t, Type_none },
// softReference
{ Type_object_nogc, Type_object_nogc, Type_object_nogc, Type_object, Type_none },
// stackOverflowError
{ Type_object, Type_object, Type_object, Type_none },
// stackTraceElement
{ Type_object, Type_object, Type_object, Type_uint32_t, Type_none },
// string
{ Type_object, Type_uint32_t, Type_uint32_t, Type_uint32_t, Type_none },
// systemClassLoader
{ Type_object, Type_object, Type_object, Type_word, Type_none },
// thread
{ Type_object, Type_uint64_t, Type_uint8_t, Type_uint8_t, Type_uint8_t, Type_uint8_t, Type_uint8_t, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_object, Type_none },
// threadGroup
{ Type_object, Type_object, Type_object, Type_none },
// throwable
{ Type_object, Type_object, Type_object, Type_none },
// traceElement
{ Type_object, Type_int32_t, Type_none },
// treeNode
{ Type_object, Type_object, Type_object, Type_none },
// triple
{ Type_object, Type_object, Type_object, Type_none },
// unsatisfiedLinkError
{ Type_object, Type_object, Type_object, Type_none },
// unwindResult
{ Type_object, Type_object, Type_object, Type_none },
// vector
{ Type_uint32_t, Type_uintptr_t, Type_array, Type_object, Type_none },
// virtualMachineError
{ Type_object, Type_object, Type_object, Type_none },
// weakHashMap
{ Type_uint32_t, Type_object, Type_none },
// weakReference
{ Type_object_nogc, Type_object_nogc, Type_object_nogc, Type_object, Type_none },
// wordArray
{ Type_uintptr_t, Type_array, Type_uintptr_t, Type_none }
};
