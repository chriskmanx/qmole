const unsigned TypeCount = 120;

class GcAbstractMethodError;
class GcAddendum;
class GcArithmeticException;
class GcArray;
class GcArrayIndexOutOfBoundsException;
class GcArrayStoreException;
class GcBoolean;
class GcBooleanArray;
class GcByte;
class GcByteArray;
class GcCallNode;
class GcChar;
class GcCharArray;
class GcClass;
class GcClassAddendum;
class GcClassCastException;
class GcClassLoader;
class GcClassNotFoundException;
class GcClassRuntimeData;
class GcCleaner;
class GcCloneNotSupportedException;
class GcCloneable;
class GcCode;
class GcCompileRoots;
class GcConstantPool;
class GcContinuation;
class GcContinuationContext;
class GcDouble;
class GcDoubleArray;
class GcError;
class GcException;
class GcExceptionHandlerTable;
class GcExceptionInInitializerError;
class GcField;
class GcFieldAddendum;
class GcFileNotFoundException;
class GcFinalizer;
class GcFinder;
class GcFloat;
class GcFloatArray;
class GcHashMap;
class GcIllegalArgumentException;
class GcIllegalMonitorStateException;
class GcIllegalStateException;
class GcIncompatibleClassChangeError;
class GcIncompatibleContinuationException;
class GcIndexOutOfBoundsException;
class GcInnerClassReference;
class GcInt;
class GcIntArray;
class GcInterruptedException;
class GcInvocation;
class GcInvocationTargetException;
class GcIoException;
class GcJaccessibleObject;
class GcJboolean;
class GcJbyte;
class GcJchar;
class GcJclass;
class GcJconstructor;
class GcJdouble;
class GcJfield;
class GcJfloat;
class GcJint;
class GcJlong;
class GcJmethod;
class GcJobject;
class GcJreference;
class GcJshort;
class GcJvoid;
class GcLineNumberTable;
class GcLinkageError;
class GcList;
class GcLong;
class GcLongArray;
class GcMethod;
class GcMethodAddendum;
class GcMethodRuntimeData;
class GcMonitor;
class GcMonitorNode;
class GcNative;
class GcNativeIntercept;
class GcNegativeArraySizeException;
class GcNoClassDefFoundError;
class GcNoSuchFieldError;
class GcNoSuchMethodError;
class GcNullPointerException;
class GcNumber;
class GcOutOfMemoryError;
class GcPair;
class GcPhantomReference;
class GcPointer;
class GcReference;
class GcReferenceQueue;
class GcReflectiveOperationException;
class GcRegion;
class GcRoots;
class GcRuntimeException;
class GcSerializable;
class GcShort;
class GcShortArray;
class GcSingleton;
class GcSoftReference;
class GcStackOverflowError;
class GcStackTraceElement;
class GcString;
class GcSystemClassLoader;
class GcThread;
class GcThreadGroup;
class GcThrowable;
class GcTraceElement;
class GcTreeNode;
class GcTriple;
class GcUnsatisfiedLinkError;
class GcUnwindResult;
class GcVector;
class GcVirtualMachineError;
class GcWeakHashMap;
class GcWeakReference;
class GcWordArray;

const unsigned AbstractMethodErrorMessage = 8;

#define HAVE_AbstractMethodErrorMessage 1

const unsigned AbstractMethodErrorTrace = 16;

#define HAVE_AbstractMethodErrorTrace 1

const unsigned AbstractMethodErrorCause = 24;

#define HAVE_AbstractMethodErrorCause 1

const unsigned AddendumPool = 8;

#define HAVE_AddendumPool 1

const unsigned AddendumAnnotationTable = 16;

#define HAVE_AddendumAnnotationTable 1

const unsigned AddendumSignature = 24;

#define HAVE_AddendumSignature 1

const unsigned ArithmeticExceptionMessage = 8;

#define HAVE_ArithmeticExceptionMessage 1

const unsigned ArithmeticExceptionTrace = 16;

#define HAVE_ArithmeticExceptionTrace 1

const unsigned ArithmeticExceptionCause = 24;

#define HAVE_ArithmeticExceptionCause 1

const unsigned ArrayLength = 8;

#define HAVE_ArrayLength 1

const unsigned ArrayBody = 16;

#define HAVE_ArrayBody 1

const unsigned ArrayIndexOutOfBoundsExceptionMessage = 8;

#define HAVE_ArrayIndexOutOfBoundsExceptionMessage 1

const unsigned ArrayIndexOutOfBoundsExceptionTrace = 16;

#define HAVE_ArrayIndexOutOfBoundsExceptionTrace 1

const unsigned ArrayIndexOutOfBoundsExceptionCause = 24;

#define HAVE_ArrayIndexOutOfBoundsExceptionCause 1

const unsigned ArrayStoreExceptionMessage = 8;

#define HAVE_ArrayStoreExceptionMessage 1

const unsigned ArrayStoreExceptionTrace = 16;

#define HAVE_ArrayStoreExceptionTrace 1

const unsigned ArrayStoreExceptionCause = 24;

#define HAVE_ArrayStoreExceptionCause 1

const unsigned BooleanValue = 8;

#define HAVE_BooleanValue 1

const unsigned BooleanArrayLength = 8;

#define HAVE_BooleanArrayLength 1

const unsigned BooleanArrayBody = 16;

#define HAVE_BooleanArrayBody 1

const unsigned ByteValue = 8;

#define HAVE_ByteValue 1

const unsigned ByteArrayLength = 8;

#define HAVE_ByteArrayLength 1

const unsigned ByteArrayBody = 16;

#define HAVE_ByteArrayBody 1

const unsigned CallNodeAddress = 8;

#define HAVE_CallNodeAddress 1

const unsigned CallNodeTarget = 16;

#define HAVE_CallNodeTarget 1

const unsigned CallNodeFlags = 24;

#define HAVE_CallNodeFlags 1

const unsigned CallNodeNext = 32;

#define HAVE_CallNodeNext 1

const unsigned CharValue = 8;

#define HAVE_CharValue 1

const unsigned CharArrayLength = 8;

#define HAVE_CharArrayLength 1

const unsigned CharArrayBody = 16;

#define HAVE_CharArrayBody 1

const unsigned ClassFlags = 8;

#define HAVE_ClassFlags 1

const unsigned ClassVmFlags = 10;

#define HAVE_ClassVmFlags 1

const unsigned ClassFixedSize = 12;

#define HAVE_ClassFixedSize 1

const unsigned ClassArrayElementSize = 14;

#define HAVE_ClassArrayElementSize 1

const unsigned ClassArrayDimensions = 15;

#define HAVE_ClassArrayDimensions 1

const unsigned ClassArrayElementClass = 16;

#define HAVE_ClassArrayElementClass 1

const unsigned ClassRuntimeDataIndex = 24;

#define HAVE_ClassRuntimeDataIndex 1

const unsigned ClassObjectMask = 32;

#define HAVE_ClassObjectMask 1

const unsigned ClassName = 40;

#define HAVE_ClassName 1

const unsigned ClassSourceFile = 48;

#define HAVE_ClassSourceFile 1

const unsigned ClassSuper = 56;

#define HAVE_ClassSuper 1

const unsigned ClassInterfaceTable = 64;

#define HAVE_ClassInterfaceTable 1

const unsigned ClassVirtualTable = 72;

#define HAVE_ClassVirtualTable 1

const unsigned ClassFieldTable = 80;

#define HAVE_ClassFieldTable 1

const unsigned ClassMethodTable = 88;

#define HAVE_ClassMethodTable 1

const unsigned ClassAddendum = 96;

#define HAVE_ClassAddendum 1

const unsigned ClassStaticTable = 104;

#define HAVE_ClassStaticTable 1

const unsigned ClassLoader = 112;

#define HAVE_ClassLoader 1

const unsigned ClassSource = 120;

#define HAVE_ClassSource 1

const unsigned ClassLength = 128;

#define HAVE_ClassLength 1

const unsigned ClassVtable = 136;

#define HAVE_ClassVtable 1

const unsigned ClassAddendumPool = 8;

#define HAVE_ClassAddendumPool 1

const unsigned ClassAddendumAnnotationTable = 16;

#define HAVE_ClassAddendumAnnotationTable 1

const unsigned ClassAddendumSignature = 24;

#define HAVE_ClassAddendumSignature 1

const unsigned ClassAddendumInterfaceTable = 32;

#define HAVE_ClassAddendumInterfaceTable 1

const unsigned ClassAddendumInnerClassTable = 40;

#define HAVE_ClassAddendumInnerClassTable 1

const unsigned ClassAddendumDeclaredMethodCount = 48;

#define HAVE_ClassAddendumDeclaredMethodCount 1

const unsigned ClassAddendumEnclosingClass = 56;

#define HAVE_ClassAddendumEnclosingClass 1

const unsigned ClassAddendumEnclosingMethod = 64;

#define HAVE_ClassAddendumEnclosingMethod 1

const unsigned ClassCastExceptionMessage = 8;

#define HAVE_ClassCastExceptionMessage 1

const unsigned ClassCastExceptionTrace = 16;

#define HAVE_ClassCastExceptionTrace 1

const unsigned ClassCastExceptionCause = 24;

#define HAVE_ClassCastExceptionCause 1

const unsigned ClassLoaderParent = 8;

#define HAVE_ClassLoaderParent 1

const unsigned ClassLoaderPackages = 16;

#define HAVE_ClassLoaderPackages 1

const unsigned ClassLoaderMap = 24;

#define HAVE_ClassLoaderMap 1

const unsigned ClassNotFoundExceptionMessage = 8;

#define HAVE_ClassNotFoundExceptionMessage 1

const unsigned ClassNotFoundExceptionTrace = 16;

#define HAVE_ClassNotFoundExceptionTrace 1

const unsigned ClassNotFoundExceptionCause = 24;

#define HAVE_ClassNotFoundExceptionCause 1

const unsigned ClassNotFoundExceptionCause2 = 32;

#define HAVE_ClassNotFoundExceptionCause2 1

const unsigned ClassRuntimeDataArrayClass = 8;

#define HAVE_ClassRuntimeDataArrayClass 1

const unsigned ClassRuntimeDataJclass = 16;

#define HAVE_ClassRuntimeDataJclass 1

const unsigned ClassRuntimeDataPool = 24;

#define HAVE_ClassRuntimeDataPool 1

const unsigned ClassRuntimeDataSigners = 32;

#define HAVE_ClassRuntimeDataSigners 1

const unsigned CleanerQueueNext = 8;

#define HAVE_CleanerQueueNext 1

const unsigned CloneNotSupportedExceptionMessage = 8;

#define HAVE_CloneNotSupportedExceptionMessage 1

const unsigned CloneNotSupportedExceptionTrace = 16;

#define HAVE_CloneNotSupportedExceptionTrace 1

const unsigned CloneNotSupportedExceptionCause = 24;

#define HAVE_CloneNotSupportedExceptionCause 1

const unsigned CodePool = 8;

#define HAVE_CodePool 1

const unsigned CodeStackMap = 16;

#define HAVE_CodeStackMap 1

const unsigned CodeExceptionHandlerTable = 24;

#define HAVE_CodeExceptionHandlerTable 1

const unsigned CodeLineNumberTable = 32;

#define HAVE_CodeLineNumberTable 1

const unsigned CodeCompiled = 40;

#define HAVE_CodeCompiled 1

const unsigned CodeCompiledSize = 48;

#define HAVE_CodeCompiledSize 1

const unsigned CodeMaxStack = 52;

#define HAVE_CodeMaxStack 1

const unsigned CodeMaxLocals = 54;

#define HAVE_CodeMaxLocals 1

const unsigned CodeLength = 56;

#define HAVE_CodeLength 1

const unsigned CodeBody = 64;

#define HAVE_CodeBody 1

const unsigned CompileRootsCallTable = 8;

#define HAVE_CompileRootsCallTable 1

const unsigned CompileRootsMethodTree = 16;

#define HAVE_CompileRootsMethodTree 1

const unsigned CompileRootsMethodTreeSentinal = 24;

#define HAVE_CompileRootsMethodTreeSentinal 1

const unsigned CompileRootsObjectPools = 32;

#define HAVE_CompileRootsObjectPools 1

const unsigned CompileRootsStaticTableArray = 40;

#define HAVE_CompileRootsStaticTableArray 1

const unsigned CompileRootsVirtualThunks = 48;

#define HAVE_CompileRootsVirtualThunks 1

const unsigned CompileRootsReceiveMethod = 56;

#define HAVE_CompileRootsReceiveMethod 1

const unsigned CompileRootsWindMethod = 64;

#define HAVE_CompileRootsWindMethod 1

const unsigned CompileRootsRewindMethod = 72;

#define HAVE_CompileRootsRewindMethod 1

const unsigned ContinuationNext = 8;

#define HAVE_ContinuationNext 1

const unsigned ContinuationContext = 16;

#define HAVE_ContinuationContext 1

const unsigned ContinuationMethod = 24;

#define HAVE_ContinuationMethod 1

const unsigned ContinuationAddress = 32;

#define HAVE_ContinuationAddress 1

const unsigned ContinuationReturnAddressOffset = 40;

#define HAVE_ContinuationReturnAddressOffset 1

const unsigned ContinuationFramePointerOffset = 48;

#define HAVE_ContinuationFramePointerOffset 1

const unsigned ContinuationLength = 56;

#define HAVE_ContinuationLength 1

const unsigned ContinuationBody = 64;

#define HAVE_ContinuationBody 1

const unsigned ContinuationContextNext = 8;

#define HAVE_ContinuationContextNext 1

const unsigned ContinuationContextBefore = 16;

#define HAVE_ContinuationContextBefore 1

const unsigned ContinuationContextAfter = 24;

#define HAVE_ContinuationContextAfter 1

const unsigned ContinuationContextContinuation = 32;

#define HAVE_ContinuationContextContinuation 1

const unsigned ContinuationContextMethod = 40;

#define HAVE_ContinuationContextMethod 1

const unsigned DoubleValue = 8;

#define HAVE_DoubleValue 1

const unsigned DoubleArrayLength = 8;

#define HAVE_DoubleArrayLength 1

const unsigned DoubleArrayBody = 16;

#define HAVE_DoubleArrayBody 1

const unsigned ErrorMessage = 8;

#define HAVE_ErrorMessage 1

const unsigned ErrorTrace = 16;

#define HAVE_ErrorTrace 1

const unsigned ErrorCause = 24;

#define HAVE_ErrorCause 1

const unsigned ExceptionMessage = 8;

#define HAVE_ExceptionMessage 1

const unsigned ExceptionTrace = 16;

#define HAVE_ExceptionTrace 1

const unsigned ExceptionCause = 24;

#define HAVE_ExceptionCause 1

const unsigned ExceptionHandlerTableLength = 8;

#define HAVE_ExceptionHandlerTableLength 1

const unsigned ExceptionHandlerTableBody = 16;

#define HAVE_ExceptionHandlerTableBody 1

const unsigned ExceptionInInitializerErrorMessage = 8;

#define HAVE_ExceptionInInitializerErrorMessage 1

const unsigned ExceptionInInitializerErrorTrace = 16;

#define HAVE_ExceptionInInitializerErrorTrace 1

const unsigned ExceptionInInitializerErrorCause = 24;

#define HAVE_ExceptionInInitializerErrorCause 1

const unsigned ExceptionInInitializerErrorException = 32;

#define HAVE_ExceptionInInitializerErrorException 1

const unsigned FieldVmFlags = 8;

#define HAVE_FieldVmFlags 1

const unsigned FieldCode = 9;

#define HAVE_FieldCode 1

const unsigned FieldFlags = 10;

#define HAVE_FieldFlags 1

const unsigned FieldOffset = 12;

#define HAVE_FieldOffset 1

const unsigned FieldNativeID = 16;

#define HAVE_FieldNativeID 1

const unsigned FieldName = 24;

#define HAVE_FieldName 1

const unsigned FieldSpec = 32;

#define HAVE_FieldSpec 1

const unsigned FieldAddendum = 40;

#define HAVE_FieldAddendum 1

const unsigned FieldClass = 48;

#define HAVE_FieldClass 1

const unsigned FieldAddendumPool = 8;

#define HAVE_FieldAddendumPool 1

const unsigned FieldAddendumAnnotationTable = 16;

#define HAVE_FieldAddendumAnnotationTable 1

const unsigned FieldAddendumSignature = 24;

#define HAVE_FieldAddendumSignature 1

const unsigned FileNotFoundExceptionMessage = 8;

#define HAVE_FileNotFoundExceptionMessage 1

const unsigned FileNotFoundExceptionTrace = 16;

#define HAVE_FileNotFoundExceptionTrace 1

const unsigned FileNotFoundExceptionCause = 24;

#define HAVE_FileNotFoundExceptionCause 1

const unsigned FinalizerTarget = 8;

#define HAVE_FinalizerTarget 1

const unsigned FinalizerFinalize = 16;

#define HAVE_FinalizerFinalize 1

const unsigned FinalizerNext = 24;

#define HAVE_FinalizerNext 1

const unsigned FinalizerQueueTarget = 32;

#define HAVE_FinalizerQueueTarget 1

const unsigned FinalizerQueueNext = 40;

#define HAVE_FinalizerQueueNext 1

const unsigned FinderFinder = 8;

#define HAVE_FinderFinder 1

const unsigned FinderName = 16;

#define HAVE_FinderName 1

const unsigned FinderNext = 24;

#define HAVE_FinderNext 1

const unsigned FloatValue = 8;

#define HAVE_FloatValue 1

const unsigned FloatArrayLength = 8;

#define HAVE_FloatArrayLength 1

const unsigned FloatArrayBody = 16;

#define HAVE_FloatArrayBody 1

const unsigned HashMapSize = 8;

#define HAVE_HashMapSize 1

const unsigned HashMapArray = 16;

#define HAVE_HashMapArray 1

const unsigned IllegalArgumentExceptionMessage = 8;

#define HAVE_IllegalArgumentExceptionMessage 1

const unsigned IllegalArgumentExceptionTrace = 16;

#define HAVE_IllegalArgumentExceptionTrace 1

const unsigned IllegalArgumentExceptionCause = 24;

#define HAVE_IllegalArgumentExceptionCause 1

const unsigned IllegalMonitorStateExceptionMessage = 8;

#define HAVE_IllegalMonitorStateExceptionMessage 1

const unsigned IllegalMonitorStateExceptionTrace = 16;

#define HAVE_IllegalMonitorStateExceptionTrace 1

const unsigned IllegalMonitorStateExceptionCause = 24;

#define HAVE_IllegalMonitorStateExceptionCause 1

const unsigned IllegalStateExceptionMessage = 8;

#define HAVE_IllegalStateExceptionMessage 1

const unsigned IllegalStateExceptionTrace = 16;

#define HAVE_IllegalStateExceptionTrace 1

const unsigned IllegalStateExceptionCause = 24;

#define HAVE_IllegalStateExceptionCause 1

const unsigned IncompatibleClassChangeErrorMessage = 8;

#define HAVE_IncompatibleClassChangeErrorMessage 1

const unsigned IncompatibleClassChangeErrorTrace = 16;

#define HAVE_IncompatibleClassChangeErrorTrace 1

const unsigned IncompatibleClassChangeErrorCause = 24;

#define HAVE_IncompatibleClassChangeErrorCause 1

const unsigned IncompatibleContinuationExceptionMessage = 8;

#define HAVE_IncompatibleContinuationExceptionMessage 1

const unsigned IncompatibleContinuationExceptionTrace = 16;

#define HAVE_IncompatibleContinuationExceptionTrace 1

const unsigned IncompatibleContinuationExceptionCause = 24;

#define HAVE_IncompatibleContinuationExceptionCause 1

const unsigned IndexOutOfBoundsExceptionMessage = 8;

#define HAVE_IndexOutOfBoundsExceptionMessage 1

const unsigned IndexOutOfBoundsExceptionTrace = 16;

#define HAVE_IndexOutOfBoundsExceptionTrace 1

const unsigned IndexOutOfBoundsExceptionCause = 24;

#define HAVE_IndexOutOfBoundsExceptionCause 1

const unsigned InnerClassReferenceInner = 8;

#define HAVE_InnerClassReferenceInner 1

const unsigned InnerClassReferenceOuter = 16;

#define HAVE_InnerClassReferenceOuter 1

const unsigned InnerClassReferenceName = 24;

#define HAVE_InnerClassReferenceName 1

const unsigned InnerClassReferenceFlags = 32;

#define HAVE_InnerClassReferenceFlags 1

const unsigned IntValue = 8;

#define HAVE_IntValue 1

const unsigned IntArrayLength = 8;

#define HAVE_IntArrayLength 1

const unsigned IntArrayBody = 16;

#define HAVE_IntArrayBody 1

const unsigned InterruptedExceptionMessage = 8;

#define HAVE_InterruptedExceptionMessage 1

const unsigned InterruptedExceptionTrace = 16;

#define HAVE_InterruptedExceptionTrace 1

const unsigned InterruptedExceptionCause = 24;

#define HAVE_InterruptedExceptionCause 1

const unsigned InvocationBootstrap = 8;

#define HAVE_InvocationBootstrap 1

const unsigned InvocationIndex = 12;

#define HAVE_InvocationIndex 1

const unsigned InvocationClass = 16;

#define HAVE_InvocationClass 1

const unsigned InvocationPool = 24;

#define HAVE_InvocationPool 1

const unsigned InvocationTemplate = 32;

#define HAVE_InvocationTemplate 1

const unsigned InvocationSite = 40;

#define HAVE_InvocationSite 1

const unsigned InvocationTargetExceptionMessage = 8;

#define HAVE_InvocationTargetExceptionMessage 1

const unsigned InvocationTargetExceptionTrace = 16;

#define HAVE_InvocationTargetExceptionTrace 1

const unsigned InvocationTargetExceptionCause = 24;

#define HAVE_InvocationTargetExceptionCause 1

const unsigned InvocationTargetExceptionTarget = 32;

#define HAVE_InvocationTargetExceptionTarget 1

const unsigned IoExceptionMessage = 8;

#define HAVE_IoExceptionMessage 1

const unsigned IoExceptionTrace = 16;

#define HAVE_IoExceptionTrace 1

const unsigned IoExceptionCause = 24;

#define HAVE_IoExceptionCause 1

const unsigned JclassVmClass = 8;

#define HAVE_JclassVmClass 1

const unsigned JconstructorMethod = 8;

#define HAVE_JconstructorMethod 1

const unsigned JfieldVmField = 8;

#define HAVE_JfieldVmField 1

const unsigned JfieldAccessible = 16;

#define HAVE_JfieldAccessible 1

const unsigned JmethodVmMethod = 8;

#define HAVE_JmethodVmMethod 1

const unsigned JmethodAccessible = 16;

#define HAVE_JmethodAccessible 1

const unsigned JreferenceVmNext = 8;

#define HAVE_JreferenceVmNext 1

const unsigned JreferenceTarget = 16;

#define HAVE_JreferenceTarget 1

const unsigned JreferenceQueue = 24;

#define HAVE_JreferenceQueue 1

const unsigned JreferenceJNext = 32;

#define HAVE_JreferenceJNext 1

const unsigned LineNumberTableLength = 8;

#define HAVE_LineNumberTableLength 1

const unsigned LineNumberTableBody = 16;

#define HAVE_LineNumberTableBody 1

const unsigned LinkageErrorMessage = 8;

#define HAVE_LinkageErrorMessage 1

const unsigned LinkageErrorTrace = 16;

#define HAVE_LinkageErrorTrace 1

const unsigned LinkageErrorCause = 24;

#define HAVE_LinkageErrorCause 1

const unsigned ListSize = 8;

#define HAVE_ListSize 1

const unsigned ListFront = 16;

#define HAVE_ListFront 1

const unsigned ListRear = 24;

#define HAVE_ListRear 1

const unsigned LongValue = 8;

#define HAVE_LongValue 1

const unsigned LongArrayLength = 8;

#define HAVE_LongArrayLength 1

const unsigned LongArrayBody = 16;

#define HAVE_LongArrayBody 1

const unsigned MethodVmFlags = 8;

#define HAVE_MethodVmFlags 1

const unsigned MethodReturnCode = 9;

#define HAVE_MethodReturnCode 1

const unsigned MethodParameterCount = 10;

#define HAVE_MethodParameterCount 1

const unsigned MethodParameterFootprint = 11;

#define HAVE_MethodParameterFootprint 1

const unsigned MethodFlags = 12;

#define HAVE_MethodFlags 1

const unsigned MethodOffset = 14;

#define HAVE_MethodOffset 1

const unsigned MethodNativeID = 16;

#define HAVE_MethodNativeID 1

const unsigned MethodRuntimeDataIndex = 20;

#define HAVE_MethodRuntimeDataIndex 1

const unsigned MethodName = 24;

#define HAVE_MethodName 1

const unsigned MethodSpec = 32;

#define HAVE_MethodSpec 1

const unsigned MethodAddendum = 40;

#define HAVE_MethodAddendum 1

const unsigned MethodClass = 48;

#define HAVE_MethodClass 1

const unsigned MethodCode = 56;

#define HAVE_MethodCode 1

const unsigned MethodAddendumPool = 8;

#define HAVE_MethodAddendumPool 1

const unsigned MethodAddendumAnnotationTable = 16;

#define HAVE_MethodAddendumAnnotationTable 1

const unsigned MethodAddendumSignature = 24;

#define HAVE_MethodAddendumSignature 1

const unsigned MethodAddendumExceptionTable = 32;

#define HAVE_MethodAddendumExceptionTable 1

const unsigned MethodAddendumAnnotationDefault = 40;

#define HAVE_MethodAddendumAnnotationDefault 1

const unsigned MethodAddendumParameterAnnotationTable = 48;

#define HAVE_MethodAddendumParameterAnnotationTable 1

const unsigned MethodRuntimeDataNative = 8;

#define HAVE_MethodRuntimeDataNative 1

const unsigned MonitorOwner = 8;

#define HAVE_MonitorOwner 1

const unsigned MonitorWaitHead = 16;

#define HAVE_MonitorWaitHead 1

const unsigned MonitorWaitTail = 24;

#define HAVE_MonitorWaitTail 1

const unsigned MonitorAcquireHead = 32;

#define HAVE_MonitorAcquireHead 1

const unsigned MonitorAcquireTail = 40;

#define HAVE_MonitorAcquireTail 1

const unsigned MonitorDepth = 48;

#define HAVE_MonitorDepth 1

const unsigned MonitorNodeValue = 8;

#define HAVE_MonitorNodeValue 1

const unsigned MonitorNodeNext = 16;

#define HAVE_MonitorNodeNext 1

const unsigned NativeFunction = 8;

#define HAVE_NativeFunction 1

const unsigned NativeFast = 16;

#define HAVE_NativeFast 1

const unsigned NativeInterceptFunction = 8;

#define HAVE_NativeInterceptFunction 1

const unsigned NativeInterceptFast = 16;

#define HAVE_NativeInterceptFast 1

const unsigned NativeInterceptOriginal = 24;

#define HAVE_NativeInterceptOriginal 1

const unsigned NegativeArraySizeExceptionMessage = 8;

#define HAVE_NegativeArraySizeExceptionMessage 1

const unsigned NegativeArraySizeExceptionTrace = 16;

#define HAVE_NegativeArraySizeExceptionTrace 1

const unsigned NegativeArraySizeExceptionCause = 24;

#define HAVE_NegativeArraySizeExceptionCause 1

const unsigned NoClassDefFoundErrorMessage = 8;

#define HAVE_NoClassDefFoundErrorMessage 1

const unsigned NoClassDefFoundErrorTrace = 16;

#define HAVE_NoClassDefFoundErrorTrace 1

const unsigned NoClassDefFoundErrorCause = 24;

#define HAVE_NoClassDefFoundErrorCause 1

const unsigned NoSuchFieldErrorMessage = 8;

#define HAVE_NoSuchFieldErrorMessage 1

const unsigned NoSuchFieldErrorTrace = 16;

#define HAVE_NoSuchFieldErrorTrace 1

const unsigned NoSuchFieldErrorCause = 24;

#define HAVE_NoSuchFieldErrorCause 1

const unsigned NoSuchMethodErrorMessage = 8;

#define HAVE_NoSuchMethodErrorMessage 1

const unsigned NoSuchMethodErrorTrace = 16;

#define HAVE_NoSuchMethodErrorTrace 1

const unsigned NoSuchMethodErrorCause = 24;

#define HAVE_NoSuchMethodErrorCause 1

const unsigned NullPointerExceptionMessage = 8;

#define HAVE_NullPointerExceptionMessage 1

const unsigned NullPointerExceptionTrace = 16;

#define HAVE_NullPointerExceptionTrace 1

const unsigned NullPointerExceptionCause = 24;

#define HAVE_NullPointerExceptionCause 1

const unsigned OutOfMemoryErrorMessage = 8;

#define HAVE_OutOfMemoryErrorMessage 1

const unsigned OutOfMemoryErrorTrace = 16;

#define HAVE_OutOfMemoryErrorTrace 1

const unsigned OutOfMemoryErrorCause = 24;

#define HAVE_OutOfMemoryErrorCause 1

const unsigned PairFirst = 8;

#define HAVE_PairFirst 1

const unsigned PairSecond = 16;

#define HAVE_PairSecond 1

const unsigned PhantomReferenceVmNext = 8;

#define HAVE_PhantomReferenceVmNext 1

const unsigned PhantomReferenceTarget = 16;

#define HAVE_PhantomReferenceTarget 1

const unsigned PhantomReferenceQueue = 24;

#define HAVE_PhantomReferenceQueue 1

const unsigned PhantomReferenceJNext = 32;

#define HAVE_PhantomReferenceJNext 1

const unsigned PointerValue = 8;

#define HAVE_PointerValue 1

const unsigned ReferenceKind = 8;

#define HAVE_ReferenceKind 1

const unsigned ReferenceClass = 16;

#define HAVE_ReferenceClass 1

const unsigned ReferenceName = 24;

#define HAVE_ReferenceName 1

const unsigned ReferenceSpec = 32;

#define HAVE_ReferenceSpec 1

const unsigned ReferenceQueueFront = 8;

#define HAVE_ReferenceQueueFront 1

const unsigned ReferenceQueueJnext = 16;

#define HAVE_ReferenceQueueJnext 1

const unsigned ReflectiveOperationExceptionMessage = 8;

#define HAVE_ReflectiveOperationExceptionMessage 1

const unsigned ReflectiveOperationExceptionTrace = 16;

#define HAVE_ReflectiveOperationExceptionTrace 1

const unsigned ReflectiveOperationExceptionCause = 24;

#define HAVE_ReflectiveOperationExceptionCause 1

const unsigned RegionRegion = 8;

#define HAVE_RegionRegion 1

const unsigned RegionPosition = 16;

#define HAVE_RegionPosition 1

const unsigned RootsBootLoader = 8;

#define HAVE_RootsBootLoader 1

const unsigned RootsAppLoader = 16;

#define HAVE_RootsAppLoader 1

const unsigned RootsBootstrapClassMap = 24;

#define HAVE_RootsBootstrapClassMap 1

const unsigned RootsPackageMap = 32;

#define HAVE_RootsPackageMap 1

const unsigned RootsFindLoadedClassMethod = 40;

#define HAVE_RootsFindLoadedClassMethod 1

const unsigned RootsLoadClassMethod = 48;

#define HAVE_RootsLoadClassMethod 1

const unsigned RootsMonitorMap = 56;

#define HAVE_RootsMonitorMap 1

const unsigned RootsStringMap = 64;

#define HAVE_RootsStringMap 1

const unsigned RootsByteArrayMap = 72;

#define HAVE_RootsByteArrayMap 1

const unsigned RootsPoolMap = 80;

#define HAVE_RootsPoolMap 1

const unsigned RootsClassRuntimeDataTable = 88;

#define HAVE_RootsClassRuntimeDataTable 1

const unsigned RootsMethodRuntimeDataTable = 96;

#define HAVE_RootsMethodRuntimeDataTable 1

const unsigned RootsJNIMethodTable = 104;

#define HAVE_RootsJNIMethodTable 1

const unsigned RootsJNIFieldTable = 112;

#define HAVE_RootsJNIFieldTable 1

const unsigned RootsShutdownHooks = 120;

#define HAVE_RootsShutdownHooks 1

const unsigned RootsFinalizerThread = 128;

#define HAVE_RootsFinalizerThread 1

const unsigned RootsObjectsToFinalize = 136;

#define HAVE_RootsObjectsToFinalize 1

const unsigned RootsObjectsToClean = 144;

#define HAVE_RootsObjectsToClean 1

const unsigned RootsNullPointerException = 152;

#define HAVE_RootsNullPointerException 1

const unsigned RootsArithmeticException = 160;

#define HAVE_RootsArithmeticException 1

const unsigned RootsArrayIndexOutOfBoundsException = 168;

#define HAVE_RootsArrayIndexOutOfBoundsException 1

const unsigned RootsOutOfMemoryError = 176;

#define HAVE_RootsOutOfMemoryError 1

const unsigned RootsShutdownInProgress = 184;

#define HAVE_RootsShutdownInProgress 1

const unsigned RootsVirtualFileFinders = 192;

#define HAVE_RootsVirtualFileFinders 1

const unsigned RootsVirtualFiles = 200;

#define HAVE_RootsVirtualFiles 1

const unsigned RootsArrayInterfaceTable = 208;

#define HAVE_RootsArrayInterfaceTable 1

const unsigned RootsThreadTerminated = 216;

#define HAVE_RootsThreadTerminated 1

const unsigned RuntimeExceptionMessage = 8;

#define HAVE_RuntimeExceptionMessage 1

const unsigned RuntimeExceptionTrace = 16;

#define HAVE_RuntimeExceptionTrace 1

const unsigned RuntimeExceptionCause = 24;

#define HAVE_RuntimeExceptionCause 1

const unsigned ShortValue = 8;

#define HAVE_ShortValue 1

const unsigned ShortArrayLength = 8;

#define HAVE_ShortArrayLength 1

const unsigned ShortArrayBody = 16;

#define HAVE_ShortArrayBody 1

const unsigned SingletonLength = 8;

#define HAVE_SingletonLength 1

const unsigned SingletonBody = 16;

#define HAVE_SingletonBody 1

const unsigned SoftReferenceVmNext = 8;

#define HAVE_SoftReferenceVmNext 1

const unsigned SoftReferenceTarget = 16;

#define HAVE_SoftReferenceTarget 1

const unsigned SoftReferenceQueue = 24;

#define HAVE_SoftReferenceQueue 1

const unsigned SoftReferenceJNext = 32;

#define HAVE_SoftReferenceJNext 1

const unsigned StackOverflowErrorMessage = 8;

#define HAVE_StackOverflowErrorMessage 1

const unsigned StackOverflowErrorTrace = 16;

#define HAVE_StackOverflowErrorTrace 1

const unsigned StackOverflowErrorCause = 24;

#define HAVE_StackOverflowErrorCause 1

const unsigned StackTraceElementClass = 8;

#define HAVE_StackTraceElementClass 1

const unsigned StackTraceElementMethod = 16;

#define HAVE_StackTraceElementMethod 1

const unsigned StackTraceElementFile = 24;

#define HAVE_StackTraceElementFile 1

const unsigned StackTraceElementLine = 32;

#define HAVE_StackTraceElementLine 1

const unsigned StringData = 8;

#define HAVE_StringData 1

const unsigned StringOffset = 16;

#define HAVE_StringOffset 1

const unsigned StringLength = 20;

#define HAVE_StringLength 1

const unsigned StringHashCode = 24;

#define HAVE_StringHashCode 1

const unsigned SystemClassLoaderParent = 8;

#define HAVE_SystemClassLoaderParent 1

const unsigned SystemClassLoaderPackages = 16;

#define HAVE_SystemClassLoaderPackages 1

const unsigned SystemClassLoaderMap = 24;

#define HAVE_SystemClassLoaderMap 1

const unsigned SystemClassLoaderFinder = 32;

#define HAVE_SystemClassLoaderFinder 1

const unsigned ThreadParkBlocker = 8;

#define HAVE_ThreadParkBlocker 1

const unsigned ThreadPeer = 16;

#define HAVE_ThreadPeer 1

const unsigned ThreadInterrupted = 24;

#define HAVE_ThreadInterrupted 1

const unsigned ThreadUnparked = 25;

#define HAVE_ThreadUnparked 1

const unsigned ThreadDaemon = 26;

#define HAVE_ThreadDaemon 1

const unsigned ThreadState = 27;

#define HAVE_ThreadState 1

const unsigned ThreadPriority = 28;

#define HAVE_ThreadPriority 1

const unsigned ThreadTask = 32;

#define HAVE_ThreadTask 1

const unsigned ThreadLocals = 40;

#define HAVE_ThreadLocals 1

const unsigned ThreadSleepLock = 48;

#define HAVE_ThreadSleepLock 1

const unsigned ThreadClassLoader = 56;

#define HAVE_ThreadClassLoader 1

const unsigned ThreadExceptionHandler = 64;

#define HAVE_ThreadExceptionHandler 1

const unsigned ThreadName = 72;

#define HAVE_ThreadName 1

const unsigned ThreadGroup = 80;

#define HAVE_ThreadGroup 1

const unsigned ThreadInterruptLock = 88;

#define HAVE_ThreadInterruptLock 1

const unsigned ThreadGroupParent = 8;

#define HAVE_ThreadGroupParent 1

const unsigned ThreadGroupName = 16;

#define HAVE_ThreadGroupName 1

const unsigned ThreadGroupSubgroups = 24;

#define HAVE_ThreadGroupSubgroups 1

const unsigned ThrowableMessage = 8;

#define HAVE_ThrowableMessage 1

const unsigned ThrowableTrace = 16;

#define HAVE_ThrowableTrace 1

const unsigned ThrowableCause = 24;

#define HAVE_ThrowableCause 1

const unsigned TraceElementMethod = 8;

#define HAVE_TraceElementMethod 1

const unsigned TraceElementIp = 16;

#define HAVE_TraceElementIp 1

const unsigned TreeNodeValue = 8;

#define HAVE_TreeNodeValue 1

const unsigned TreeNodeLeft = 16;

#define HAVE_TreeNodeLeft 1

const unsigned TreeNodeRight = 24;

#define HAVE_TreeNodeRight 1

const unsigned TripleFirst = 8;

#define HAVE_TripleFirst 1

const unsigned TripleSecond = 16;

#define HAVE_TripleSecond 1

const unsigned TripleThird = 24;

#define HAVE_TripleThird 1

const unsigned UnsatisfiedLinkErrorMessage = 8;

#define HAVE_UnsatisfiedLinkErrorMessage 1

const unsigned UnsatisfiedLinkErrorTrace = 16;

#define HAVE_UnsatisfiedLinkErrorTrace 1

const unsigned UnsatisfiedLinkErrorCause = 24;

#define HAVE_UnsatisfiedLinkErrorCause 1

const unsigned UnwindResultContinuation = 8;

#define HAVE_UnwindResultContinuation 1

const unsigned UnwindResultResult = 16;

#define HAVE_UnwindResultResult 1

const unsigned UnwindResultException = 24;

#define HAVE_UnwindResultException 1

const unsigned VectorSize = 8;

#define HAVE_VectorSize 1

const unsigned VectorLength = 16;

#define HAVE_VectorLength 1

const unsigned VectorBody = 24;

#define HAVE_VectorBody 1

const unsigned VirtualMachineErrorMessage = 8;

#define HAVE_VirtualMachineErrorMessage 1

const unsigned VirtualMachineErrorTrace = 16;

#define HAVE_VirtualMachineErrorTrace 1

const unsigned VirtualMachineErrorCause = 24;

#define HAVE_VirtualMachineErrorCause 1

const unsigned WeakHashMapSize = 8;

#define HAVE_WeakHashMapSize 1

const unsigned WeakHashMapArray = 16;

#define HAVE_WeakHashMapArray 1

const unsigned WeakReferenceVmNext = 8;

#define HAVE_WeakReferenceVmNext 1

const unsigned WeakReferenceTarget = 16;

#define HAVE_WeakReferenceTarget 1

const unsigned WeakReferenceQueue = 24;

#define HAVE_WeakReferenceQueue 1

const unsigned WeakReferenceJNext = 32;

#define HAVE_WeakReferenceJNext 1

const unsigned WordArrayLength = 8;

#define HAVE_WordArrayLength 1

const unsigned WordArrayBody = 16;

#define HAVE_WordArrayBody 1

const unsigned FixedSizeOfAbstractMethodError = 32;

const unsigned FixedSizeOfAddendum = 32;

const unsigned FixedSizeOfArithmeticException = 32;

const unsigned FixedSizeOfArray = 16;

const unsigned ArrayElementSizeOfArray = 8;

const unsigned FixedSizeOfArrayIndexOutOfBoundsException = 32;

const unsigned FixedSizeOfArrayStoreException = 32;

const unsigned FixedSizeOfBoolean = 9;

const unsigned FixedSizeOfBooleanArray = 16;

const unsigned ArrayElementSizeOfBooleanArray = 1;

const unsigned FixedSizeOfByte = 9;

const unsigned FixedSizeOfByteArray = 16;

const unsigned ArrayElementSizeOfByteArray = 1;

const unsigned FixedSizeOfCallNode = 40;

const unsigned FixedSizeOfChar = 10;

const unsigned FixedSizeOfCharArray = 16;

const unsigned ArrayElementSizeOfCharArray = 2;

const unsigned FixedSizeOfClass = 136;

const unsigned ArrayElementSizeOfClass = 8;

const unsigned FixedSizeOfClassAddendum = 72;

const unsigned FixedSizeOfClassCastException = 32;

const unsigned FixedSizeOfClassLoader = 32;

const unsigned FixedSizeOfClassNotFoundException = 40;

const unsigned FixedSizeOfClassRuntimeData = 40;

const unsigned FixedSizeOfCleaner = 16;

const unsigned FixedSizeOfCloneNotSupportedException = 32;

const unsigned FixedSizeOfCloneable = 8;

const unsigned FixedSizeOfCode = 64;

const unsigned ArrayElementSizeOfCode = 1;

const unsigned FixedSizeOfCompileRoots = 80;

const unsigned FixedSizeOfConstantPool = 8;

const unsigned FixedSizeOfContinuation = 64;

const unsigned ArrayElementSizeOfContinuation = 8;

const unsigned FixedSizeOfContinuationContext = 48;

const unsigned FixedSizeOfDouble = 16;

const unsigned FixedSizeOfDoubleArray = 16;

const unsigned ArrayElementSizeOfDoubleArray = 8;

const unsigned FixedSizeOfError = 32;

const unsigned FixedSizeOfException = 32;

const unsigned FixedSizeOfExceptionHandlerTable = 16;

const unsigned ArrayElementSizeOfExceptionHandlerTable = 8;

const unsigned FixedSizeOfExceptionInInitializerError = 40;

const unsigned FixedSizeOfField = 56;

const unsigned FixedSizeOfFieldAddendum = 32;

const unsigned FixedSizeOfFileNotFoundException = 32;

const unsigned FixedSizeOfFinalizer = 48;

const unsigned FixedSizeOfFinder = 32;

const unsigned FixedSizeOfFloat = 12;

const unsigned FixedSizeOfFloatArray = 16;

const unsigned ArrayElementSizeOfFloatArray = 4;

const unsigned FixedSizeOfHashMap = 24;

const unsigned FixedSizeOfIllegalArgumentException = 32;

const unsigned FixedSizeOfIllegalMonitorStateException = 32;

const unsigned FixedSizeOfIllegalStateException = 32;

const unsigned FixedSizeOfIncompatibleClassChangeError = 32;

const unsigned FixedSizeOfIncompatibleContinuationException = 32;

const unsigned FixedSizeOfIndexOutOfBoundsException = 32;

const unsigned FixedSizeOfInnerClassReference = 34;

const unsigned FixedSizeOfInt = 12;

const unsigned FixedSizeOfIntArray = 16;

const unsigned ArrayElementSizeOfIntArray = 4;

const unsigned FixedSizeOfInterruptedException = 32;

const unsigned FixedSizeOfInvocation = 48;

const unsigned FixedSizeOfInvocationTargetException = 40;

const unsigned FixedSizeOfIoException = 32;

const unsigned FixedSizeOfJaccessibleObject = 8;

const unsigned FixedSizeOfJboolean = 8;

const unsigned FixedSizeOfJbyte = 8;

const unsigned FixedSizeOfJchar = 8;

const unsigned FixedSizeOfJclass = 16;

const unsigned FixedSizeOfJconstructor = 16;

const unsigned FixedSizeOfJdouble = 8;

const unsigned FixedSizeOfJfield = 17;

const unsigned FixedSizeOfJfloat = 8;

const unsigned FixedSizeOfJint = 8;

const unsigned FixedSizeOfJlong = 8;

const unsigned FixedSizeOfJmethod = 17;

const unsigned FixedSizeOfJobject = 8;

const unsigned FixedSizeOfJreference = 40;

const unsigned FixedSizeOfJshort = 8;

const unsigned FixedSizeOfJvoid = 8;

const unsigned FixedSizeOfLineNumberTable = 16;

const unsigned ArrayElementSizeOfLineNumberTable = 8;

const unsigned FixedSizeOfLinkageError = 32;

const unsigned FixedSizeOfList = 32;

const unsigned FixedSizeOfLong = 16;

const unsigned FixedSizeOfLongArray = 16;

const unsigned ArrayElementSizeOfLongArray = 8;

const unsigned FixedSizeOfMethod = 64;

const unsigned FixedSizeOfMethodAddendum = 56;

const unsigned FixedSizeOfMethodRuntimeData = 16;

const unsigned FixedSizeOfMonitor = 52;

const unsigned FixedSizeOfMonitorNode = 24;

const unsigned FixedSizeOfNative = 17;

const unsigned FixedSizeOfNativeIntercept = 32;

const unsigned FixedSizeOfNegativeArraySizeException = 32;

const unsigned FixedSizeOfNoClassDefFoundError = 32;

const unsigned FixedSizeOfNoSuchFieldError = 32;

const unsigned FixedSizeOfNoSuchMethodError = 32;

const unsigned FixedSizeOfNullPointerException = 32;

const unsigned FixedSizeOfNumber = 8;

const unsigned FixedSizeOfOutOfMemoryError = 32;

const unsigned FixedSizeOfPair = 24;

const unsigned FixedSizeOfPhantomReference = 40;

const unsigned FixedSizeOfPointer = 16;

const unsigned FixedSizeOfReference = 40;

const unsigned FixedSizeOfReferenceQueue = 24;

const unsigned FixedSizeOfReflectiveOperationException = 32;

const unsigned FixedSizeOfRegion = 20;

const unsigned FixedSizeOfRoots = 224;

const unsigned FixedSizeOfRuntimeException = 32;

const unsigned FixedSizeOfSerializable = 8;

const unsigned FixedSizeOfShort = 10;

const unsigned FixedSizeOfShortArray = 16;

const unsigned ArrayElementSizeOfShortArray = 2;

const unsigned FixedSizeOfSingleton = 16;

const unsigned ArrayElementSizeOfSingleton = 8;

const unsigned FixedSizeOfSoftReference = 40;

const unsigned FixedSizeOfStackOverflowError = 32;

const unsigned FixedSizeOfStackTraceElement = 36;

const unsigned FixedSizeOfString = 28;

const unsigned FixedSizeOfSystemClassLoader = 40;

const unsigned FixedSizeOfThread = 96;

const unsigned FixedSizeOfThreadGroup = 32;

const unsigned FixedSizeOfThrowable = 32;

const unsigned FixedSizeOfTraceElement = 20;

const unsigned FixedSizeOfTreeNode = 32;

const unsigned FixedSizeOfTriple = 32;

const unsigned FixedSizeOfUnsatisfiedLinkError = 32;

const unsigned FixedSizeOfUnwindResult = 32;

const unsigned FixedSizeOfVector = 24;

const unsigned ArrayElementSizeOfVector = 8;

const unsigned FixedSizeOfVirtualMachineError = 32;

const unsigned FixedSizeOfWeakHashMap = 24;

const unsigned FixedSizeOfWeakReference = 40;

const unsigned FixedSizeOfWordArray = 16;

const unsigned ArrayElementSizeOfWordArray = 8;

class GcAbstractMethodError: public GcObject {
 public:
  static const Gc::Type Type = Gc::AbstractMethodErrorType;
  static const size_t FixedSize = FixedSizeOfAbstractMethodError;

  static GcAbstractMethodError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , AbstractMethodErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(AbstractMethodErrorMessage); }
  GcString* message() { return field_at<GcString*>(AbstractMethodErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , AbstractMethodErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(AbstractMethodErrorTrace); }
  object trace() { return field_at<object>(AbstractMethodErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , AbstractMethodErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(AbstractMethodErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(AbstractMethodErrorCause); }
};

class GcAddendum: public GcObject {
 public:
  static const Gc::Type Type = Gc::AddendumType;
  static const size_t FixedSize = FixedSizeOfAddendum;

  static GcAddendum* makeZeroed(Thread* t);
  void setPool(Thread* t UNUSED, GcSingleton* value) { setField(t, this , AddendumPool, reinterpret_cast<object>(value)); }
  GcSingleton** poolPtr() { return &field_at<GcSingleton*>(AddendumPool); }
  GcSingleton* pool() { return field_at<GcSingleton*>(AddendumPool); }
  void setAnnotationTable(Thread* t UNUSED, object value) { setField(t, this , AddendumAnnotationTable, reinterpret_cast<object>(value)); }
  object* annotationTablePtr() { return &field_at<object>(AddendumAnnotationTable); }
  object annotationTable() { return field_at<object>(AddendumAnnotationTable); }
  void setSignature(Thread* t UNUSED, object value) { setField(t, this , AddendumSignature, reinterpret_cast<object>(value)); }
  object* signaturePtr() { return &field_at<object>(AddendumSignature); }
  object signature() { return field_at<object>(AddendumSignature); }
};

class GcArithmeticException: public GcObject {
 public:
  static const Gc::Type Type = Gc::ArithmeticExceptionType;
  static const size_t FixedSize = FixedSizeOfArithmeticException;

  static GcArithmeticException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ArithmeticExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ArithmeticExceptionMessage); }
  GcString* message() { return field_at<GcString*>(ArithmeticExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ArithmeticExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ArithmeticExceptionTrace); }
  object trace() { return field_at<object>(ArithmeticExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ArithmeticExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ArithmeticExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ArithmeticExceptionCause); }
};

class GcArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::ArrayType;
  static const size_t FixedSize = FixedSizeOfArray;

  static GcArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(ArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(ArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(ArrayLength); }
  avian::util::Slice<const object> body() { return avian::util::Slice<const object> (&field_at<const object>(ArrayBody), field_at<uintptr_t>(ArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, object value) { setField(t, this , ArrayBody + index * (8), reinterpret_cast<object>(value)); }
};

class GcArrayIndexOutOfBoundsException: public GcObject {
 public:
  static const Gc::Type Type = Gc::ArrayIndexOutOfBoundsExceptionType;
  static const size_t FixedSize = FixedSizeOfArrayIndexOutOfBoundsException;

  static GcArrayIndexOutOfBoundsException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ArrayIndexOutOfBoundsExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ArrayIndexOutOfBoundsExceptionMessage); }
  GcString* message() { return field_at<GcString*>(ArrayIndexOutOfBoundsExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ArrayIndexOutOfBoundsExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ArrayIndexOutOfBoundsExceptionTrace); }
  object trace() { return field_at<object>(ArrayIndexOutOfBoundsExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ArrayIndexOutOfBoundsExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ArrayIndexOutOfBoundsExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ArrayIndexOutOfBoundsExceptionCause); }
};

class GcArrayStoreException: public GcObject {
 public:
  static const Gc::Type Type = Gc::ArrayStoreExceptionType;
  static const size_t FixedSize = FixedSizeOfArrayStoreException;

  static GcArrayStoreException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ArrayStoreExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ArrayStoreExceptionMessage); }
  GcString* message() { return field_at<GcString*>(ArrayStoreExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ArrayStoreExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ArrayStoreExceptionTrace); }
  object trace() { return field_at<object>(ArrayStoreExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ArrayStoreExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ArrayStoreExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ArrayStoreExceptionCause); }
};

class GcBoolean: public GcObject {
 public:
  static const Gc::Type Type = Gc::BooleanType;
  static const size_t FixedSize = FixedSizeOfBoolean;

  static GcBoolean* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(BooleanValue) = value; }
  uint8_t* valuePtr() { return &field_at<uint8_t>(BooleanValue); }
  uint8_t& value() { return field_at<uint8_t>(BooleanValue); }
};

class GcBooleanArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::BooleanArrayType;
  static const size_t FixedSize = FixedSizeOfBooleanArray;

  static GcBooleanArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(BooleanArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(BooleanArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(BooleanArrayLength); }
  avian::util::Slice<uint8_t> body() { return avian::util::Slice<uint8_t> (&field_at<uint8_t>(BooleanArrayBody), field_at<uintptr_t>(BooleanArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uint8_t value) { field_at<uint8_t>(BooleanArrayBody + index * (1)) = value; }
};

class GcByte: public GcObject {
 public:
  static const Gc::Type Type = Gc::ByteType;
  static const size_t FixedSize = FixedSizeOfByte;

  static GcByte* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(ByteValue) = value; }
  uint8_t* valuePtr() { return &field_at<uint8_t>(ByteValue); }
  uint8_t& value() { return field_at<uint8_t>(ByteValue); }
};

class GcByteArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::ByteArrayType;
  static const size_t FixedSize = FixedSizeOfByteArray;

  static GcByteArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(ByteArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(ByteArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(ByteArrayLength); }
  avian::util::Slice<int8_t> body() { return avian::util::Slice<int8_t> (&field_at<int8_t>(ByteArrayBody), field_at<uintptr_t>(ByteArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, int8_t value) { field_at<int8_t>(ByteArrayBody + index * (1)) = value; }
};

class GcCallNode: public GcObject {
 public:
  static const Gc::Type Type = Gc::CallNodeType;
  static const size_t FixedSize = FixedSizeOfCallNode;

  static GcCallNode* makeZeroed(Thread* t);
  void setAddress(Thread* t UNUSED, intptr_t value) { field_at<intptr_t>(CallNodeAddress) = value; }
  intptr_t* addressPtr() { return &field_at<intptr_t>(CallNodeAddress); }
  intptr_t& address() { return field_at<intptr_t>(CallNodeAddress); }
  void setTarget(Thread* t UNUSED, GcMethod* value) { setField(t, this , CallNodeTarget, reinterpret_cast<object>(value)); }
  GcMethod** targetPtr() { return &field_at<GcMethod*>(CallNodeTarget); }
  GcMethod* target() { return field_at<GcMethod*>(CallNodeTarget); }
  void setFlags(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(CallNodeFlags) = value; }
  uintptr_t* flagsPtr() { return &field_at<uintptr_t>(CallNodeFlags); }
  uintptr_t& flags() { return field_at<uintptr_t>(CallNodeFlags); }
  void setNext(Thread* t UNUSED, GcCallNode* value) { setField(t, this , CallNodeNext, reinterpret_cast<object>(value)); }
  GcCallNode** nextPtr() { return &field_at<GcCallNode*>(CallNodeNext); }
  GcCallNode* next() { return field_at<GcCallNode*>(CallNodeNext); }
};

class GcChar: public GcObject {
 public:
  static const Gc::Type Type = Gc::CharType;
  static const size_t FixedSize = FixedSizeOfChar;

  static GcChar* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(CharValue) = value; }
  uint16_t* valuePtr() { return &field_at<uint16_t>(CharValue); }
  uint16_t& value() { return field_at<uint16_t>(CharValue); }
};

class GcCharArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::CharArrayType;
  static const size_t FixedSize = FixedSizeOfCharArray;

  static GcCharArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(CharArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(CharArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(CharArrayLength); }
  avian::util::Slice<uint16_t> body() { return avian::util::Slice<uint16_t> (&field_at<uint16_t>(CharArrayBody), field_at<uintptr_t>(CharArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uint16_t value) { field_at<uint16_t>(CharArrayBody + index * (2)) = value; }
};

class GcClass: public GcObject {
 public:
  static const Gc::Type Type = Gc::ClassType;
  static const size_t FixedSize = FixedSizeOfClass;

  static GcClass* makeZeroed(Thread* t, uintptr_t length);
  void setFlags(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(ClassFlags) = value; }
  uint16_t* flagsPtr() { return &field_at<uint16_t>(ClassFlags); }
  uint16_t& flags() { return field_at<uint16_t>(ClassFlags); }
  void setVmFlags(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(ClassVmFlags) = value; }
  uint16_t* vmFlagsPtr() { return &field_at<uint16_t>(ClassVmFlags); }
  uint16_t& vmFlags() { return field_at<uint16_t>(ClassVmFlags); }
  void setFixedSize(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(ClassFixedSize) = value; }
  uint16_t* fixedSizePtr() { return &field_at<uint16_t>(ClassFixedSize); }
  uint16_t& fixedSize() { return field_at<uint16_t>(ClassFixedSize); }
  void setArrayElementSize(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(ClassArrayElementSize) = value; }
  uint8_t* arrayElementSizePtr() { return &field_at<uint8_t>(ClassArrayElementSize); }
  uint8_t& arrayElementSize() { return field_at<uint8_t>(ClassArrayElementSize); }
  void setArrayDimensions(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(ClassArrayDimensions) = value; }
  uint8_t* arrayDimensionsPtr() { return &field_at<uint8_t>(ClassArrayDimensions); }
  uint8_t& arrayDimensions() { return field_at<uint8_t>(ClassArrayDimensions); }
  void setArrayElementClass(Thread* t UNUSED, GcClass* value) { setField(t, this , ClassArrayElementClass, reinterpret_cast<object>(value)); }
  GcClass** arrayElementClassPtr() { return &field_at<GcClass*>(ClassArrayElementClass); }
  GcClass* arrayElementClass() { return field_at<GcClass*>(ClassArrayElementClass); }
  void setRuntimeDataIndex(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(ClassRuntimeDataIndex) = value; }
  uint32_t* runtimeDataIndexPtr() { return &field_at<uint32_t>(ClassRuntimeDataIndex); }
  uint32_t& runtimeDataIndex() { return field_at<uint32_t>(ClassRuntimeDataIndex); }
  void setObjectMask(Thread* t UNUSED, GcIntArray* value) { setField(t, this , ClassObjectMask, reinterpret_cast<object>(value)); }
  GcIntArray** objectMaskPtr() { return &field_at<GcIntArray*>(ClassObjectMask); }
  GcIntArray* objectMask() { return field_at<GcIntArray*>(ClassObjectMask); }
  void setName(Thread* t UNUSED, GcByteArray* value) { setField(t, this , ClassName, reinterpret_cast<object>(value)); }
  GcByteArray** namePtr() { return &field_at<GcByteArray*>(ClassName); }
  GcByteArray* name() { return field_at<GcByteArray*>(ClassName); }
  void setSourceFile(Thread* t UNUSED, GcByteArray* value) { setField(t, this , ClassSourceFile, reinterpret_cast<object>(value)); }
  GcByteArray** sourceFilePtr() { return &field_at<GcByteArray*>(ClassSourceFile); }
  GcByteArray* sourceFile() { return field_at<GcByteArray*>(ClassSourceFile); }
  void setSuper(Thread* t UNUSED, GcClass* value) { setField(t, this , ClassSuper, reinterpret_cast<object>(value)); }
  GcClass** superPtr() { return &field_at<GcClass*>(ClassSuper); }
  GcClass* super() { return field_at<GcClass*>(ClassSuper); }
  void setInterfaceTable(Thread* t UNUSED, object value) { setField(t, this , ClassInterfaceTable, reinterpret_cast<object>(value)); }
  object* interfaceTablePtr() { return &field_at<object>(ClassInterfaceTable); }
  object interfaceTable() { return field_at<object>(ClassInterfaceTable); }
  void setVirtualTable(Thread* t UNUSED, object value) { setField(t, this , ClassVirtualTable, reinterpret_cast<object>(value)); }
  object* virtualTablePtr() { return &field_at<object>(ClassVirtualTable); }
  object virtualTable() { return field_at<object>(ClassVirtualTable); }
  void setFieldTable(Thread* t UNUSED, object value) { setField(t, this , ClassFieldTable, reinterpret_cast<object>(value)); }
  object* fieldTablePtr() { return &field_at<object>(ClassFieldTable); }
  object fieldTable() { return field_at<object>(ClassFieldTable); }
  void setMethodTable(Thread* t UNUSED, object value) { setField(t, this , ClassMethodTable, reinterpret_cast<object>(value)); }
  object* methodTablePtr() { return &field_at<object>(ClassMethodTable); }
  object methodTable() { return field_at<object>(ClassMethodTable); }
  void setAddendum(Thread* t UNUSED, GcClassAddendum* value) { setField(t, this , ClassAddendum, reinterpret_cast<object>(value)); }
  GcClassAddendum** addendumPtr() { return &field_at<GcClassAddendum*>(ClassAddendum); }
  GcClassAddendum* addendum() { return field_at<GcClassAddendum*>(ClassAddendum); }
  void setStaticTable(Thread* t UNUSED, GcSingleton* value) { setField(t, this , ClassStaticTable, reinterpret_cast<object>(value)); }
  GcSingleton** staticTablePtr() { return &field_at<GcSingleton*>(ClassStaticTable); }
  GcSingleton* staticTable() { return field_at<GcSingleton*>(ClassStaticTable); }
  void setLoader(Thread* t UNUSED, GcClassLoader* value) { setField(t, this , ClassLoader, reinterpret_cast<object>(value)); }
  GcClassLoader** loaderPtr() { return &field_at<GcClassLoader*>(ClassLoader); }
  GcClassLoader* loader() { return field_at<GcClassLoader*>(ClassLoader); }
  void setSource(Thread* t UNUSED, GcByteArray* value) { setField(t, this , ClassSource, reinterpret_cast<object>(value)); }
  GcByteArray** sourcePtr() { return &field_at<GcByteArray*>(ClassSource); }
  GcByteArray* source() { return field_at<GcByteArray*>(ClassSource); }
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(ClassLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(ClassLength); }
  uintptr_t& length() { return field_at<uintptr_t>(ClassLength); }
  avian::util::Slice<void*> vtable() { return avian::util::Slice<void*> (&field_at<void*>(ClassVtable), field_at<uintptr_t>(ClassLength)); }
  void setVtableElement(Thread* t UNUSED, size_t index, void* value) { field_at<void*>(ClassVtable + index * (8)) = value; }
};

class GcClassAddendum: public GcObject {
 public:
  static const Gc::Type Type = Gc::ClassAddendumType;
  static const size_t FixedSize = FixedSizeOfClassAddendum;

  static GcClassAddendum* makeZeroed(Thread* t);
  void setPool(Thread* t UNUSED, GcSingleton* value) { setField(t, this , ClassAddendumPool, reinterpret_cast<object>(value)); }
  GcSingleton** poolPtr() { return &field_at<GcSingleton*>(ClassAddendumPool); }
  GcSingleton* pool() { return field_at<GcSingleton*>(ClassAddendumPool); }
  void setAnnotationTable(Thread* t UNUSED, object value) { setField(t, this , ClassAddendumAnnotationTable, reinterpret_cast<object>(value)); }
  object* annotationTablePtr() { return &field_at<object>(ClassAddendumAnnotationTable); }
  object annotationTable() { return field_at<object>(ClassAddendumAnnotationTable); }
  void setSignature(Thread* t UNUSED, object value) { setField(t, this , ClassAddendumSignature, reinterpret_cast<object>(value)); }
  object* signaturePtr() { return &field_at<object>(ClassAddendumSignature); }
  object signature() { return field_at<object>(ClassAddendumSignature); }
  void setInterfaceTable(Thread* t UNUSED, object value) { setField(t, this , ClassAddendumInterfaceTable, reinterpret_cast<object>(value)); }
  object* interfaceTablePtr() { return &field_at<object>(ClassAddendumInterfaceTable); }
  object interfaceTable() { return field_at<object>(ClassAddendumInterfaceTable); }
  void setInnerClassTable(Thread* t UNUSED, object value) { setField(t, this , ClassAddendumInnerClassTable, reinterpret_cast<object>(value)); }
  object* innerClassTablePtr() { return &field_at<object>(ClassAddendumInnerClassTable); }
  object innerClassTable() { return field_at<object>(ClassAddendumInnerClassTable); }
  void setDeclaredMethodCount(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(ClassAddendumDeclaredMethodCount) = value; }
  uint32_t* declaredMethodCountPtr() { return &field_at<uint32_t>(ClassAddendumDeclaredMethodCount); }
  uint32_t& declaredMethodCount() { return field_at<uint32_t>(ClassAddendumDeclaredMethodCount); }
  void setEnclosingClass(Thread* t UNUSED, GcByteArray* value) { setField(t, this , ClassAddendumEnclosingClass, reinterpret_cast<object>(value)); }
  GcByteArray** enclosingClassPtr() { return &field_at<GcByteArray*>(ClassAddendumEnclosingClass); }
  GcByteArray* enclosingClass() { return field_at<GcByteArray*>(ClassAddendumEnclosingClass); }
  void setEnclosingMethod(Thread* t UNUSED, GcPair* value) { setField(t, this , ClassAddendumEnclosingMethod, reinterpret_cast<object>(value)); }
  GcPair** enclosingMethodPtr() { return &field_at<GcPair*>(ClassAddendumEnclosingMethod); }
  GcPair* enclosingMethod() { return field_at<GcPair*>(ClassAddendumEnclosingMethod); }
};

class GcClassCastException: public GcObject {
 public:
  static const Gc::Type Type = Gc::ClassCastExceptionType;
  static const size_t FixedSize = FixedSizeOfClassCastException;

  static GcClassCastException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ClassCastExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ClassCastExceptionMessage); }
  GcString* message() { return field_at<GcString*>(ClassCastExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ClassCastExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ClassCastExceptionTrace); }
  object trace() { return field_at<object>(ClassCastExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ClassCastExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ClassCastExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ClassCastExceptionCause); }
};

class GcClassLoader: public GcObject {
 public:
  static const Gc::Type Type = Gc::ClassLoaderType;
  static const size_t FixedSize = FixedSizeOfClassLoader;

  static GcClassLoader* makeZeroed(Thread* t);
  void setParent(Thread* t UNUSED, GcClassLoader* value) { setField(t, this , ClassLoaderParent, reinterpret_cast<object>(value)); }
  GcClassLoader** parentPtr() { return &field_at<GcClassLoader*>(ClassLoaderParent); }
  GcClassLoader* parent() { return field_at<GcClassLoader*>(ClassLoaderParent); }
  void setPackages(Thread* t UNUSED, object value) { setField(t, this , ClassLoaderPackages, reinterpret_cast<object>(value)); }
  object* packagesPtr() { return &field_at<object>(ClassLoaderPackages); }
  object packages() { return field_at<object>(ClassLoaderPackages); }
  void setMap(Thread* t UNUSED, object value) { setField(t, this , ClassLoaderMap, reinterpret_cast<object>(value)); }
  object* mapPtr() { return &field_at<object>(ClassLoaderMap); }
  object map() { return field_at<object>(ClassLoaderMap); }
};

class GcClassNotFoundException: public GcObject {
 public:
  static const Gc::Type Type = Gc::ClassNotFoundExceptionType;
  static const size_t FixedSize = FixedSizeOfClassNotFoundException;

  static GcClassNotFoundException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ClassNotFoundExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ClassNotFoundExceptionMessage); }
  GcString* message() { return field_at<GcString*>(ClassNotFoundExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ClassNotFoundExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ClassNotFoundExceptionTrace); }
  object trace() { return field_at<object>(ClassNotFoundExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ClassNotFoundExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ClassNotFoundExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ClassNotFoundExceptionCause); }
  void setCause2(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ClassNotFoundExceptionCause2, reinterpret_cast<object>(value)); }
  GcThrowable** cause2Ptr() { return &field_at<GcThrowable*>(ClassNotFoundExceptionCause2); }
  GcThrowable* cause2() { return field_at<GcThrowable*>(ClassNotFoundExceptionCause2); }
};

class GcClassRuntimeData: public GcObject {
 public:
  static const Gc::Type Type = Gc::ClassRuntimeDataType;
  static const size_t FixedSize = FixedSizeOfClassRuntimeData;

  static GcClassRuntimeData* makeZeroed(Thread* t);
  void setArrayClass(Thread* t UNUSED, object value) { setField(t, this , ClassRuntimeDataArrayClass, reinterpret_cast<object>(value)); }
  object* arrayClassPtr() { return &field_at<object>(ClassRuntimeDataArrayClass); }
  object arrayClass() { return field_at<object>(ClassRuntimeDataArrayClass); }
  void setJclass(Thread* t UNUSED, object value) { setField(t, this , ClassRuntimeDataJclass, reinterpret_cast<object>(value)); }
  object* jclassPtr() { return &field_at<object>(ClassRuntimeDataJclass); }
  object jclass() { return field_at<object>(ClassRuntimeDataJclass); }
  void setPool(Thread* t UNUSED, object value) { setField(t, this , ClassRuntimeDataPool, reinterpret_cast<object>(value)); }
  object* poolPtr() { return &field_at<object>(ClassRuntimeDataPool); }
  object pool() { return field_at<object>(ClassRuntimeDataPool); }
  void setSigners(Thread* t UNUSED, object value) { setField(t, this , ClassRuntimeDataSigners, reinterpret_cast<object>(value)); }
  object* signersPtr() { return &field_at<object>(ClassRuntimeDataSigners); }
  object signers() { return field_at<object>(ClassRuntimeDataSigners); }
};

class GcCleaner: public GcObject {
 public:
  static const Gc::Type Type = Gc::CleanerType;
  static const size_t FixedSize = FixedSizeOfCleaner;

  static GcCleaner* makeZeroed(Thread* t);
  void setQueueNext(Thread* t UNUSED, GcCleaner* value) { setField(t, this , CleanerQueueNext, reinterpret_cast<object>(value)); }
  GcCleaner** queueNextPtr() { return &field_at<GcCleaner*>(CleanerQueueNext); }
  GcCleaner* queueNext() { return field_at<GcCleaner*>(CleanerQueueNext); }
};

class GcCloneNotSupportedException: public GcObject {
 public:
  static const Gc::Type Type = Gc::CloneNotSupportedExceptionType;
  static const size_t FixedSize = FixedSizeOfCloneNotSupportedException;

  static GcCloneNotSupportedException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , CloneNotSupportedExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(CloneNotSupportedExceptionMessage); }
  GcString* message() { return field_at<GcString*>(CloneNotSupportedExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , CloneNotSupportedExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(CloneNotSupportedExceptionTrace); }
  object trace() { return field_at<object>(CloneNotSupportedExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , CloneNotSupportedExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(CloneNotSupportedExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(CloneNotSupportedExceptionCause); }
};

class GcCloneable: public GcObject {
 public:
  static const Gc::Type Type = Gc::CloneableType;
  static const size_t FixedSize = FixedSizeOfCloneable;

  static GcCloneable* makeZeroed(Thread* t);
};

class GcCode: public GcObject {
 public:
  static const Gc::Type Type = Gc::CodeType;
  static const size_t FixedSize = FixedSizeOfCode;

  static GcCode* makeZeroed(Thread* t, uintptr_t length);
  void setPool(Thread* t UNUSED, GcSingleton* value) { setField(t, this , CodePool, reinterpret_cast<object>(value)); }
  GcSingleton** poolPtr() { return &field_at<GcSingleton*>(CodePool); }
  GcSingleton* pool() { return field_at<GcSingleton*>(CodePool); }
  void setStackMap(Thread* t UNUSED, GcIntArray* value) { setField(t, this , CodeStackMap, reinterpret_cast<object>(value)); }
  GcIntArray** stackMapPtr() { return &field_at<GcIntArray*>(CodeStackMap); }
  GcIntArray* stackMap() { return field_at<GcIntArray*>(CodeStackMap); }
  void setExceptionHandlerTable(Thread* t UNUSED, object value) { setField(t, this , CodeExceptionHandlerTable, reinterpret_cast<object>(value)); }
  object* exceptionHandlerTablePtr() { return &field_at<object>(CodeExceptionHandlerTable); }
  object exceptionHandlerTable() { return field_at<object>(CodeExceptionHandlerTable); }
  void setLineNumberTable(Thread* t UNUSED, GcLineNumberTable* value) { setField(t, this , CodeLineNumberTable, reinterpret_cast<object>(value)); }
  GcLineNumberTable** lineNumberTablePtr() { return &field_at<GcLineNumberTable*>(CodeLineNumberTable); }
  GcLineNumberTable* lineNumberTable() { return field_at<GcLineNumberTable*>(CodeLineNumberTable); }
  void setCompiled(Thread* t UNUSED, intptr_t value) { field_at<intptr_t>(CodeCompiled) = value; }
  intptr_t* compiledPtr() { return &field_at<intptr_t>(CodeCompiled); }
  intptr_t& compiled() { return field_at<intptr_t>(CodeCompiled); }
  void setCompiledSize(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(CodeCompiledSize) = value; }
  uint32_t* compiledSizePtr() { return &field_at<uint32_t>(CodeCompiledSize); }
  uint32_t& compiledSize() { return field_at<uint32_t>(CodeCompiledSize); }
  void setMaxStack(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(CodeMaxStack) = value; }
  uint16_t* maxStackPtr() { return &field_at<uint16_t>(CodeMaxStack); }
  uint16_t& maxStack() { return field_at<uint16_t>(CodeMaxStack); }
  void setMaxLocals(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(CodeMaxLocals) = value; }
  uint16_t* maxLocalsPtr() { return &field_at<uint16_t>(CodeMaxLocals); }
  uint16_t& maxLocals() { return field_at<uint16_t>(CodeMaxLocals); }
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(CodeLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(CodeLength); }
  uintptr_t& length() { return field_at<uintptr_t>(CodeLength); }
  avian::util::Slice<uint8_t> body() { return avian::util::Slice<uint8_t> (&field_at<uint8_t>(CodeBody), field_at<uintptr_t>(CodeLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uint8_t value) { field_at<uint8_t>(CodeBody + index * (1)) = value; }
};

class GcCompileRoots: public GcObject {
 public:
  static const Gc::Type Type = Gc::CompileRootsType;
  static const size_t FixedSize = FixedSizeOfCompileRoots;

  static GcCompileRoots* makeZeroed(Thread* t);
  void setCallTable(Thread* t UNUSED, GcArray* value) { setField(t, this , CompileRootsCallTable, reinterpret_cast<object>(value)); }
  GcArray** callTablePtr() { return &field_at<GcArray*>(CompileRootsCallTable); }
  GcArray* callTable() { return field_at<GcArray*>(CompileRootsCallTable); }
  void setMethodTree(Thread* t UNUSED, GcTreeNode* value) { setField(t, this , CompileRootsMethodTree, reinterpret_cast<object>(value)); }
  GcTreeNode** methodTreePtr() { return &field_at<GcTreeNode*>(CompileRootsMethodTree); }
  GcTreeNode* methodTree() { return field_at<GcTreeNode*>(CompileRootsMethodTree); }
  void setMethodTreeSentinal(Thread* t UNUSED, GcTreeNode* value) { setField(t, this , CompileRootsMethodTreeSentinal, reinterpret_cast<object>(value)); }
  GcTreeNode** methodTreeSentinalPtr() { return &field_at<GcTreeNode*>(CompileRootsMethodTreeSentinal); }
  GcTreeNode* methodTreeSentinal() { return field_at<GcTreeNode*>(CompileRootsMethodTreeSentinal); }
  void setObjectPools(Thread* t UNUSED, object value) { setField(t, this , CompileRootsObjectPools, reinterpret_cast<object>(value)); }
  object* objectPoolsPtr() { return &field_at<object>(CompileRootsObjectPools); }
  object objectPools() { return field_at<object>(CompileRootsObjectPools); }
  void setStaticTableArray(Thread* t UNUSED, object value) { setField(t, this , CompileRootsStaticTableArray, reinterpret_cast<object>(value)); }
  object* staticTableArrayPtr() { return &field_at<object>(CompileRootsStaticTableArray); }
  object staticTableArray() { return field_at<object>(CompileRootsStaticTableArray); }
  void setVirtualThunks(Thread* t UNUSED, GcWordArray* value) { setField(t, this , CompileRootsVirtualThunks, reinterpret_cast<object>(value)); }
  GcWordArray** virtualThunksPtr() { return &field_at<GcWordArray*>(CompileRootsVirtualThunks); }
  GcWordArray* virtualThunks() { return field_at<GcWordArray*>(CompileRootsVirtualThunks); }
  void setReceiveMethod(Thread* t UNUSED, GcMethod* value) { setField(t, this , CompileRootsReceiveMethod, reinterpret_cast<object>(value)); }
  GcMethod** receiveMethodPtr() { return &field_at<GcMethod*>(CompileRootsReceiveMethod); }
  GcMethod* receiveMethod() { return field_at<GcMethod*>(CompileRootsReceiveMethod); }
  void setWindMethod(Thread* t UNUSED, GcMethod* value) { setField(t, this , CompileRootsWindMethod, reinterpret_cast<object>(value)); }
  GcMethod** windMethodPtr() { return &field_at<GcMethod*>(CompileRootsWindMethod); }
  GcMethod* windMethod() { return field_at<GcMethod*>(CompileRootsWindMethod); }
  void setRewindMethod(Thread* t UNUSED, GcMethod* value) { setField(t, this , CompileRootsRewindMethod, reinterpret_cast<object>(value)); }
  GcMethod** rewindMethodPtr() { return &field_at<GcMethod*>(CompileRootsRewindMethod); }
  GcMethod* rewindMethod() { return field_at<GcMethod*>(CompileRootsRewindMethod); }
};

class GcConstantPool: public GcObject {
 public:
  static const Gc::Type Type = Gc::ConstantPoolType;
  static const size_t FixedSize = FixedSizeOfConstantPool;

  static GcConstantPool* makeZeroed(Thread* t);
};

class GcContinuation: public GcObject {
 public:
  static const Gc::Type Type = Gc::ContinuationType;
  static const size_t FixedSize = FixedSizeOfContinuation;

  static GcContinuation* makeZeroed(Thread* t, uintptr_t length);
  void setNext(Thread* t UNUSED, GcContinuation* value) { setField(t, this , ContinuationNext, reinterpret_cast<object>(value)); }
  GcContinuation** nextPtr() { return &field_at<GcContinuation*>(ContinuationNext); }
  GcContinuation* next() { return field_at<GcContinuation*>(ContinuationNext); }
  void setContext(Thread* t UNUSED, GcContinuationContext* value) { setField(t, this , ContinuationContext, reinterpret_cast<object>(value)); }
  GcContinuationContext** contextPtr() { return &field_at<GcContinuationContext*>(ContinuationContext); }
  GcContinuationContext* context() { return field_at<GcContinuationContext*>(ContinuationContext); }
  void setMethod(Thread* t UNUSED, GcMethod* value) { setField(t, this , ContinuationMethod, reinterpret_cast<object>(value)); }
  GcMethod** methodPtr() { return &field_at<GcMethod*>(ContinuationMethod); }
  GcMethod* method() { return field_at<GcMethod*>(ContinuationMethod); }
  void setAddress(Thread* t UNUSED, void* value) { field_at<void*>(ContinuationAddress) = value; }
  void** addressPtr() { return &field_at<void*>(ContinuationAddress); }
  void*& address() { return field_at<void*>(ContinuationAddress); }
  void setReturnAddressOffset(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(ContinuationReturnAddressOffset) = value; }
  uintptr_t* returnAddressOffsetPtr() { return &field_at<uintptr_t>(ContinuationReturnAddressOffset); }
  uintptr_t& returnAddressOffset() { return field_at<uintptr_t>(ContinuationReturnAddressOffset); }
  void setFramePointerOffset(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(ContinuationFramePointerOffset) = value; }
  uintptr_t* framePointerOffsetPtr() { return &field_at<uintptr_t>(ContinuationFramePointerOffset); }
  uintptr_t& framePointerOffset() { return field_at<uintptr_t>(ContinuationFramePointerOffset); }
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(ContinuationLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(ContinuationLength); }
  uintptr_t& length() { return field_at<uintptr_t>(ContinuationLength); }
  avian::util::Slice<uintptr_t> body() { return avian::util::Slice<uintptr_t> (&field_at<uintptr_t>(ContinuationBody), field_at<uintptr_t>(ContinuationLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uintptr_t value) { field_at<uintptr_t>(ContinuationBody + index * (8)) = value; }
};

class GcContinuationContext: public GcObject {
 public:
  static const Gc::Type Type = Gc::ContinuationContextType;
  static const size_t FixedSize = FixedSizeOfContinuationContext;

  static GcContinuationContext* makeZeroed(Thread* t);
  void setNext(Thread* t UNUSED, GcContinuationContext* value) { setField(t, this , ContinuationContextNext, reinterpret_cast<object>(value)); }
  GcContinuationContext** nextPtr() { return &field_at<GcContinuationContext*>(ContinuationContextNext); }
  GcContinuationContext* next() { return field_at<GcContinuationContext*>(ContinuationContextNext); }
  void setBefore(Thread* t UNUSED, object value) { setField(t, this , ContinuationContextBefore, reinterpret_cast<object>(value)); }
  object* beforePtr() { return &field_at<object>(ContinuationContextBefore); }
  object before() { return field_at<object>(ContinuationContextBefore); }
  void setAfter(Thread* t UNUSED, object value) { setField(t, this , ContinuationContextAfter, reinterpret_cast<object>(value)); }
  object* afterPtr() { return &field_at<object>(ContinuationContextAfter); }
  object after() { return field_at<object>(ContinuationContextAfter); }
  void setContinuation(Thread* t UNUSED, object value) { setField(t, this , ContinuationContextContinuation, reinterpret_cast<object>(value)); }
  object* continuationPtr() { return &field_at<object>(ContinuationContextContinuation); }
  object continuation() { return field_at<object>(ContinuationContextContinuation); }
  void setMethod(Thread* t UNUSED, GcMethod* value) { setField(t, this , ContinuationContextMethod, reinterpret_cast<object>(value)); }
  GcMethod** methodPtr() { return &field_at<GcMethod*>(ContinuationContextMethod); }
  GcMethod* method() { return field_at<GcMethod*>(ContinuationContextMethod); }
};

class GcDouble: public GcObject {
 public:
  static const Gc::Type Type = Gc::DoubleType;
  static const size_t FixedSize = FixedSizeOfDouble;

  static GcDouble* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, uint64_t value) { field_at<uint64_t>(DoubleValue) = value; }
  uint64_t* valuePtr() { return &field_at<uint64_t>(DoubleValue); }
  uint64_t& value() { return field_at<uint64_t>(DoubleValue); }
};

class GcDoubleArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::DoubleArrayType;
  static const size_t FixedSize = FixedSizeOfDoubleArray;

  static GcDoubleArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(DoubleArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(DoubleArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(DoubleArrayLength); }
  avian::util::Slice<uint64_t> body() { return avian::util::Slice<uint64_t> (&field_at<uint64_t>(DoubleArrayBody), field_at<uintptr_t>(DoubleArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uint64_t value) { field_at<uint64_t>(DoubleArrayBody + index * (8)) = value; }
};

class GcError: public GcObject {
 public:
  static const Gc::Type Type = Gc::ErrorType;
  static const size_t FixedSize = FixedSizeOfError;

  static GcError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ErrorMessage); }
  GcString* message() { return field_at<GcString*>(ErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ErrorTrace); }
  object trace() { return field_at<object>(ErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ErrorCause); }
};

class GcException: public GcObject {
 public:
  static const Gc::Type Type = Gc::ExceptionType;
  static const size_t FixedSize = FixedSizeOfException;

  static GcException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ExceptionMessage); }
  GcString* message() { return field_at<GcString*>(ExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ExceptionTrace); }
  object trace() { return field_at<object>(ExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ExceptionCause); }
};

class GcExceptionHandlerTable: public GcObject {
 public:
  static const Gc::Type Type = Gc::ExceptionHandlerTableType;
  static const size_t FixedSize = FixedSizeOfExceptionHandlerTable;

  static GcExceptionHandlerTable* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(ExceptionHandlerTableLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(ExceptionHandlerTableLength); }
  uintptr_t& length() { return field_at<uintptr_t>(ExceptionHandlerTableLength); }
  avian::util::Slice<uint64_t> body() { return avian::util::Slice<uint64_t> (&field_at<uint64_t>(ExceptionHandlerTableBody), field_at<uintptr_t>(ExceptionHandlerTableLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uint64_t value) { field_at<uint64_t>(ExceptionHandlerTableBody + index * (8)) = value; }
};

class GcExceptionInInitializerError: public GcObject {
 public:
  static const Gc::Type Type = Gc::ExceptionInInitializerErrorType;
  static const size_t FixedSize = FixedSizeOfExceptionInInitializerError;

  static GcExceptionInInitializerError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ExceptionInInitializerErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ExceptionInInitializerErrorMessage); }
  GcString* message() { return field_at<GcString*>(ExceptionInInitializerErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ExceptionInInitializerErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ExceptionInInitializerErrorTrace); }
  object trace() { return field_at<object>(ExceptionInInitializerErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ExceptionInInitializerErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ExceptionInInitializerErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ExceptionInInitializerErrorCause); }
  void setException(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ExceptionInInitializerErrorException, reinterpret_cast<object>(value)); }
  GcThrowable** exceptionPtr() { return &field_at<GcThrowable*>(ExceptionInInitializerErrorException); }
  GcThrowable* exception() { return field_at<GcThrowable*>(ExceptionInInitializerErrorException); }
};

class GcField: public GcObject {
 public:
  static const Gc::Type Type = Gc::FieldType;
  static const size_t FixedSize = FixedSizeOfField;

  static GcField* makeZeroed(Thread* t);
  void setVmFlags(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(FieldVmFlags) = value; }
  uint8_t* vmFlagsPtr() { return &field_at<uint8_t>(FieldVmFlags); }
  uint8_t& vmFlags() { return field_at<uint8_t>(FieldVmFlags); }
  void setCode(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(FieldCode) = value; }
  uint8_t* codePtr() { return &field_at<uint8_t>(FieldCode); }
  uint8_t& code() { return field_at<uint8_t>(FieldCode); }
  void setFlags(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(FieldFlags) = value; }
  uint16_t* flagsPtr() { return &field_at<uint16_t>(FieldFlags); }
  uint16_t& flags() { return field_at<uint16_t>(FieldFlags); }
  void setOffset(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(FieldOffset) = value; }
  uint16_t* offsetPtr() { return &field_at<uint16_t>(FieldOffset); }
  uint16_t& offset() { return field_at<uint16_t>(FieldOffset); }
  void setNativeID(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(FieldNativeID) = value; }
  uint32_t* nativeIDPtr() { return &field_at<uint32_t>(FieldNativeID); }
  uint32_t& nativeID() { return field_at<uint32_t>(FieldNativeID); }
  void setName(Thread* t UNUSED, GcByteArray* value) { setField(t, this , FieldName, reinterpret_cast<object>(value)); }
  GcByteArray** namePtr() { return &field_at<GcByteArray*>(FieldName); }
  GcByteArray* name() { return field_at<GcByteArray*>(FieldName); }
  void setSpec(Thread* t UNUSED, GcByteArray* value) { setField(t, this , FieldSpec, reinterpret_cast<object>(value)); }
  GcByteArray** specPtr() { return &field_at<GcByteArray*>(FieldSpec); }
  GcByteArray* spec() { return field_at<GcByteArray*>(FieldSpec); }
  void setAddendum(Thread* t UNUSED, GcFieldAddendum* value) { setField(t, this , FieldAddendum, reinterpret_cast<object>(value)); }
  GcFieldAddendum** addendumPtr() { return &field_at<GcFieldAddendum*>(FieldAddendum); }
  GcFieldAddendum* addendum() { return field_at<GcFieldAddendum*>(FieldAddendum); }
  void setClass(Thread* t UNUSED, GcClass* value) { setField(t, this , FieldClass, reinterpret_cast<object>(value)); }
  GcClass** class_Ptr() { return &field_at<GcClass*>(FieldClass); }
  GcClass* class_() { return field_at<GcClass*>(FieldClass); }
};

class GcFieldAddendum: public GcObject {
 public:
  static const Gc::Type Type = Gc::FieldAddendumType;
  static const size_t FixedSize = FixedSizeOfFieldAddendum;

  static GcFieldAddendum* makeZeroed(Thread* t);
  void setPool(Thread* t UNUSED, GcSingleton* value) { setField(t, this , FieldAddendumPool, reinterpret_cast<object>(value)); }
  GcSingleton** poolPtr() { return &field_at<GcSingleton*>(FieldAddendumPool); }
  GcSingleton* pool() { return field_at<GcSingleton*>(FieldAddendumPool); }
  void setAnnotationTable(Thread* t UNUSED, object value) { setField(t, this , FieldAddendumAnnotationTable, reinterpret_cast<object>(value)); }
  object* annotationTablePtr() { return &field_at<object>(FieldAddendumAnnotationTable); }
  object annotationTable() { return field_at<object>(FieldAddendumAnnotationTable); }
  void setSignature(Thread* t UNUSED, object value) { setField(t, this , FieldAddendumSignature, reinterpret_cast<object>(value)); }
  object* signaturePtr() { return &field_at<object>(FieldAddendumSignature); }
  object signature() { return field_at<object>(FieldAddendumSignature); }
};

class GcFileNotFoundException: public GcObject {
 public:
  static const Gc::Type Type = Gc::FileNotFoundExceptionType;
  static const size_t FixedSize = FixedSizeOfFileNotFoundException;

  static GcFileNotFoundException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , FileNotFoundExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(FileNotFoundExceptionMessage); }
  GcString* message() { return field_at<GcString*>(FileNotFoundExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , FileNotFoundExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(FileNotFoundExceptionTrace); }
  object trace() { return field_at<object>(FileNotFoundExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , FileNotFoundExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(FileNotFoundExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(FileNotFoundExceptionCause); }
};

class GcFinalizer: public GcObject {
 public:
  static const Gc::Type Type = Gc::FinalizerType;
  static const size_t FixedSize = FixedSizeOfFinalizer;

  static GcFinalizer* makeZeroed(Thread* t);
  void setTarget(Thread* t UNUSED, object value) { field_at<object>(FinalizerTarget) = value; }
  object* targetPtr() { return &field_at<object>(FinalizerTarget); }
  object& target() { return field_at<object>(FinalizerTarget); }
  void setFinalize(Thread* t UNUSED, void* value) { field_at<void*>(FinalizerFinalize) = value; }
  void** finalizePtr() { return &field_at<void*>(FinalizerFinalize); }
  void*& finalize() { return field_at<void*>(FinalizerFinalize); }
  void setNext(Thread* t UNUSED, object value) { field_at<object>(FinalizerNext) = value; }
  object* nextPtr() { return &field_at<object>(FinalizerNext); }
  object& next() { return field_at<object>(FinalizerNext); }
  void setQueueTarget(Thread* t UNUSED, object value) { setField(t, this , FinalizerQueueTarget, reinterpret_cast<object>(value)); }
  object* queueTargetPtr() { return &field_at<object>(FinalizerQueueTarget); }
  object queueTarget() { return field_at<object>(FinalizerQueueTarget); }
  void setQueueNext(Thread* t UNUSED, GcFinalizer* value) { setField(t, this , FinalizerQueueNext, reinterpret_cast<object>(value)); }
  GcFinalizer** queueNextPtr() { return &field_at<GcFinalizer*>(FinalizerQueueNext); }
  GcFinalizer* queueNext() { return field_at<GcFinalizer*>(FinalizerQueueNext); }
};

class GcFinder: public GcObject {
 public:
  static const Gc::Type Type = Gc::FinderType;
  static const size_t FixedSize = FixedSizeOfFinder;

  static GcFinder* makeZeroed(Thread* t);
  void setFinder(Thread* t UNUSED, void* value) { field_at<void*>(FinderFinder) = value; }
  void** finderPtr() { return &field_at<void*>(FinderFinder); }
  void*& finder() { return field_at<void*>(FinderFinder); }
  void setName(Thread* t UNUSED, GcByteArray* value) { setField(t, this , FinderName, reinterpret_cast<object>(value)); }
  GcByteArray** namePtr() { return &field_at<GcByteArray*>(FinderName); }
  GcByteArray* name() { return field_at<GcByteArray*>(FinderName); }
  void setNext(Thread* t UNUSED, GcFinder* value) { setField(t, this , FinderNext, reinterpret_cast<object>(value)); }
  GcFinder** nextPtr() { return &field_at<GcFinder*>(FinderNext); }
  GcFinder* next() { return field_at<GcFinder*>(FinderNext); }
};

class GcFloat: public GcObject {
 public:
  static const Gc::Type Type = Gc::FloatType;
  static const size_t FixedSize = FixedSizeOfFloat;

  static GcFloat* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(FloatValue) = value; }
  uint32_t* valuePtr() { return &field_at<uint32_t>(FloatValue); }
  uint32_t& value() { return field_at<uint32_t>(FloatValue); }
};

class GcFloatArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::FloatArrayType;
  static const size_t FixedSize = FixedSizeOfFloatArray;

  static GcFloatArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(FloatArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(FloatArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(FloatArrayLength); }
  avian::util::Slice<uint32_t> body() { return avian::util::Slice<uint32_t> (&field_at<uint32_t>(FloatArrayBody), field_at<uintptr_t>(FloatArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uint32_t value) { field_at<uint32_t>(FloatArrayBody + index * (4)) = value; }
};

class GcHashMap: public GcObject {
 public:
  static const Gc::Type Type = Gc::HashMapType;
  static const size_t FixedSize = FixedSizeOfHashMap;

  static GcHashMap* makeZeroed(Thread* t);
  void setSize(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(HashMapSize) = value; }
  uint32_t* sizePtr() { return &field_at<uint32_t>(HashMapSize); }
  uint32_t& size() { return field_at<uint32_t>(HashMapSize); }
  void setArray(Thread* t UNUSED, GcArray* value) { setField(t, this , HashMapArray, reinterpret_cast<object>(value)); }
  GcArray** arrayPtr() { return &field_at<GcArray*>(HashMapArray); }
  GcArray* array() { return field_at<GcArray*>(HashMapArray); }
};

class GcIllegalArgumentException: public GcObject {
 public:
  static const Gc::Type Type = Gc::IllegalArgumentExceptionType;
  static const size_t FixedSize = FixedSizeOfIllegalArgumentException;

  static GcIllegalArgumentException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , IllegalArgumentExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(IllegalArgumentExceptionMessage); }
  GcString* message() { return field_at<GcString*>(IllegalArgumentExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , IllegalArgumentExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(IllegalArgumentExceptionTrace); }
  object trace() { return field_at<object>(IllegalArgumentExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , IllegalArgumentExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(IllegalArgumentExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(IllegalArgumentExceptionCause); }
};

class GcIllegalMonitorStateException: public GcObject {
 public:
  static const Gc::Type Type = Gc::IllegalMonitorStateExceptionType;
  static const size_t FixedSize = FixedSizeOfIllegalMonitorStateException;

  static GcIllegalMonitorStateException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , IllegalMonitorStateExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(IllegalMonitorStateExceptionMessage); }
  GcString* message() { return field_at<GcString*>(IllegalMonitorStateExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , IllegalMonitorStateExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(IllegalMonitorStateExceptionTrace); }
  object trace() { return field_at<object>(IllegalMonitorStateExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , IllegalMonitorStateExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(IllegalMonitorStateExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(IllegalMonitorStateExceptionCause); }
};

class GcIllegalStateException: public GcObject {
 public:
  static const Gc::Type Type = Gc::IllegalStateExceptionType;
  static const size_t FixedSize = FixedSizeOfIllegalStateException;

  static GcIllegalStateException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , IllegalStateExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(IllegalStateExceptionMessage); }
  GcString* message() { return field_at<GcString*>(IllegalStateExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , IllegalStateExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(IllegalStateExceptionTrace); }
  object trace() { return field_at<object>(IllegalStateExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , IllegalStateExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(IllegalStateExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(IllegalStateExceptionCause); }
};

class GcIncompatibleClassChangeError: public GcObject {
 public:
  static const Gc::Type Type = Gc::IncompatibleClassChangeErrorType;
  static const size_t FixedSize = FixedSizeOfIncompatibleClassChangeError;

  static GcIncompatibleClassChangeError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , IncompatibleClassChangeErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(IncompatibleClassChangeErrorMessage); }
  GcString* message() { return field_at<GcString*>(IncompatibleClassChangeErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , IncompatibleClassChangeErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(IncompatibleClassChangeErrorTrace); }
  object trace() { return field_at<object>(IncompatibleClassChangeErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , IncompatibleClassChangeErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(IncompatibleClassChangeErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(IncompatibleClassChangeErrorCause); }
};

class GcIncompatibleContinuationException: public GcObject {
 public:
  static const Gc::Type Type = Gc::IncompatibleContinuationExceptionType;
  static const size_t FixedSize = FixedSizeOfIncompatibleContinuationException;

  static GcIncompatibleContinuationException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , IncompatibleContinuationExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(IncompatibleContinuationExceptionMessage); }
  GcString* message() { return field_at<GcString*>(IncompatibleContinuationExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , IncompatibleContinuationExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(IncompatibleContinuationExceptionTrace); }
  object trace() { return field_at<object>(IncompatibleContinuationExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , IncompatibleContinuationExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(IncompatibleContinuationExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(IncompatibleContinuationExceptionCause); }
};

class GcIndexOutOfBoundsException: public GcObject {
 public:
  static const Gc::Type Type = Gc::IndexOutOfBoundsExceptionType;
  static const size_t FixedSize = FixedSizeOfIndexOutOfBoundsException;

  static GcIndexOutOfBoundsException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , IndexOutOfBoundsExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(IndexOutOfBoundsExceptionMessage); }
  GcString* message() { return field_at<GcString*>(IndexOutOfBoundsExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , IndexOutOfBoundsExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(IndexOutOfBoundsExceptionTrace); }
  object trace() { return field_at<object>(IndexOutOfBoundsExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , IndexOutOfBoundsExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(IndexOutOfBoundsExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(IndexOutOfBoundsExceptionCause); }
};

class GcInnerClassReference: public GcObject {
 public:
  static const Gc::Type Type = Gc::InnerClassReferenceType;
  static const size_t FixedSize = FixedSizeOfInnerClassReference;

  static GcInnerClassReference* makeZeroed(Thread* t);
  void setInner(Thread* t UNUSED, GcByteArray* value) { setField(t, this , InnerClassReferenceInner, reinterpret_cast<object>(value)); }
  GcByteArray** innerPtr() { return &field_at<GcByteArray*>(InnerClassReferenceInner); }
  GcByteArray* inner() { return field_at<GcByteArray*>(InnerClassReferenceInner); }
  void setOuter(Thread* t UNUSED, GcByteArray* value) { setField(t, this , InnerClassReferenceOuter, reinterpret_cast<object>(value)); }
  GcByteArray** outerPtr() { return &field_at<GcByteArray*>(InnerClassReferenceOuter); }
  GcByteArray* outer() { return field_at<GcByteArray*>(InnerClassReferenceOuter); }
  void setName(Thread* t UNUSED, GcByteArray* value) { setField(t, this , InnerClassReferenceName, reinterpret_cast<object>(value)); }
  GcByteArray** namePtr() { return &field_at<GcByteArray*>(InnerClassReferenceName); }
  GcByteArray* name() { return field_at<GcByteArray*>(InnerClassReferenceName); }
  void setFlags(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(InnerClassReferenceFlags) = value; }
  uint16_t* flagsPtr() { return &field_at<uint16_t>(InnerClassReferenceFlags); }
  uint16_t& flags() { return field_at<uint16_t>(InnerClassReferenceFlags); }
};

class GcInt: public GcObject {
 public:
  static const Gc::Type Type = Gc::IntType;
  static const size_t FixedSize = FixedSizeOfInt;

  static GcInt* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(IntValue) = value; }
  uint32_t* valuePtr() { return &field_at<uint32_t>(IntValue); }
  uint32_t& value() { return field_at<uint32_t>(IntValue); }
};

class GcIntArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::IntArrayType;
  static const size_t FixedSize = FixedSizeOfIntArray;

  static GcIntArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(IntArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(IntArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(IntArrayLength); }
  avian::util::Slice<int32_t> body() { return avian::util::Slice<int32_t> (&field_at<int32_t>(IntArrayBody), field_at<uintptr_t>(IntArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, int32_t value) { field_at<int32_t>(IntArrayBody + index * (4)) = value; }
};

class GcInterruptedException: public GcObject {
 public:
  static const Gc::Type Type = Gc::InterruptedExceptionType;
  static const size_t FixedSize = FixedSizeOfInterruptedException;

  static GcInterruptedException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , InterruptedExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(InterruptedExceptionMessage); }
  GcString* message() { return field_at<GcString*>(InterruptedExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , InterruptedExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(InterruptedExceptionTrace); }
  object trace() { return field_at<object>(InterruptedExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , InterruptedExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(InterruptedExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(InterruptedExceptionCause); }
};

class GcInvocation: public GcObject {
 public:
  static const Gc::Type Type = Gc::InvocationType;
  static const size_t FixedSize = FixedSizeOfInvocation;

  static GcInvocation* makeZeroed(Thread* t);
  void setBootstrap(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(InvocationBootstrap) = value; }
  uint16_t* bootstrapPtr() { return &field_at<uint16_t>(InvocationBootstrap); }
  uint16_t& bootstrap() { return field_at<uint16_t>(InvocationBootstrap); }
  void setIndex(Thread* t UNUSED, int32_t value) { field_at<int32_t>(InvocationIndex) = value; }
  int32_t* indexPtr() { return &field_at<int32_t>(InvocationIndex); }
  int32_t& index() { return field_at<int32_t>(InvocationIndex); }
  void setClass(Thread* t UNUSED, object value) { setField(t, this , InvocationClass, reinterpret_cast<object>(value)); }
  object* class_Ptr() { return &field_at<object>(InvocationClass); }
  object class_() { return field_at<object>(InvocationClass); }
  void setPool(Thread* t UNUSED, object value) { setField(t, this , InvocationPool, reinterpret_cast<object>(value)); }
  object* poolPtr() { return &field_at<object>(InvocationPool); }
  object pool() { return field_at<object>(InvocationPool); }
  void setTemplate(Thread* t UNUSED, object value) { setField(t, this , InvocationTemplate, reinterpret_cast<object>(value)); }
  object* template_Ptr() { return &field_at<object>(InvocationTemplate); }
  object template_() { return field_at<object>(InvocationTemplate); }
  void setSite(Thread* t UNUSED, object value) { setField(t, this , InvocationSite, reinterpret_cast<object>(value)); }
  object* sitePtr() { return &field_at<object>(InvocationSite); }
  object site() { return field_at<object>(InvocationSite); }
};

class GcInvocationTargetException: public GcObject {
 public:
  static const Gc::Type Type = Gc::InvocationTargetExceptionType;
  static const size_t FixedSize = FixedSizeOfInvocationTargetException;

  static GcInvocationTargetException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , InvocationTargetExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(InvocationTargetExceptionMessage); }
  GcString* message() { return field_at<GcString*>(InvocationTargetExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , InvocationTargetExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(InvocationTargetExceptionTrace); }
  object trace() { return field_at<object>(InvocationTargetExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , InvocationTargetExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(InvocationTargetExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(InvocationTargetExceptionCause); }
  void setTarget(Thread* t UNUSED, GcThrowable* value) { setField(t, this , InvocationTargetExceptionTarget, reinterpret_cast<object>(value)); }
  GcThrowable** targetPtr() { return &field_at<GcThrowable*>(InvocationTargetExceptionTarget); }
  GcThrowable* target() { return field_at<GcThrowable*>(InvocationTargetExceptionTarget); }
};

class GcIoException: public GcObject {
 public:
  static const Gc::Type Type = Gc::IoExceptionType;
  static const size_t FixedSize = FixedSizeOfIoException;

  static GcIoException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , IoExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(IoExceptionMessage); }
  GcString* message() { return field_at<GcString*>(IoExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , IoExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(IoExceptionTrace); }
  object trace() { return field_at<object>(IoExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , IoExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(IoExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(IoExceptionCause); }
};

class GcJaccessibleObject: public GcObject {
 public:
  static const Gc::Type Type = Gc::JaccessibleObjectType;
  static const size_t FixedSize = FixedSizeOfJaccessibleObject;

  static GcJaccessibleObject* makeZeroed(Thread* t);
};

class GcJboolean: public GcObject {
 public:
  static const Gc::Type Type = Gc::JbooleanType;
  static const size_t FixedSize = FixedSizeOfJboolean;

  static GcJboolean* makeZeroed(Thread* t);
};

class GcJbyte: public GcObject {
 public:
  static const Gc::Type Type = Gc::JbyteType;
  static const size_t FixedSize = FixedSizeOfJbyte;

  static GcJbyte* makeZeroed(Thread* t);
};

class GcJchar: public GcObject {
 public:
  static const Gc::Type Type = Gc::JcharType;
  static const size_t FixedSize = FixedSizeOfJchar;

  static GcJchar* makeZeroed(Thread* t);
};

class GcJclass: public GcObject {
 public:
  static const Gc::Type Type = Gc::JclassType;
  static const size_t FixedSize = FixedSizeOfJclass;

  static GcJclass* makeZeroed(Thread* t);
  void setVmClass(Thread* t UNUSED, GcClass* value) { setField(t, this , JclassVmClass, reinterpret_cast<object>(value)); }
  GcClass** vmClassPtr() { return &field_at<GcClass*>(JclassVmClass); }
  GcClass* vmClass() { return field_at<GcClass*>(JclassVmClass); }
};

class GcJconstructor: public GcObject {
 public:
  static const Gc::Type Type = Gc::JconstructorType;
  static const size_t FixedSize = FixedSizeOfJconstructor;

  static GcJconstructor* makeZeroed(Thread* t);
  void setMethod(Thread* t UNUSED, GcJmethod* value) { setField(t, this , JconstructorMethod, reinterpret_cast<object>(value)); }
  GcJmethod** methodPtr() { return &field_at<GcJmethod*>(JconstructorMethod); }
  GcJmethod* method() { return field_at<GcJmethod*>(JconstructorMethod); }
};

class GcJdouble: public GcObject {
 public:
  static const Gc::Type Type = Gc::JdoubleType;
  static const size_t FixedSize = FixedSizeOfJdouble;

  static GcJdouble* makeZeroed(Thread* t);
};

class GcJfield: public GcObject {
 public:
  static const Gc::Type Type = Gc::JfieldType;
  static const size_t FixedSize = FixedSizeOfJfield;

  static GcJfield* makeZeroed(Thread* t);
  void setVmField(Thread* t UNUSED, GcField* value) { setField(t, this , JfieldVmField, reinterpret_cast<object>(value)); }
  GcField** vmFieldPtr() { return &field_at<GcField*>(JfieldVmField); }
  GcField* vmField() { return field_at<GcField*>(JfieldVmField); }
  void setAccessible(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(JfieldAccessible) = value; }
  uint8_t* accessiblePtr() { return &field_at<uint8_t>(JfieldAccessible); }
  uint8_t& accessible() { return field_at<uint8_t>(JfieldAccessible); }
};

class GcJfloat: public GcObject {
 public:
  static const Gc::Type Type = Gc::JfloatType;
  static const size_t FixedSize = FixedSizeOfJfloat;

  static GcJfloat* makeZeroed(Thread* t);
};

class GcJint: public GcObject {
 public:
  static const Gc::Type Type = Gc::JintType;
  static const size_t FixedSize = FixedSizeOfJint;

  static GcJint* makeZeroed(Thread* t);
};

class GcJlong: public GcObject {
 public:
  static const Gc::Type Type = Gc::JlongType;
  static const size_t FixedSize = FixedSizeOfJlong;

  static GcJlong* makeZeroed(Thread* t);
};

class GcJmethod: public GcObject {
 public:
  static const Gc::Type Type = Gc::JmethodType;
  static const size_t FixedSize = FixedSizeOfJmethod;

  static GcJmethod* makeZeroed(Thread* t);
  void setVmMethod(Thread* t UNUSED, GcMethod* value) { setField(t, this , JmethodVmMethod, reinterpret_cast<object>(value)); }
  GcMethod** vmMethodPtr() { return &field_at<GcMethod*>(JmethodVmMethod); }
  GcMethod* vmMethod() { return field_at<GcMethod*>(JmethodVmMethod); }
  void setAccessible(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(JmethodAccessible) = value; }
  uint8_t* accessiblePtr() { return &field_at<uint8_t>(JmethodAccessible); }
  uint8_t& accessible() { return field_at<uint8_t>(JmethodAccessible); }
};

class GcJobject: public GcObject {
 public:
  static const Gc::Type Type = Gc::JobjectType;
  static const size_t FixedSize = FixedSizeOfJobject;

  static GcJobject* makeZeroed(Thread* t);
};

class GcJreference: public GcObject {
 public:
  static const Gc::Type Type = Gc::JreferenceType;
  static const size_t FixedSize = FixedSizeOfJreference;

  static GcJreference* makeZeroed(Thread* t);
  void setVmNext(Thread* t UNUSED, object value) { field_at<object>(JreferenceVmNext) = value; }
  object* vmNextPtr() { return &field_at<object>(JreferenceVmNext); }
  object& vmNext() { return field_at<object>(JreferenceVmNext); }
  void setTarget(Thread* t UNUSED, object value) { field_at<object>(JreferenceTarget) = value; }
  object* targetPtr() { return &field_at<object>(JreferenceTarget); }
  object& target() { return field_at<object>(JreferenceTarget); }
  void setQueue(Thread* t UNUSED, GcReferenceQueue* value) { field_at<GcReferenceQueue*>(JreferenceQueue) = value; }
  GcReferenceQueue** queuePtr() { return &field_at<GcReferenceQueue*>(JreferenceQueue); }
  GcReferenceQueue*& queue() { return field_at<GcReferenceQueue*>(JreferenceQueue); }
  void setJNext(Thread* t UNUSED, GcJreference* value) { setField(t, this , JreferenceJNext, reinterpret_cast<object>(value)); }
  GcJreference** jNextPtr() { return &field_at<GcJreference*>(JreferenceJNext); }
  GcJreference* jNext() { return field_at<GcJreference*>(JreferenceJNext); }
};

class GcJshort: public GcObject {
 public:
  static const Gc::Type Type = Gc::JshortType;
  static const size_t FixedSize = FixedSizeOfJshort;

  static GcJshort* makeZeroed(Thread* t);
};

class GcJvoid: public GcObject {
 public:
  static const Gc::Type Type = Gc::JvoidType;
  static const size_t FixedSize = FixedSizeOfJvoid;

  static GcJvoid* makeZeroed(Thread* t);
};

class GcLineNumberTable: public GcObject {
 public:
  static const Gc::Type Type = Gc::LineNumberTableType;
  static const size_t FixedSize = FixedSizeOfLineNumberTable;

  static GcLineNumberTable* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(LineNumberTableLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(LineNumberTableLength); }
  uintptr_t& length() { return field_at<uintptr_t>(LineNumberTableLength); }
  avian::util::Slice<uint64_t> body() { return avian::util::Slice<uint64_t> (&field_at<uint64_t>(LineNumberTableBody), field_at<uintptr_t>(LineNumberTableLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uint64_t value) { field_at<uint64_t>(LineNumberTableBody + index * (8)) = value; }
};

class GcLinkageError: public GcObject {
 public:
  static const Gc::Type Type = Gc::LinkageErrorType;
  static const size_t FixedSize = FixedSizeOfLinkageError;

  static GcLinkageError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , LinkageErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(LinkageErrorMessage); }
  GcString* message() { return field_at<GcString*>(LinkageErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , LinkageErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(LinkageErrorTrace); }
  object trace() { return field_at<object>(LinkageErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , LinkageErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(LinkageErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(LinkageErrorCause); }
};

class GcList: public GcObject {
 public:
  static const Gc::Type Type = Gc::ListType;
  static const size_t FixedSize = FixedSizeOfList;

  static GcList* makeZeroed(Thread* t);
  void setSize(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(ListSize) = value; }
  uint32_t* sizePtr() { return &field_at<uint32_t>(ListSize); }
  uint32_t& size() { return field_at<uint32_t>(ListSize); }
  void setFront(Thread* t UNUSED, object value) { setField(t, this , ListFront, reinterpret_cast<object>(value)); }
  object* frontPtr() { return &field_at<object>(ListFront); }
  object front() { return field_at<object>(ListFront); }
  void setRear(Thread* t UNUSED, object value) { setField(t, this , ListRear, reinterpret_cast<object>(value)); }
  object* rearPtr() { return &field_at<object>(ListRear); }
  object rear() { return field_at<object>(ListRear); }
};

class GcLong: public GcObject {
 public:
  static const Gc::Type Type = Gc::LongType;
  static const size_t FixedSize = FixedSizeOfLong;

  static GcLong* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, uint64_t value) { field_at<uint64_t>(LongValue) = value; }
  uint64_t* valuePtr() { return &field_at<uint64_t>(LongValue); }
  uint64_t& value() { return field_at<uint64_t>(LongValue); }
};

class GcLongArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::LongArrayType;
  static const size_t FixedSize = FixedSizeOfLongArray;

  static GcLongArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(LongArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(LongArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(LongArrayLength); }
  avian::util::Slice<int64_t> body() { return avian::util::Slice<int64_t> (&field_at<int64_t>(LongArrayBody), field_at<uintptr_t>(LongArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, int64_t value) { field_at<int64_t>(LongArrayBody + index * (8)) = value; }
};

class GcMethod: public GcObject {
 public:
  static const Gc::Type Type = Gc::MethodType;
  static const size_t FixedSize = FixedSizeOfMethod;

  static GcMethod* makeZeroed(Thread* t);
  void setVmFlags(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(MethodVmFlags) = value; }
  uint8_t* vmFlagsPtr() { return &field_at<uint8_t>(MethodVmFlags); }
  uint8_t& vmFlags() { return field_at<uint8_t>(MethodVmFlags); }
  void setReturnCode(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(MethodReturnCode) = value; }
  uint8_t* returnCodePtr() { return &field_at<uint8_t>(MethodReturnCode); }
  uint8_t& returnCode() { return field_at<uint8_t>(MethodReturnCode); }
  void setParameterCount(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(MethodParameterCount) = value; }
  uint8_t* parameterCountPtr() { return &field_at<uint8_t>(MethodParameterCount); }
  uint8_t& parameterCount() { return field_at<uint8_t>(MethodParameterCount); }
  void setParameterFootprint(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(MethodParameterFootprint) = value; }
  uint8_t* parameterFootprintPtr() { return &field_at<uint8_t>(MethodParameterFootprint); }
  uint8_t& parameterFootprint() { return field_at<uint8_t>(MethodParameterFootprint); }
  void setFlags(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(MethodFlags) = value; }
  uint16_t* flagsPtr() { return &field_at<uint16_t>(MethodFlags); }
  uint16_t& flags() { return field_at<uint16_t>(MethodFlags); }
  void setOffset(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(MethodOffset) = value; }
  uint16_t* offsetPtr() { return &field_at<uint16_t>(MethodOffset); }
  uint16_t& offset() { return field_at<uint16_t>(MethodOffset); }
  void setNativeID(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(MethodNativeID) = value; }
  uint32_t* nativeIDPtr() { return &field_at<uint32_t>(MethodNativeID); }
  uint32_t& nativeID() { return field_at<uint32_t>(MethodNativeID); }
  void setRuntimeDataIndex(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(MethodRuntimeDataIndex) = value; }
  uint32_t* runtimeDataIndexPtr() { return &field_at<uint32_t>(MethodRuntimeDataIndex); }
  uint32_t& runtimeDataIndex() { return field_at<uint32_t>(MethodRuntimeDataIndex); }
  void setName(Thread* t UNUSED, GcByteArray* value) { setField(t, this , MethodName, reinterpret_cast<object>(value)); }
  GcByteArray** namePtr() { return &field_at<GcByteArray*>(MethodName); }
  GcByteArray* name() { return field_at<GcByteArray*>(MethodName); }
  void setSpec(Thread* t UNUSED, GcByteArray* value) { setField(t, this , MethodSpec, reinterpret_cast<object>(value)); }
  GcByteArray** specPtr() { return &field_at<GcByteArray*>(MethodSpec); }
  GcByteArray* spec() { return field_at<GcByteArray*>(MethodSpec); }
  void setAddendum(Thread* t UNUSED, GcMethodAddendum* value) { setField(t, this , MethodAddendum, reinterpret_cast<object>(value)); }
  GcMethodAddendum** addendumPtr() { return &field_at<GcMethodAddendum*>(MethodAddendum); }
  GcMethodAddendum* addendum() { return field_at<GcMethodAddendum*>(MethodAddendum); }
  void setClass(Thread* t UNUSED, GcClass* value) { setField(t, this , MethodClass, reinterpret_cast<object>(value)); }
  GcClass** class_Ptr() { return &field_at<GcClass*>(MethodClass); }
  GcClass* class_() { return field_at<GcClass*>(MethodClass); }
  void setCode(Thread* t UNUSED, GcCode* value) { setField(t, this , MethodCode, reinterpret_cast<object>(value)); }
  GcCode** codePtr() { return &field_at<GcCode*>(MethodCode); }
  GcCode* code() { return field_at<GcCode*>(MethodCode); }
};

class GcMethodAddendum: public GcObject {
 public:
  static const Gc::Type Type = Gc::MethodAddendumType;
  static const size_t FixedSize = FixedSizeOfMethodAddendum;

  static GcMethodAddendum* makeZeroed(Thread* t);
  void setPool(Thread* t UNUSED, GcSingleton* value) { setField(t, this , MethodAddendumPool, reinterpret_cast<object>(value)); }
  GcSingleton** poolPtr() { return &field_at<GcSingleton*>(MethodAddendumPool); }
  GcSingleton* pool() { return field_at<GcSingleton*>(MethodAddendumPool); }
  void setAnnotationTable(Thread* t UNUSED, object value) { setField(t, this , MethodAddendumAnnotationTable, reinterpret_cast<object>(value)); }
  object* annotationTablePtr() { return &field_at<object>(MethodAddendumAnnotationTable); }
  object annotationTable() { return field_at<object>(MethodAddendumAnnotationTable); }
  void setSignature(Thread* t UNUSED, object value) { setField(t, this , MethodAddendumSignature, reinterpret_cast<object>(value)); }
  object* signaturePtr() { return &field_at<object>(MethodAddendumSignature); }
  object signature() { return field_at<object>(MethodAddendumSignature); }
  void setExceptionTable(Thread* t UNUSED, object value) { setField(t, this , MethodAddendumExceptionTable, reinterpret_cast<object>(value)); }
  object* exceptionTablePtr() { return &field_at<object>(MethodAddendumExceptionTable); }
  object exceptionTable() { return field_at<object>(MethodAddendumExceptionTable); }
  void setAnnotationDefault(Thread* t UNUSED, object value) { setField(t, this , MethodAddendumAnnotationDefault, reinterpret_cast<object>(value)); }
  object* annotationDefaultPtr() { return &field_at<object>(MethodAddendumAnnotationDefault); }
  object annotationDefault() { return field_at<object>(MethodAddendumAnnotationDefault); }
  void setParameterAnnotationTable(Thread* t UNUSED, object value) { setField(t, this , MethodAddendumParameterAnnotationTable, reinterpret_cast<object>(value)); }
  object* parameterAnnotationTablePtr() { return &field_at<object>(MethodAddendumParameterAnnotationTable); }
  object parameterAnnotationTable() { return field_at<object>(MethodAddendumParameterAnnotationTable); }
};

class GcMethodRuntimeData: public GcObject {
 public:
  static const Gc::Type Type = Gc::MethodRuntimeDataType;
  static const size_t FixedSize = FixedSizeOfMethodRuntimeData;

  static GcMethodRuntimeData* makeZeroed(Thread* t);
  void setNative(Thread* t UNUSED, GcNative* value) { setField(t, this , MethodRuntimeDataNative, reinterpret_cast<object>(value)); }
  GcNative** nativePtr() { return &field_at<GcNative*>(MethodRuntimeDataNative); }
  GcNative* native() { return field_at<GcNative*>(MethodRuntimeDataNative); }
};

class GcMonitor: public GcObject {
 public:
  static const Gc::Type Type = Gc::MonitorType;
  static const size_t FixedSize = FixedSizeOfMonitor;

  static GcMonitor* makeZeroed(Thread* t);
  void setOwner(Thread* t UNUSED, void* value) { field_at<void*>(MonitorOwner) = value; }
  void** ownerPtr() { return &field_at<void*>(MonitorOwner); }
  void*& owner() { return field_at<void*>(MonitorOwner); }
  void setWaitHead(Thread* t UNUSED, void* value) { field_at<void*>(MonitorWaitHead) = value; }
  void** waitHeadPtr() { return &field_at<void*>(MonitorWaitHead); }
  void*& waitHead() { return field_at<void*>(MonitorWaitHead); }
  void setWaitTail(Thread* t UNUSED, void* value) { field_at<void*>(MonitorWaitTail) = value; }
  void** waitTailPtr() { return &field_at<void*>(MonitorWaitTail); }
  void*& waitTail() { return field_at<void*>(MonitorWaitTail); }
  void setAcquireHead(Thread* t UNUSED, object value) { setField(t, this , MonitorAcquireHead, reinterpret_cast<object>(value)); }
  object* acquireHeadPtr() { return &field_at<object>(MonitorAcquireHead); }
  object acquireHead() { return field_at<object>(MonitorAcquireHead); }
  void setAcquireTail(Thread* t UNUSED, object value) { setField(t, this , MonitorAcquireTail, reinterpret_cast<object>(value)); }
  object* acquireTailPtr() { return &field_at<object>(MonitorAcquireTail); }
  object acquireTail() { return field_at<object>(MonitorAcquireTail); }
  void setDepth(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(MonitorDepth) = value; }
  uint32_t* depthPtr() { return &field_at<uint32_t>(MonitorDepth); }
  uint32_t& depth() { return field_at<uint32_t>(MonitorDepth); }
};

class GcMonitorNode: public GcObject {
 public:
  static const Gc::Type Type = Gc::MonitorNodeType;
  static const size_t FixedSize = FixedSizeOfMonitorNode;

  static GcMonitorNode* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, void* value) { field_at<void*>(MonitorNodeValue) = value; }
  void** valuePtr() { return &field_at<void*>(MonitorNodeValue); }
  void*& value() { return field_at<void*>(MonitorNodeValue); }
  void setNext(Thread* t UNUSED, object value) { setField(t, this , MonitorNodeNext, reinterpret_cast<object>(value)); }
  object* nextPtr() { return &field_at<object>(MonitorNodeNext); }
  object next() { return field_at<object>(MonitorNodeNext); }
};

class GcNative: public GcObject {
 public:
  static const Gc::Type Type = Gc::NativeType;
  static const size_t FixedSize = FixedSizeOfNative;

  static GcNative* makeZeroed(Thread* t);
  void setFunction(Thread* t UNUSED, void* value) { field_at<void*>(NativeFunction) = value; }
  void** functionPtr() { return &field_at<void*>(NativeFunction); }
  void*& function() { return field_at<void*>(NativeFunction); }
  void setFast(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(NativeFast) = value; }
  uint8_t* fastPtr() { return &field_at<uint8_t>(NativeFast); }
  uint8_t& fast() { return field_at<uint8_t>(NativeFast); }
};

class GcNativeIntercept: public GcObject {
 public:
  static const Gc::Type Type = Gc::NativeInterceptType;
  static const size_t FixedSize = FixedSizeOfNativeIntercept;

  static GcNativeIntercept* makeZeroed(Thread* t);
  void setFunction(Thread* t UNUSED, void* value) { field_at<void*>(NativeInterceptFunction) = value; }
  void** functionPtr() { return &field_at<void*>(NativeInterceptFunction); }
  void*& function() { return field_at<void*>(NativeInterceptFunction); }
  void setFast(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(NativeInterceptFast) = value; }
  uint8_t* fastPtr() { return &field_at<uint8_t>(NativeInterceptFast); }
  uint8_t& fast() { return field_at<uint8_t>(NativeInterceptFast); }
  void setOriginal(Thread* t UNUSED, object value) { setField(t, this , NativeInterceptOriginal, reinterpret_cast<object>(value)); }
  object* originalPtr() { return &field_at<object>(NativeInterceptOriginal); }
  object original() { return field_at<object>(NativeInterceptOriginal); }
};

class GcNegativeArraySizeException: public GcObject {
 public:
  static const Gc::Type Type = Gc::NegativeArraySizeExceptionType;
  static const size_t FixedSize = FixedSizeOfNegativeArraySizeException;

  static GcNegativeArraySizeException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , NegativeArraySizeExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(NegativeArraySizeExceptionMessage); }
  GcString* message() { return field_at<GcString*>(NegativeArraySizeExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , NegativeArraySizeExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(NegativeArraySizeExceptionTrace); }
  object trace() { return field_at<object>(NegativeArraySizeExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , NegativeArraySizeExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(NegativeArraySizeExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(NegativeArraySizeExceptionCause); }
};

class GcNoClassDefFoundError: public GcObject {
 public:
  static const Gc::Type Type = Gc::NoClassDefFoundErrorType;
  static const size_t FixedSize = FixedSizeOfNoClassDefFoundError;

  static GcNoClassDefFoundError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , NoClassDefFoundErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(NoClassDefFoundErrorMessage); }
  GcString* message() { return field_at<GcString*>(NoClassDefFoundErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , NoClassDefFoundErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(NoClassDefFoundErrorTrace); }
  object trace() { return field_at<object>(NoClassDefFoundErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , NoClassDefFoundErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(NoClassDefFoundErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(NoClassDefFoundErrorCause); }
};

class GcNoSuchFieldError: public GcObject {
 public:
  static const Gc::Type Type = Gc::NoSuchFieldErrorType;
  static const size_t FixedSize = FixedSizeOfNoSuchFieldError;

  static GcNoSuchFieldError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , NoSuchFieldErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(NoSuchFieldErrorMessage); }
  GcString* message() { return field_at<GcString*>(NoSuchFieldErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , NoSuchFieldErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(NoSuchFieldErrorTrace); }
  object trace() { return field_at<object>(NoSuchFieldErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , NoSuchFieldErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(NoSuchFieldErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(NoSuchFieldErrorCause); }
};

class GcNoSuchMethodError: public GcObject {
 public:
  static const Gc::Type Type = Gc::NoSuchMethodErrorType;
  static const size_t FixedSize = FixedSizeOfNoSuchMethodError;

  static GcNoSuchMethodError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , NoSuchMethodErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(NoSuchMethodErrorMessage); }
  GcString* message() { return field_at<GcString*>(NoSuchMethodErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , NoSuchMethodErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(NoSuchMethodErrorTrace); }
  object trace() { return field_at<object>(NoSuchMethodErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , NoSuchMethodErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(NoSuchMethodErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(NoSuchMethodErrorCause); }
};

class GcNullPointerException: public GcObject {
 public:
  static const Gc::Type Type = Gc::NullPointerExceptionType;
  static const size_t FixedSize = FixedSizeOfNullPointerException;

  static GcNullPointerException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , NullPointerExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(NullPointerExceptionMessage); }
  GcString* message() { return field_at<GcString*>(NullPointerExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , NullPointerExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(NullPointerExceptionTrace); }
  object trace() { return field_at<object>(NullPointerExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , NullPointerExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(NullPointerExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(NullPointerExceptionCause); }
};

class GcNumber: public GcObject {
 public:
  static const Gc::Type Type = Gc::NumberType;
  static const size_t FixedSize = FixedSizeOfNumber;

  static GcNumber* makeZeroed(Thread* t);
};

class GcOutOfMemoryError: public GcObject {
 public:
  static const Gc::Type Type = Gc::OutOfMemoryErrorType;
  static const size_t FixedSize = FixedSizeOfOutOfMemoryError;

  static GcOutOfMemoryError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , OutOfMemoryErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(OutOfMemoryErrorMessage); }
  GcString* message() { return field_at<GcString*>(OutOfMemoryErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , OutOfMemoryErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(OutOfMemoryErrorTrace); }
  object trace() { return field_at<object>(OutOfMemoryErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , OutOfMemoryErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(OutOfMemoryErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(OutOfMemoryErrorCause); }
};

class GcPair: public GcObject {
 public:
  static const Gc::Type Type = Gc::PairType;
  static const size_t FixedSize = FixedSizeOfPair;

  static GcPair* makeZeroed(Thread* t);
  void setFirst(Thread* t UNUSED, object value) { setField(t, this , PairFirst, reinterpret_cast<object>(value)); }
  object* firstPtr() { return &field_at<object>(PairFirst); }
  object first() { return field_at<object>(PairFirst); }
  void setSecond(Thread* t UNUSED, object value) { setField(t, this , PairSecond, reinterpret_cast<object>(value)); }
  object* secondPtr() { return &field_at<object>(PairSecond); }
  object second() { return field_at<object>(PairSecond); }
};

class GcPhantomReference: public GcObject {
 public:
  static const Gc::Type Type = Gc::PhantomReferenceType;
  static const size_t FixedSize = FixedSizeOfPhantomReference;

  static GcPhantomReference* makeZeroed(Thread* t);
  void setVmNext(Thread* t UNUSED, object value) { field_at<object>(PhantomReferenceVmNext) = value; }
  object* vmNextPtr() { return &field_at<object>(PhantomReferenceVmNext); }
  object& vmNext() { return field_at<object>(PhantomReferenceVmNext); }
  void setTarget(Thread* t UNUSED, object value) { field_at<object>(PhantomReferenceTarget) = value; }
  object* targetPtr() { return &field_at<object>(PhantomReferenceTarget); }
  object& target() { return field_at<object>(PhantomReferenceTarget); }
  void setQueue(Thread* t UNUSED, GcReferenceQueue* value) { field_at<GcReferenceQueue*>(PhantomReferenceQueue) = value; }
  GcReferenceQueue** queuePtr() { return &field_at<GcReferenceQueue*>(PhantomReferenceQueue); }
  GcReferenceQueue*& queue() { return field_at<GcReferenceQueue*>(PhantomReferenceQueue); }
  void setJNext(Thread* t UNUSED, GcJreference* value) { setField(t, this , PhantomReferenceJNext, reinterpret_cast<object>(value)); }
  GcJreference** jNextPtr() { return &field_at<GcJreference*>(PhantomReferenceJNext); }
  GcJreference* jNext() { return field_at<GcJreference*>(PhantomReferenceJNext); }
};

class GcPointer: public GcObject {
 public:
  static const Gc::Type Type = Gc::PointerType;
  static const size_t FixedSize = FixedSizeOfPointer;

  static GcPointer* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, void* value) { field_at<void*>(PointerValue) = value; }
  void** valuePtr() { return &field_at<void*>(PointerValue); }
  void*& value() { return field_at<void*>(PointerValue); }
};

class GcReference: public GcObject {
 public:
  static const Gc::Type Type = Gc::ReferenceType;
  static const size_t FixedSize = FixedSizeOfReference;

  static GcReference* makeZeroed(Thread* t);
  void setKind(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(ReferenceKind) = value; }
  uint8_t* kindPtr() { return &field_at<uint8_t>(ReferenceKind); }
  uint8_t& kind() { return field_at<uint8_t>(ReferenceKind); }
  void setClass(Thread* t UNUSED, GcByteArray* value) { setField(t, this , ReferenceClass, reinterpret_cast<object>(value)); }
  GcByteArray** class_Ptr() { return &field_at<GcByteArray*>(ReferenceClass); }
  GcByteArray* class_() { return field_at<GcByteArray*>(ReferenceClass); }
  void setName(Thread* t UNUSED, GcByteArray* value) { setField(t, this , ReferenceName, reinterpret_cast<object>(value)); }
  GcByteArray** namePtr() { return &field_at<GcByteArray*>(ReferenceName); }
  GcByteArray* name() { return field_at<GcByteArray*>(ReferenceName); }
  void setSpec(Thread* t UNUSED, GcByteArray* value) { setField(t, this , ReferenceSpec, reinterpret_cast<object>(value)); }
  GcByteArray** specPtr() { return &field_at<GcByteArray*>(ReferenceSpec); }
  GcByteArray* spec() { return field_at<GcByteArray*>(ReferenceSpec); }
};

class GcReferenceQueue: public GcObject {
 public:
  static const Gc::Type Type = Gc::ReferenceQueueType;
  static const size_t FixedSize = FixedSizeOfReferenceQueue;

  static GcReferenceQueue* makeZeroed(Thread* t);
  void setFront(Thread* t UNUSED, GcJreference* value) { setField(t, this , ReferenceQueueFront, reinterpret_cast<object>(value)); }
  GcJreference** frontPtr() { return &field_at<GcJreference*>(ReferenceQueueFront); }
  GcJreference* front() { return field_at<GcJreference*>(ReferenceQueueFront); }
  void setJnext(Thread* t UNUSED, object value) { setField(t, this , ReferenceQueueJnext, reinterpret_cast<object>(value)); }
  object* jnextPtr() { return &field_at<object>(ReferenceQueueJnext); }
  object jnext() { return field_at<object>(ReferenceQueueJnext); }
};

class GcReflectiveOperationException: public GcObject {
 public:
  static const Gc::Type Type = Gc::ReflectiveOperationExceptionType;
  static const size_t FixedSize = FixedSizeOfReflectiveOperationException;

  static GcReflectiveOperationException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ReflectiveOperationExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ReflectiveOperationExceptionMessage); }
  GcString* message() { return field_at<GcString*>(ReflectiveOperationExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ReflectiveOperationExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ReflectiveOperationExceptionTrace); }
  object trace() { return field_at<object>(ReflectiveOperationExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ReflectiveOperationExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ReflectiveOperationExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ReflectiveOperationExceptionCause); }
};

class GcRegion: public GcObject {
 public:
  static const Gc::Type Type = Gc::RegionType;
  static const size_t FixedSize = FixedSizeOfRegion;

  static GcRegion* makeZeroed(Thread* t);
  void setRegion(Thread* t UNUSED, void* value) { field_at<void*>(RegionRegion) = value; }
  void** regionPtr() { return &field_at<void*>(RegionRegion); }
  void*& region() { return field_at<void*>(RegionRegion); }
  void setPosition(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(RegionPosition) = value; }
  uint32_t* positionPtr() { return &field_at<uint32_t>(RegionPosition); }
  uint32_t& position() { return field_at<uint32_t>(RegionPosition); }
};

class GcRoots: public GcObject {
 public:
  static const Gc::Type Type = Gc::RootsType;
  static const size_t FixedSize = FixedSizeOfRoots;

  static GcRoots* makeZeroed(Thread* t);
  void setBootLoader(Thread* t UNUSED, GcClassLoader* value) { setField(t, this , RootsBootLoader, reinterpret_cast<object>(value)); }
  GcClassLoader** bootLoaderPtr() { return &field_at<GcClassLoader*>(RootsBootLoader); }
  GcClassLoader* bootLoader() { return field_at<GcClassLoader*>(RootsBootLoader); }
  void setAppLoader(Thread* t UNUSED, GcClassLoader* value) { setField(t, this , RootsAppLoader, reinterpret_cast<object>(value)); }
  GcClassLoader** appLoaderPtr() { return &field_at<GcClassLoader*>(RootsAppLoader); }
  GcClassLoader* appLoader() { return field_at<GcClassLoader*>(RootsAppLoader); }
  void setBootstrapClassMap(Thread* t UNUSED, GcHashMap* value) { setField(t, this , RootsBootstrapClassMap, reinterpret_cast<object>(value)); }
  GcHashMap** bootstrapClassMapPtr() { return &field_at<GcHashMap*>(RootsBootstrapClassMap); }
  GcHashMap* bootstrapClassMap() { return field_at<GcHashMap*>(RootsBootstrapClassMap); }
  void setPackageMap(Thread* t UNUSED, GcHashMap* value) { setField(t, this , RootsPackageMap, reinterpret_cast<object>(value)); }
  GcHashMap** packageMapPtr() { return &field_at<GcHashMap*>(RootsPackageMap); }
  GcHashMap* packageMap() { return field_at<GcHashMap*>(RootsPackageMap); }
  void setFindLoadedClassMethod(Thread* t UNUSED, GcMethod* value) { setField(t, this , RootsFindLoadedClassMethod, reinterpret_cast<object>(value)); }
  GcMethod** findLoadedClassMethodPtr() { return &field_at<GcMethod*>(RootsFindLoadedClassMethod); }
  GcMethod* findLoadedClassMethod() { return field_at<GcMethod*>(RootsFindLoadedClassMethod); }
  void setLoadClassMethod(Thread* t UNUSED, GcMethod* value) { setField(t, this , RootsLoadClassMethod, reinterpret_cast<object>(value)); }
  GcMethod** loadClassMethodPtr() { return &field_at<GcMethod*>(RootsLoadClassMethod); }
  GcMethod* loadClassMethod() { return field_at<GcMethod*>(RootsLoadClassMethod); }
  void setMonitorMap(Thread* t UNUSED, GcHashMap* value) { setField(t, this , RootsMonitorMap, reinterpret_cast<object>(value)); }
  GcHashMap** monitorMapPtr() { return &field_at<GcHashMap*>(RootsMonitorMap); }
  GcHashMap* monitorMap() { return field_at<GcHashMap*>(RootsMonitorMap); }
  void setStringMap(Thread* t UNUSED, GcHashMap* value) { setField(t, this , RootsStringMap, reinterpret_cast<object>(value)); }
  GcHashMap** stringMapPtr() { return &field_at<GcHashMap*>(RootsStringMap); }
  GcHashMap* stringMap() { return field_at<GcHashMap*>(RootsStringMap); }
  void setByteArrayMap(Thread* t UNUSED, GcHashMap* value) { setField(t, this , RootsByteArrayMap, reinterpret_cast<object>(value)); }
  GcHashMap** byteArrayMapPtr() { return &field_at<GcHashMap*>(RootsByteArrayMap); }
  GcHashMap* byteArrayMap() { return field_at<GcHashMap*>(RootsByteArrayMap); }
  void setPoolMap(Thread* t UNUSED, GcHashMap* value) { setField(t, this , RootsPoolMap, reinterpret_cast<object>(value)); }
  GcHashMap** poolMapPtr() { return &field_at<GcHashMap*>(RootsPoolMap); }
  GcHashMap* poolMap() { return field_at<GcHashMap*>(RootsPoolMap); }
  void setClassRuntimeDataTable(Thread* t UNUSED, GcVector* value) { setField(t, this , RootsClassRuntimeDataTable, reinterpret_cast<object>(value)); }
  GcVector** classRuntimeDataTablePtr() { return &field_at<GcVector*>(RootsClassRuntimeDataTable); }
  GcVector* classRuntimeDataTable() { return field_at<GcVector*>(RootsClassRuntimeDataTable); }
  void setMethodRuntimeDataTable(Thread* t UNUSED, GcVector* value) { setField(t, this , RootsMethodRuntimeDataTable, reinterpret_cast<object>(value)); }
  GcVector** methodRuntimeDataTablePtr() { return &field_at<GcVector*>(RootsMethodRuntimeDataTable); }
  GcVector* methodRuntimeDataTable() { return field_at<GcVector*>(RootsMethodRuntimeDataTable); }
  void setJNIMethodTable(Thread* t UNUSED, GcVector* value) { setField(t, this , RootsJNIMethodTable, reinterpret_cast<object>(value)); }
  GcVector** jNIMethodTablePtr() { return &field_at<GcVector*>(RootsJNIMethodTable); }
  GcVector* jNIMethodTable() { return field_at<GcVector*>(RootsJNIMethodTable); }
  void setJNIFieldTable(Thread* t UNUSED, GcVector* value) { setField(t, this , RootsJNIFieldTable, reinterpret_cast<object>(value)); }
  GcVector** jNIFieldTablePtr() { return &field_at<GcVector*>(RootsJNIFieldTable); }
  GcVector* jNIFieldTable() { return field_at<GcVector*>(RootsJNIFieldTable); }
  void setShutdownHooks(Thread* t UNUSED, GcPair* value) { setField(t, this , RootsShutdownHooks, reinterpret_cast<object>(value)); }
  GcPair** shutdownHooksPtr() { return &field_at<GcPair*>(RootsShutdownHooks); }
  GcPair* shutdownHooks() { return field_at<GcPair*>(RootsShutdownHooks); }
  void setFinalizerThread(Thread* t UNUSED, GcThread* value) { setField(t, this , RootsFinalizerThread, reinterpret_cast<object>(value)); }
  GcThread** finalizerThreadPtr() { return &field_at<GcThread*>(RootsFinalizerThread); }
  GcThread* finalizerThread() { return field_at<GcThread*>(RootsFinalizerThread); }
  void setObjectsToFinalize(Thread* t UNUSED, GcFinalizer* value) { setField(t, this , RootsObjectsToFinalize, reinterpret_cast<object>(value)); }
  GcFinalizer** objectsToFinalizePtr() { return &field_at<GcFinalizer*>(RootsObjectsToFinalize); }
  GcFinalizer* objectsToFinalize() { return field_at<GcFinalizer*>(RootsObjectsToFinalize); }
  void setObjectsToClean(Thread* t UNUSED, GcCleaner* value) { setField(t, this , RootsObjectsToClean, reinterpret_cast<object>(value)); }
  GcCleaner** objectsToCleanPtr() { return &field_at<GcCleaner*>(RootsObjectsToClean); }
  GcCleaner* objectsToClean() { return field_at<GcCleaner*>(RootsObjectsToClean); }
  void setNullPointerException(Thread* t UNUSED, GcThrowable* value) { setField(t, this , RootsNullPointerException, reinterpret_cast<object>(value)); }
  GcThrowable** nullPointerExceptionPtr() { return &field_at<GcThrowable*>(RootsNullPointerException); }
  GcThrowable* nullPointerException() { return field_at<GcThrowable*>(RootsNullPointerException); }
  void setArithmeticException(Thread* t UNUSED, GcThrowable* value) { setField(t, this , RootsArithmeticException, reinterpret_cast<object>(value)); }
  GcThrowable** arithmeticExceptionPtr() { return &field_at<GcThrowable*>(RootsArithmeticException); }
  GcThrowable* arithmeticException() { return field_at<GcThrowable*>(RootsArithmeticException); }
  void setArrayIndexOutOfBoundsException(Thread* t UNUSED, GcThrowable* value) { setField(t, this , RootsArrayIndexOutOfBoundsException, reinterpret_cast<object>(value)); }
  GcThrowable** arrayIndexOutOfBoundsExceptionPtr() { return &field_at<GcThrowable*>(RootsArrayIndexOutOfBoundsException); }
  GcThrowable* arrayIndexOutOfBoundsException() { return field_at<GcThrowable*>(RootsArrayIndexOutOfBoundsException); }
  void setOutOfMemoryError(Thread* t UNUSED, GcThrowable* value) { setField(t, this , RootsOutOfMemoryError, reinterpret_cast<object>(value)); }
  GcThrowable** outOfMemoryErrorPtr() { return &field_at<GcThrowable*>(RootsOutOfMemoryError); }
  GcThrowable* outOfMemoryError() { return field_at<GcThrowable*>(RootsOutOfMemoryError); }
  void setShutdownInProgress(Thread* t UNUSED, GcThrowable* value) { setField(t, this , RootsShutdownInProgress, reinterpret_cast<object>(value)); }
  GcThrowable** shutdownInProgressPtr() { return &field_at<GcThrowable*>(RootsShutdownInProgress); }
  GcThrowable* shutdownInProgress() { return field_at<GcThrowable*>(RootsShutdownInProgress); }
  void setVirtualFileFinders(Thread* t UNUSED, GcFinder* value) { setField(t, this , RootsVirtualFileFinders, reinterpret_cast<object>(value)); }
  GcFinder** virtualFileFindersPtr() { return &field_at<GcFinder*>(RootsVirtualFileFinders); }
  GcFinder* virtualFileFinders() { return field_at<GcFinder*>(RootsVirtualFileFinders); }
  void setVirtualFiles(Thread* t UNUSED, GcArray* value) { setField(t, this , RootsVirtualFiles, reinterpret_cast<object>(value)); }
  GcArray** virtualFilesPtr() { return &field_at<GcArray*>(RootsVirtualFiles); }
  GcArray* virtualFiles() { return field_at<GcArray*>(RootsVirtualFiles); }
  void setArrayInterfaceTable(Thread* t UNUSED, GcArray* value) { setField(t, this , RootsArrayInterfaceTable, reinterpret_cast<object>(value)); }
  GcArray** arrayInterfaceTablePtr() { return &field_at<GcArray*>(RootsArrayInterfaceTable); }
  GcArray* arrayInterfaceTable() { return field_at<GcArray*>(RootsArrayInterfaceTable); }
  void setThreadTerminated(Thread* t UNUSED, object value) { setField(t, this , RootsThreadTerminated, reinterpret_cast<object>(value)); }
  object* threadTerminatedPtr() { return &field_at<object>(RootsThreadTerminated); }
  object threadTerminated() { return field_at<object>(RootsThreadTerminated); }
};

class GcRuntimeException: public GcObject {
 public:
  static const Gc::Type Type = Gc::RuntimeExceptionType;
  static const size_t FixedSize = FixedSizeOfRuntimeException;

  static GcRuntimeException* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , RuntimeExceptionMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(RuntimeExceptionMessage); }
  GcString* message() { return field_at<GcString*>(RuntimeExceptionMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , RuntimeExceptionTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(RuntimeExceptionTrace); }
  object trace() { return field_at<object>(RuntimeExceptionTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , RuntimeExceptionCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(RuntimeExceptionCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(RuntimeExceptionCause); }
};

class GcSerializable: public GcObject {
 public:
  static const Gc::Type Type = Gc::SerializableType;
  static const size_t FixedSize = FixedSizeOfSerializable;

  static GcSerializable* makeZeroed(Thread* t);
};

class GcShort: public GcObject {
 public:
  static const Gc::Type Type = Gc::ShortType;
  static const size_t FixedSize = FixedSizeOfShort;

  static GcShort* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, uint16_t value) { field_at<uint16_t>(ShortValue) = value; }
  uint16_t* valuePtr() { return &field_at<uint16_t>(ShortValue); }
  uint16_t& value() { return field_at<uint16_t>(ShortValue); }
};

class GcShortArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::ShortArrayType;
  static const size_t FixedSize = FixedSizeOfShortArray;

  static GcShortArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(ShortArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(ShortArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(ShortArrayLength); }
  avian::util::Slice<int16_t> body() { return avian::util::Slice<int16_t> (&field_at<int16_t>(ShortArrayBody), field_at<uintptr_t>(ShortArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, int16_t value) { field_at<int16_t>(ShortArrayBody + index * (2)) = value; }
};

class GcSingleton: public GcObject {
 public:
  static const Gc::Type Type = Gc::SingletonType;
  static const size_t FixedSize = FixedSizeOfSingleton;

  static GcSingleton* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(SingletonLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(SingletonLength); }
  uintptr_t& length() { return field_at<uintptr_t>(SingletonLength); }
  avian::util::Slice<uintptr_t> body() { return avian::util::Slice<uintptr_t> (&field_at<uintptr_t>(SingletonBody), field_at<uintptr_t>(SingletonLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uintptr_t value) { setField(t, this , SingletonBody + index * (8), reinterpret_cast<object>(value)); }
};

class GcSoftReference: public GcObject {
 public:
  static const Gc::Type Type = Gc::SoftReferenceType;
  static const size_t FixedSize = FixedSizeOfSoftReference;

  static GcSoftReference* makeZeroed(Thread* t);
  void setVmNext(Thread* t UNUSED, object value) { field_at<object>(SoftReferenceVmNext) = value; }
  object* vmNextPtr() { return &field_at<object>(SoftReferenceVmNext); }
  object& vmNext() { return field_at<object>(SoftReferenceVmNext); }
  void setTarget(Thread* t UNUSED, object value) { field_at<object>(SoftReferenceTarget) = value; }
  object* targetPtr() { return &field_at<object>(SoftReferenceTarget); }
  object& target() { return field_at<object>(SoftReferenceTarget); }
  void setQueue(Thread* t UNUSED, GcReferenceQueue* value) { field_at<GcReferenceQueue*>(SoftReferenceQueue) = value; }
  GcReferenceQueue** queuePtr() { return &field_at<GcReferenceQueue*>(SoftReferenceQueue); }
  GcReferenceQueue*& queue() { return field_at<GcReferenceQueue*>(SoftReferenceQueue); }
  void setJNext(Thread* t UNUSED, GcJreference* value) { setField(t, this , SoftReferenceJNext, reinterpret_cast<object>(value)); }
  GcJreference** jNextPtr() { return &field_at<GcJreference*>(SoftReferenceJNext); }
  GcJreference* jNext() { return field_at<GcJreference*>(SoftReferenceJNext); }
};

class GcStackOverflowError: public GcObject {
 public:
  static const Gc::Type Type = Gc::StackOverflowErrorType;
  static const size_t FixedSize = FixedSizeOfStackOverflowError;

  static GcStackOverflowError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , StackOverflowErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(StackOverflowErrorMessage); }
  GcString* message() { return field_at<GcString*>(StackOverflowErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , StackOverflowErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(StackOverflowErrorTrace); }
  object trace() { return field_at<object>(StackOverflowErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , StackOverflowErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(StackOverflowErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(StackOverflowErrorCause); }
};

class GcStackTraceElement: public GcObject {
 public:
  static const Gc::Type Type = Gc::StackTraceElementType;
  static const size_t FixedSize = FixedSizeOfStackTraceElement;

  static GcStackTraceElement* makeZeroed(Thread* t);
  void setClass(Thread* t UNUSED, GcString* value) { setField(t, this , StackTraceElementClass, reinterpret_cast<object>(value)); }
  GcString** class_Ptr() { return &field_at<GcString*>(StackTraceElementClass); }
  GcString* class_() { return field_at<GcString*>(StackTraceElementClass); }
  void setMethod(Thread* t UNUSED, GcString* value) { setField(t, this , StackTraceElementMethod, reinterpret_cast<object>(value)); }
  GcString** methodPtr() { return &field_at<GcString*>(StackTraceElementMethod); }
  GcString* method() { return field_at<GcString*>(StackTraceElementMethod); }
  void setFile(Thread* t UNUSED, GcString* value) { setField(t, this , StackTraceElementFile, reinterpret_cast<object>(value)); }
  GcString** filePtr() { return &field_at<GcString*>(StackTraceElementFile); }
  GcString* file() { return field_at<GcString*>(StackTraceElementFile); }
  void setLine(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(StackTraceElementLine) = value; }
  uint32_t* linePtr() { return &field_at<uint32_t>(StackTraceElementLine); }
  uint32_t& line() { return field_at<uint32_t>(StackTraceElementLine); }
};

class GcString: public GcObject {
 public:
  static const Gc::Type Type = Gc::StringType;
  static const size_t FixedSize = FixedSizeOfString;

  static GcString* makeZeroed(Thread* t);
  void setData(Thread* t UNUSED, object value) { setField(t, this , StringData, reinterpret_cast<object>(value)); }
  object* dataPtr() { return &field_at<object>(StringData); }
  object data() { return field_at<object>(StringData); }
  void setOffset(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(StringOffset) = value; }
  uint32_t* offsetPtr() { return &field_at<uint32_t>(StringOffset); }
  uint32_t& offset(Thread*) { return field_at<uint32_t>(StringOffset); }
  void setLength(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(StringLength) = value; }
  uint32_t* lengthPtr() { return &field_at<uint32_t>(StringLength); }
  uint32_t& length(Thread*) { return field_at<uint32_t>(StringLength); }
  void setHashCode(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(StringHashCode) = value; }
  uint32_t* hashCodePtr() { return &field_at<uint32_t>(StringHashCode); }
  uint32_t& hashCode() { return field_at<uint32_t>(StringHashCode); }
};

class GcSystemClassLoader: public GcObject {
 public:
  static const Gc::Type Type = Gc::SystemClassLoaderType;
  static const size_t FixedSize = FixedSizeOfSystemClassLoader;

  static GcSystemClassLoader* makeZeroed(Thread* t);
  void setParent(Thread* t UNUSED, GcClassLoader* value) { setField(t, this , SystemClassLoaderParent, reinterpret_cast<object>(value)); }
  GcClassLoader** parentPtr() { return &field_at<GcClassLoader*>(SystemClassLoaderParent); }
  GcClassLoader* parent() { return field_at<GcClassLoader*>(SystemClassLoaderParent); }
  void setPackages(Thread* t UNUSED, object value) { setField(t, this , SystemClassLoaderPackages, reinterpret_cast<object>(value)); }
  object* packagesPtr() { return &field_at<object>(SystemClassLoaderPackages); }
  object packages() { return field_at<object>(SystemClassLoaderPackages); }
  void setMap(Thread* t UNUSED, object value) { setField(t, this , SystemClassLoaderMap, reinterpret_cast<object>(value)); }
  object* mapPtr() { return &field_at<object>(SystemClassLoaderMap); }
  object map() { return field_at<object>(SystemClassLoaderMap); }
  void setFinder(Thread* t UNUSED, void* value) { field_at<void*>(SystemClassLoaderFinder) = value; }
  void** finderPtr() { return &field_at<void*>(SystemClassLoaderFinder); }
  void*& finder() { return field_at<void*>(SystemClassLoaderFinder); }
};

class GcThread: public GcObject {
 public:
  static const Gc::Type Type = Gc::ThreadType;
  static const size_t FixedSize = FixedSizeOfThread;

  static GcThread* makeZeroed(Thread* t);
  void setParkBlocker(Thread* t UNUSED, object value) { setField(t, this , ThreadParkBlocker, reinterpret_cast<object>(value)); }
  object* parkBlockerPtr() { return &field_at<object>(ThreadParkBlocker); }
  object parkBlocker() { return field_at<object>(ThreadParkBlocker); }
  void setPeer(Thread* t UNUSED, uint64_t value) { field_at<uint64_t>(ThreadPeer) = value; }
  uint64_t* peerPtr() { return &field_at<uint64_t>(ThreadPeer); }
  uint64_t& peer() { return field_at<uint64_t>(ThreadPeer); }
  void setInterrupted(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(ThreadInterrupted) = value; }
  uint8_t* interruptedPtr() { return &field_at<uint8_t>(ThreadInterrupted); }
  uint8_t& interrupted() { return field_at<uint8_t>(ThreadInterrupted); }
  void setUnparked(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(ThreadUnparked) = value; }
  uint8_t* unparkedPtr() { return &field_at<uint8_t>(ThreadUnparked); }
  uint8_t& unparked() { return field_at<uint8_t>(ThreadUnparked); }
  void setDaemon(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(ThreadDaemon) = value; }
  uint8_t* daemonPtr() { return &field_at<uint8_t>(ThreadDaemon); }
  uint8_t& daemon() { return field_at<uint8_t>(ThreadDaemon); }
  void setState(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(ThreadState) = value; }
  uint8_t* statePtr() { return &field_at<uint8_t>(ThreadState); }
  uint8_t& state() { return field_at<uint8_t>(ThreadState); }
  void setPriority(Thread* t UNUSED, uint8_t value) { field_at<uint8_t>(ThreadPriority) = value; }
  uint8_t* priorityPtr() { return &field_at<uint8_t>(ThreadPriority); }
  uint8_t& priority() { return field_at<uint8_t>(ThreadPriority); }
  void setTask(Thread* t UNUSED, object value) { setField(t, this , ThreadTask, reinterpret_cast<object>(value)); }
  object* taskPtr() { return &field_at<object>(ThreadTask); }
  object task() { return field_at<object>(ThreadTask); }
  void setLocals(Thread* t UNUSED, object value) { setField(t, this , ThreadLocals, reinterpret_cast<object>(value)); }
  object* localsPtr() { return &field_at<object>(ThreadLocals); }
  object locals() { return field_at<object>(ThreadLocals); }
  void setSleepLock(Thread* t UNUSED, object value) { setField(t, this , ThreadSleepLock, reinterpret_cast<object>(value)); }
  object* sleepLockPtr() { return &field_at<object>(ThreadSleepLock); }
  object sleepLock() { return field_at<object>(ThreadSleepLock); }
  void setClassLoader(Thread* t UNUSED, GcClassLoader* value) { setField(t, this , ThreadClassLoader, reinterpret_cast<object>(value)); }
  GcClassLoader** classLoaderPtr() { return &field_at<GcClassLoader*>(ThreadClassLoader); }
  GcClassLoader* classLoader() { return field_at<GcClassLoader*>(ThreadClassLoader); }
  void setExceptionHandler(Thread* t UNUSED, object value) { setField(t, this , ThreadExceptionHandler, reinterpret_cast<object>(value)); }
  object* exceptionHandlerPtr() { return &field_at<object>(ThreadExceptionHandler); }
  object exceptionHandler() { return field_at<object>(ThreadExceptionHandler); }
  void setName(Thread* t UNUSED, GcString* value) { setField(t, this , ThreadName, reinterpret_cast<object>(value)); }
  GcString** namePtr() { return &field_at<GcString*>(ThreadName); }
  GcString* name() { return field_at<GcString*>(ThreadName); }
  void setGroup(Thread* t UNUSED, GcThreadGroup* value) { setField(t, this , ThreadGroup, reinterpret_cast<object>(value)); }
  GcThreadGroup** groupPtr() { return &field_at<GcThreadGroup*>(ThreadGroup); }
  GcThreadGroup* group() { return field_at<GcThreadGroup*>(ThreadGroup); }
  void setInterruptLock(Thread* t UNUSED, object value) { setField(t, this , ThreadInterruptLock, reinterpret_cast<object>(value)); }
  object* interruptLockPtr() { return &field_at<object>(ThreadInterruptLock); }
  object interruptLock() { return field_at<object>(ThreadInterruptLock); }
};

class GcThreadGroup: public GcObject {
 public:
  static const Gc::Type Type = Gc::ThreadGroupType;
  static const size_t FixedSize = FixedSizeOfThreadGroup;

  static GcThreadGroup* makeZeroed(Thread* t);
  void setParent(Thread* t UNUSED, GcThreadGroup* value) { setField(t, this , ThreadGroupParent, reinterpret_cast<object>(value)); }
  GcThreadGroup** parentPtr() { return &field_at<GcThreadGroup*>(ThreadGroupParent); }
  GcThreadGroup* parent() { return field_at<GcThreadGroup*>(ThreadGroupParent); }
  void setName(Thread* t UNUSED, GcString* value) { setField(t, this , ThreadGroupName, reinterpret_cast<object>(value)); }
  GcString** namePtr() { return &field_at<GcString*>(ThreadGroupName); }
  GcString* name() { return field_at<GcString*>(ThreadGroupName); }
  void setSubgroups(Thread* t UNUSED, object value) { setField(t, this , ThreadGroupSubgroups, reinterpret_cast<object>(value)); }
  object* subgroupsPtr() { return &field_at<object>(ThreadGroupSubgroups); }
  object subgroups() { return field_at<object>(ThreadGroupSubgroups); }
};

class GcThrowable: public GcObject {
 public:
  static const Gc::Type Type = Gc::ThrowableType;
  static const size_t FixedSize = FixedSizeOfThrowable;

  static GcThrowable* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , ThrowableMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(ThrowableMessage); }
  GcString* message() { return field_at<GcString*>(ThrowableMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , ThrowableTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(ThrowableTrace); }
  object trace() { return field_at<object>(ThrowableTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , ThrowableCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(ThrowableCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(ThrowableCause); }
};

class GcTraceElement: public GcObject {
 public:
  static const Gc::Type Type = Gc::TraceElementType;
  static const size_t FixedSize = FixedSizeOfTraceElement;

  static GcTraceElement* makeZeroed(Thread* t);
  void setMethod(Thread* t UNUSED, object value) { setField(t, this , TraceElementMethod, reinterpret_cast<object>(value)); }
  object* methodPtr() { return &field_at<object>(TraceElementMethod); }
  object method() { return field_at<object>(TraceElementMethod); }
  void setIp(Thread* t UNUSED, int32_t value) { field_at<int32_t>(TraceElementIp) = value; }
  int32_t* ipPtr() { return &field_at<int32_t>(TraceElementIp); }
  int32_t& ip() { return field_at<int32_t>(TraceElementIp); }
};

class GcTreeNode: public GcObject {
 public:
  static const Gc::Type Type = Gc::TreeNodeType;
  static const size_t FixedSize = FixedSizeOfTreeNode;

  static GcTreeNode* makeZeroed(Thread* t);
  void setValue(Thread* t UNUSED, object value) { setField(t, this , TreeNodeValue, reinterpret_cast<object>(value)); }
  object* valuePtr() { return &field_at<object>(TreeNodeValue); }
  object value() { return field_at<object>(TreeNodeValue); }
  void setLeft(Thread* t UNUSED, GcTreeNode* value) { setField(t, this , TreeNodeLeft, reinterpret_cast<object>(value)); }
  GcTreeNode** leftPtr() { return &field_at<GcTreeNode*>(TreeNodeLeft); }
  GcTreeNode* left() { return field_at<GcTreeNode*>(TreeNodeLeft); }
  void setRight(Thread* t UNUSED, GcTreeNode* value) { setField(t, this , TreeNodeRight, reinterpret_cast<object>(value)); }
  GcTreeNode** rightPtr() { return &field_at<GcTreeNode*>(TreeNodeRight); }
  GcTreeNode* right() { return field_at<GcTreeNode*>(TreeNodeRight); }
};

class GcTriple: public GcObject {
 public:
  static const Gc::Type Type = Gc::TripleType;
  static const size_t FixedSize = FixedSizeOfTriple;

  static GcTriple* makeZeroed(Thread* t);
  void setFirst(Thread* t UNUSED, object value) { setField(t, this , TripleFirst, reinterpret_cast<object>(value)); }
  object* firstPtr() { return &field_at<object>(TripleFirst); }
  object first() { return field_at<object>(TripleFirst); }
  void setSecond(Thread* t UNUSED, object value) { setField(t, this , TripleSecond, reinterpret_cast<object>(value)); }
  object* secondPtr() { return &field_at<object>(TripleSecond); }
  object second() { return field_at<object>(TripleSecond); }
  void setThird(Thread* t UNUSED, object value) { setField(t, this , TripleThird, reinterpret_cast<object>(value)); }
  object* thirdPtr() { return &field_at<object>(TripleThird); }
  object third() { return field_at<object>(TripleThird); }
};

class GcUnsatisfiedLinkError: public GcObject {
 public:
  static const Gc::Type Type = Gc::UnsatisfiedLinkErrorType;
  static const size_t FixedSize = FixedSizeOfUnsatisfiedLinkError;

  static GcUnsatisfiedLinkError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , UnsatisfiedLinkErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(UnsatisfiedLinkErrorMessage); }
  GcString* message() { return field_at<GcString*>(UnsatisfiedLinkErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , UnsatisfiedLinkErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(UnsatisfiedLinkErrorTrace); }
  object trace() { return field_at<object>(UnsatisfiedLinkErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , UnsatisfiedLinkErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(UnsatisfiedLinkErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(UnsatisfiedLinkErrorCause); }
};

class GcUnwindResult: public GcObject {
 public:
  static const Gc::Type Type = Gc::UnwindResultType;
  static const size_t FixedSize = FixedSizeOfUnwindResult;

  static GcUnwindResult* makeZeroed(Thread* t);
  void setContinuation(Thread* t UNUSED, GcContinuation* value) { setField(t, this , UnwindResultContinuation, reinterpret_cast<object>(value)); }
  GcContinuation** continuationPtr() { return &field_at<GcContinuation*>(UnwindResultContinuation); }
  GcContinuation* continuation() { return field_at<GcContinuation*>(UnwindResultContinuation); }
  void setResult(Thread* t UNUSED, object value) { setField(t, this , UnwindResultResult, reinterpret_cast<object>(value)); }
  object* resultPtr() { return &field_at<object>(UnwindResultResult); }
  object result() { return field_at<object>(UnwindResultResult); }
  void setException(Thread* t UNUSED, GcThrowable* value) { setField(t, this , UnwindResultException, reinterpret_cast<object>(value)); }
  GcThrowable** exceptionPtr() { return &field_at<GcThrowable*>(UnwindResultException); }
  GcThrowable* exception() { return field_at<GcThrowable*>(UnwindResultException); }
};

class GcVector: public GcObject {
 public:
  static const Gc::Type Type = Gc::VectorType;
  static const size_t FixedSize = FixedSizeOfVector;

  static GcVector* makeZeroed(Thread* t, uintptr_t length);
  void setSize(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(VectorSize) = value; }
  uint32_t* sizePtr() { return &field_at<uint32_t>(VectorSize); }
  uint32_t& size() { return field_at<uint32_t>(VectorSize); }
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(VectorLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(VectorLength); }
  uintptr_t& length() { return field_at<uintptr_t>(VectorLength); }
  avian::util::Slice<const object> body() { return avian::util::Slice<const object> (&field_at<const object>(VectorBody), field_at<uintptr_t>(VectorLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, object value) { setField(t, this , VectorBody + index * (8), reinterpret_cast<object>(value)); }
};

class GcVirtualMachineError: public GcObject {
 public:
  static const Gc::Type Type = Gc::VirtualMachineErrorType;
  static const size_t FixedSize = FixedSizeOfVirtualMachineError;

  static GcVirtualMachineError* makeZeroed(Thread* t);
  void setMessage(Thread* t UNUSED, GcString* value) { setField(t, this , VirtualMachineErrorMessage, reinterpret_cast<object>(value)); }
  GcString** messagePtr() { return &field_at<GcString*>(VirtualMachineErrorMessage); }
  GcString* message() { return field_at<GcString*>(VirtualMachineErrorMessage); }
  void setTrace(Thread* t UNUSED, object value) { setField(t, this , VirtualMachineErrorTrace, reinterpret_cast<object>(value)); }
  object* tracePtr() { return &field_at<object>(VirtualMachineErrorTrace); }
  object trace() { return field_at<object>(VirtualMachineErrorTrace); }
  void setCause(Thread* t UNUSED, GcThrowable* value) { setField(t, this , VirtualMachineErrorCause, reinterpret_cast<object>(value)); }
  GcThrowable** causePtr() { return &field_at<GcThrowable*>(VirtualMachineErrorCause); }
  GcThrowable* cause() { return field_at<GcThrowable*>(VirtualMachineErrorCause); }
};

class GcWeakHashMap: public GcObject {
 public:
  static const Gc::Type Type = Gc::WeakHashMapType;
  static const size_t FixedSize = FixedSizeOfWeakHashMap;

  static GcWeakHashMap* makeZeroed(Thread* t);
  void setSize(Thread* t UNUSED, uint32_t value) { field_at<uint32_t>(WeakHashMapSize) = value; }
  uint32_t* sizePtr() { return &field_at<uint32_t>(WeakHashMapSize); }
  uint32_t& size() { return field_at<uint32_t>(WeakHashMapSize); }
  void setArray(Thread* t UNUSED, GcArray* value) { setField(t, this , WeakHashMapArray, reinterpret_cast<object>(value)); }
  GcArray** arrayPtr() { return &field_at<GcArray*>(WeakHashMapArray); }
  GcArray* array() { return field_at<GcArray*>(WeakHashMapArray); }
};

class GcWeakReference: public GcObject {
 public:
  static const Gc::Type Type = Gc::WeakReferenceType;
  static const size_t FixedSize = FixedSizeOfWeakReference;

  static GcWeakReference* makeZeroed(Thread* t);
  void setVmNext(Thread* t UNUSED, object value) { field_at<object>(WeakReferenceVmNext) = value; }
  object* vmNextPtr() { return &field_at<object>(WeakReferenceVmNext); }
  object& vmNext() { return field_at<object>(WeakReferenceVmNext); }
  void setTarget(Thread* t UNUSED, object value) { field_at<object>(WeakReferenceTarget) = value; }
  object* targetPtr() { return &field_at<object>(WeakReferenceTarget); }
  object& target() { return field_at<object>(WeakReferenceTarget); }
  void setQueue(Thread* t UNUSED, GcReferenceQueue* value) { field_at<GcReferenceQueue*>(WeakReferenceQueue) = value; }
  GcReferenceQueue** queuePtr() { return &field_at<GcReferenceQueue*>(WeakReferenceQueue); }
  GcReferenceQueue*& queue() { return field_at<GcReferenceQueue*>(WeakReferenceQueue); }
  void setJNext(Thread* t UNUSED, GcJreference* value) { setField(t, this , WeakReferenceJNext, reinterpret_cast<object>(value)); }
  GcJreference** jNextPtr() { return &field_at<GcJreference*>(WeakReferenceJNext); }
  GcJreference* jNext() { return field_at<GcJreference*>(WeakReferenceJNext); }
};

class GcWordArray: public GcObject {
 public:
  static const Gc::Type Type = Gc::WordArrayType;
  static const size_t FixedSize = FixedSizeOfWordArray;

  static GcWordArray* makeZeroed(Thread* t, uintptr_t length);
  void setLength(Thread* t UNUSED, uintptr_t value) { field_at<uintptr_t>(WordArrayLength) = value; }
  uintptr_t* lengthPtr() { return &field_at<uintptr_t>(WordArrayLength); }
  uintptr_t& length() { return field_at<uintptr_t>(WordArrayLength); }
  avian::util::Slice<uintptr_t> body() { return avian::util::Slice<uintptr_t> (&field_at<uintptr_t>(WordArrayBody), field_at<uintptr_t>(WordArrayLength)); }
  void setBodyElement(Thread* t UNUSED, size_t index, uintptr_t value) { field_at<uintptr_t>(WordArrayBody + index * (8)) = value; }
};

void initAbstractMethodError(Thread* t, GcAbstractMethodError* o, GcString* message, object trace, GcThrowable* cause);

void initAddendum(Thread* t, GcAddendum* o, GcSingleton* pool, object annotationTable, object signature);

void initArithmeticException(Thread* t, GcArithmeticException* o, GcString* message, object trace, GcThrowable* cause);

void initArray(Thread* t, GcArray* o, uintptr_t length);

void initArrayIndexOutOfBoundsException(Thread* t, GcArrayIndexOutOfBoundsException* o, GcString* message, object trace, GcThrowable* cause);

void initArrayStoreException(Thread* t, GcArrayStoreException* o, GcString* message, object trace, GcThrowable* cause);

void initBoolean(Thread* t, GcBoolean* o, uint8_t value);

void initBooleanArray(Thread* t, GcBooleanArray* o, uintptr_t length);

void initByte(Thread* t, GcByte* o, uint8_t value);

void initByteArray(Thread* t, GcByteArray* o, uintptr_t length);

void initCallNode(Thread* t, GcCallNode* o, intptr_t address, GcMethod* target, uintptr_t flags, GcCallNode* next);

void initChar(Thread* t, GcChar* o, uint16_t value);

void initCharArray(Thread* t, GcCharArray* o, uintptr_t length);

void initClass(Thread* t, GcClass* o, uint16_t flags, uint16_t vmFlags, uint16_t fixedSize, uint8_t arrayElementSize, uint8_t arrayDimensions, GcClass* arrayElementClass, uint32_t runtimeDataIndex, GcIntArray* objectMask, GcByteArray* name, GcByteArray* sourceFile, GcClass* super, object interfaceTable, object virtualTable, object fieldTable, object methodTable, GcClassAddendum* addendum, GcSingleton* staticTable, GcClassLoader* loader, GcByteArray* source, uintptr_t length);

void initClassAddendum(Thread* t, GcClassAddendum* o, GcSingleton* pool, object annotationTable, object signature, object interfaceTable, object innerClassTable, uint32_t declaredMethodCount, GcByteArray* enclosingClass, GcPair* enclosingMethod);

void initClassCastException(Thread* t, GcClassCastException* o, GcString* message, object trace, GcThrowable* cause);

void initClassLoader(Thread* t, GcClassLoader* o, GcClassLoader* parent, object packages, object map);

void initClassNotFoundException(Thread* t, GcClassNotFoundException* o, GcString* message, object trace, GcThrowable* cause, GcThrowable* cause2);

void initClassRuntimeData(Thread* t, GcClassRuntimeData* o, object arrayClass, object jclass, object pool, object signers);

void initCleaner(Thread* t, GcCleaner* o, GcCleaner* queueNext);

void initCloneNotSupportedException(Thread* t, GcCloneNotSupportedException* o, GcString* message, object trace, GcThrowable* cause);

void initCloneable(Thread* t, GcCloneable* o);

void initCode(Thread* t, GcCode* o, GcSingleton* pool, GcIntArray* stackMap, object exceptionHandlerTable, GcLineNumberTable* lineNumberTable, intptr_t compiled, uint32_t compiledSize, uint16_t maxStack, uint16_t maxLocals, uintptr_t length);

void initCompileRoots(Thread* t, GcCompileRoots* o, GcArray* callTable, GcTreeNode* methodTree, GcTreeNode* methodTreeSentinal, object objectPools, object staticTableArray, GcWordArray* virtualThunks, GcMethod* receiveMethod, GcMethod* windMethod, GcMethod* rewindMethod);

void initConstantPool(Thread* t, GcConstantPool* o);

void initContinuation(Thread* t, GcContinuation* o, GcContinuation* next, GcContinuationContext* context, GcMethod* method, void* address, uintptr_t returnAddressOffset, uintptr_t framePointerOffset, uintptr_t length);

void initContinuationContext(Thread* t, GcContinuationContext* o, GcContinuationContext* next, object before, object after, object continuation, GcMethod* method);

void initDouble(Thread* t, GcDouble* o, uint64_t value);

void initDoubleArray(Thread* t, GcDoubleArray* o, uintptr_t length);

void initError(Thread* t, GcError* o, GcString* message, object trace, GcThrowable* cause);

void initException(Thread* t, GcException* o, GcString* message, object trace, GcThrowable* cause);

void initExceptionHandlerTable(Thread* t, GcExceptionHandlerTable* o, uintptr_t length);

void initExceptionInInitializerError(Thread* t, GcExceptionInInitializerError* o, GcString* message, object trace, GcThrowable* cause, GcThrowable* exception);

void initField(Thread* t, GcField* o, uint8_t vmFlags, uint8_t code, uint16_t flags, uint16_t offset, uint32_t nativeID, GcByteArray* name, GcByteArray* spec, GcFieldAddendum* addendum, GcClass* class_);

void initFieldAddendum(Thread* t, GcFieldAddendum* o, GcSingleton* pool, object annotationTable, object signature);

void initFileNotFoundException(Thread* t, GcFileNotFoundException* o, GcString* message, object trace, GcThrowable* cause);

void initFinalizer(Thread* t, GcFinalizer* o, object target, void* finalize, object next, object queueTarget, GcFinalizer* queueNext);

void initFinder(Thread* t, GcFinder* o, void* finder, GcByteArray* name, GcFinder* next);

void initFloat(Thread* t, GcFloat* o, uint32_t value);

void initFloatArray(Thread* t, GcFloatArray* o, uintptr_t length);

void initHashMap(Thread* t, GcHashMap* o, uint32_t size, GcArray* array);

void initIllegalArgumentException(Thread* t, GcIllegalArgumentException* o, GcString* message, object trace, GcThrowable* cause);

void initIllegalMonitorStateException(Thread* t, GcIllegalMonitorStateException* o, GcString* message, object trace, GcThrowable* cause);

void initIllegalStateException(Thread* t, GcIllegalStateException* o, GcString* message, object trace, GcThrowable* cause);

void initIncompatibleClassChangeError(Thread* t, GcIncompatibleClassChangeError* o, GcString* message, object trace, GcThrowable* cause);

void initIncompatibleContinuationException(Thread* t, GcIncompatibleContinuationException* o, GcString* message, object trace, GcThrowable* cause);

void initIndexOutOfBoundsException(Thread* t, GcIndexOutOfBoundsException* o, GcString* message, object trace, GcThrowable* cause);

void initInnerClassReference(Thread* t, GcInnerClassReference* o, GcByteArray* inner, GcByteArray* outer, GcByteArray* name, uint16_t flags);

void initInt(Thread* t, GcInt* o, uint32_t value);

void initIntArray(Thread* t, GcIntArray* o, uintptr_t length);

void initInterruptedException(Thread* t, GcInterruptedException* o, GcString* message, object trace, GcThrowable* cause);

void initInvocation(Thread* t, GcInvocation* o, uint16_t bootstrap, int32_t index, object class_, object pool, object template_, object site);

void initInvocationTargetException(Thread* t, GcInvocationTargetException* o, GcString* message, object trace, GcThrowable* cause, GcThrowable* target);

void initIoException(Thread* t, GcIoException* o, GcString* message, object trace, GcThrowable* cause);

void initJaccessibleObject(Thread* t, GcJaccessibleObject* o);

void initJboolean(Thread* t, GcJboolean* o);

void initJbyte(Thread* t, GcJbyte* o);

void initJchar(Thread* t, GcJchar* o);

void initJclass(Thread* t, GcJclass* o, GcClass* vmClass);

void initJconstructor(Thread* t, GcJconstructor* o, GcJmethod* method);

void initJdouble(Thread* t, GcJdouble* o);

void initJfield(Thread* t, GcJfield* o, GcField* vmField, uint8_t accessible);

void initJfloat(Thread* t, GcJfloat* o);

void initJint(Thread* t, GcJint* o);

void initJlong(Thread* t, GcJlong* o);

void initJmethod(Thread* t, GcJmethod* o, GcMethod* vmMethod, uint8_t accessible);

void initJobject(Thread* t, GcJobject* o);

void initJreference(Thread* t, GcJreference* o, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext);

void initJshort(Thread* t, GcJshort* o);

void initJvoid(Thread* t, GcJvoid* o);

void initLineNumberTable(Thread* t, GcLineNumberTable* o, uintptr_t length);

void initLinkageError(Thread* t, GcLinkageError* o, GcString* message, object trace, GcThrowable* cause);

void initList(Thread* t, GcList* o, uint32_t size, object front, object rear);

void initLong(Thread* t, GcLong* o, uint64_t value);

void initLongArray(Thread* t, GcLongArray* o, uintptr_t length);

void initMethod(Thread* t, GcMethod* o, uint8_t vmFlags, uint8_t returnCode, uint8_t parameterCount, uint8_t parameterFootprint, uint16_t flags, uint16_t offset, uint32_t nativeID, uint32_t runtimeDataIndex, GcByteArray* name, GcByteArray* spec, GcMethodAddendum* addendum, GcClass* class_, GcCode* code);

void initMethodAddendum(Thread* t, GcMethodAddendum* o, GcSingleton* pool, object annotationTable, object signature, object exceptionTable, object annotationDefault, object parameterAnnotationTable);

void initMethodRuntimeData(Thread* t, GcMethodRuntimeData* o, GcNative* native);

void initMonitor(Thread* t, GcMonitor* o, void* owner, void* waitHead, void* waitTail, object acquireHead, object acquireTail, uint32_t depth);

void initMonitorNode(Thread* t, GcMonitorNode* o, void* value, object next);

void initNative(Thread* t, GcNative* o, void* function, uint8_t fast);

void initNativeIntercept(Thread* t, GcNativeIntercept* o, void* function, uint8_t fast, object original);

void initNegativeArraySizeException(Thread* t, GcNegativeArraySizeException* o, GcString* message, object trace, GcThrowable* cause);

void initNoClassDefFoundError(Thread* t, GcNoClassDefFoundError* o, GcString* message, object trace, GcThrowable* cause);

void initNoSuchFieldError(Thread* t, GcNoSuchFieldError* o, GcString* message, object trace, GcThrowable* cause);

void initNoSuchMethodError(Thread* t, GcNoSuchMethodError* o, GcString* message, object trace, GcThrowable* cause);

void initNullPointerException(Thread* t, GcNullPointerException* o, GcString* message, object trace, GcThrowable* cause);

void initNumber(Thread* t, GcNumber* o);

void initOutOfMemoryError(Thread* t, GcOutOfMemoryError* o, GcString* message, object trace, GcThrowable* cause);

void initPair(Thread* t, GcPair* o, object first, object second);

void initPhantomReference(Thread* t, GcPhantomReference* o, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext);

void initPointer(Thread* t, GcPointer* o, void* value);

void initReference(Thread* t, GcReference* o, uint8_t kind, GcByteArray* class_, GcByteArray* name, GcByteArray* spec);

void initReferenceQueue(Thread* t, GcReferenceQueue* o, GcJreference* front, object jnext);

void initReflectiveOperationException(Thread* t, GcReflectiveOperationException* o, GcString* message, object trace, GcThrowable* cause);

void initRegion(Thread* t, GcRegion* o, void* region, uint32_t position);

void initRoots(Thread* t, GcRoots* o, GcClassLoader* bootLoader, GcClassLoader* appLoader, GcHashMap* bootstrapClassMap, GcHashMap* packageMap, GcMethod* findLoadedClassMethod, GcMethod* loadClassMethod, GcHashMap* monitorMap, GcHashMap* stringMap, GcHashMap* byteArrayMap, GcHashMap* poolMap, GcVector* classRuntimeDataTable, GcVector* methodRuntimeDataTable, GcVector* jNIMethodTable, GcVector* jNIFieldTable, GcPair* shutdownHooks, GcThread* finalizerThread, GcFinalizer* objectsToFinalize, GcCleaner* objectsToClean, GcThrowable* nullPointerException, GcThrowable* arithmeticException, GcThrowable* arrayIndexOutOfBoundsException, GcThrowable* outOfMemoryError, GcThrowable* shutdownInProgress, GcFinder* virtualFileFinders, GcArray* virtualFiles, GcArray* arrayInterfaceTable, object threadTerminated);

void initRuntimeException(Thread* t, GcRuntimeException* o, GcString* message, object trace, GcThrowable* cause);

void initSerializable(Thread* t, GcSerializable* o);

void initShort(Thread* t, GcShort* o, uint16_t value);

void initShortArray(Thread* t, GcShortArray* o, uintptr_t length);

void initSingleton(Thread* t, GcSingleton* o, uintptr_t length);

void initSoftReference(Thread* t, GcSoftReference* o, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext);

void initStackOverflowError(Thread* t, GcStackOverflowError* o, GcString* message, object trace, GcThrowable* cause);

void initStackTraceElement(Thread* t, GcStackTraceElement* o, GcString* class_, GcString* method, GcString* file, uint32_t line);

void initString(Thread* t, GcString* o, object data, uint32_t offset, uint32_t length, uint32_t hashCode);

void initSystemClassLoader(Thread* t, GcSystemClassLoader* o, GcClassLoader* parent, object packages, object map, void* finder);

void initThread(Thread* t, GcThread* o, object parkBlocker, uint64_t peer, uint8_t interrupted, uint8_t unparked, uint8_t daemon, uint8_t state, uint8_t priority, object task, object locals, object sleepLock, GcClassLoader* classLoader, object exceptionHandler, GcString* name, GcThreadGroup* group, object interruptLock);

void initThreadGroup(Thread* t, GcThreadGroup* o, GcThreadGroup* parent, GcString* name, object subgroups);

void initThrowable(Thread* t, GcThrowable* o, GcString* message, object trace, GcThrowable* cause);

void initTraceElement(Thread* t, GcTraceElement* o, object method, int32_t ip);

void initTreeNode(Thread* t, GcTreeNode* o, object value, GcTreeNode* left, GcTreeNode* right);

void initTriple(Thread* t, GcTriple* o, object first, object second, object third);

void initUnsatisfiedLinkError(Thread* t, GcUnsatisfiedLinkError* o, GcString* message, object trace, GcThrowable* cause);

void initUnwindResult(Thread* t, GcUnwindResult* o, GcContinuation* continuation, object result, GcThrowable* exception);

void initVector(Thread* t, GcVector* o, uint32_t size, uintptr_t length);

void initVirtualMachineError(Thread* t, GcVirtualMachineError* o, GcString* message, object trace, GcThrowable* cause);

void initWeakHashMap(Thread* t, GcWeakHashMap* o, uint32_t size, GcArray* array);

void initWeakReference(Thread* t, GcWeakReference* o, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext);

void initWordArray(Thread* t, GcWordArray* o, uintptr_t length);

GcAbstractMethodError* makeAbstractMethodError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcAddendum* makeAddendum(Thread* t, GcSingleton* pool, object annotationTable, object signature);

GcArithmeticException* makeArithmeticException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcArray* makeArray(Thread* t, uintptr_t length);

GcArrayIndexOutOfBoundsException* makeArrayIndexOutOfBoundsException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcArrayStoreException* makeArrayStoreException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcBoolean* makeBoolean(Thread* t, uint8_t value);

GcBooleanArray* makeBooleanArray(Thread* t, uintptr_t length);

GcByte* makeByte(Thread* t, uint8_t value);

GcByteArray* makeByteArray(Thread* t, uintptr_t length);

GcCallNode* makeCallNode(Thread* t, intptr_t address, GcMethod* target, uintptr_t flags, GcCallNode* next);

GcChar* makeChar(Thread* t, uint16_t value);

GcCharArray* makeCharArray(Thread* t, uintptr_t length);

GcClass* makeClass(Thread* t, uint16_t flags, uint16_t vmFlags, uint16_t fixedSize, uint8_t arrayElementSize, uint8_t arrayDimensions, GcClass* arrayElementClass, uint32_t runtimeDataIndex, GcIntArray* objectMask, GcByteArray* name, GcByteArray* sourceFile, GcClass* super, object interfaceTable, object virtualTable, object fieldTable, object methodTable, GcClassAddendum* addendum, GcSingleton* staticTable, GcClassLoader* loader, GcByteArray* source, uintptr_t length);

GcClassAddendum* makeClassAddendum(Thread* t, GcSingleton* pool, object annotationTable, object signature, object interfaceTable, object innerClassTable, uint32_t declaredMethodCount, GcByteArray* enclosingClass, GcPair* enclosingMethod);

GcClassCastException* makeClassCastException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcClassLoader* makeClassLoader(Thread* t, GcClassLoader* parent, object packages, object map);

GcClassNotFoundException* makeClassNotFoundException(Thread* t, GcString* message, object trace, GcThrowable* cause, GcThrowable* cause2);

GcClassRuntimeData* makeClassRuntimeData(Thread* t, object arrayClass, object jclass, object pool, object signers);

GcCleaner* makeCleaner(Thread* t, GcCleaner* queueNext);

GcCloneNotSupportedException* makeCloneNotSupportedException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcCloneable* makeCloneable(Thread* t);

GcCode* makeCode(Thread* t, GcSingleton* pool, GcIntArray* stackMap, object exceptionHandlerTable, GcLineNumberTable* lineNumberTable, intptr_t compiled, uint32_t compiledSize, uint16_t maxStack, uint16_t maxLocals, uintptr_t length);

GcCompileRoots* makeCompileRoots(Thread* t, GcArray* callTable, GcTreeNode* methodTree, GcTreeNode* methodTreeSentinal, object objectPools, object staticTableArray, GcWordArray* virtualThunks, GcMethod* receiveMethod, GcMethod* windMethod, GcMethod* rewindMethod);

GcConstantPool* makeConstantPool(Thread* t);

GcContinuation* makeContinuation(Thread* t, GcContinuation* next, GcContinuationContext* context, GcMethod* method, void* address, uintptr_t returnAddressOffset, uintptr_t framePointerOffset, uintptr_t length);

GcContinuationContext* makeContinuationContext(Thread* t, GcContinuationContext* next, object before, object after, object continuation, GcMethod* method);

GcDouble* makeDouble(Thread* t, uint64_t value);

GcDoubleArray* makeDoubleArray(Thread* t, uintptr_t length);

GcError* makeError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcException* makeException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcExceptionHandlerTable* makeExceptionHandlerTable(Thread* t, uintptr_t length);

GcExceptionInInitializerError* makeExceptionInInitializerError(Thread* t, GcString* message, object trace, GcThrowable* cause, GcThrowable* exception);

GcField* makeField(Thread* t, uint8_t vmFlags, uint8_t code, uint16_t flags, uint16_t offset, uint32_t nativeID, GcByteArray* name, GcByteArray* spec, GcFieldAddendum* addendum, GcClass* class_);

GcFieldAddendum* makeFieldAddendum(Thread* t, GcSingleton* pool, object annotationTable, object signature);

GcFileNotFoundException* makeFileNotFoundException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcFinalizer* makeFinalizer(Thread* t, object target, void* finalize, object next, object queueTarget, GcFinalizer* queueNext);

GcFinder* makeFinder(Thread* t, void* finder, GcByteArray* name, GcFinder* next);

GcFloat* makeFloat(Thread* t, uint32_t value);

GcFloatArray* makeFloatArray(Thread* t, uintptr_t length);

GcHashMap* makeHashMap(Thread* t, uint32_t size, GcArray* array);

GcIllegalArgumentException* makeIllegalArgumentException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcIllegalMonitorStateException* makeIllegalMonitorStateException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcIllegalStateException* makeIllegalStateException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcIncompatibleClassChangeError* makeIncompatibleClassChangeError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcIncompatibleContinuationException* makeIncompatibleContinuationException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcIndexOutOfBoundsException* makeIndexOutOfBoundsException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcInnerClassReference* makeInnerClassReference(Thread* t, GcByteArray* inner, GcByteArray* outer, GcByteArray* name, uint16_t flags);

GcInt* makeInt(Thread* t, uint32_t value);

GcIntArray* makeIntArray(Thread* t, uintptr_t length);

GcInterruptedException* makeInterruptedException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcInvocation* makeInvocation(Thread* t, uint16_t bootstrap, int32_t index, object class_, object pool, object template_, object site);

GcInvocationTargetException* makeInvocationTargetException(Thread* t, GcString* message, object trace, GcThrowable* cause, GcThrowable* target);

GcIoException* makeIoException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcJaccessibleObject* makeJaccessibleObject(Thread* t);

GcJboolean* makeJboolean(Thread* t);

GcJbyte* makeJbyte(Thread* t);

GcJchar* makeJchar(Thread* t);

GcJclass* makeJclass(Thread* t, GcClass* vmClass);

GcJconstructor* makeJconstructor(Thread* t, GcJmethod* method);

GcJdouble* makeJdouble(Thread* t);

GcJfield* makeJfield(Thread* t, GcField* vmField, uint8_t accessible);

GcJfloat* makeJfloat(Thread* t);

GcJint* makeJint(Thread* t);

GcJlong* makeJlong(Thread* t);

GcJmethod* makeJmethod(Thread* t, GcMethod* vmMethod, uint8_t accessible);

GcJobject* makeJobject(Thread* t);

GcJreference* makeJreference(Thread* t, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext);

GcJshort* makeJshort(Thread* t);

GcJvoid* makeJvoid(Thread* t);

GcLineNumberTable* makeLineNumberTable(Thread* t, uintptr_t length);

GcLinkageError* makeLinkageError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcList* makeList(Thread* t, uint32_t size, object front, object rear);

GcLong* makeLong(Thread* t, uint64_t value);

GcLongArray* makeLongArray(Thread* t, uintptr_t length);

GcMethod* makeMethod(Thread* t, uint8_t vmFlags, uint8_t returnCode, uint8_t parameterCount, uint8_t parameterFootprint, uint16_t flags, uint16_t offset, uint32_t nativeID, uint32_t runtimeDataIndex, GcByteArray* name, GcByteArray* spec, GcMethodAddendum* addendum, GcClass* class_, GcCode* code);

GcMethodAddendum* makeMethodAddendum(Thread* t, GcSingleton* pool, object annotationTable, object signature, object exceptionTable, object annotationDefault, object parameterAnnotationTable);

GcMethodRuntimeData* makeMethodRuntimeData(Thread* t, GcNative* native);

GcMonitor* makeMonitor(Thread* t, void* owner, void* waitHead, void* waitTail, object acquireHead, object acquireTail, uint32_t depth);

GcMonitorNode* makeMonitorNode(Thread* t, void* value, object next);

GcNative* makeNative(Thread* t, void* function, uint8_t fast);

GcNativeIntercept* makeNativeIntercept(Thread* t, void* function, uint8_t fast, object original);

GcNegativeArraySizeException* makeNegativeArraySizeException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcNoClassDefFoundError* makeNoClassDefFoundError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcNoSuchFieldError* makeNoSuchFieldError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcNoSuchMethodError* makeNoSuchMethodError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcNullPointerException* makeNullPointerException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcNumber* makeNumber(Thread* t);

GcOutOfMemoryError* makeOutOfMemoryError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcPair* makePair(Thread* t, object first, object second);

GcPhantomReference* makePhantomReference(Thread* t, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext);

GcPointer* makePointer(Thread* t, void* value);

GcReference* makeReference(Thread* t, uint8_t kind, GcByteArray* class_, GcByteArray* name, GcByteArray* spec);

GcReferenceQueue* makeReferenceQueue(Thread* t, GcJreference* front, object jnext);

GcReflectiveOperationException* makeReflectiveOperationException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcRegion* makeRegion(Thread* t, void* region, uint32_t position);

GcRoots* makeRoots(Thread* t, GcClassLoader* bootLoader, GcClassLoader* appLoader, GcHashMap* bootstrapClassMap, GcHashMap* packageMap, GcMethod* findLoadedClassMethod, GcMethod* loadClassMethod, GcHashMap* monitorMap, GcHashMap* stringMap, GcHashMap* byteArrayMap, GcHashMap* poolMap, GcVector* classRuntimeDataTable, GcVector* methodRuntimeDataTable, GcVector* jNIMethodTable, GcVector* jNIFieldTable, GcPair* shutdownHooks, GcThread* finalizerThread, GcFinalizer* objectsToFinalize, GcCleaner* objectsToClean, GcThrowable* nullPointerException, GcThrowable* arithmeticException, GcThrowable* arrayIndexOutOfBoundsException, GcThrowable* outOfMemoryError, GcThrowable* shutdownInProgress, GcFinder* virtualFileFinders, GcArray* virtualFiles, GcArray* arrayInterfaceTable, object threadTerminated);

GcRuntimeException* makeRuntimeException(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcSerializable* makeSerializable(Thread* t);

GcShort* makeShort(Thread* t, uint16_t value);

GcShortArray* makeShortArray(Thread* t, uintptr_t length);

GcSingleton* makeSingleton(Thread* t, uintptr_t length);

GcSoftReference* makeSoftReference(Thread* t, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext);

GcStackOverflowError* makeStackOverflowError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcStackTraceElement* makeStackTraceElement(Thread* t, GcString* class_, GcString* method, GcString* file, uint32_t line);

GcString* makeString(Thread* t, object data, uint32_t offset, uint32_t length, uint32_t hashCode);

GcSystemClassLoader* makeSystemClassLoader(Thread* t, GcClassLoader* parent, object packages, object map, void* finder);

GcThread* makeThread(Thread* t, object parkBlocker, uint64_t peer, uint8_t interrupted, uint8_t unparked, uint8_t daemon, uint8_t state, uint8_t priority, object task, object locals, object sleepLock, GcClassLoader* classLoader, object exceptionHandler, GcString* name, GcThreadGroup* group, object interruptLock);

GcThreadGroup* makeThreadGroup(Thread* t, GcThreadGroup* parent, GcString* name, object subgroups);

GcThrowable* makeThrowable(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcTraceElement* makeTraceElement(Thread* t, object method, int32_t ip);

GcTreeNode* makeTreeNode(Thread* t, object value, GcTreeNode* left, GcTreeNode* right);

GcTriple* makeTriple(Thread* t, object first, object second, object third);

GcUnsatisfiedLinkError* makeUnsatisfiedLinkError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcUnwindResult* makeUnwindResult(Thread* t, GcContinuation* continuation, object result, GcThrowable* exception);

GcVector* makeVector(Thread* t, uint32_t size, uintptr_t length);

GcVirtualMachineError* makeVirtualMachineError(Thread* t, GcString* message, object trace, GcThrowable* cause);

GcWeakHashMap* makeWeakHashMap(Thread* t, uint32_t size, GcArray* array);

GcWeakReference* makeWeakReference(Thread* t, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext);

GcWordArray* makeWordArray(Thread* t, uintptr_t length);


