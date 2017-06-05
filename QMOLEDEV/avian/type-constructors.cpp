void initAbstractMethodError(Thread* t, GcAbstractMethodError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::AbstractMethodErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initAddendum(Thread* t, GcAddendum* o, GcSingleton* pool, object annotationTable, object signature)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::AddendumType]));
  o->setPool(t, pool);
  o->setAnnotationTable(t, annotationTable);
  o->setSignature(t, signature);
}

void initArithmeticException(Thread* t, GcArithmeticException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ArithmeticExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initArray(Thread* t, GcArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ArrayType]));
  o->setLength(t, length);
}

void initArrayIndexOutOfBoundsException(Thread* t, GcArrayIndexOutOfBoundsException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ArrayIndexOutOfBoundsExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initArrayStoreException(Thread* t, GcArrayStoreException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ArrayStoreExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initBoolean(Thread* t, GcBoolean* o, uint8_t value)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::BooleanType]));
  o->setValue(t, value);
}

void initBooleanArray(Thread* t, GcBooleanArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::BooleanArrayType]));
  o->setLength(t, length);
}

void initByte(Thread* t, GcByte* o, uint8_t value)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ByteType]));
  o->setValue(t, value);
}

void initByteArray(Thread* t, GcByteArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ByteArrayType]));
  o->setLength(t, length);
}

void initCallNode(Thread* t, GcCallNode* o, intptr_t address, GcMethod* target, uintptr_t flags, GcCallNode* next)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CallNodeType]));
  o->setAddress(t, address);
  o->setTarget(t, target);
  o->setFlags(t, flags);
  o->setNext(t, next);
}

void initChar(Thread* t, GcChar* o, uint16_t value)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CharType]));
  o->setValue(t, value);
}

void initCharArray(Thread* t, GcCharArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CharArrayType]));
  o->setLength(t, length);
}

void initClass(Thread* t, GcClass* o, uint16_t flags, uint16_t vmFlags, uint16_t fixedSize, uint8_t arrayElementSize, uint8_t arrayDimensions, GcClass* arrayElementClass, uint32_t runtimeDataIndex, GcIntArray* objectMask, GcByteArray* name, GcByteArray* sourceFile, GcClass* super, object interfaceTable, object virtualTable, object fieldTable, object methodTable, GcClassAddendum* addendum, GcSingleton* staticTable, GcClassLoader* loader, GcByteArray* source, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassType]));
  o->setFlags(t, flags);
  o->setVmFlags(t, vmFlags);
  o->setFixedSize(t, fixedSize);
  o->setArrayElementSize(t, arrayElementSize);
  o->setArrayDimensions(t, arrayDimensions);
  o->setArrayElementClass(t, arrayElementClass);
  o->setRuntimeDataIndex(t, runtimeDataIndex);
  o->setObjectMask(t, objectMask);
  o->setName(t, name);
  o->setSourceFile(t, sourceFile);
  o->setSuper(t, super);
  o->setInterfaceTable(t, interfaceTable);
  o->setVirtualTable(t, virtualTable);
  o->setFieldTable(t, fieldTable);
  o->setMethodTable(t, methodTable);
  o->setAddendum(t, addendum);
  o->setStaticTable(t, staticTable);
  o->setLoader(t, loader);
  o->setSource(t, source);
  o->setLength(t, length);
}

void initClassAddendum(Thread* t, GcClassAddendum* o, GcSingleton* pool, object annotationTable, object signature, object interfaceTable, object innerClassTable, uint32_t declaredMethodCount, GcByteArray* enclosingClass, GcPair* enclosingMethod)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassAddendumType]));
  o->setPool(t, pool);
  o->setAnnotationTable(t, annotationTable);
  o->setSignature(t, signature);
  o->setInterfaceTable(t, interfaceTable);
  o->setInnerClassTable(t, innerClassTable);
  o->setDeclaredMethodCount(t, declaredMethodCount);
  o->setEnclosingClass(t, enclosingClass);
  o->setEnclosingMethod(t, enclosingMethod);
}

void initClassCastException(Thread* t, GcClassCastException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassCastExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initClassLoader(Thread* t, GcClassLoader* o, GcClassLoader* parent, object packages, object map)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassLoaderType]));
  o->setParent(t, parent);
  o->setPackages(t, packages);
  o->setMap(t, map);
}

void initClassNotFoundException(Thread* t, GcClassNotFoundException* o, GcString* message, object trace, GcThrowable* cause, GcThrowable* cause2)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassNotFoundExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
  o->setCause2(t, cause2);
}

void initClassRuntimeData(Thread* t, GcClassRuntimeData* o, object arrayClass, object jclass, object pool, object signers)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassRuntimeDataType]));
  o->setArrayClass(t, arrayClass);
  o->setJclass(t, jclass);
  o->setPool(t, pool);
  o->setSigners(t, signers);
}

void initCleaner(Thread* t, GcCleaner* o, GcCleaner* queueNext)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CleanerType]));
  o->setQueueNext(t, queueNext);
}

void initCloneNotSupportedException(Thread* t, GcCloneNotSupportedException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CloneNotSupportedExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initCloneable(Thread* t, GcCloneable* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CloneableType]));
}

void initCode(Thread* t, GcCode* o, GcSingleton* pool, GcIntArray* stackMap, object exceptionHandlerTable, GcLineNumberTable* lineNumberTable, intptr_t compiled, uint32_t compiledSize, uint16_t maxStack, uint16_t maxLocals, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CodeType]));
  o->setPool(t, pool);
  o->setStackMap(t, stackMap);
  o->setExceptionHandlerTable(t, exceptionHandlerTable);
  o->setLineNumberTable(t, lineNumberTable);
  o->setCompiled(t, compiled);
  o->setCompiledSize(t, compiledSize);
  o->setMaxStack(t, maxStack);
  o->setMaxLocals(t, maxLocals);
  o->setLength(t, length);
}

void initCompileRoots(Thread* t, GcCompileRoots* o, GcArray* callTable, GcTreeNode* methodTree, GcTreeNode* methodTreeSentinal, object objectPools, object staticTableArray, GcWordArray* virtualThunks, GcMethod* receiveMethod, GcMethod* windMethod, GcMethod* rewindMethod)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CompileRootsType]));
  o->setCallTable(t, callTable);
  o->setMethodTree(t, methodTree);
  o->setMethodTreeSentinal(t, methodTreeSentinal);
  o->setObjectPools(t, objectPools);
  o->setStaticTableArray(t, staticTableArray);
  o->setVirtualThunks(t, virtualThunks);
  o->setReceiveMethod(t, receiveMethod);
  o->setWindMethod(t, windMethod);
  o->setRewindMethod(t, rewindMethod);
}

void initConstantPool(Thread* t, GcConstantPool* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ConstantPoolType]));
}

void initContinuation(Thread* t, GcContinuation* o, GcContinuation* next, GcContinuationContext* context, GcMethod* method, void* address, uintptr_t returnAddressOffset, uintptr_t framePointerOffset, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ContinuationType]));
  o->setNext(t, next);
  o->setContext(t, context);
  o->setMethod(t, method);
  o->setAddress(t, address);
  o->setReturnAddressOffset(t, returnAddressOffset);
  o->setFramePointerOffset(t, framePointerOffset);
  o->setLength(t, length);
}

void initContinuationContext(Thread* t, GcContinuationContext* o, GcContinuationContext* next, object before, object after, object continuation, GcMethod* method)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ContinuationContextType]));
  o->setNext(t, next);
  o->setBefore(t, before);
  o->setAfter(t, after);
  o->setContinuation(t, continuation);
  o->setMethod(t, method);
}

void initDouble(Thread* t, GcDouble* o, uint64_t value)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::DoubleType]));
  o->setValue(t, value);
}

void initDoubleArray(Thread* t, GcDoubleArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::DoubleArrayType]));
  o->setLength(t, length);
}

void initError(Thread* t, GcError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initException(Thread* t, GcException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initExceptionHandlerTable(Thread* t, GcExceptionHandlerTable* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ExceptionHandlerTableType]));
  o->setLength(t, length);
}

void initExceptionInInitializerError(Thread* t, GcExceptionInInitializerError* o, GcString* message, object trace, GcThrowable* cause, GcThrowable* exception)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ExceptionInInitializerErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
  o->setException(t, exception);
}

void initField(Thread* t, GcField* o, uint8_t vmFlags, uint8_t code, uint16_t flags, uint16_t offset, uint32_t nativeID, GcByteArray* name, GcByteArray* spec, GcFieldAddendum* addendum, GcClass* class_)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FieldType]));
  o->setVmFlags(t, vmFlags);
  o->setCode(t, code);
  o->setFlags(t, flags);
  o->setOffset(t, offset);
  o->setNativeID(t, nativeID);
  o->setName(t, name);
  o->setSpec(t, spec);
  o->setAddendum(t, addendum);
  o->setClass(t, class_);
}

void initFieldAddendum(Thread* t, GcFieldAddendum* o, GcSingleton* pool, object annotationTable, object signature)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FieldAddendumType]));
  o->setPool(t, pool);
  o->setAnnotationTable(t, annotationTable);
  o->setSignature(t, signature);
}

void initFileNotFoundException(Thread* t, GcFileNotFoundException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FileNotFoundExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initFinalizer(Thread* t, GcFinalizer* o, object target, void* finalize, object next, object queueTarget, GcFinalizer* queueNext)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FinalizerType]));
  o->setTarget(t, target);
  o->setFinalize(t, finalize);
  o->setNext(t, next);
  o->setQueueTarget(t, queueTarget);
  o->setQueueNext(t, queueNext);
}

void initFinder(Thread* t, GcFinder* o, void* finder, GcByteArray* name, GcFinder* next)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FinderType]));
  o->setFinder(t, finder);
  o->setName(t, name);
  o->setNext(t, next);
}

void initFloat(Thread* t, GcFloat* o, uint32_t value)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FloatType]));
  o->setValue(t, value);
}

void initFloatArray(Thread* t, GcFloatArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FloatArrayType]));
  o->setLength(t, length);
}

void initHashMap(Thread* t, GcHashMap* o, uint32_t size, GcArray* array)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::HashMapType]));
  o->setSize(t, size);
  o->setArray(t, array);
}

void initIllegalArgumentException(Thread* t, GcIllegalArgumentException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IllegalArgumentExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initIllegalMonitorStateException(Thread* t, GcIllegalMonitorStateException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IllegalMonitorStateExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initIllegalStateException(Thread* t, GcIllegalStateException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IllegalStateExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initIncompatibleClassChangeError(Thread* t, GcIncompatibleClassChangeError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IncompatibleClassChangeErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initIncompatibleContinuationException(Thread* t, GcIncompatibleContinuationException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IncompatibleContinuationExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initIndexOutOfBoundsException(Thread* t, GcIndexOutOfBoundsException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IndexOutOfBoundsExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initInnerClassReference(Thread* t, GcInnerClassReference* o, GcByteArray* inner, GcByteArray* outer, GcByteArray* name, uint16_t flags)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::InnerClassReferenceType]));
  o->setInner(t, inner);
  o->setOuter(t, outer);
  o->setName(t, name);
  o->setFlags(t, flags);
}

void initInt(Thread* t, GcInt* o, uint32_t value)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IntType]));
  o->setValue(t, value);
}

void initIntArray(Thread* t, GcIntArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IntArrayType]));
  o->setLength(t, length);
}

void initInterruptedException(Thread* t, GcInterruptedException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::InterruptedExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initInvocation(Thread* t, GcInvocation* o, uint16_t bootstrap, int32_t index, object class_, object pool, object template_, object site)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::InvocationType]));
  o->setBootstrap(t, bootstrap);
  o->setIndex(t, index);
  o->setClass(t, class_);
  o->setPool(t, pool);
  o->setTemplate(t, template_);
  o->setSite(t, site);
}

void initInvocationTargetException(Thread* t, GcInvocationTargetException* o, GcString* message, object trace, GcThrowable* cause, GcThrowable* target)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::InvocationTargetExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
  o->setTarget(t, target);
}

void initIoException(Thread* t, GcIoException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IoExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initJaccessibleObject(Thread* t, GcJaccessibleObject* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JaccessibleObjectType]));
}

void initJboolean(Thread* t, GcJboolean* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JbooleanType]));
}

void initJbyte(Thread* t, GcJbyte* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JbyteType]));
}

void initJchar(Thread* t, GcJchar* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JcharType]));
}

void initJclass(Thread* t, GcJclass* o, GcClass* vmClass)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JclassType]));
  o->setVmClass(t, vmClass);
}

void initJconstructor(Thread* t, GcJconstructor* o, GcJmethod* method)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JconstructorType]));
  o->setMethod(t, method);
}

void initJdouble(Thread* t, GcJdouble* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JdoubleType]));
}

void initJfield(Thread* t, GcJfield* o, GcField* vmField, uint8_t accessible)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JfieldType]));
  o->setVmField(t, vmField);
  o->setAccessible(t, accessible);
}

void initJfloat(Thread* t, GcJfloat* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JfloatType]));
}

void initJint(Thread* t, GcJint* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JintType]));
}

void initJlong(Thread* t, GcJlong* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JlongType]));
}

void initJmethod(Thread* t, GcJmethod* o, GcMethod* vmMethod, uint8_t accessible)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JmethodType]));
  o->setVmMethod(t, vmMethod);
  o->setAccessible(t, accessible);
}

void initJobject(Thread* t, GcJobject* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JobjectType]));
}

void initJreference(Thread* t, GcJreference* o, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JreferenceType]));
  o->setVmNext(t, vmNext);
  o->setTarget(t, target);
  o->setQueue(t, queue);
  o->setJNext(t, jNext);
}

void initJshort(Thread* t, GcJshort* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JshortType]));
}

void initJvoid(Thread* t, GcJvoid* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JvoidType]));
}

void initLineNumberTable(Thread* t, GcLineNumberTable* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::LineNumberTableType]));
  o->setLength(t, length);
}

void initLinkageError(Thread* t, GcLinkageError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::LinkageErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initList(Thread* t, GcList* o, uint32_t size, object front, object rear)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ListType]));
  o->setSize(t, size);
  o->setFront(t, front);
  o->setRear(t, rear);
}

void initLong(Thread* t, GcLong* o, uint64_t value)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::LongType]));
  o->setValue(t, value);
}

void initLongArray(Thread* t, GcLongArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::LongArrayType]));
  o->setLength(t, length);
}

void initMethod(Thread* t, GcMethod* o, uint8_t vmFlags, uint8_t returnCode, uint8_t parameterCount, uint8_t parameterFootprint, uint16_t flags, uint16_t offset, uint32_t nativeID, uint32_t runtimeDataIndex, GcByteArray* name, GcByteArray* spec, GcMethodAddendum* addendum, GcClass* class_, GcCode* code)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MethodType]));
  o->setVmFlags(t, vmFlags);
  o->setReturnCode(t, returnCode);
  o->setParameterCount(t, parameterCount);
  o->setParameterFootprint(t, parameterFootprint);
  o->setFlags(t, flags);
  o->setOffset(t, offset);
  o->setNativeID(t, nativeID);
  o->setRuntimeDataIndex(t, runtimeDataIndex);
  o->setName(t, name);
  o->setSpec(t, spec);
  o->setAddendum(t, addendum);
  o->setClass(t, class_);
  o->setCode(t, code);
}

void initMethodAddendum(Thread* t, GcMethodAddendum* o, GcSingleton* pool, object annotationTable, object signature, object exceptionTable, object annotationDefault, object parameterAnnotationTable)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MethodAddendumType]));
  o->setPool(t, pool);
  o->setAnnotationTable(t, annotationTable);
  o->setSignature(t, signature);
  o->setExceptionTable(t, exceptionTable);
  o->setAnnotationDefault(t, annotationDefault);
  o->setParameterAnnotationTable(t, parameterAnnotationTable);
}

void initMethodRuntimeData(Thread* t, GcMethodRuntimeData* o, GcNative* native)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MethodRuntimeDataType]));
  o->setNative(t, native);
}

void initMonitor(Thread* t, GcMonitor* o, void* owner, void* waitHead, void* waitTail, object acquireHead, object acquireTail, uint32_t depth)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MonitorType]));
  o->setOwner(t, owner);
  o->setWaitHead(t, waitHead);
  o->setWaitTail(t, waitTail);
  o->setAcquireHead(t, acquireHead);
  o->setAcquireTail(t, acquireTail);
  o->setDepth(t, depth);
}

void initMonitorNode(Thread* t, GcMonitorNode* o, void* value, object next)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MonitorNodeType]));
  o->setValue(t, value);
  o->setNext(t, next);
}

void initNative(Thread* t, GcNative* o, void* function, uint8_t fast)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NativeType]));
  o->setFunction(t, function);
  o->setFast(t, fast);
}

void initNativeIntercept(Thread* t, GcNativeIntercept* o, void* function, uint8_t fast, object original)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NativeInterceptType]));
  o->setFunction(t, function);
  o->setFast(t, fast);
  o->setOriginal(t, original);
}

void initNegativeArraySizeException(Thread* t, GcNegativeArraySizeException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NegativeArraySizeExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initNoClassDefFoundError(Thread* t, GcNoClassDefFoundError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NoClassDefFoundErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initNoSuchFieldError(Thread* t, GcNoSuchFieldError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NoSuchFieldErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initNoSuchMethodError(Thread* t, GcNoSuchMethodError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NoSuchMethodErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initNullPointerException(Thread* t, GcNullPointerException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NullPointerExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initNumber(Thread* t, GcNumber* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NumberType]));
}

void initOutOfMemoryError(Thread* t, GcOutOfMemoryError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::OutOfMemoryErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initPair(Thread* t, GcPair* o, object first, object second)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::PairType]));
  o->setFirst(t, first);
  o->setSecond(t, second);
}

void initPhantomReference(Thread* t, GcPhantomReference* o, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::PhantomReferenceType]));
  o->setVmNext(t, vmNext);
  o->setTarget(t, target);
  o->setQueue(t, queue);
  o->setJNext(t, jNext);
}

void initPointer(Thread* t, GcPointer* o, void* value)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::PointerType]));
  o->setValue(t, value);
}

void initReference(Thread* t, GcReference* o, uint8_t kind, GcByteArray* class_, GcByteArray* name, GcByteArray* spec)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ReferenceType]));
  o->setKind(t, kind);
  o->setClass(t, class_);
  o->setName(t, name);
  o->setSpec(t, spec);
}

void initReferenceQueue(Thread* t, GcReferenceQueue* o, GcJreference* front, object jnext)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ReferenceQueueType]));
  o->setFront(t, front);
  o->setJnext(t, jnext);
}

void initReflectiveOperationException(Thread* t, GcReflectiveOperationException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ReflectiveOperationExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initRegion(Thread* t, GcRegion* o, void* region, uint32_t position)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::RegionType]));
  o->setRegion(t, region);
  o->setPosition(t, position);
}

void initRoots(Thread* t, GcRoots* o, GcClassLoader* bootLoader, GcClassLoader* appLoader, GcHashMap* bootstrapClassMap, GcHashMap* packageMap, GcMethod* findLoadedClassMethod, GcMethod* loadClassMethod, GcHashMap* monitorMap, GcHashMap* stringMap, GcHashMap* byteArrayMap, GcHashMap* poolMap, GcVector* classRuntimeDataTable, GcVector* methodRuntimeDataTable, GcVector* jNIMethodTable, GcVector* jNIFieldTable, GcPair* shutdownHooks, GcThread* finalizerThread, GcFinalizer* objectsToFinalize, GcCleaner* objectsToClean, GcThrowable* nullPointerException, GcThrowable* arithmeticException, GcThrowable* arrayIndexOutOfBoundsException, GcThrowable* outOfMemoryError, GcThrowable* shutdownInProgress, GcFinder* virtualFileFinders, GcArray* virtualFiles, GcArray* arrayInterfaceTable, object threadTerminated)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::RootsType]));
  o->setBootLoader(t, bootLoader);
  o->setAppLoader(t, appLoader);
  o->setBootstrapClassMap(t, bootstrapClassMap);
  o->setPackageMap(t, packageMap);
  o->setFindLoadedClassMethod(t, findLoadedClassMethod);
  o->setLoadClassMethod(t, loadClassMethod);
  o->setMonitorMap(t, monitorMap);
  o->setStringMap(t, stringMap);
  o->setByteArrayMap(t, byteArrayMap);
  o->setPoolMap(t, poolMap);
  o->setClassRuntimeDataTable(t, classRuntimeDataTable);
  o->setMethodRuntimeDataTable(t, methodRuntimeDataTable);
  o->setJNIMethodTable(t, jNIMethodTable);
  o->setJNIFieldTable(t, jNIFieldTable);
  o->setShutdownHooks(t, shutdownHooks);
  o->setFinalizerThread(t, finalizerThread);
  o->setObjectsToFinalize(t, objectsToFinalize);
  o->setObjectsToClean(t, objectsToClean);
  o->setNullPointerException(t, nullPointerException);
  o->setArithmeticException(t, arithmeticException);
  o->setArrayIndexOutOfBoundsException(t, arrayIndexOutOfBoundsException);
  o->setOutOfMemoryError(t, outOfMemoryError);
  o->setShutdownInProgress(t, shutdownInProgress);
  o->setVirtualFileFinders(t, virtualFileFinders);
  o->setVirtualFiles(t, virtualFiles);
  o->setArrayInterfaceTable(t, arrayInterfaceTable);
  o->setThreadTerminated(t, threadTerminated);
}

void initRuntimeException(Thread* t, GcRuntimeException* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::RuntimeExceptionType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initSerializable(Thread* t, GcSerializable* o)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::SerializableType]));
}

void initShort(Thread* t, GcShort* o, uint16_t value)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ShortType]));
  o->setValue(t, value);
}

void initShortArray(Thread* t, GcShortArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ShortArrayType]));
  o->setLength(t, length);
}

void initSingleton(Thread* t, GcSingleton* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::SingletonType]));
  o->setLength(t, length);
}

void initSoftReference(Thread* t, GcSoftReference* o, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::SoftReferenceType]));
  o->setVmNext(t, vmNext);
  o->setTarget(t, target);
  o->setQueue(t, queue);
  o->setJNext(t, jNext);
}

void initStackOverflowError(Thread* t, GcStackOverflowError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::StackOverflowErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initStackTraceElement(Thread* t, GcStackTraceElement* o, GcString* class_, GcString* method, GcString* file, uint32_t line)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::StackTraceElementType]));
  o->setClass(t, class_);
  o->setMethod(t, method);
  o->setFile(t, file);
  o->setLine(t, line);
}

void initString(Thread* t, GcString* o, object data, uint32_t offset, uint32_t length, uint32_t hashCode)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::StringType]));
  o->setData(t, data);
  o->setOffset(t, offset);
  o->setLength(t, length);
  o->setHashCode(t, hashCode);
}

void initSystemClassLoader(Thread* t, GcSystemClassLoader* o, GcClassLoader* parent, object packages, object map, void* finder)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::SystemClassLoaderType]));
  o->setParent(t, parent);
  o->setPackages(t, packages);
  o->setMap(t, map);
  o->setFinder(t, finder);
}

void initThread(Thread* t, GcThread* o, object parkBlocker, uint64_t peer, uint8_t interrupted, uint8_t unparked, uint8_t daemon, uint8_t state, uint8_t priority, object task, object locals, object sleepLock, GcClassLoader* classLoader, object exceptionHandler, GcString* name, GcThreadGroup* group, object interruptLock)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ThreadType]));
  o->setParkBlocker(t, parkBlocker);
  o->setPeer(t, peer);
  o->setInterrupted(t, interrupted);
  o->setUnparked(t, unparked);
  o->setDaemon(t, daemon);
  o->setState(t, state);
  o->setPriority(t, priority);
  o->setTask(t, task);
  o->setLocals(t, locals);
  o->setSleepLock(t, sleepLock);
  o->setClassLoader(t, classLoader);
  o->setExceptionHandler(t, exceptionHandler);
  o->setName(t, name);
  o->setGroup(t, group);
  o->setInterruptLock(t, interruptLock);
}

void initThreadGroup(Thread* t, GcThreadGroup* o, GcThreadGroup* parent, GcString* name, object subgroups)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ThreadGroupType]));
  o->setParent(t, parent);
  o->setName(t, name);
  o->setSubgroups(t, subgroups);
}

void initThrowable(Thread* t, GcThrowable* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ThrowableType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initTraceElement(Thread* t, GcTraceElement* o, object method, int32_t ip)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::TraceElementType]));
  o->setMethod(t, method);
  o->setIp(t, ip);
}

void initTreeNode(Thread* t, GcTreeNode* o, object value, GcTreeNode* left, GcTreeNode* right)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::TreeNodeType]));
  o->setValue(t, value);
  o->setLeft(t, left);
  o->setRight(t, right);
}

void initTriple(Thread* t, GcTriple* o, object first, object second, object third)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::TripleType]));
  o->setFirst(t, first);
  o->setSecond(t, second);
  o->setThird(t, third);
}

void initUnsatisfiedLinkError(Thread* t, GcUnsatisfiedLinkError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::UnsatisfiedLinkErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initUnwindResult(Thread* t, GcUnwindResult* o, GcContinuation* continuation, object result, GcThrowable* exception)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::UnwindResultType]));
  o->setContinuation(t, continuation);
  o->setResult(t, result);
  o->setException(t, exception);
}

void initVector(Thread* t, GcVector* o, uint32_t size, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::VectorType]));
  o->setSize(t, size);
  o->setLength(t, length);
}

void initVirtualMachineError(Thread* t, GcVirtualMachineError* o, GcString* message, object trace, GcThrowable* cause)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::VirtualMachineErrorType]));
  o->setMessage(t, message);
  o->setTrace(t, trace);
  o->setCause(t, cause);
}

void initWeakHashMap(Thread* t, GcWeakHashMap* o, uint32_t size, GcArray* array)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::WeakHashMapType]));
  o->setSize(t, size);
  o->setArray(t, array);
}

void initWeakReference(Thread* t, GcWeakReference* o, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::WeakReferenceType]));
  o->setVmNext(t, vmNext);
  o->setTarget(t, target);
  o->setQueue(t, queue);
  o->setJNext(t, jNext);
}

void initWordArray(Thread* t, GcWordArray* o, uintptr_t length)
{
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::WordArrayType]));
  o->setLength(t, length);
}

GcAbstractMethodError* GcAbstractMethodError::makeZeroed(Thread* t)
{
  GcAbstractMethodError* o = reinterpret_cast<GcAbstractMethodError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::AbstractMethodErrorType]));
  return o;
}

GcAbstractMethodError* makeAbstractMethodError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcAbstractMethodError* o = reinterpret_cast<GcAbstractMethodError*>(allocate(t, 16, true));
  initAbstractMethodError(t, o, message, trace, cause);
  return o;
}

GcAddendum* GcAddendum::makeZeroed(Thread* t)
{
  GcAddendum* o = reinterpret_cast<GcAddendum*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::AddendumType]));
  return o;
}

GcAddendum* makeAddendum(Thread* t, GcSingleton* pool, object annotationTable, object signature)
{
  PROTECT(t, pool);
  PROTECT(t, annotationTable);
  PROTECT(t, signature);
  GcAddendum* o = reinterpret_cast<GcAddendum*>(allocate(t, 16, true));
  initAddendum(t, o, pool, annotationTable, signature);
  return o;
}

GcArithmeticException* GcArithmeticException::makeZeroed(Thread* t)
{
  GcArithmeticException* o = reinterpret_cast<GcArithmeticException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ArithmeticExceptionType]));
  return o;
}

GcArithmeticException* makeArithmeticException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcArithmeticException* o = reinterpret_cast<GcArithmeticException*>(allocate(t, 16, true));
  initArithmeticException(t, o, message, trace, cause);
  return o;
}

GcArray* GcArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcArray* o = reinterpret_cast<GcArray*>(allocate(t, 8 + pad(length * 4), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ArrayType]));
  return o;
}

GcArray* makeArray(Thread* t, uintptr_t length)
{
  GcArray* o = reinterpret_cast<GcArray*>(allocate(t, 8 + pad(length * 4), true));
  initArray(t, o, length);
  return o;
}

GcArrayIndexOutOfBoundsException* GcArrayIndexOutOfBoundsException::makeZeroed(Thread* t)
{
  GcArrayIndexOutOfBoundsException* o = reinterpret_cast<GcArrayIndexOutOfBoundsException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ArrayIndexOutOfBoundsExceptionType]));
  return o;
}

GcArrayIndexOutOfBoundsException* makeArrayIndexOutOfBoundsException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcArrayIndexOutOfBoundsException* o = reinterpret_cast<GcArrayIndexOutOfBoundsException*>(allocate(t, 16, true));
  initArrayIndexOutOfBoundsException(t, o, message, trace, cause);
  return o;
}

GcArrayStoreException* GcArrayStoreException::makeZeroed(Thread* t)
{
  GcArrayStoreException* o = reinterpret_cast<GcArrayStoreException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ArrayStoreExceptionType]));
  return o;
}

GcArrayStoreException* makeArrayStoreException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcArrayStoreException* o = reinterpret_cast<GcArrayStoreException*>(allocate(t, 16, true));
  initArrayStoreException(t, o, message, trace, cause);
  return o;
}

GcBoolean* GcBoolean::makeZeroed(Thread* t)
{
  GcBoolean* o = reinterpret_cast<GcBoolean*>(allocate(t, 5, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::BooleanType]));
  return o;
}

GcBoolean* makeBoolean(Thread* t, uint8_t value)
{
  GcBoolean* o = reinterpret_cast<GcBoolean*>(allocate(t, 5, false));
  initBoolean(t, o, value);
  return o;
}

GcBooleanArray* GcBooleanArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcBooleanArray* o = reinterpret_cast<GcBooleanArray*>(allocate(t, 8 + pad(length * 1), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::BooleanArrayType]));
  return o;
}

GcBooleanArray* makeBooleanArray(Thread* t, uintptr_t length)
{
  GcBooleanArray* o = reinterpret_cast<GcBooleanArray*>(allocate(t, 8 + pad(length * 1), false));
  initBooleanArray(t, o, length);
  return o;
}

GcByte* GcByte::makeZeroed(Thread* t)
{
  GcByte* o = reinterpret_cast<GcByte*>(allocate(t, 5, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ByteType]));
  return o;
}

GcByte* makeByte(Thread* t, uint8_t value)
{
  GcByte* o = reinterpret_cast<GcByte*>(allocate(t, 5, false));
  initByte(t, o, value);
  return o;
}

GcByteArray* GcByteArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcByteArray* o = reinterpret_cast<GcByteArray*>(allocate(t, 8 + pad(length * 1), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ByteArrayType]));
  return o;
}

GcByteArray* makeByteArray(Thread* t, uintptr_t length)
{
  GcByteArray* o = reinterpret_cast<GcByteArray*>(allocate(t, 8 + pad(length * 1), false));
  initByteArray(t, o, length);
  return o;
}

GcCallNode* GcCallNode::makeZeroed(Thread* t)
{
  GcCallNode* o = reinterpret_cast<GcCallNode*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CallNodeType]));
  return o;
}

GcCallNode* makeCallNode(Thread* t, intptr_t address, GcMethod* target, uintptr_t flags, GcCallNode* next)
{
  PROTECT(t, target);
  PROTECT(t, next);
  GcCallNode* o = reinterpret_cast<GcCallNode*>(allocate(t, 20, true));
  initCallNode(t, o, address, target, flags, next);
  return o;
}

GcChar* GcChar::makeZeroed(Thread* t)
{
  GcChar* o = reinterpret_cast<GcChar*>(allocate(t, 6, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CharType]));
  return o;
}

GcChar* makeChar(Thread* t, uint16_t value)
{
  GcChar* o = reinterpret_cast<GcChar*>(allocate(t, 6, false));
  initChar(t, o, value);
  return o;
}

GcCharArray* GcCharArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcCharArray* o = reinterpret_cast<GcCharArray*>(allocate(t, 8 + pad(length * 2), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CharArrayType]));
  return o;
}

GcCharArray* makeCharArray(Thread* t, uintptr_t length)
{
  GcCharArray* o = reinterpret_cast<GcCharArray*>(allocate(t, 8 + pad(length * 2), false));
  initCharArray(t, o, length);
  return o;
}

GcClass* GcClass::makeZeroed(Thread* t, uintptr_t length)
{
  GcClass* o = reinterpret_cast<GcClass*>(allocate(t, 72 + pad(length * 4), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassType]));
  return o;
}

GcClass* makeClass(Thread* t, uint16_t flags, uint16_t vmFlags, uint16_t fixedSize, uint8_t arrayElementSize, uint8_t arrayDimensions, GcClass* arrayElementClass, uint32_t runtimeDataIndex, GcIntArray* objectMask, GcByteArray* name, GcByteArray* sourceFile, GcClass* super, object interfaceTable, object virtualTable, object fieldTable, object methodTable, GcClassAddendum* addendum, GcSingleton* staticTable, GcClassLoader* loader, GcByteArray* source, uintptr_t length)
{
  PROTECT(t, arrayElementClass);
  PROTECT(t, objectMask);
  PROTECT(t, name);
  PROTECT(t, sourceFile);
  PROTECT(t, super);
  PROTECT(t, interfaceTable);
  PROTECT(t, virtualTable);
  PROTECT(t, fieldTable);
  PROTECT(t, methodTable);
  PROTECT(t, addendum);
  PROTECT(t, staticTable);
  PROTECT(t, loader);
  PROTECT(t, source);
  GcClass* o = reinterpret_cast<GcClass*>(allocate(t, 72 + pad(length * 4), true));
  initClass(t, o, flags, vmFlags, fixedSize, arrayElementSize, arrayDimensions, arrayElementClass, runtimeDataIndex, objectMask, name, sourceFile, super, interfaceTable, virtualTable, fieldTable, methodTable, addendum, staticTable, loader, source, length);
  return o;
}

GcClassAddendum* GcClassAddendum::makeZeroed(Thread* t)
{
  GcClassAddendum* o = reinterpret_cast<GcClassAddendum*>(allocate(t, 36, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassAddendumType]));
  return o;
}

GcClassAddendum* makeClassAddendum(Thread* t, GcSingleton* pool, object annotationTable, object signature, object interfaceTable, object innerClassTable, uint32_t declaredMethodCount, GcByteArray* enclosingClass, GcPair* enclosingMethod)
{
  PROTECT(t, pool);
  PROTECT(t, annotationTable);
  PROTECT(t, signature);
  PROTECT(t, interfaceTable);
  PROTECT(t, innerClassTable);
  PROTECT(t, enclosingClass);
  PROTECT(t, enclosingMethod);
  GcClassAddendum* o = reinterpret_cast<GcClassAddendum*>(allocate(t, 36, true));
  initClassAddendum(t, o, pool, annotationTable, signature, interfaceTable, innerClassTable, declaredMethodCount, enclosingClass, enclosingMethod);
  return o;
}

GcClassCastException* GcClassCastException::makeZeroed(Thread* t)
{
  GcClassCastException* o = reinterpret_cast<GcClassCastException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassCastExceptionType]));
  return o;
}

GcClassCastException* makeClassCastException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcClassCastException* o = reinterpret_cast<GcClassCastException*>(allocate(t, 16, true));
  initClassCastException(t, o, message, trace, cause);
  return o;
}

GcClassLoader* GcClassLoader::makeZeroed(Thread* t)
{
  GcClassLoader* o = reinterpret_cast<GcClassLoader*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassLoaderType]));
  return o;
}

GcClassLoader* makeClassLoader(Thread* t, GcClassLoader* parent, object packages, object map)
{
  PROTECT(t, parent);
  PROTECT(t, packages);
  PROTECT(t, map);
  GcClassLoader* o = reinterpret_cast<GcClassLoader*>(allocate(t, 16, true));
  initClassLoader(t, o, parent, packages, map);
  return o;
}

GcClassNotFoundException* GcClassNotFoundException::makeZeroed(Thread* t)
{
  GcClassNotFoundException* o = reinterpret_cast<GcClassNotFoundException*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassNotFoundExceptionType]));
  return o;
}

GcClassNotFoundException* makeClassNotFoundException(Thread* t, GcString* message, object trace, GcThrowable* cause, GcThrowable* cause2)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  PROTECT(t, cause2);
  GcClassNotFoundException* o = reinterpret_cast<GcClassNotFoundException*>(allocate(t, 20, true));
  initClassNotFoundException(t, o, message, trace, cause, cause2);
  return o;
}

GcClassRuntimeData* GcClassRuntimeData::makeZeroed(Thread* t)
{
  GcClassRuntimeData* o = reinterpret_cast<GcClassRuntimeData*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ClassRuntimeDataType]));
  return o;
}

GcClassRuntimeData* makeClassRuntimeData(Thread* t, object arrayClass, object jclass, object pool, object signers)
{
  PROTECT(t, arrayClass);
  PROTECT(t, jclass);
  PROTECT(t, pool);
  PROTECT(t, signers);
  GcClassRuntimeData* o = reinterpret_cast<GcClassRuntimeData*>(allocate(t, 20, true));
  initClassRuntimeData(t, o, arrayClass, jclass, pool, signers);
  return o;
}

GcCleaner* GcCleaner::makeZeroed(Thread* t)
{
  GcCleaner* o = reinterpret_cast<GcCleaner*>(allocate(t, 8, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CleanerType]));
  return o;
}

GcCleaner* makeCleaner(Thread* t, GcCleaner* queueNext)
{
  PROTECT(t, queueNext);
  GcCleaner* o = reinterpret_cast<GcCleaner*>(allocate(t, 8, true));
  initCleaner(t, o, queueNext);
  return o;
}

GcCloneNotSupportedException* GcCloneNotSupportedException::makeZeroed(Thread* t)
{
  GcCloneNotSupportedException* o = reinterpret_cast<GcCloneNotSupportedException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CloneNotSupportedExceptionType]));
  return o;
}

GcCloneNotSupportedException* makeCloneNotSupportedException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcCloneNotSupportedException* o = reinterpret_cast<GcCloneNotSupportedException*>(allocate(t, 16, true));
  initCloneNotSupportedException(t, o, message, trace, cause);
  return o;
}

GcCloneable* GcCloneable::makeZeroed(Thread* t)
{
  GcCloneable* o = reinterpret_cast<GcCloneable*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CloneableType]));
  return o;
}

GcCloneable* makeCloneable(Thread* t)
{
  GcCloneable* o = reinterpret_cast<GcCloneable*>(allocate(t, 4, false));
  initCloneable(t, o);
  return o;
}

GcCode* GcCode::makeZeroed(Thread* t, uintptr_t length)
{
  GcCode* o = reinterpret_cast<GcCode*>(allocate(t, 36 + pad(length * 1), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CodeType]));
  return o;
}

GcCode* makeCode(Thread* t, GcSingleton* pool, GcIntArray* stackMap, object exceptionHandlerTable, GcLineNumberTable* lineNumberTable, intptr_t compiled, uint32_t compiledSize, uint16_t maxStack, uint16_t maxLocals, uintptr_t length)
{
  PROTECT(t, pool);
  PROTECT(t, stackMap);
  PROTECT(t, exceptionHandlerTable);
  PROTECT(t, lineNumberTable);
  GcCode* o = reinterpret_cast<GcCode*>(allocate(t, 36 + pad(length * 1), true));
  initCode(t, o, pool, stackMap, exceptionHandlerTable, lineNumberTable, compiled, compiledSize, maxStack, maxLocals, length);
  return o;
}

GcCompileRoots* GcCompileRoots::makeZeroed(Thread* t)
{
  GcCompileRoots* o = reinterpret_cast<GcCompileRoots*>(allocate(t, 40, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::CompileRootsType]));
  return o;
}

GcCompileRoots* makeCompileRoots(Thread* t, GcArray* callTable, GcTreeNode* methodTree, GcTreeNode* methodTreeSentinal, object objectPools, object staticTableArray, GcWordArray* virtualThunks, GcMethod* receiveMethod, GcMethod* windMethod, GcMethod* rewindMethod)
{
  PROTECT(t, callTable);
  PROTECT(t, methodTree);
  PROTECT(t, methodTreeSentinal);
  PROTECT(t, objectPools);
  PROTECT(t, staticTableArray);
  PROTECT(t, virtualThunks);
  PROTECT(t, receiveMethod);
  PROTECT(t, windMethod);
  PROTECT(t, rewindMethod);
  GcCompileRoots* o = reinterpret_cast<GcCompileRoots*>(allocate(t, 40, true));
  initCompileRoots(t, o, callTable, methodTree, methodTreeSentinal, objectPools, staticTableArray, virtualThunks, receiveMethod, windMethod, rewindMethod);
  return o;
}

GcConstantPool* GcConstantPool::makeZeroed(Thread* t)
{
  GcConstantPool* o = reinterpret_cast<GcConstantPool*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ConstantPoolType]));
  return o;
}

GcConstantPool* makeConstantPool(Thread* t)
{
  GcConstantPool* o = reinterpret_cast<GcConstantPool*>(allocate(t, 4, false));
  initConstantPool(t, o);
  return o;
}

GcContinuation* GcContinuation::makeZeroed(Thread* t, uintptr_t length)
{
  GcContinuation* o = reinterpret_cast<GcContinuation*>(allocate(t, 32 + pad(length * 4), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ContinuationType]));
  return o;
}

GcContinuation* makeContinuation(Thread* t, GcContinuation* next, GcContinuationContext* context, GcMethod* method, void* address, uintptr_t returnAddressOffset, uintptr_t framePointerOffset, uintptr_t length)
{
  PROTECT(t, next);
  PROTECT(t, context);
  PROTECT(t, method);
  GcContinuation* o = reinterpret_cast<GcContinuation*>(allocate(t, 32 + pad(length * 4), true));
  initContinuation(t, o, next, context, method, address, returnAddressOffset, framePointerOffset, length);
  return o;
}

GcContinuationContext* GcContinuationContext::makeZeroed(Thread* t)
{
  GcContinuationContext* o = reinterpret_cast<GcContinuationContext*>(allocate(t, 24, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ContinuationContextType]));
  return o;
}

GcContinuationContext* makeContinuationContext(Thread* t, GcContinuationContext* next, object before, object after, object continuation, GcMethod* method)
{
  PROTECT(t, next);
  PROTECT(t, before);
  PROTECT(t, after);
  PROTECT(t, continuation);
  PROTECT(t, method);
  GcContinuationContext* o = reinterpret_cast<GcContinuationContext*>(allocate(t, 24, true));
  initContinuationContext(t, o, next, before, after, continuation, method);
  return o;
}

GcDouble* GcDouble::makeZeroed(Thread* t)
{
  GcDouble* o = reinterpret_cast<GcDouble*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::DoubleType]));
  return o;
}

GcDouble* makeDouble(Thread* t, uint64_t value)
{
  GcDouble* o = reinterpret_cast<GcDouble*>(allocate(t, 16, false));
  initDouble(t, o, value);
  return o;
}

GcDoubleArray* GcDoubleArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcDoubleArray* o = reinterpret_cast<GcDoubleArray*>(allocate(t, 8 + pad(length * 8), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::DoubleArrayType]));
  return o;
}

GcDoubleArray* makeDoubleArray(Thread* t, uintptr_t length)
{
  GcDoubleArray* o = reinterpret_cast<GcDoubleArray*>(allocate(t, 8 + pad(length * 8), false));
  initDoubleArray(t, o, length);
  return o;
}

GcError* GcError::makeZeroed(Thread* t)
{
  GcError* o = reinterpret_cast<GcError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ErrorType]));
  return o;
}

GcError* makeError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcError* o = reinterpret_cast<GcError*>(allocate(t, 16, true));
  initError(t, o, message, trace, cause);
  return o;
}

GcException* GcException::makeZeroed(Thread* t)
{
  GcException* o = reinterpret_cast<GcException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ExceptionType]));
  return o;
}

GcException* makeException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcException* o = reinterpret_cast<GcException*>(allocate(t, 16, true));
  initException(t, o, message, trace, cause);
  return o;
}

GcExceptionHandlerTable* GcExceptionHandlerTable::makeZeroed(Thread* t, uintptr_t length)
{
  GcExceptionHandlerTable* o = reinterpret_cast<GcExceptionHandlerTable*>(allocate(t, 8 + pad(length * 8), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ExceptionHandlerTableType]));
  return o;
}

GcExceptionHandlerTable* makeExceptionHandlerTable(Thread* t, uintptr_t length)
{
  GcExceptionHandlerTable* o = reinterpret_cast<GcExceptionHandlerTable*>(allocate(t, 8 + pad(length * 8), false));
  initExceptionHandlerTable(t, o, length);
  return o;
}

GcExceptionInInitializerError* GcExceptionInInitializerError::makeZeroed(Thread* t)
{
  GcExceptionInInitializerError* o = reinterpret_cast<GcExceptionInInitializerError*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ExceptionInInitializerErrorType]));
  return o;
}

GcExceptionInInitializerError* makeExceptionInInitializerError(Thread* t, GcString* message, object trace, GcThrowable* cause, GcThrowable* exception)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  PROTECT(t, exception);
  GcExceptionInInitializerError* o = reinterpret_cast<GcExceptionInInitializerError*>(allocate(t, 20, true));
  initExceptionInInitializerError(t, o, message, trace, cause, exception);
  return o;
}

GcField* GcField::makeZeroed(Thread* t)
{
  GcField* o = reinterpret_cast<GcField*>(allocate(t, 32, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FieldType]));
  return o;
}

GcField* makeField(Thread* t, uint8_t vmFlags, uint8_t code, uint16_t flags, uint16_t offset, uint32_t nativeID, GcByteArray* name, GcByteArray* spec, GcFieldAddendum* addendum, GcClass* class_)
{
  PROTECT(t, name);
  PROTECT(t, spec);
  PROTECT(t, addendum);
  PROTECT(t, class_);
  GcField* o = reinterpret_cast<GcField*>(allocate(t, 32, true));
  initField(t, o, vmFlags, code, flags, offset, nativeID, name, spec, addendum, class_);
  return o;
}

GcFieldAddendum* GcFieldAddendum::makeZeroed(Thread* t)
{
  GcFieldAddendum* o = reinterpret_cast<GcFieldAddendum*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FieldAddendumType]));
  return o;
}

GcFieldAddendum* makeFieldAddendum(Thread* t, GcSingleton* pool, object annotationTable, object signature)
{
  PROTECT(t, pool);
  PROTECT(t, annotationTable);
  PROTECT(t, signature);
  GcFieldAddendum* o = reinterpret_cast<GcFieldAddendum*>(allocate(t, 16, true));
  initFieldAddendum(t, o, pool, annotationTable, signature);
  return o;
}

GcFileNotFoundException* GcFileNotFoundException::makeZeroed(Thread* t)
{
  GcFileNotFoundException* o = reinterpret_cast<GcFileNotFoundException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FileNotFoundExceptionType]));
  return o;
}

GcFileNotFoundException* makeFileNotFoundException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcFileNotFoundException* o = reinterpret_cast<GcFileNotFoundException*>(allocate(t, 16, true));
  initFileNotFoundException(t, o, message, trace, cause);
  return o;
}

GcFinalizer* GcFinalizer::makeZeroed(Thread* t)
{
  GcFinalizer* o = reinterpret_cast<GcFinalizer*>(allocate(t, 24, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FinalizerType]));
  return o;
}

GcFinalizer* makeFinalizer(Thread* t, object target, void* finalize, object next, object queueTarget, GcFinalizer* queueNext)
{
  PROTECT(t, queueTarget);
  PROTECT(t, queueNext);
  GcFinalizer* o = reinterpret_cast<GcFinalizer*>(allocate(t, 24, true));
  initFinalizer(t, o, target, finalize, next, queueTarget, queueNext);
  return o;
}

GcFinder* GcFinder::makeZeroed(Thread* t)
{
  GcFinder* o = reinterpret_cast<GcFinder*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FinderType]));
  return o;
}

GcFinder* makeFinder(Thread* t, void* finder, GcByteArray* name, GcFinder* next)
{
  PROTECT(t, name);
  PROTECT(t, next);
  GcFinder* o = reinterpret_cast<GcFinder*>(allocate(t, 16, true));
  initFinder(t, o, finder, name, next);
  return o;
}

GcFloat* GcFloat::makeZeroed(Thread* t)
{
  GcFloat* o = reinterpret_cast<GcFloat*>(allocate(t, 8, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FloatType]));
  return o;
}

GcFloat* makeFloat(Thread* t, uint32_t value)
{
  GcFloat* o = reinterpret_cast<GcFloat*>(allocate(t, 8, false));
  initFloat(t, o, value);
  return o;
}

GcFloatArray* GcFloatArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcFloatArray* o = reinterpret_cast<GcFloatArray*>(allocate(t, 8 + pad(length * 4), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::FloatArrayType]));
  return o;
}

GcFloatArray* makeFloatArray(Thread* t, uintptr_t length)
{
  GcFloatArray* o = reinterpret_cast<GcFloatArray*>(allocate(t, 8 + pad(length * 4), false));
  initFloatArray(t, o, length);
  return o;
}

GcHashMap* GcHashMap::makeZeroed(Thread* t)
{
  GcHashMap* o = reinterpret_cast<GcHashMap*>(allocate(t, 12, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::HashMapType]));
  return o;
}

GcHashMap* makeHashMap(Thread* t, uint32_t size, GcArray* array)
{
  PROTECT(t, array);
  GcHashMap* o = reinterpret_cast<GcHashMap*>(allocate(t, 12, true));
  initHashMap(t, o, size, array);
  return o;
}

GcIllegalArgumentException* GcIllegalArgumentException::makeZeroed(Thread* t)
{
  GcIllegalArgumentException* o = reinterpret_cast<GcIllegalArgumentException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IllegalArgumentExceptionType]));
  return o;
}

GcIllegalArgumentException* makeIllegalArgumentException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcIllegalArgumentException* o = reinterpret_cast<GcIllegalArgumentException*>(allocate(t, 16, true));
  initIllegalArgumentException(t, o, message, trace, cause);
  return o;
}

GcIllegalMonitorStateException* GcIllegalMonitorStateException::makeZeroed(Thread* t)
{
  GcIllegalMonitorStateException* o = reinterpret_cast<GcIllegalMonitorStateException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IllegalMonitorStateExceptionType]));
  return o;
}

GcIllegalMonitorStateException* makeIllegalMonitorStateException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcIllegalMonitorStateException* o = reinterpret_cast<GcIllegalMonitorStateException*>(allocate(t, 16, true));
  initIllegalMonitorStateException(t, o, message, trace, cause);
  return o;
}

GcIllegalStateException* GcIllegalStateException::makeZeroed(Thread* t)
{
  GcIllegalStateException* o = reinterpret_cast<GcIllegalStateException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IllegalStateExceptionType]));
  return o;
}

GcIllegalStateException* makeIllegalStateException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcIllegalStateException* o = reinterpret_cast<GcIllegalStateException*>(allocate(t, 16, true));
  initIllegalStateException(t, o, message, trace, cause);
  return o;
}

GcIncompatibleClassChangeError* GcIncompatibleClassChangeError::makeZeroed(Thread* t)
{
  GcIncompatibleClassChangeError* o = reinterpret_cast<GcIncompatibleClassChangeError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IncompatibleClassChangeErrorType]));
  return o;
}

GcIncompatibleClassChangeError* makeIncompatibleClassChangeError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcIncompatibleClassChangeError* o = reinterpret_cast<GcIncompatibleClassChangeError*>(allocate(t, 16, true));
  initIncompatibleClassChangeError(t, o, message, trace, cause);
  return o;
}

GcIncompatibleContinuationException* GcIncompatibleContinuationException::makeZeroed(Thread* t)
{
  GcIncompatibleContinuationException* o = reinterpret_cast<GcIncompatibleContinuationException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IncompatibleContinuationExceptionType]));
  return o;
}

GcIncompatibleContinuationException* makeIncompatibleContinuationException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcIncompatibleContinuationException* o = reinterpret_cast<GcIncompatibleContinuationException*>(allocate(t, 16, true));
  initIncompatibleContinuationException(t, o, message, trace, cause);
  return o;
}

GcIndexOutOfBoundsException* GcIndexOutOfBoundsException::makeZeroed(Thread* t)
{
  GcIndexOutOfBoundsException* o = reinterpret_cast<GcIndexOutOfBoundsException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IndexOutOfBoundsExceptionType]));
  return o;
}

GcIndexOutOfBoundsException* makeIndexOutOfBoundsException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcIndexOutOfBoundsException* o = reinterpret_cast<GcIndexOutOfBoundsException*>(allocate(t, 16, true));
  initIndexOutOfBoundsException(t, o, message, trace, cause);
  return o;
}

GcInnerClassReference* GcInnerClassReference::makeZeroed(Thread* t)
{
  GcInnerClassReference* o = reinterpret_cast<GcInnerClassReference*>(allocate(t, 18, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::InnerClassReferenceType]));
  return o;
}

GcInnerClassReference* makeInnerClassReference(Thread* t, GcByteArray* inner, GcByteArray* outer, GcByteArray* name, uint16_t flags)
{
  PROTECT(t, inner);
  PROTECT(t, outer);
  PROTECT(t, name);
  GcInnerClassReference* o = reinterpret_cast<GcInnerClassReference*>(allocate(t, 18, true));
  initInnerClassReference(t, o, inner, outer, name, flags);
  return o;
}

GcInt* GcInt::makeZeroed(Thread* t)
{
  GcInt* o = reinterpret_cast<GcInt*>(allocate(t, 8, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IntType]));
  return o;
}

GcInt* makeInt(Thread* t, uint32_t value)
{
  GcInt* o = reinterpret_cast<GcInt*>(allocate(t, 8, false));
  initInt(t, o, value);
  return o;
}

GcIntArray* GcIntArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcIntArray* o = reinterpret_cast<GcIntArray*>(allocate(t, 8 + pad(length * 4), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IntArrayType]));
  return o;
}

GcIntArray* makeIntArray(Thread* t, uintptr_t length)
{
  GcIntArray* o = reinterpret_cast<GcIntArray*>(allocate(t, 8 + pad(length * 4), false));
  initIntArray(t, o, length);
  return o;
}

GcInterruptedException* GcInterruptedException::makeZeroed(Thread* t)
{
  GcInterruptedException* o = reinterpret_cast<GcInterruptedException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::InterruptedExceptionType]));
  return o;
}

GcInterruptedException* makeInterruptedException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcInterruptedException* o = reinterpret_cast<GcInterruptedException*>(allocate(t, 16, true));
  initInterruptedException(t, o, message, trace, cause);
  return o;
}

GcInvocation* GcInvocation::makeZeroed(Thread* t)
{
  GcInvocation* o = reinterpret_cast<GcInvocation*>(allocate(t, 28, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::InvocationType]));
  return o;
}

GcInvocation* makeInvocation(Thread* t, uint16_t bootstrap, int32_t index, object class_, object pool, object template_, object site)
{
  PROTECT(t, class_);
  PROTECT(t, pool);
  PROTECT(t, template_);
  PROTECT(t, site);
  GcInvocation* o = reinterpret_cast<GcInvocation*>(allocate(t, 28, true));
  initInvocation(t, o, bootstrap, index, class_, pool, template_, site);
  return o;
}

GcInvocationTargetException* GcInvocationTargetException::makeZeroed(Thread* t)
{
  GcInvocationTargetException* o = reinterpret_cast<GcInvocationTargetException*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::InvocationTargetExceptionType]));
  return o;
}

GcInvocationTargetException* makeInvocationTargetException(Thread* t, GcString* message, object trace, GcThrowable* cause, GcThrowable* target)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  PROTECT(t, target);
  GcInvocationTargetException* o = reinterpret_cast<GcInvocationTargetException*>(allocate(t, 20, true));
  initInvocationTargetException(t, o, message, trace, cause, target);
  return o;
}

GcIoException* GcIoException::makeZeroed(Thread* t)
{
  GcIoException* o = reinterpret_cast<GcIoException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::IoExceptionType]));
  return o;
}

GcIoException* makeIoException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcIoException* o = reinterpret_cast<GcIoException*>(allocate(t, 16, true));
  initIoException(t, o, message, trace, cause);
  return o;
}

GcJaccessibleObject* GcJaccessibleObject::makeZeroed(Thread* t)
{
  GcJaccessibleObject* o = reinterpret_cast<GcJaccessibleObject*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JaccessibleObjectType]));
  return o;
}

GcJaccessibleObject* makeJaccessibleObject(Thread* t)
{
  GcJaccessibleObject* o = reinterpret_cast<GcJaccessibleObject*>(allocate(t, 4, false));
  initJaccessibleObject(t, o);
  return o;
}

GcJboolean* GcJboolean::makeZeroed(Thread* t)
{
  GcJboolean* o = reinterpret_cast<GcJboolean*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JbooleanType]));
  return o;
}

GcJboolean* makeJboolean(Thread* t)
{
  GcJboolean* o = reinterpret_cast<GcJboolean*>(allocate(t, 4, false));
  initJboolean(t, o);
  return o;
}

GcJbyte* GcJbyte::makeZeroed(Thread* t)
{
  GcJbyte* o = reinterpret_cast<GcJbyte*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JbyteType]));
  return o;
}

GcJbyte* makeJbyte(Thread* t)
{
  GcJbyte* o = reinterpret_cast<GcJbyte*>(allocate(t, 4, false));
  initJbyte(t, o);
  return o;
}

GcJchar* GcJchar::makeZeroed(Thread* t)
{
  GcJchar* o = reinterpret_cast<GcJchar*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JcharType]));
  return o;
}

GcJchar* makeJchar(Thread* t)
{
  GcJchar* o = reinterpret_cast<GcJchar*>(allocate(t, 4, false));
  initJchar(t, o);
  return o;
}

GcJclass* GcJclass::makeZeroed(Thread* t)
{
  GcJclass* o = reinterpret_cast<GcJclass*>(allocate(t, 8, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JclassType]));
  return o;
}

GcJclass* makeJclass(Thread* t, GcClass* vmClass)
{
  PROTECT(t, vmClass);
  GcJclass* o = reinterpret_cast<GcJclass*>(allocate(t, 8, true));
  initJclass(t, o, vmClass);
  return o;
}

GcJconstructor* GcJconstructor::makeZeroed(Thread* t)
{
  GcJconstructor* o = reinterpret_cast<GcJconstructor*>(allocate(t, 8, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JconstructorType]));
  return o;
}

GcJconstructor* makeJconstructor(Thread* t, GcJmethod* method)
{
  PROTECT(t, method);
  GcJconstructor* o = reinterpret_cast<GcJconstructor*>(allocate(t, 8, true));
  initJconstructor(t, o, method);
  return o;
}

GcJdouble* GcJdouble::makeZeroed(Thread* t)
{
  GcJdouble* o = reinterpret_cast<GcJdouble*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JdoubleType]));
  return o;
}

GcJdouble* makeJdouble(Thread* t)
{
  GcJdouble* o = reinterpret_cast<GcJdouble*>(allocate(t, 4, false));
  initJdouble(t, o);
  return o;
}

GcJfield* GcJfield::makeZeroed(Thread* t)
{
  GcJfield* o = reinterpret_cast<GcJfield*>(allocate(t, 9, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JfieldType]));
  return o;
}

GcJfield* makeJfield(Thread* t, GcField* vmField, uint8_t accessible)
{
  PROTECT(t, vmField);
  GcJfield* o = reinterpret_cast<GcJfield*>(allocate(t, 9, true));
  initJfield(t, o, vmField, accessible);
  return o;
}

GcJfloat* GcJfloat::makeZeroed(Thread* t)
{
  GcJfloat* o = reinterpret_cast<GcJfloat*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JfloatType]));
  return o;
}

GcJfloat* makeJfloat(Thread* t)
{
  GcJfloat* o = reinterpret_cast<GcJfloat*>(allocate(t, 4, false));
  initJfloat(t, o);
  return o;
}

GcJint* GcJint::makeZeroed(Thread* t)
{
  GcJint* o = reinterpret_cast<GcJint*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JintType]));
  return o;
}

GcJint* makeJint(Thread* t)
{
  GcJint* o = reinterpret_cast<GcJint*>(allocate(t, 4, false));
  initJint(t, o);
  return o;
}

GcJlong* GcJlong::makeZeroed(Thread* t)
{
  GcJlong* o = reinterpret_cast<GcJlong*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JlongType]));
  return o;
}

GcJlong* makeJlong(Thread* t)
{
  GcJlong* o = reinterpret_cast<GcJlong*>(allocate(t, 4, false));
  initJlong(t, o);
  return o;
}

GcJmethod* GcJmethod::makeZeroed(Thread* t)
{
  GcJmethod* o = reinterpret_cast<GcJmethod*>(allocate(t, 9, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JmethodType]));
  return o;
}

GcJmethod* makeJmethod(Thread* t, GcMethod* vmMethod, uint8_t accessible)
{
  PROTECT(t, vmMethod);
  GcJmethod* o = reinterpret_cast<GcJmethod*>(allocate(t, 9, true));
  initJmethod(t, o, vmMethod, accessible);
  return o;
}

GcJobject* GcJobject::makeZeroed(Thread* t)
{
  GcJobject* o = reinterpret_cast<GcJobject*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JobjectType]));
  return o;
}

GcJobject* makeJobject(Thread* t)
{
  GcJobject* o = reinterpret_cast<GcJobject*>(allocate(t, 4, false));
  initJobject(t, o);
  return o;
}

GcJreference* GcJreference::makeZeroed(Thread* t)
{
  GcJreference* o = reinterpret_cast<GcJreference*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JreferenceType]));
  return o;
}

GcJreference* makeJreference(Thread* t, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext)
{
  PROTECT(t, jNext);
  GcJreference* o = reinterpret_cast<GcJreference*>(allocate(t, 20, true));
  initJreference(t, o, vmNext, target, queue, jNext);
  return o;
}

GcJshort* GcJshort::makeZeroed(Thread* t)
{
  GcJshort* o = reinterpret_cast<GcJshort*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JshortType]));
  return o;
}

GcJshort* makeJshort(Thread* t)
{
  GcJshort* o = reinterpret_cast<GcJshort*>(allocate(t, 4, false));
  initJshort(t, o);
  return o;
}

GcJvoid* GcJvoid::makeZeroed(Thread* t)
{
  GcJvoid* o = reinterpret_cast<GcJvoid*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::JvoidType]));
  return o;
}

GcJvoid* makeJvoid(Thread* t)
{
  GcJvoid* o = reinterpret_cast<GcJvoid*>(allocate(t, 4, false));
  initJvoid(t, o);
  return o;
}

GcLineNumberTable* GcLineNumberTable::makeZeroed(Thread* t, uintptr_t length)
{
  GcLineNumberTable* o = reinterpret_cast<GcLineNumberTable*>(allocate(t, 8 + pad(length * 8), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::LineNumberTableType]));
  return o;
}

GcLineNumberTable* makeLineNumberTable(Thread* t, uintptr_t length)
{
  GcLineNumberTable* o = reinterpret_cast<GcLineNumberTable*>(allocate(t, 8 + pad(length * 8), false));
  initLineNumberTable(t, o, length);
  return o;
}

GcLinkageError* GcLinkageError::makeZeroed(Thread* t)
{
  GcLinkageError* o = reinterpret_cast<GcLinkageError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::LinkageErrorType]));
  return o;
}

GcLinkageError* makeLinkageError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcLinkageError* o = reinterpret_cast<GcLinkageError*>(allocate(t, 16, true));
  initLinkageError(t, o, message, trace, cause);
  return o;
}

GcList* GcList::makeZeroed(Thread* t)
{
  GcList* o = reinterpret_cast<GcList*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ListType]));
  return o;
}

GcList* makeList(Thread* t, uint32_t size, object front, object rear)
{
  PROTECT(t, front);
  PROTECT(t, rear);
  GcList* o = reinterpret_cast<GcList*>(allocate(t, 16, true));
  initList(t, o, size, front, rear);
  return o;
}

GcLong* GcLong::makeZeroed(Thread* t)
{
  GcLong* o = reinterpret_cast<GcLong*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::LongType]));
  return o;
}

GcLong* makeLong(Thread* t, uint64_t value)
{
  GcLong* o = reinterpret_cast<GcLong*>(allocate(t, 16, false));
  initLong(t, o, value);
  return o;
}

GcLongArray* GcLongArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcLongArray* o = reinterpret_cast<GcLongArray*>(allocate(t, 8 + pad(length * 8), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::LongArrayType]));
  return o;
}

GcLongArray* makeLongArray(Thread* t, uintptr_t length)
{
  GcLongArray* o = reinterpret_cast<GcLongArray*>(allocate(t, 8 + pad(length * 8), false));
  initLongArray(t, o, length);
  return o;
}

GcMethod* GcMethod::makeZeroed(Thread* t)
{
  GcMethod* o = reinterpret_cast<GcMethod*>(allocate(t, 40, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MethodType]));
  return o;
}

GcMethod* makeMethod(Thread* t, uint8_t vmFlags, uint8_t returnCode, uint8_t parameterCount, uint8_t parameterFootprint, uint16_t flags, uint16_t offset, uint32_t nativeID, uint32_t runtimeDataIndex, GcByteArray* name, GcByteArray* spec, GcMethodAddendum* addendum, GcClass* class_, GcCode* code)
{
  PROTECT(t, name);
  PROTECT(t, spec);
  PROTECT(t, addendum);
  PROTECT(t, class_);
  PROTECT(t, code);
  GcMethod* o = reinterpret_cast<GcMethod*>(allocate(t, 40, true));
  initMethod(t, o, vmFlags, returnCode, parameterCount, parameterFootprint, flags, offset, nativeID, runtimeDataIndex, name, spec, addendum, class_, code);
  return o;
}

GcMethodAddendum* GcMethodAddendum::makeZeroed(Thread* t)
{
  GcMethodAddendum* o = reinterpret_cast<GcMethodAddendum*>(allocate(t, 28, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MethodAddendumType]));
  return o;
}

GcMethodAddendum* makeMethodAddendum(Thread* t, GcSingleton* pool, object annotationTable, object signature, object exceptionTable, object annotationDefault, object parameterAnnotationTable)
{
  PROTECT(t, pool);
  PROTECT(t, annotationTable);
  PROTECT(t, signature);
  PROTECT(t, exceptionTable);
  PROTECT(t, annotationDefault);
  PROTECT(t, parameterAnnotationTable);
  GcMethodAddendum* o = reinterpret_cast<GcMethodAddendum*>(allocate(t, 28, true));
  initMethodAddendum(t, o, pool, annotationTable, signature, exceptionTable, annotationDefault, parameterAnnotationTable);
  return o;
}

GcMethodRuntimeData* GcMethodRuntimeData::makeZeroed(Thread* t)
{
  GcMethodRuntimeData* o = reinterpret_cast<GcMethodRuntimeData*>(allocate(t, 8, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MethodRuntimeDataType]));
  return o;
}

GcMethodRuntimeData* makeMethodRuntimeData(Thread* t, GcNative* native)
{
  PROTECT(t, native);
  GcMethodRuntimeData* o = reinterpret_cast<GcMethodRuntimeData*>(allocate(t, 8, true));
  initMethodRuntimeData(t, o, native);
  return o;
}

GcMonitor* GcMonitor::makeZeroed(Thread* t)
{
  GcMonitor* o = reinterpret_cast<GcMonitor*>(allocate(t, 28, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MonitorType]));
  return o;
}

GcMonitor* makeMonitor(Thread* t, void* owner, void* waitHead, void* waitTail, object acquireHead, object acquireTail, uint32_t depth)
{
  PROTECT(t, acquireHead);
  PROTECT(t, acquireTail);
  GcMonitor* o = reinterpret_cast<GcMonitor*>(allocate(t, 28, true));
  initMonitor(t, o, owner, waitHead, waitTail, acquireHead, acquireTail, depth);
  return o;
}

GcMonitorNode* GcMonitorNode::makeZeroed(Thread* t)
{
  GcMonitorNode* o = reinterpret_cast<GcMonitorNode*>(allocate(t, 12, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::MonitorNodeType]));
  return o;
}

GcMonitorNode* makeMonitorNode(Thread* t, void* value, object next)
{
  PROTECT(t, next);
  GcMonitorNode* o = reinterpret_cast<GcMonitorNode*>(allocate(t, 12, true));
  initMonitorNode(t, o, value, next);
  return o;
}

GcNative* GcNative::makeZeroed(Thread* t)
{
  GcNative* o = reinterpret_cast<GcNative*>(allocate(t, 9, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NativeType]));
  return o;
}

GcNative* makeNative(Thread* t, void* function, uint8_t fast)
{
  GcNative* o = reinterpret_cast<GcNative*>(allocate(t, 9, false));
  initNative(t, o, function, fast);
  return o;
}

GcNativeIntercept* GcNativeIntercept::makeZeroed(Thread* t)
{
  GcNativeIntercept* o = reinterpret_cast<GcNativeIntercept*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NativeInterceptType]));
  return o;
}

GcNativeIntercept* makeNativeIntercept(Thread* t, void* function, uint8_t fast, object original)
{
  PROTECT(t, original);
  GcNativeIntercept* o = reinterpret_cast<GcNativeIntercept*>(allocate(t, 16, true));
  initNativeIntercept(t, o, function, fast, original);
  return o;
}

GcNegativeArraySizeException* GcNegativeArraySizeException::makeZeroed(Thread* t)
{
  GcNegativeArraySizeException* o = reinterpret_cast<GcNegativeArraySizeException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NegativeArraySizeExceptionType]));
  return o;
}

GcNegativeArraySizeException* makeNegativeArraySizeException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcNegativeArraySizeException* o = reinterpret_cast<GcNegativeArraySizeException*>(allocate(t, 16, true));
  initNegativeArraySizeException(t, o, message, trace, cause);
  return o;
}

GcNoClassDefFoundError* GcNoClassDefFoundError::makeZeroed(Thread* t)
{
  GcNoClassDefFoundError* o = reinterpret_cast<GcNoClassDefFoundError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NoClassDefFoundErrorType]));
  return o;
}

GcNoClassDefFoundError* makeNoClassDefFoundError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcNoClassDefFoundError* o = reinterpret_cast<GcNoClassDefFoundError*>(allocate(t, 16, true));
  initNoClassDefFoundError(t, o, message, trace, cause);
  return o;
}

GcNoSuchFieldError* GcNoSuchFieldError::makeZeroed(Thread* t)
{
  GcNoSuchFieldError* o = reinterpret_cast<GcNoSuchFieldError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NoSuchFieldErrorType]));
  return o;
}

GcNoSuchFieldError* makeNoSuchFieldError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcNoSuchFieldError* o = reinterpret_cast<GcNoSuchFieldError*>(allocate(t, 16, true));
  initNoSuchFieldError(t, o, message, trace, cause);
  return o;
}

GcNoSuchMethodError* GcNoSuchMethodError::makeZeroed(Thread* t)
{
  GcNoSuchMethodError* o = reinterpret_cast<GcNoSuchMethodError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NoSuchMethodErrorType]));
  return o;
}

GcNoSuchMethodError* makeNoSuchMethodError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcNoSuchMethodError* o = reinterpret_cast<GcNoSuchMethodError*>(allocate(t, 16, true));
  initNoSuchMethodError(t, o, message, trace, cause);
  return o;
}

GcNullPointerException* GcNullPointerException::makeZeroed(Thread* t)
{
  GcNullPointerException* o = reinterpret_cast<GcNullPointerException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NullPointerExceptionType]));
  return o;
}

GcNullPointerException* makeNullPointerException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcNullPointerException* o = reinterpret_cast<GcNullPointerException*>(allocate(t, 16, true));
  initNullPointerException(t, o, message, trace, cause);
  return o;
}

GcNumber* GcNumber::makeZeroed(Thread* t)
{
  GcNumber* o = reinterpret_cast<GcNumber*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::NumberType]));
  return o;
}

GcNumber* makeNumber(Thread* t)
{
  GcNumber* o = reinterpret_cast<GcNumber*>(allocate(t, 4, false));
  initNumber(t, o);
  return o;
}

GcOutOfMemoryError* GcOutOfMemoryError::makeZeroed(Thread* t)
{
  GcOutOfMemoryError* o = reinterpret_cast<GcOutOfMemoryError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::OutOfMemoryErrorType]));
  return o;
}

GcOutOfMemoryError* makeOutOfMemoryError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcOutOfMemoryError* o = reinterpret_cast<GcOutOfMemoryError*>(allocate(t, 16, true));
  initOutOfMemoryError(t, o, message, trace, cause);
  return o;
}

GcPair* GcPair::makeZeroed(Thread* t)
{
  GcPair* o = reinterpret_cast<GcPair*>(allocate(t, 12, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::PairType]));
  return o;
}

GcPair* makePair(Thread* t, object first, object second)
{
  PROTECT(t, first);
  PROTECT(t, second);
  GcPair* o = reinterpret_cast<GcPair*>(allocate(t, 12, true));
  initPair(t, o, first, second);
  return o;
}

GcPhantomReference* GcPhantomReference::makeZeroed(Thread* t)
{
  GcPhantomReference* o = reinterpret_cast<GcPhantomReference*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::PhantomReferenceType]));
  return o;
}

GcPhantomReference* makePhantomReference(Thread* t, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext)
{
  PROTECT(t, jNext);
  GcPhantomReference* o = reinterpret_cast<GcPhantomReference*>(allocate(t, 20, true));
  initPhantomReference(t, o, vmNext, target, queue, jNext);
  return o;
}

GcPointer* GcPointer::makeZeroed(Thread* t)
{
  GcPointer* o = reinterpret_cast<GcPointer*>(allocate(t, 8, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::PointerType]));
  return o;
}

GcPointer* makePointer(Thread* t, void* value)
{
  GcPointer* o = reinterpret_cast<GcPointer*>(allocate(t, 8, false));
  initPointer(t, o, value);
  return o;
}

GcReference* GcReference::makeZeroed(Thread* t)
{
  GcReference* o = reinterpret_cast<GcReference*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ReferenceType]));
  return o;
}

GcReference* makeReference(Thread* t, uint8_t kind, GcByteArray* class_, GcByteArray* name, GcByteArray* spec)
{
  PROTECT(t, class_);
  PROTECT(t, name);
  PROTECT(t, spec);
  GcReference* o = reinterpret_cast<GcReference*>(allocate(t, 20, true));
  initReference(t, o, kind, class_, name, spec);
  return o;
}

GcReferenceQueue* GcReferenceQueue::makeZeroed(Thread* t)
{
  GcReferenceQueue* o = reinterpret_cast<GcReferenceQueue*>(allocate(t, 12, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ReferenceQueueType]));
  return o;
}

GcReferenceQueue* makeReferenceQueue(Thread* t, GcJreference* front, object jnext)
{
  PROTECT(t, front);
  PROTECT(t, jnext);
  GcReferenceQueue* o = reinterpret_cast<GcReferenceQueue*>(allocate(t, 12, true));
  initReferenceQueue(t, o, front, jnext);
  return o;
}

GcReflectiveOperationException* GcReflectiveOperationException::makeZeroed(Thread* t)
{
  GcReflectiveOperationException* o = reinterpret_cast<GcReflectiveOperationException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ReflectiveOperationExceptionType]));
  return o;
}

GcReflectiveOperationException* makeReflectiveOperationException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcReflectiveOperationException* o = reinterpret_cast<GcReflectiveOperationException*>(allocate(t, 16, true));
  initReflectiveOperationException(t, o, message, trace, cause);
  return o;
}

GcRegion* GcRegion::makeZeroed(Thread* t)
{
  GcRegion* o = reinterpret_cast<GcRegion*>(allocate(t, 12, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::RegionType]));
  return o;
}

GcRegion* makeRegion(Thread* t, void* region, uint32_t position)
{
  GcRegion* o = reinterpret_cast<GcRegion*>(allocate(t, 12, false));
  initRegion(t, o, region, position);
  return o;
}

GcRoots* GcRoots::makeZeroed(Thread* t)
{
  GcRoots* o = reinterpret_cast<GcRoots*>(allocate(t, 112, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::RootsType]));
  return o;
}

GcRoots* makeRoots(Thread* t, GcClassLoader* bootLoader, GcClassLoader* appLoader, GcHashMap* bootstrapClassMap, GcHashMap* packageMap, GcMethod* findLoadedClassMethod, GcMethod* loadClassMethod, GcHashMap* monitorMap, GcHashMap* stringMap, GcHashMap* byteArrayMap, GcHashMap* poolMap, GcVector* classRuntimeDataTable, GcVector* methodRuntimeDataTable, GcVector* jNIMethodTable, GcVector* jNIFieldTable, GcPair* shutdownHooks, GcThread* finalizerThread, GcFinalizer* objectsToFinalize, GcCleaner* objectsToClean, GcThrowable* nullPointerException, GcThrowable* arithmeticException, GcThrowable* arrayIndexOutOfBoundsException, GcThrowable* outOfMemoryError, GcThrowable* shutdownInProgress, GcFinder* virtualFileFinders, GcArray* virtualFiles, GcArray* arrayInterfaceTable, object threadTerminated)
{
  PROTECT(t, bootLoader);
  PROTECT(t, appLoader);
  PROTECT(t, bootstrapClassMap);
  PROTECT(t, packageMap);
  PROTECT(t, findLoadedClassMethod);
  PROTECT(t, loadClassMethod);
  PROTECT(t, monitorMap);
  PROTECT(t, stringMap);
  PROTECT(t, byteArrayMap);
  PROTECT(t, poolMap);
  PROTECT(t, classRuntimeDataTable);
  PROTECT(t, methodRuntimeDataTable);
  PROTECT(t, jNIMethodTable);
  PROTECT(t, jNIFieldTable);
  PROTECT(t, shutdownHooks);
  PROTECT(t, finalizerThread);
  PROTECT(t, objectsToFinalize);
  PROTECT(t, objectsToClean);
  PROTECT(t, nullPointerException);
  PROTECT(t, arithmeticException);
  PROTECT(t, arrayIndexOutOfBoundsException);
  PROTECT(t, outOfMemoryError);
  PROTECT(t, shutdownInProgress);
  PROTECT(t, virtualFileFinders);
  PROTECT(t, virtualFiles);
  PROTECT(t, arrayInterfaceTable);
  PROTECT(t, threadTerminated);
  GcRoots* o = reinterpret_cast<GcRoots*>(allocate(t, 112, true));
  initRoots(t, o, bootLoader, appLoader, bootstrapClassMap, packageMap, findLoadedClassMethod, loadClassMethod, monitorMap, stringMap, byteArrayMap, poolMap, classRuntimeDataTable, methodRuntimeDataTable, jNIMethodTable, jNIFieldTable, shutdownHooks, finalizerThread, objectsToFinalize, objectsToClean, nullPointerException, arithmeticException, arrayIndexOutOfBoundsException, outOfMemoryError, shutdownInProgress, virtualFileFinders, virtualFiles, arrayInterfaceTable, threadTerminated);
  return o;
}

GcRuntimeException* GcRuntimeException::makeZeroed(Thread* t)
{
  GcRuntimeException* o = reinterpret_cast<GcRuntimeException*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::RuntimeExceptionType]));
  return o;
}

GcRuntimeException* makeRuntimeException(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcRuntimeException* o = reinterpret_cast<GcRuntimeException*>(allocate(t, 16, true));
  initRuntimeException(t, o, message, trace, cause);
  return o;
}

GcSerializable* GcSerializable::makeZeroed(Thread* t)
{
  GcSerializable* o = reinterpret_cast<GcSerializable*>(allocate(t, 4, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::SerializableType]));
  return o;
}

GcSerializable* makeSerializable(Thread* t)
{
  GcSerializable* o = reinterpret_cast<GcSerializable*>(allocate(t, 4, false));
  initSerializable(t, o);
  return o;
}

GcShort* GcShort::makeZeroed(Thread* t)
{
  GcShort* o = reinterpret_cast<GcShort*>(allocate(t, 6, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ShortType]));
  return o;
}

GcShort* makeShort(Thread* t, uint16_t value)
{
  GcShort* o = reinterpret_cast<GcShort*>(allocate(t, 6, false));
  initShort(t, o, value);
  return o;
}

GcShortArray* GcShortArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcShortArray* o = reinterpret_cast<GcShortArray*>(allocate(t, 8 + pad(length * 2), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ShortArrayType]));
  return o;
}

GcShortArray* makeShortArray(Thread* t, uintptr_t length)
{
  GcShortArray* o = reinterpret_cast<GcShortArray*>(allocate(t, 8 + pad(length * 2), false));
  initShortArray(t, o, length);
  return o;
}

GcSingleton* GcSingleton::makeZeroed(Thread* t, uintptr_t length)
{
  GcSingleton* o = reinterpret_cast<GcSingleton*>(allocate(t, 8 + pad(length * 4), true));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::SingletonType]));
  return o;
}

GcSingleton* makeSingleton(Thread* t, uintptr_t length)
{
  GcSingleton* o = reinterpret_cast<GcSingleton*>(allocate(t, 8 + pad(length * 4), true));
  initSingleton(t, o, length);
  return o;
}

GcSoftReference* GcSoftReference::makeZeroed(Thread* t)
{
  GcSoftReference* o = reinterpret_cast<GcSoftReference*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::SoftReferenceType]));
  return o;
}

GcSoftReference* makeSoftReference(Thread* t, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext)
{
  PROTECT(t, jNext);
  GcSoftReference* o = reinterpret_cast<GcSoftReference*>(allocate(t, 20, true));
  initSoftReference(t, o, vmNext, target, queue, jNext);
  return o;
}

GcStackOverflowError* GcStackOverflowError::makeZeroed(Thread* t)
{
  GcStackOverflowError* o = reinterpret_cast<GcStackOverflowError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::StackOverflowErrorType]));
  return o;
}

GcStackOverflowError* makeStackOverflowError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcStackOverflowError* o = reinterpret_cast<GcStackOverflowError*>(allocate(t, 16, true));
  initStackOverflowError(t, o, message, trace, cause);
  return o;
}

GcStackTraceElement* GcStackTraceElement::makeZeroed(Thread* t)
{
  GcStackTraceElement* o = reinterpret_cast<GcStackTraceElement*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::StackTraceElementType]));
  return o;
}

GcStackTraceElement* makeStackTraceElement(Thread* t, GcString* class_, GcString* method, GcString* file, uint32_t line)
{
  PROTECT(t, class_);
  PROTECT(t, method);
  PROTECT(t, file);
  GcStackTraceElement* o = reinterpret_cast<GcStackTraceElement*>(allocate(t, 20, true));
  initStackTraceElement(t, o, class_, method, file, line);
  return o;
}

GcString* GcString::makeZeroed(Thread* t)
{
  GcString* o = reinterpret_cast<GcString*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::StringType]));
  return o;
}

GcString* makeString(Thread* t, object data, uint32_t offset, uint32_t length, uint32_t hashCode)
{
  PROTECT(t, data);
  GcString* o = reinterpret_cast<GcString*>(allocate(t, 20, true));
  initString(t, o, data, offset, length, hashCode);
  return o;
}

GcSystemClassLoader* GcSystemClassLoader::makeZeroed(Thread* t)
{
  GcSystemClassLoader* o = reinterpret_cast<GcSystemClassLoader*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::SystemClassLoaderType]));
  return o;
}

GcSystemClassLoader* makeSystemClassLoader(Thread* t, GcClassLoader* parent, object packages, object map, void* finder)
{
  PROTECT(t, parent);
  PROTECT(t, packages);
  PROTECT(t, map);
  GcSystemClassLoader* o = reinterpret_cast<GcSystemClassLoader*>(allocate(t, 20, true));
  initSystemClassLoader(t, o, parent, packages, map, finder);
  return o;
}

GcThread* GcThread::makeZeroed(Thread* t)
{
  GcThread* o = reinterpret_cast<GcThread*>(allocate(t, 56, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ThreadType]));
  return o;
}

GcThread* makeThread(Thread* t, object parkBlocker, uint64_t peer, uint8_t interrupted, uint8_t unparked, uint8_t daemon, uint8_t state, uint8_t priority, object task, object locals, object sleepLock, GcClassLoader* classLoader, object exceptionHandler, GcString* name, GcThreadGroup* group, object interruptLock)
{
  PROTECT(t, parkBlocker);
  PROTECT(t, task);
  PROTECT(t, locals);
  PROTECT(t, sleepLock);
  PROTECT(t, classLoader);
  PROTECT(t, exceptionHandler);
  PROTECT(t, name);
  PROTECT(t, group);
  PROTECT(t, interruptLock);
  GcThread* o = reinterpret_cast<GcThread*>(allocate(t, 56, true));
  initThread(t, o, parkBlocker, peer, interrupted, unparked, daemon, state, priority, task, locals, sleepLock, classLoader, exceptionHandler, name, group, interruptLock);
  return o;
}

GcThreadGroup* GcThreadGroup::makeZeroed(Thread* t)
{
  GcThreadGroup* o = reinterpret_cast<GcThreadGroup*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ThreadGroupType]));
  return o;
}

GcThreadGroup* makeThreadGroup(Thread* t, GcThreadGroup* parent, GcString* name, object subgroups)
{
  PROTECT(t, parent);
  PROTECT(t, name);
  PROTECT(t, subgroups);
  GcThreadGroup* o = reinterpret_cast<GcThreadGroup*>(allocate(t, 16, true));
  initThreadGroup(t, o, parent, name, subgroups);
  return o;
}

GcThrowable* GcThrowable::makeZeroed(Thread* t)
{
  GcThrowable* o = reinterpret_cast<GcThrowable*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::ThrowableType]));
  return o;
}

GcThrowable* makeThrowable(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcThrowable* o = reinterpret_cast<GcThrowable*>(allocate(t, 16, true));
  initThrowable(t, o, message, trace, cause);
  return o;
}

GcTraceElement* GcTraceElement::makeZeroed(Thread* t)
{
  GcTraceElement* o = reinterpret_cast<GcTraceElement*>(allocate(t, 12, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::TraceElementType]));
  return o;
}

GcTraceElement* makeTraceElement(Thread* t, object method, int32_t ip)
{
  PROTECT(t, method);
  GcTraceElement* o = reinterpret_cast<GcTraceElement*>(allocate(t, 12, true));
  initTraceElement(t, o, method, ip);
  return o;
}

GcTreeNode* GcTreeNode::makeZeroed(Thread* t)
{
  GcTreeNode* o = reinterpret_cast<GcTreeNode*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::TreeNodeType]));
  return o;
}

GcTreeNode* makeTreeNode(Thread* t, object value, GcTreeNode* left, GcTreeNode* right)
{
  PROTECT(t, value);
  PROTECT(t, left);
  PROTECT(t, right);
  GcTreeNode* o = reinterpret_cast<GcTreeNode*>(allocate(t, 16, true));
  initTreeNode(t, o, value, left, right);
  return o;
}

GcTriple* GcTriple::makeZeroed(Thread* t)
{
  GcTriple* o = reinterpret_cast<GcTriple*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::TripleType]));
  return o;
}

GcTriple* makeTriple(Thread* t, object first, object second, object third)
{
  PROTECT(t, first);
  PROTECT(t, second);
  PROTECT(t, third);
  GcTriple* o = reinterpret_cast<GcTriple*>(allocate(t, 16, true));
  initTriple(t, o, first, second, third);
  return o;
}

GcUnsatisfiedLinkError* GcUnsatisfiedLinkError::makeZeroed(Thread* t)
{
  GcUnsatisfiedLinkError* o = reinterpret_cast<GcUnsatisfiedLinkError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::UnsatisfiedLinkErrorType]));
  return o;
}

GcUnsatisfiedLinkError* makeUnsatisfiedLinkError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcUnsatisfiedLinkError* o = reinterpret_cast<GcUnsatisfiedLinkError*>(allocate(t, 16, true));
  initUnsatisfiedLinkError(t, o, message, trace, cause);
  return o;
}

GcUnwindResult* GcUnwindResult::makeZeroed(Thread* t)
{
  GcUnwindResult* o = reinterpret_cast<GcUnwindResult*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::UnwindResultType]));
  return o;
}

GcUnwindResult* makeUnwindResult(Thread* t, GcContinuation* continuation, object result, GcThrowable* exception)
{
  PROTECT(t, continuation);
  PROTECT(t, result);
  PROTECT(t, exception);
  GcUnwindResult* o = reinterpret_cast<GcUnwindResult*>(allocate(t, 16, true));
  initUnwindResult(t, o, continuation, result, exception);
  return o;
}

GcVector* GcVector::makeZeroed(Thread* t, uintptr_t length)
{
  GcVector* o = reinterpret_cast<GcVector*>(allocate(t, 12 + pad(length * 4), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::VectorType]));
  return o;
}

GcVector* makeVector(Thread* t, uint32_t size, uintptr_t length)
{
  GcVector* o = reinterpret_cast<GcVector*>(allocate(t, 12 + pad(length * 4), true));
  initVector(t, o, size, length);
  return o;
}

GcVirtualMachineError* GcVirtualMachineError::makeZeroed(Thread* t)
{
  GcVirtualMachineError* o = reinterpret_cast<GcVirtualMachineError*>(allocate(t, 16, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::VirtualMachineErrorType]));
  return o;
}

GcVirtualMachineError* makeVirtualMachineError(Thread* t, GcString* message, object trace, GcThrowable* cause)
{
  PROTECT(t, message);
  PROTECT(t, trace);
  PROTECT(t, cause);
  GcVirtualMachineError* o = reinterpret_cast<GcVirtualMachineError*>(allocate(t, 16, true));
  initVirtualMachineError(t, o, message, trace, cause);
  return o;
}

GcWeakHashMap* GcWeakHashMap::makeZeroed(Thread* t)
{
  GcWeakHashMap* o = reinterpret_cast<GcWeakHashMap*>(allocate(t, 12, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::WeakHashMapType]));
  return o;
}

GcWeakHashMap* makeWeakHashMap(Thread* t, uint32_t size, GcArray* array)
{
  PROTECT(t, array);
  GcWeakHashMap* o = reinterpret_cast<GcWeakHashMap*>(allocate(t, 12, true));
  initWeakHashMap(t, o, size, array);
  return o;
}

GcWeakReference* GcWeakReference::makeZeroed(Thread* t)
{
  GcWeakReference* o = reinterpret_cast<GcWeakReference*>(allocate(t, 20, false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::WeakReferenceType]));
  return o;
}

GcWeakReference* makeWeakReference(Thread* t, object vmNext, object target, GcReferenceQueue* queue, GcJreference* jNext)
{
  PROTECT(t, jNext);
  GcWeakReference* o = reinterpret_cast<GcWeakReference*>(allocate(t, 20, true));
  initWeakReference(t, o, vmNext, target, queue, jNext);
  return o;
}

GcWordArray* GcWordArray::makeZeroed(Thread* t, uintptr_t length)
{
  GcWordArray* o = reinterpret_cast<GcWordArray*>(allocate(t, 8 + pad(length * 4), false));
  setObjectClass(t, reinterpret_cast<object>(o), reinterpret_cast<GcClass*>(reinterpret_cast<GcArray*>(t->m->types)->body()[Gc::WordArrayType]));
  return o;
}

GcWordArray* makeWordArray(Thread* t, uintptr_t length)
{
  GcWordArray* o = reinterpret_cast<GcWordArray*>(allocate(t, 8 + pad(length * 4), false));
  initWordArray(t, o, length);
  return o;
}


