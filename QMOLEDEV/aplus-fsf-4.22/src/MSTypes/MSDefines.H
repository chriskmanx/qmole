#ifndef MSDefinesHEADER
 #define MSDefinesHEADER

 ///////////////////////////////////////////////////////////////////////////////
 //
 // Copyright (c) 1997-2008 Morgan Stanley All rights reserved. 
 // See .../src/LICENSE for terms of distribution
 //
 ///////////////////////////////////////////////////////////////////////////////
 
 
 /*****************************************************************************/
 // MSDefines.H
 /*****************************************************************************/
 
 /*****************************************************************************/
 // General conrols.
 /*****************************************************************************/
 /*
  * MS_NO_EXCEPTIONS      -- Defined if compiler does not support exceptions
  *
  * MS_NO_BOOL            -- Defined if compiler does not support the bool type
  *
  * MS_NO_WIDE_CHAR       -- Defined If compiler does not have a wchar_t type
  *
  * MS_NO_INDEX_ERROR     -- turns off indexError reports from Vectors and Matrices
  *
  * MS_NO_INLINES         -- turns off usage of inlines
  *
  * MS_WINDOWS            -- compiling on Windows platform.
  *
  * MS_NEED_CTL           -- uses ctl loop instead of our own Main Loop.
  *
  * MS_DIR_SEPARATOR      -- defined to '\\' or '/' depending on platform (windows vs. unix)
  *
  * MS_KEYPAD_BUG         -- Defined if correction is needed to fix keypad bug.
  */
 
 /*****************************************************************************/
 // Template Controls
 /*****************************************************************************/
 /*
  * MS_NO_SIMPLE_TEMPLATE_DEFAULTS         -- The compiler does not support simple
  *                                           default template parameters (SunCC,
  *					     xlC, Visual C++ 4.x)
  *                                           e.g.:  template <class T =int>
  *
  * MS_SIMPLE_TEMPLATE_DEFAULT(a)          -- Produces '=a' if simple default template
  *                                           parameters are supported by the compiler
  *                                           (as determined by the flag above) or
  *                                           nothing otherwise
  *
  * MS_NO_COMPLEX_TEMPLATE_DEFAULTS        -- The compiler does not support complex
  *                                           default template parameters (all
  *                                           compilers at this point)
  *                                           e.g.:  template <class T = Foo<T> >
  *
  * MS_COMPLEX_TEMPLATE_DEFAULT(a)         -- Same as MS_SIMPLE_TEMPLATE_DEFAULT(a)
  */
 
 /*****************************************************************************/
 // Issues related to template instantiation
 /*****************************************************************************/
 /*
  * MS_NEED_IMPLEMENTATION_PRAGMA          -- Need #pragma implementation keyword in
  *                                           the header file of the temaplte. (xlC)
  *
  * MS_PREDECLARE_TEMPLATE_FRIENDS         -- Need to predeclare friend functions of
  *                                           template.  While it's actually required
  *                                           by some compilers (VC, SUNPRO), it's a good
  *                                           practice in general, and helps avoid confusion
  *                                           by the compiler during automatic instantiation.
  *                                           Thus, we will define it to 1 for all compilers.
  *
  * MS_TEMPLATE_TYPE_DESTROY_BUG           -- Bug call ::typeDestroy from vector template.(DCC)
  *
  * MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG   -- Bug when overloading template constructor (VC 4.x) 
  *
  * MS_TEMPLATE_MANUAL_FRIEND_BUG          -- Bug of access from friend during manual
  *                                           instantiation. (DCC)
  *
  * MS_TEMPLATE_BEFORE_SPECIALIZATION      -- Need to see the template declaration before
  *                                           its specialization. (xlC)
  *
  * MS_NEED_TEMPLATE_SPECIALIZATION_BUG    -- Need template specialization before template
  *                                           declaration.(VC)
  *
  * MS_NO_PREDECLARE_SPECIALIZATION        -- Can not predeclare template specializations. (xlC, SUNPRO)
  * 
  * MS_TEMPLATE_FUNCTION_SPECILIZATON_BUG  -- Can not specilize individual function of the template.
  * 
  *
  * MS_DECLARE_TEMPLATE_SPECIALIZATION     -- template specialization need to be declared to
  *                                           class users with "template <>" syntax, except when
  *                                           it is compiled in the library.
  *
  * MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION -- Forces instantiatoin of certain classes, like Vectors
  *                                             in the library even if MSTK_MANUAL_INSTANTIATION is not set.
  *
  * MSTK_MANUAL_INSTANTIATION              -- Templates are manually instantiated inside MStk libraries
  *
  * MS_EDG_TEMPLATE_INSTANTIATION          -- Use EDG's manual instantiation directives (edenCC, DCC, SGI CC)
  *
  * MS_XLC_TEMPLATE_INSTANTIATION          -- Use C++ standard manual instantiation directives (SUNPRO)
  *
  * MS_STD_TEMPLATE_INSTANTIATION          -- Use xlC's manual instantiation directives (xlC)
  *
  * MS_VC_TEMPLATE_INSTANTIATION           -- Use Visual C++ manual instantiation directives (VC)
  *
  * MS_DO_NOT_INSTANTIATE_BUG              -- Compiler erroneously generates DNI symbols during *manual*
  *                                           instantiation, when "do_not_instantiate" directives are used,
  *                                           leading to large binaries (SGI CC)
  */
 /*****************************************************************************/
 // Issues concerning time and zone
 /*****************************************************************************/
 /*
  * MS_NEED_STRPTIME_DECLARATION           -- Need to declare strptime  (HAVE_SVR4)
  *
  * MS_NO_STRPTIME                         -- There is no strptime (__sgi || _WINDOWS)
  *
  * MS_NO_CONSTCHAR_STRPTIME               -- Need cast to char* when calling strptime
  *
  * MS_TMS_RESET_BY_STRPTIME               -- strptime() zeroes out the unset fields of tms structure
  *
  * MS_HAS_MKTIME                          -- Has mktime.
  *
  * MS_NO_ALTZONE                          -- No altzone variable for DST.(AIX, Windows) 
  *
  * MS_HAS_TMS_STRUCT                      -- Defined if tms and times.h exist. 
  *                                           else use rusage.  (HAVE_SVR4)
  *
  * MS_STRUCT_TM_TZ                        -- Defined if <time.h> has a struct tm with
  *                                           extra member data "tm_zone" and "tm_gmtoff." 
  *
  * MS_NO_CPU_TIME                         -- there is no getrusage or times functions.
  */
 /*****************************************************************************/
 // Issues concerning wait and signal functions.
 /*****************************************************************************/
 /*
  * MS_SIGNAL_HAS_DOT_PARAM                -- Defined if signal callback takes ... instead of int
  *
  * MS_WAIT_RETURNS_STATUS                 -- Defined if status is actualy returned from wait().
  *
  * MS_WAIT_MACROS_NEED_UNION_CAST         -- Defined if WEXITSTATUS requires parameter of type "union wait"
  *
  */
 /*****************************************************************************/
 // Issues dealing with elementForOps();
 /*****************************************************************************/
 /*
  * MS_NEED_CONST_ELEMENT_FOR_OPS          -- For not ANSI C++ complient compilers need const version
  *                                           of msElementForOps global template function (xlC)
  *
  * MS_NO_NONCONST_ELEMENT_FOR_OPS         -- Visual C++  compilers only understands const version
  *                                            of elementforOps
  *
  * MS_CONST_ELEMENT_FOR_OPS_BUG           -- Bug that screws up calling elementForOps on const classes.
  * 
  *
  * MS_SPECIALIZE_ELEMENT_FOR_OPS          -- elementForOps can't be defined for template pointers
  *                                           but needs to specialized for each defined one. (VC++)
  *
  * MS_SPECIALIZE_ELEMENT_FOR_OPS_TYPE      -- elementForOps can't be defined for as a template at ALL.
  *                                            needs to specialized for everything! (VC++ 5.0)
  *
  */
 /*****************************************************************************/
 // Misc. compiler specific workarounds and bugs
 /*****************************************************************************/
 /*
  * MS_NO_ISTRSTREAM_CONSTCHAR_CONSTRUCTOR -- istrstream class has no "const char*" constructor.(VC)
  *
  * MS_REQUIRE_IEEEFP                      -- finite() is in ieefp. (HAVE_SVR4, VC)
  *
  * MS_NESTED_CLASSES_ACCESS_BUG           -- compiler requires that nested classes be public (SUNPRO)
  *
  * MS_STATIC_VARIABLE_INITIALIZATION_BUG  -- Bug dealing with initaliazation of static variables (Lucid)
  *
  * MS_NEED_FSYNC_DECLARATION              -- Need to declare fsync  (__xlC__)
  *
  * MS_HAS_GETHOSTID_DECLARATION           -- platform declares gethostid in unistd.h  (AIX41)
  *
  * MS_HAS_STRNICMP                        -- strnicmp exists [ ??] ??????????????????????????
  *
  * MS_MATH_EXCEPTION_CONFLICT             -- Conflict between stdlib exception and  math.h 
  *
  * MS_NEED_MUNMAP_DECLARATION             -- sys/mman.h does not have munmap declaration.
  *
  * MS_OSTRSTREAM_FORMATING_BUG            -- lucid ostrstream bug.
  *                                           and suncc UMR from ostrstream bug.
  *
  * MS_HAS_SYSTEMINFO                      -- Defined if <sys/systeminfo.h> and sysinfo() exist.
  *
  * MS_HAS_MSG                             -- Defined if platform already has MSG keyword. (WINDOWS)
  *
  * MS_NEED_ERRNO_DECLARATION              -- Defined if errno is not defined in <errno.h>
  *
  * MS_NEED_STRCASECMP_DECLARATION         -- Defined if strcasecmp is not declared in <string.h>
  *
  * MS_UNISTD_HAS_GETHOSTNAME              -- Defined if <unistd.h> defines gethostname
  *
  * MS_64BIT                               -- Defined on 64-bit platforms.
  *
  * MS_ENUM_COMPARE_BUG                    -- Compiler ambiguity in compare of enum types if 
  *                                           that enum type is used in compare with another type (xlC)
  *
  * MS_NO_EXPLICIT_KEYWORD                 -- Compiler doesn't support explicit keyword
  *
  * MS_MAPNOTIFY_ON_MAP_BUG                -- in modal map() shouldn't wait for MapNotify,
  *                                           this is true for Windows NT X11 emulation layer.
  *
  * MS_HAS_M_2PI                           -- platform system headers define M_2PI 
  *
  * MS_NEED_INLINE_SQRT_INT                -- compiler requires sqrt(int) defined to avoid
  *                                           ambiguities between sqrt(double) and sqrt(lont)
  *
  * MS_INLINE_TEMPLATE_NESTED_CLASS        -- nested template classes need to be fully inlined [Visual C++]
  *
  * MS_PREDECLARE_TEMPLATE_OP_FUNCTIONS    -- operation functions like key, compare, etc.. need to
  *                                           be predeclared before the classs that is deriving from collection.
  *
  * MS_CLASS_ENUM_OVERLOADING_BUG          -- Overloaded functions in template classes that take an 
  *                                           enum defined inside that class, as one of the parameters 
  *                                           don't get instantiated. [VC++]  
  *
  * MS_METHOD_POINTER_DEFAULTARG_BUG       -- Method pointers cannot take 0 as a default argument in 
  *                                           template functions.  VC++
  *
  * MS_OFFSETOF_IN_STDDEF                  -- offsetof macro is defined in <stddef.h>
  */
 /*****************************************************************************/
 // Thread Support
 /*****************************************************************************/
 /*
  * MS_NO_THREADS                          -- Can be defined externally, to compile without threads.
  *                                           But must link with non-multithread libraries.
  *
  * MS_MULTI_THREAD                        -- Defined if threads are supported and MS_NO_THREADS is
  *                                           not defined
  *
  * MS_SOLARIS_THREADS                     -- Native Solaris Threads are supported
  *
  * MS_POSIX_THREADS                       -- Platform supports standard Posix Threads
  *
  * MS_WINDOWS_THREADS                     -- Window Threads are supported
  *
  * MS_NO_MUTEX_INIT                       -- Platform doesn't need mutex initialization
  *
  * MS_POSIX_D10_THREADS                   -- Platform meets Posix Draft 10 threads description
  *
  * MS_MUTEXATTR_DEFAULT                   -- pthread_mutexattr_default is provided.
  *
  * MS_THREAD_SAFE_FUNCTIONS               -- Defined if "_r" (re-entrant) versions of system
  *                                           functions are available on that architecture
  *                                           (e.g., gmtime_r())
  */
 /*****************************************************************************/
 // MSIPC related defines.
 /*****************************************************************************/
 /*
  * MS_WINSOCK                             -- use Microsoft Windows WinSock interface.
  *
  * MS_USE_MSSELECT                        -- use MStk's select instead of ::select call.
  *
  * MS_NEED_CTL                            -- use ctl's main loop instead of our own.
  *
  * MS_USE_FCNTL_BLOCKING                  -- use fcntl() to set [non]blocking state of sockets.
  *                                           otherwise use ioctl.  (HAVE_SVR4)
  * MS_NO_GETRLIMIT                        -- there is not getrlimit function.
  *
  * MS_FDSET_IN_SYS_SELECT                 -- fd_set is <sys/select.h> which needs to be included.
  *
  * MS_NEED_SYS_NERR_DECLARATION           -- need to declare sys_nerr & sys_errlist
  *
  * MS_NO_LIMITS_HEADER                    -- defined if <limits.h> doesn't exist.
  *
  * MS_HAS_SYS_LIMITS_HEADER               -- defined if <sys/limits.h> exists. [used only if no <limits.h>
  *
  * MS_NO_FILIO_HEADER                     -- Platform doesn't have sys/filio.h (AIX)
  *
  * MS_NO_FCNTL                            -- there is no fcntl. (windows)
  */
 /*****************************************************************************/

 // Architecture specific settings - a beginning in the cleanup of this file

#if defined(_ABI64)
# define MS_64BIT	1
#endif

#if defined(__alpha) || defined(__ia64)
# define MS_LITTLE_ENDIAN                    1
# define MS_64BIT                            1
#endif

#if defined(__i386)
# define MS_LITTLE_ENDIAN                    1
#endif
 
 
 /*****************************************************************************/
 // Compiler specific settings
 /*****************************************************************************/
 
# if defined (__EDEN__) && defined (__sun) && !defined (HAVE_SVR4)    /* TAG: Lexa,SunOS   */
#  define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
#  define MS_LEXA_SUNOS                        1
#  define MSTK_MANUAL_INSTANTIATION            1 
#  define MS_EDG_TEMPLATE_INSTANTIATION        1 
#  define MS_STRUCT_TM_TZ                      1
#  define MS_NO_CONSTCHAR_STRPTIME             1
#  define MS_NEED_MUNMAP_DECLARATION           1
#  define MS_USE_FCNTL_BLOCKING                1
#  define MS_NO_EXPLICIT_KEYWORD               1
#  define MS_NO_COMPLEX_TEMPLATE_DEFAULTS      1 
#  define MS_PREDECLARE_TEMPLATE_FRIENDS       1
#  define MS_KEYPAD_BUG                        1       
# endif

/*****************************************************************************/

# if defined(__EDEN__) && defined (HAVE_SVR4)                        /* TAG: EDG,Lexa,Solaris */
#  define MS_LEXA_SOLARIS                      1
#  define MSTK_MANUAL_INSTANTIATION            1 
#  define MS_EDG_TEMPLATE_INSTANTIATION        1 
#  define MS_REQUIRE_IEEEFP                    1
#  define MS_NEED_STRPTIME_DECLARATION         1
#  define MS_TMS_RESET_BY_STRPTIME             1
#  define MS_HAS_MKTIME                        1
#  define MS_HAS_TMS_STRUCT                    1
#  define MS_HAS_SYSTEMINFO                    1
#  define MS_USE_FCNTL_BLOCKING                1
#  define MS_NEED_SYS_NERR_DECLARATION         1
#  define MS_SOLARIS_THREADS                   1
#  define MS_NO_MUTEX_INIT                     1
#  define MS_NO_EXPLICIT_KEYWORD               1
#  define MS_NO_COMPLEX_TEMPLATE_DEFAULTS      1
#  define MS_PREDECLARE_TEMPLATE_FRIENDS       1
#  define MS_MULTI_THREAD                      1

#  ifdef  MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  
#   error  Ambiguous define settings: more than one platform possible
#  endif
#  define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
# endif

 /*****************************************************************************/

# if defined(__SUNPRO_CC) && defined(HAVE_SVR4)                     /* TAG: sunCC,Solaris    */
#  define MS_SUNPRO_CC                         1
#  define MS_NO_BOOL                           1
#  define MS_STD_TEMPLATE_INSTANTIATION        1 
#  define MS_PREDECLARE_TEMPLATE_FRIENDS       1
#  define MS_NO_PREDECLARE_SPECIALIZATION      1
#  define MS_REQUIRE_IEEEFP                    1
#  define MS_TMS_RESET_BY_STRPTIME             1
#  define MS_HAS_MKTIME                        1
#  define MS_HAS_TMS_STRUCT                    1
#  define MS_HAS_SYSTEMINFO                    1
#  define MS_USE_FCNTL_BLOCKING                1
#  define MS_NEED_SYS_NERR_DECLARATION         1
#  define MS_SOLARIS_THREADS                   1
#  define MS_NO_MUTEX_INIT                     1
#  define MS_OPERATOR_CONVERSION_BUG           1
#  define MS_MULTI_THREAD                      1
#  define MS_THREAD_SAFE_FUNCTIONS             1
#  if !defined(_POSIX_PTHREAD_SEMANTICS)
#   define MS_OLD_POSIX_THREAD_SAFE_FUNCTIONS   1
#  endif
#  define MS_NO_COMPLEX_TEMPLATE_DEFAULTS      1

#  define MS_TEMPLATE_FUNCTION_SPECILIZATON_BUG 1
#  define MS_CONST_ELEMENT_FOR_OPS_BUG         1

#  define MS_STD_TEMPLATE_SPECIALIZATION   1

#  if __SUNPRO_CC < 0x500
  //SunCC 4.2 only

#   define MS_OSTRSTREAM_FORMATING_BUG          1
#   define MS_NO_SIMPLE_TEMPLATE_DEFAULTS       1
#   define MS_NEED_STRPTIME_DECLARATION         1
#   define MS_NESTED_CLASSES_ACCESS_BUG         1
#   define MS_NO_EXPLICIT_KEYWORD               1

#   define MS_FORCE_INTERNAL_TEMPLATE_INSTANTIATION 1
#   define MS_TEMPLATE_BEFORE_SPECIALIZATION    1
#   define MS_DECLARE_TEMPLATE_SPECIALIZATION   1

#  else
  // SunCC 5.0 only

#   define MS_NEED_TEMPLATE_SPECIALIZATION_BUG     1
#   define MS_C_LINKAGE_INCOMPATIBILITY            1
#   define MS_PREDECLARE_NONTEMPLATE_FRIEND_FUNCTIONS 1
#   define MS_PREDECLARE_TEMPLATE_FRIENDS_BUG      1

#  endif

#  ifdef  MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  
#   error  Ambiguous define settings: more than one platform possible
#  endif
#  define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
# endif

 /*****************************************************************************/

# if defined(_AIX)
#  define MS_NO_BOOL                           1
#  define MS_NO_PREDECLARE_SPECIALIZATION      1
#  define MS_TEMPLATE_BEFORE_SPECIALIZATION    1
#  define MS_NEED_FSYNC_DECLARATION            1
#  define MS_HAS_MKTIME                        1
#  define MS_NEED_CONST_ELEMENT_FOR_OPS        1
#  define MS_HAS_GETHOSTID_DECLARATION         1
#  define MS_NO_GETRLIMIT                      1
#  define MS_FDSET_IN_SYS_SELECT               1
#  define MS_NO_FILIO_HEADER                   1
#  define MS_NEED_SYS_NERR_DECLARATION         1
#  define MS_POSIX_THREADS                     1
#  define MS_POSIX_D10_THREADS                 1
#  define MS_NEED_STRCASECMP_DECLARATION       1
#  define MS_WAIT_RETURNS_STATUS               1
#  define MS_ENUM_COMPARE_BUG                  1
#  define MS_NO_EXPLICIT_KEYWORD               1
#  define MS_NO_SIMPLE_TEMPLATE_DEFAULTS       1
#  define MS_HAS_M_2PI                         1
#  define MS_PREDECLARE_TEMPLATE_OP_FUNCTIONS  1
#  define MS_KEYPAD_BUG                        1

#  ifdef _BSD
#   define MS_WAIT_MACROS_NEED_UNION_CAST     1
#  endif

# endif

 /*****************************************************************************/

# if defined(__sgi) && defined(__DELTA)                          /* TAG: DCC              */
#  define MS_SGI_DCC                           1
#  define MS_NO_BOOL                           1
#  define MS_NO_WIDE_CHAR                      1
#  define MS_NO_EXCEPTIONS                     1
#  define MSTK_MANUAL_INSTANTIATION            1 
#  define MS_EDG_TEMPLATE_INSTANTIATION        1 
#  define MS_TEMPLATE_TYPE_DESTROY_BUG         1
#  define MS_TEMPLATE_MANUAL_FRIEND_BUG        1
#  define MS_REQUIRE_IEEEFP                    1
#  define MS_NEED_STRPTIME_DECLARATION         1
#  define MS_HAS_MKTIME                        1
#  define MS_HAS_TMS_STRUCT                    1
#  define MS_HAS_SYSTEMINFO                    1
#  define MS_NO_EXPLICIT_KEYWORD               1
#  define MS_NO_COMPLEX_TEMPLATE_DEFAULTS      1
#  define MS_PREDECLARE_TEMPLATE_FRIENDS       1
#  define MS_KEYPAD_BUG                        1

#  ifdef  MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  
#   error  Ambiguous define settings: more than one platform possible
#  endif
#  define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
# endif

 /*****************************************************************************/

# if defined(__sgi) && defined(_ABIO32)                          /* TAG: sgi32            */
#  define MS_SGI32                             1
#  define MS_NO_BOOL                           1
#  define MS_NO_WIDE_CHAR                      1
#  define MSTK_MANUAL_INSTANTIATION            1 
#  define MS_EDG_TEMPLATE_INSTANTIATION        1 
#  define MS_REQUIRE_IEEEFP                    1
#  define MS_TMS_RESET_BY_STRPTIME             1
#  define MS_HAS_MKTIME                        1
#  define MS_HAS_TMS_STRUCT                    1
#  define MS_HAS_SYSTEMINFO                    1
#  define MS_USE_FCNTL_BLOCKING                1
#  define MS_POSIX_THREADS                     1
#  define MS_POSIX_D10_THREADS                 1
#  define MS_NO_EXPLICIT_KEYWORD               1
#  define MS_NO_COMPLEX_TEMPLATE_DEFAULTS      1
#  define MS_PREDECLARE_TEMPLATE_FRIENDS       1
#  define MS_MULTI_THREAD                      1
#  define MS_THREAD_SAFE_FUNCTIONS             1
#  define MS_KEYPAD_BUG                        1       
#  ifdef  MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  
#   error  Ambiguous define settings: more than one platform possible
#  endif
#  define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
# endif

 /*****************************************************************************/

# if defined(__sgi) && defined(_ABIN32)                          /* TAG: sgin32           */
#  define MS_SGIN32                            1
#  define MSTK_MANUAL_INSTANTIATION            1 
#  define MS_EDG_TEMPLATE_INSTANTIATION        1 
#  if defined(_COMPILER_VERSION) && _COMPILER_VERSION<720
#   define MS_DO_NOT_INSTANTIATE_BUG           1
#  endif  //_COMPILER_VERSION<720
#  define MS_REQUIRE_IEEEFP                    1
#  define MS_TMS_RESET_BY_STRPTIME             1
#  define MS_HAS_MKTIME                        1
#  define MS_HAS_TMS_STRUCT                    1
#  define MS_HAS_SYSTEMINFO                    1
#  define MS_USE_FCNTL_BLOCKING                1
#  define MS_POSIX_THREADS                     1
#  define MS_POSIX_D10_THREADS                 1
#  define MS_NO_EXPLICIT_KEYWORD               1
#  define MS_NO_COMPLEX_TEMPLATE_DEFAULTS      1
#  define MS_PREDECLARE_TEMPLATE_FRIENDS       1
#  define MS_MULTI_THREAD                      1
#  define MS_THREAD_SAFE_FUNCTIONS             1
#  define MS_KEYPAD_BUG                        1
#  define MS_UNISTD_HAS_GETHOSTNAME            1
#  if !defined(__GNUC__)
#   define __MSTYPESLIBRARY_BUILD__	       1
#   define __MSIPCLIBRARY_BUILD__	       1
#   define __MSGUILIBRARY_BUILD__	       1
#  endif

#  if defined(__GNUC__)                        /* TAG: g++             */
#   define MS_GCC                               1
#   define MS_NO_ALTZONE                        1
#   define MS_STD_TEMPLATE_INSTANTIATION        1
#   define MS_PREDECLARE_TEMPLATE_FRIENDS       1
#   define MS_GCC_FUNCTION_CAST_BUG             1
 
  //egcs 1.1.1
#   define MS_TEMPLATE_FUNCTION_SPECILIZATON_BUG 1

#   if defined(HAVE_SVR4)
#    define MS_USE_FCNTL_BLOCKING                1
#    define MS_NEED_SYS_NERR_DECLARATION         1
#    define MS_IOSTREAM_IPFX_NEEDS_ARGUMENT      1
#   endif
#  endif
 
//temporary
# define MS_SPECIALIZE_ELEMENT_FOR_OPS  1

# ifdef  MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  
#  error  Ambiguous define settings: more than one platform possible
# endif
# define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
#endif


/*****************************************************************************/

#if defined(__sgi) && defined(_ABI64)                          /* TAG: sgi64           */
#define MS_SGI64                             1
#define MS_64BIT                             1
#define MSTK_MANUAL_INSTANTIATION            1 
#define MS_EDG_TEMPLATE_INSTANTIATION        1 
#if defined(_COMPILER_VERSION) && _COMPILER_VERSION<720
#define MS_DO_NOT_INSTANTIATE_BUG           1
#endif  //_COMPILER_VERSION<720
#define MS_REQUIRE_IEEEFP                    1
#define MS_TMS_RESET_BY_STRPTIME             1
#define MS_HAS_MKTIME                        1
#define MS_HAS_TMS_STRUCT                    1
#define MS_HAS_SYSTEMINFO                    1
#define MS_USE_FCNTL_BLOCKING                1
#define MS_POSIX_THREADS                     1
#define MS_POSIX_D10_THREADS                 1
#define MS_NO_EXPLICIT_KEYWORD               1
#define MS_NO_COMPLEX_TEMPLATE_DEFAULTS      1
#define MS_PREDECLARE_TEMPLATE_FRIENDS       1
#define MS_MULTI_THREAD                      1
#define MS_THREAD_SAFE_FUNCTIONS            1
#define MS_KEYPAD_BUG                        1
#  if !defined(__GNUC__)
#   define __MSTYPESLIBRARY_BUILD__            1
#   define __MSIPCLIBRARY_BUILD__              1
#   define __MSGUILIBRARY_BUILD__              1
#  endif

#  if defined(__GNUC__)                        /* TAG: g++             */
#   define MS_GCC                               1
#   define MS_NO_ALTZONE                        1
#   define MS_STD_TEMPLATE_INSTANTIATION        1
#   define MS_PREDECLARE_TEMPLATE_FRIENDS       1
#   define MS_GCC_FUNCTION_CAST_BUG             1

  //egcs 1.1.1
#   define MS_TEMPLATE_FUNCTION_SPECILIZATON_BUG 1

#   if defined(HAVE_SVR4)
#    define MS_USE_FCNTL_BLOCKING                1
#    define MS_NEED_SYS_NERR_DECLARATION         1
#    define MS_IOSTREAM_IPFX_NEEDS_ARGUMENT      1
#   endif
#  endif

//temporary
# define MS_SPECIALIZE_ELEMENT_FOR_OPS  1

# ifdef  MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  
#  error  Ambiguous define settings: more than one platform possible
# endif
# define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
#endif


/*****************************************************************************/

#if defined(_MSC_VER)                                          /* TAG: Visual C++       */
#define MS_VISUALC                             1
#define MS_WINDOWS                             1
#define MSTK_MANUAL_INSTANTIATION              1 
#define MS_VC_TEMPLATE_INSTANTIATION           1
#define MS_NO_DEFAULT_COMPILER_DEFINES         1
#define MS_TEMPLATE_CONSTRUCTOR_OVERLOAD_BUG   1
#define MS_NO_ISTRSTREAM_CONSTCHAR_CONSTRUCTOR 1
#define MS_REQUIRE_IEEEFP                      1
#define MS_NO_STRPTIME                         1
#define MS_NEED_TEMPLATE_SPECIALIZATION_BUG    1
#define MS_NO_ALTZONE                          1
#define MS_NO_CONSTCHAR_STRPTIME               1
#define MS_HAS_MSG                             1
#define MS_WINSOCK                             1
#define MS_USE_MSSELECT                        1
#define MS_NO_GETRLIMIT                        1
#define MS_WINDOWS_THREADS                     1
#define MS_UNISTD_HAS_GETHOSTNAME              1
#define MS_MAPNOTIFY_ON_MAP_BUG                1
// #define MS_MULTI_THREAD                        1
#define MS_HAS_MKTIME                          1
#define MS_NEED_INLINE_SQRT_INT                1
#define MS_NO_CPU_TIME                         1
#define MS_GETPID_IN_PROCESS                   1
#define MS_NO_FILIO_HEADER                     1
#define MS_NO_FCNTL                            1
#define MS_FDSET_IN_SYS_SELECT                 1
#define MS_INLINE_TEMPLATE_NESTED_CLASS        1
//#define MS_KEYPAD_BUG                        1
#define MS_PREDECLARE_TEMPLATE_OP_FUNCTIONS    1
#define MS_PREDECLARE_TEMPLATE_FRIENDS         1
#define MS_COMPILE_INSTANTIATE                  1

#ifndef NOMINMAX
#define NOMINMAX
#endif

#ifdef max
#undef max
#endif

#ifdef min
#undef min
#endif


#define  DllExport  __declspec(dllexport)
#define  DllImport  __declspec(dllimport)
#pragma warning( disable: 4275 4251 4273 4018 4660 4305 4309)

//This bug is only in VC++ 5.0 BUT
//we must turn it on for both 4.2 and 5.0 if we
//want to be able to compile with both compilers against
//one set of libraries [4.2]
#define MS_CLASS_ENUM_OVERLOADING_BUG          1

#if  _MSC_VER < 1100 
//  VC++ 4.x

#define MS_NO_EXPLICIT_KEYWORD                 1
#define MS_NO_COMPLEX_TEMPLATE_DEFAULTS        1

#define MS_NEED_CONST_ELEMENT_FOR_OPS          1
#define MS_NO_NONCONST_ELEMENT_FOR_OPS         1
#define MS_SPECIALIZE_ELEMENT_FOR_OPS          1

#endif


#if _MSC_VER <= 1100  
//VC++ 4.x AND 5.0
#define MS_VC_NEED_TEMPLATE_EXPORT             1

#endif

#if _MSC_VER >= 1100
//VC++ 5.0 and later


//#define MS_NEED_CONST_ELEMENT_FOR_OPS          1
#define MS_NO_NONCONST_ELEMENT_FOR_OPS         1
#define MS_SPECIALIZE_ELEMENT_FOR_OPS          1
#define MS_SPECIALIZE_ELEMENT_FOR_OPS_TYPE     1

#define MS_METHOD_POINTER_DEFAULTARG_BUG       1
#define MS_OFFSETOF_IN_STDDEF                  1

#endif

#if _MSC_VER > 1100
//VC++ 6.0 and later
#define MS_TEMPLATE_FRIEND_CLASS_BUG           1
#pragma warning( disable: 4291)

#endif

#ifdef  MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  
#error  Ambiguous define settings: more than one platform possible
#endif
#define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1

#else   //_MSC_VER

#define  DllExport
#define  DllImport

#endif  //_MSC_VER

/*****************************************************************************/

#if defined (__osf__)
#define MS_HAS_MKTIME                          1
#define MS_NO_ALTZONE                          1
#define MS_NO_COMPLEX_TEMPLATE_DEFAULTS        1
#define MS_CXX_MANUAL_INSTANTIATION            1 
#define MS_NO_FILIO_HEADER                     1
//#define MS_NO_EXPLICIT_KEYWORD                 1
#define MS_64BIT                               1
#define MS_LITTLE_ENDIAN                       1
#define MS_HAS_M_2PI                           1
#define MS_FRIEND_TEMPLATEID_NOT_NEEDED		1

#ifdef  MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  
#error  Ambiguous define settings: more than one platform possible
#endif
#define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
#endif


/*****************************************************************************/

#if defined(__GNUC__) && !defined(MS_GCC)                        /* TAG: g++             */
# define MS_GCC                               1
# define MS_NO_COMPLEX_TEMPLATE_DEFAULTS      1 
# define MS_NO_ALTZONE                        1
# define MSTK_MANUAL_INSTANTIATION            1
# define MS_STD_TEMPLATE_INSTANTIATION        1
# define MS_PREDECLARE_TEMPLATE_FRIENDS       1
# define MS_GCC_FUNCTION_CAST_BUG             1

//egcs 1.1.1
# define MS_TEMPLATE_FUNCTION_SPECILIZATON_BUG 1

# if defined(HAVE_SVR4)
#  define MS_USE_FCNTL_BLOCKING                1
#  define MS_NEED_SYS_NERR_DECLARATION         1
#  define MS_IOSTREAM_IPFX_NEEDS_ARGUMENT      1
#  define MS_HAS_MKTIME                        1
#  define MS_REQUIRE_IEEEFP                    1
# endif

# if defined(__APPLE__)
#  define MS_NO_STRPTIME
# endif

# if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__APPLE__)
#  define MS_NO_GETRLIMIT 1
#  define MS_STRUCT_TM_TZ 1
# endif

# if defined(linux)
#  define MS_LINUX                       1
#  define MS_NEED_STRPTIME_DECLARATION   1
#  define MS_XSYNCHRONIZE_FULL_SIGNATURE 1
#  define MS_HAS_GETRUSAGE_DECLARATION   1
#  define MS_FD_SET_NOT_STRUCT           1
#  define MS_NO_FILIO_HEADER             1
#  define MS_HOWMANY_IN_SYS_PARAM        1
#  define MS_LITTLE_ENDIAN                       1
# endif

//temporary
# define MS_SPECIALIZE_ELEMENT_FOR_OPS  1

//#define MS_TEMPLATE_MANUAL_FRIEND_BUG        1

# ifdef  MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  
#  error  Ambiguous define settings: more than one platform possible
# endif
#  define MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
#endif


/*****************************************************************************/
#ifndef MS_NEVER_ENTER_DEFAULT_HEADER_BLOCK                     /*TAG                    */
#error  Unsupported compiler & platform combination
#endif 

/*****************************************************************************/
// Misc. settings
/*****************************************************************************/

// Windows DLL settings
#if !defined(_MSTKLIB)    //Building or using MStk dll

#ifdef __MSTYPESLIBRARY_BUILD__
#  define MSTypesExport DllExport
#else
#  define MSTypesExport DllImport
#endif

#ifdef __MSIPCLIBRARY_BUILD__
#  define MSIPCExport      DllExport
#else
#  define MSIPCExport      DllImport
#endif

#ifdef __MSGUILIBRARY_BUILD__
#  define MSGUIExport	   DllExport
#else
#  define MSGUIExport      DllImport
#endif

#ifdef __MS3dLIBRARY_BUILD__
#  define MS3dExport	   DllExport
#else
#  define MS3dExport      DllImport
#endif

#ifdef __MSUTILLIBRARY_BUILD__
#  define MSUtilExport     DllExport
#else
#  define MSUtilExport     DllImport
#endif

#ifdef __MSPROTOLIBRARY_BUILD__
#  define MSProtoExport    DllExport
#else
#  define MSProtoExport    DllImport
#endif

#else

#   define MS3dExport
#   define MSTypesExport
#   define MSIPCExport  
#   define MSGUIExport  
#   define MSUtilExport  
#   define MSProtoExport  

#endif  //_MSTKDLL

// exception handling
#if defined(MS_NO_EXCEPTIONS)
// If we don't have exceptions, then we'll use assert.
// We don't allow users to turn off the assert() in which such
// a thrown exception would result.
#ifdef NDEBUG
#define __MS_NDEBUG
#undef NDEBUG
#endif

#include <assert.h>
#define MSTKTHROWEXCEPTION(exc)      assert(0)

#ifdef __MS_NDEBUG
#define NDEBUG
#undef __MS_NDEBUG
#endif

#else // we have exception support
#define MSTKTHROWEXCEPTION(exc)      throw(exc)
#endif // MS_NO_EXCEPTIONS


//directory 
#ifdef MS_WINDOWS
#define MS_DIR_SEPARATOR  '\\'
#else
#define MS_DIR_SEPARATOR  '/'
#endif

// threads
#if defined(MS_NO_THREADS)
#undef MS_MULTI_THREAD
#endif

// Wrappers around system function that have mt-safe (_r) versions.
// The macros take as many parameters as the _r versions. The regular
// versions of these macros ignore extra parameters and thus they don't
// have to be declared when MS_THREAD_SAFE_FUNCTIONS is not defined.
// To declare a direntry struct forREADDIR macro MS_DECLARE_DIRENT should be
// used because it correctly allocates enough memory for dirEntry.
#ifdef MS_THREAD_SAFE_FUNCTIONS
#define MS_DECLARE_DIRENT(dirEntry)  \
  char dirEntry##Buffer[1025+sizeof(struct dirent)];\
  struct dirent *dirEntry = (struct dirent *)dirEntry##Buffer;

#define MS_LOCALTIME(numSecs,pTm)                localtime_r(numSecs,pTm)
#define MS_GMTIME(numSecs,pTm)                   gmtime_r(numSecs,pTm)

#if defined(MS_OLD_POSIX_THREAD_SAFE_FUNCTIONS)
#define MS_ASCTIME(tp,charBuf,n)                 asctime_r(tp,charBuf,n)
#define MS_GETPWUID(uid,pStruct,charBuf,n,pwd)   (pwd=getpwuid_r(uid,pStruct,charBuf,n))
#define MS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (pwd=getpwnam_r(name,pStruct,charBuf,n))
#define MS_READDIR(dp,entry,res)                 (res=readdir_r(dp,entry))
#else
#define MS_ASCTIME(tp,charBuf,n)                 asctime_r(tp,charBuf)
#define MS_GETPWUID(uid,pStruct,charBuf,n,pwd)   (getpwuid_r(uid,pStruct,charBuf,n,&pwd),pwd)
#define MS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (getpwnam_r(name,pStruct,charBuf,n,&pwd),pwd)
#define MS_READDIR(dp,entry,res)                 (readdir_r(dp,entry,&res),res)
#endif

#else
#define MS_DECLARE_DIRENT(dirEntry)
#define MS_LOCALTIME(numSecs,pTm)                localtime(numSecs)
#define MS_GMTIME(numSecs,pTm)                   gmtime(numSecs)
#define MS_ASCTIME(tp,charBuf,n)                 asctime(tp)
#define MS_GETPWUID(uid,pStruct,charBuf,n,pwd)   (pwd=getpwuid(uid))
#define MS_GETPWNAM(name,pStruct,charBuf,n,pwd)  (pwd=getpwnam(name))
#define MS_READDIR(dp,entry,res)                 (res=readdir(dp))
#endif //MS_THREAD_SAFE_FUNCTIONS

// inlining 
#if defined(MS_NO_INLINES)
#define INLINELINKAGE 
#else
#define INLINELINKAGE inline
#endif  //MS_NO_INLINES

#if defined(__GNUC__) || ((defined(_ABIN32)||defined(_ABI64)) && (_COMPILER_VERSION > 720))
  #define INLINELINKAGE2
#else
  #define INLINELINKAGE2 inline
#endif // __GNUC__



// explicit keyword
#if defined (MS_NO_EXPLICIT_KEYWORD)
#define MSExplicit 
#else
#define MSExplicit explicit
#endif

// Macro for forming or ommitting default template parameters.
#ifndef MS_NO_SIMPLE_TEMPLATE_DEFAULTS
#  define MS_SIMPLE_TEMLATE_DEFAULT(a)  =a
#  ifndef MS_NO_COMPLEX_TEMPLATE_DEFAULTS
#    define MS_COMPLEX_TEMPLATE_DEFAULT(a)  =a
#  else
#    define MS_COMPLEX_TEMPLATE_DEFAULT(a)
#  endif  //MS_NO_COMPLEX_TEMPLATE_DEFAULTS
#else
#  ifndef MS_NO_COMPLEX_TEMPLATE_DEFAULTS
#    define MS_NO_COMPLEX_TEMPLATE_DEFAULTS
#  endif  //MS_NO_COMPLEX_TEMPLATE_DEFAULTS
#  define MS_SIMPLE_TEMPLATE_DEFAULT(a)
#  define MS_COMPLEX_TEMPLATE_DEFAULT(a)
#endif  //MS_NO_SIMPLE_TEMPLATE_DEFAULTS


#ifdef __makedep__
#undef MS_NEED_IMPLEMENTATION_PRAGMA
#undef MS_VC_TEMPLATE_INSTANTIATION
#undef MS_XLC_TEMPLATE_INSTANTIATION
#undef MS_EDG_TEMPLATE_INSTANTIATION
#undef MS_STD_TEMPLATE_INSTANTIATION
#endif

// assume RW stdlib
#if !defined(MS_NO_STL)
#define MS_USE_RW_STDLIB      1
#endif

// MS_DO_NOT_INSTANTIATE controls whether "do_not_instantiate" directives are
// provided for the templates that are instantiated inside MStk.  It is only
// necessary if MStk has been built with manual instantiation AND if the compiler
// does not have a bug related to "do_not_instantiate" (see the description of
// of MS_DO_NOT_INSTANTIATE_BUG above).  If MS_DO_NOT_INSTANTIATE is not defined,
// the only "do_not_instantiate" directives will be those for specializations
// inside MStk.
//
// This flag is user-controllable.
//
#if defined(MSTK_MANUAL_INSTANTIATION) && !defined(MS_DO_NOT_INSTANTIATE_BUG)
#define MS_DO_NOT_INSTANTIATE
#endif // MSTK_MANUAL_INSTANTIATION && !MS_DO_NOT_INSTANTIATE_BUG

// A pivot year is the number used to deduce the century of a 2-digit year specification.
// The standard pivot year is 70, which means that all 2-digit years less than or equal to
// 70 refer to the 21st century, while all 2-digit years greater than 70 refer to the 20th
// century.  For example, 1/1/69 means 1/1/2069, but 1/1/71 means 1/1/1971.
//
#define MS_PIVOT_YEAR 70

//
// A macro for MT-safe static initialization
//
#ifdef MS_MULTI_THREAD
#define MS_SAFE_STATIC_INIT(Type,mutex)\
  static Type *p##Type=0;\
\
  if (p##Type==0)\
    {\
      MSGUARD(mutex);\
      if (p##Type==0)\
	{\
	  static char Type##Buf[sizeof(Type)];\
	  p##Type = ::new (&Type##Buf[0]) Type;\
	}\
    }\
\
  return *p##Type;

#else

#define MS_SAFE_STATIC_INIT(Type,mutex)\
  static Type *p##Type=0;\
\
  if (p##Type==0)\
    {\
       static char Type##Buf[sizeof(Type)];\
       p##Type = ::new (&Type##Buf[0]) Type;\
    }\
\
  return *p##Type;

#endif

#if !defined (MS_FRIEND_TEMPLATEID_NOT_NEEDED)
# define MS_FRIEND_TEMPLATEID <>
#else
# define MS_FRIEND_TEMPLATEID
#endif

#if defined(__APPLE__)
#define APLUS_ISPACE aplus_isspace
extern "C" int aplus_isspace(int); 
#else
#define APLUS_ISPACE isspace
#endif

#endif //MSDefinesHEADER
