#cmakedefine HAVE__LONGJMP @HAVE__LONGJMP@
#cmakedefine HAVE__SETJMP @HAVE__SETJMP@
#cmakedefine HAVE_STRCASECMP @HAVE_STRCASECMP@
#cmakedefine HAVE_STRNCASECMP @HAVE_STRNCASECMP@
#cmakedefine HAVE_DAYLIGHT @HAVE_DAYLIGHT@
#cmakedefine HAVE_LIBREADLINE @HAVE_LIBREADLINE@
#cmakedefine HAVE_READLINE_READLINE_H @HAVE_READLINE_READLINE_H@
#cmakedefine HAVE_SINCOS @HAVE_SINCOS@
#cmakedefine HAVE___SINCOS @HAVE___SINCOS@
#cmakedefine HAVE_TM_GMTOFF @HAVE_TM_GMTOFF@
#cmakedefine MINDY_TARGET_PLATFORM "@MINDY_TARGET_PLATFORM@"
#cmakedefine MINDY_USE_SIGNALS @MINDY_USE_SIGNALS@
#cmakedefine MINDY_VERSION "@MINDY_VERSION@"
#define MINDY_VERSION_MAJOR ${MINDY_VERSION_MAJOR}
#define MINDY_VERSION_MINOR ${MINDY_VERSION_MINOR}
#define MINDY_VERSION_PATCH ${MINDY_VERSION_PATCH}
#cmakedefine SIZEOF_VOID_P @SIZEOF_VOID_P@

#cmakedefine MINDY_SLOW_FUNCTION_POINTERS @MINDY_SLOW_FUNCTION_POINTERS@
#cmakedefine MINDY_SLOW_LONGJMP @MINDY_SLOW_LONGJMP@

#ifndef HAVE__SETJMP
#   define _setjmp setjmp
#endif
#ifndef HAVE__LONGJMP
#   define _longjmp longjmp
#endif

#ifdef MINGW_USE_BUILTIN_SETJMP_LONGJMP
#   undef _setjmp
#   define _setjmp __builtin_setjmp
#   undef _longjmp
#   define _longjmp __builtin_longjmp
#endif

#ifndef HAVE_STRCASECMP
#   define strcasecmp stricmp
#endif
#ifndef HAVE_STRNCASECMP
#   define strncasecmp strnicmp
#endif
