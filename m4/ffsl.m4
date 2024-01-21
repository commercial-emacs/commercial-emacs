# ffsl.m4 serial 3
dnl Copyright (C) 2011-2024 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_FFSL],
[
  AC_REQUIRE([gl_STRING_H_DEFAULTS])

  dnl Persuade glibc <string.h> to declare ffsl().
  AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])

  dnl We can't use AC_CHECK_FUNC here, because ffsl() is defined as a
  dnl static inline function when compiling for Android 13 or older.
  dnl But require that ffsl() is declared; otherwise we may be using
  dnl the GCC built-in function, which leads to warnings
  dnl "warning: implicit declaration of function 'ffsl'".
  AC_CACHE_CHECK([for ffsl], [gl_cv_func_ffsl],
    [AC_LINK_IFELSE(
       [AC_LANG_PROGRAM(
          [[#include <string.h>
            #include <strings.h>
            long x;
          ]],
          [[int (*func) (long) = ffsl;
            return func (x);
          ]])
       ],
       [gl_cv_func_ffsl=yes],
       [gl_cv_func_ffsl=no])
    ])
  if test $gl_cv_func_ffsl = no; then
    HAVE_FFSL=0
  fi
])
