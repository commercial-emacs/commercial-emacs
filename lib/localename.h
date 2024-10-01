/* Determine name of the currently selected locale.
   Copyright (C) 2007, 2009-2024 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 2.1 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef _GL_LOCALENAME_H
#define _GL_LOCALENAME_H

/* This file uses _GL_ATTRIBUTE_CONST, HAVE_CFPREFERENCESCOPYAPPVALUE.  */
#if !_GL_CONFIG_H_INCLUDED
 #error "Please include config.h first."
#endif

#ifdef __cplusplus
extern "C" {
#endif


/* Determine the current locale's name.
   It considers both the POSIX notion of locale name (see functions
   gl_locale_name_thread and gl_locale_name_posix) and the system notion
   of locale name (see function gl_locale_name_default).
   CATEGORY is a locale category abbreviation, as defined in <locale.h>,
   but not LC_ALL. E.g. LC_MESSAGES.
   CATEGORYNAME is the name of CATEGORY as a string, e.g. "LC_MESSAGES".
   Return the locale category's name, canonicalized into XPG syntax
     language[_territory][.codeset][@modifier]
   The codeset part in the result is not reliable; the locale_charset()
   should be used for codeset information instead.
   The result must not be freed; it is statically allocated.  */
extern const char * gl_locale_name (int category, const char *categoryname);

/* Determine the current per-thread locale's name, as specified by uselocale()
   calls.
   CATEGORY is a locale category abbreviation, as defined in <locale.h>,
   but not LC_ALL. E.g. LC_MESSAGES.
   CATEGORYNAME is the name of CATEGORY as a string, e.g. "LC_MESSAGES".
   Return the locale category's name, canonicalized into XPG syntax
     language[_territory][.codeset][@modifier]
   or NULL if no locale has been specified for the current thread.
   The codeset part in the result is not reliable; the locale_charset()
   should be used for codeset information instead.
   The result must not be freed; it is statically allocated.  */
extern const char * gl_locale_name_thread (int category, const char *categoryname);

/* Determine the thread-independent current locale's name, as specified by
   setlocale() calls or by environment variables.
   CATEGORY is a locale category abbreviation, as defined in <locale.h>,
   but not LC_ALL. E.g. LC_MESSAGES.
   CATEGORYNAME is the name of CATEGORY as a string, e.g. "LC_MESSAGES".
   Return the locale category's name, canonicalized into XPG syntax
     language[_territory][.codeset][@modifier]
   or NULL if no locale has been specified to setlocale() or by environment
   variables.
   The codeset part in the result is not reliable; the locale_charset()
   should be used for codeset information instead.
   The result must not be freed; it is statically allocated.  */
extern const char * gl_locale_name_posix (int category, const char *categoryname);

/* Determine the default locale's name, as specified by environment
   variables.
   Return the locale category's name, or NULL if no locale has been specified
   by environment variables.
   The result must not be freed; it is statically allocated.  */
extern const char * gl_locale_name_environ (int category, const char *categoryname);

/* Determine the default locale's name.  This is the current locale's name,
   if not specified by uselocale() calls, by setlocale() calls, or by
   environment variables.  This locale name is usually determined by systems
   settings that the user can manipulate through a GUI.

   Quoting POSIX:2001:
     "All implementations shall define a locale as the default locale,
      to be invoked when no environment variables are set, or set to the
      empty string.  This default locale can be the C locale or any other
      implementation-defined locale.  Some implementations may provide
      facilities for local installation administrators to set the default
      locale, customizing it for each location.  IEEE Std 1003.1-2001 does
      not require such a facility."

   The result must not be freed; it is statically allocated.  */
extern const char * gl_locale_name_default (void)
#if !(HAVE_CFPREFERENCESCOPYAPPVALUE || defined _WIN32 || defined __CYGWIN__)
  _GL_ATTRIBUTE_CONST
#endif
  ;


/* These functions with the '_unsafe' suffix are like the functions without
   this suffix, above, except that the result is not statically allocated, but
   instead only valid in the current thread, until the next uselocale(),
   setlocale(), newlocale(), or freelocale() call.  */
extern const char * gl_locale_name_unsafe (int category,
                                           const char *categoryname);
extern const char * gl_locale_name_thread_unsafe (int category,
                                                  const char *categoryname);
extern const char * gl_locale_name_posix_unsafe (int category,
                                                 const char *categoryname);


#ifdef __cplusplus
}
#endif

#endif /* _GL_LOCALENAME_H */
