/* Header file for the sqlite3 integration.

Copyright (C) 2021 Free Software Foundation, Inc.

This file is NOT part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef EMACS_SQLITE_H
#define EMACS_SQLITE_H

#include "lisp.h"

INLINE_HEADER_BEGIN

struct Lisp_Sqlite
{
  union vectorlike_header header;
  void *db;
  void *stmt;
  char *name;
  void (*finalizer) (void *);
  bool eof;
  bool is_statement;
};

INLINE bool
SQLITEP (Lisp_Object x)
{
  return PSEUDOVECTORP (x, PVEC_SQLITE);
}

INLINE struct Lisp_Sqlite *
XSQLITE (Lisp_Object a)
{
  eassert (SQLITEP (a));
  return XUNTAG (a, Lisp_Vectorlike, struct Lisp_Sqlite);
}

INLINE void
CHECK_SQLITE (Lisp_Object sql)
{
  CHECK_TYPE (SQLITEP (sql), Qsqlitep, sql);
}

INLINE_HEADER_END

#endif /* EMACS_SQLITE_H */
