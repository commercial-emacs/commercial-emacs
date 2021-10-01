/* Tree-sitter integration for GNU Emacs.

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

#include <config.h>

#include "lisp.h"
#include "buffer.h"
#include "tree-sitter.h"

typedef TSLanguage *(*TSLanguageFunctor) (void);

static Lisp_Object
make_tree_sitter (TSParser *parser, TSTree *tree, Lisp_Object progmode_arg)
{
  struct Lisp_Tree_Sitter *sitter
    = ALLOCATE_PSEUDOVECTOR
    (struct Lisp_Tree_Sitter, progmode, PVEC_TREE_SITTER);
  CHECK_SYMBOL (progmode_arg);

  sitter->progmode = Fsymbol_name (progmode_arg);
  sitter->parser = parser;
  sitter->tree = tree;
  return make_lisp_ptr (sitter, Lisp_Vectorlike);
}

static void
tree_sitter_parse (Lisp_Object buffer)
{
  Lisp_Object state;
  CHECK_BUFFER (buffer);
  state = Fbuffer_local_value (Qtree_sitter_buffer_state, buffer);
  if (NILP (state))
    {

    }

  if (! NILP (state))
    {

      TSTree *tree = XTREE_SITTER (state)->tree;

      TSInput input = (TSInput) { current_buffer, tree_sitter_read_buffer, TSInputEncodingUTF8 };


      TSTree *new_tree = ts_parser_parse(ts_parser, tree, input);
      if (new_tree == NULL)
	{
	  Lisp_Object buf;
	  XSETBUFFER (buf, buffer);
	  xsignal1 (Qtree_sitter_parse_error, buf);
	}

      ts_tree_delete (tree);
      XTS_PARSER (parser)->tree = new_tree;
    }
}

static TSLanguageFunctor
tree_sitter_language_functor (Lisp_Object progmode)
{
  static Lisp_Object cache = LISPSYM_INITIALLY (Qnil);
  struct Lisp_Hash_Table *h;
  Lisp_Object hash;
  ptrdiff_t i;

  if (NILP (cache))
    {
      cache = make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE,
			       DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
			       Qnil, false);
      staticpro (&cache);
    }

  CHECK_SYMBOL (progmode);
  h = XHASH_TABLE (cache);
  i = hash_lookup (h, progmode, &hash);
  if (i < 0)
    {
      Lisp_Object language =
	Fcdr_safe (Fassq (progmode, Fsymbol_value (Qtree_sitter_mode_alist)));
      if (! NILP (language))
	{
	  Lisp_Object module_stem = concat2 (build_string ("bin/"), language);
	  Lisp_Object module = Flocate_file_internal (module_stem,
						      Vload_path,
						      list1 (Vmodule_file_suffix),
						      Qnil);
	  if (NILP (module))
	    xsignal2 (Qtree_sitter_language_error,
		      module_stem, build_string ("Could not locate module"));
	  else
	    {
	      dynlib_handle_ptr handle = dynlib_open (SSDATA (module));
	      if (handle == NULL)
		xsignal2 (Qtree_sitter_language_error,
			  module, build_string (dynlib_error ()));
	      else
		{
		  TSLanguageFunctor fn;
		  dynlib_error ();
		  fn = dynlib_sym (handle,
				   SSDATA (concat2 (build_string ("tree_sitter_"),
						    language)));
		  if (fn == NULL)
		    xsignal2 (Qtree_sitter_language_error,
			      module, build_string (dynlib_error ()));
		  else
		    i = hash_put (h, progmode, make_misc_ptr (fn), hash);
		}
	    }
	}
    }
  return i >= 0 ? (TSLanguageFunctor) (xmint_pointer (HASH_VALUE (h, i))) : NULL;
}

/* The best approximation of byte_index in tree-sitter space is
   a one-indexed character position (not a byte position) for a buffer
   assumed to be utf-8-clean.  */
void
tree_sitter_record_change (ptrdiff_t start_char, ptrdiff_t old_end_char,
			   ptrdiff_t new_end_char)
{
  Lisp_Object state = Fbuffer_local_value (Qtree_sitter_buffer_state, Fcurrent_buffer ());
  if (! NILP (state))
    {
      TSTree *tree = XTREE_SITTER (state)->tree;
      if (tree != NULL)
	{
	  static const TSPoint dummy_point = { 0, 0 };
	  TSInputEdit edit = {
	    (uint32_t) CHAR_TO_BYTE (start_char),
	    (uint32_t) CHAR_TO_BYTE (old_end_char),
	    (uint32_t) CHAR_TO_BYTE (new_end_char),
	    dummy_point,
	    dummy_point,
	    dummy_point
	  };
	  ts_tree_edit (tree, &edit);
	}
    }
}

/* The best approximation of byte_index in tree-sitter space is
   a one-indexed character position (not a byte position) for a buffer
   assumed to be utf-8-clean.  */
static const char*
tree_sitter_read_buffer (void *payload, uint32_t byte_index,
			 TSPoint position, uint32_t *bytes_read)
{
  static int thread_unsafe_last_scan_characters = -1;
  static char *thread_unsafe_return_value = NULL;
  EMACS_INT start = (EMACS_INT) byte_index + 1;
  struct buffer *bp = (struct buffer *) payload;

  if (thread_unsafe_last_scan_characters != tree_sitter_scan_characters)
    {
      if (thread_unsafe_return_value == NULL)
	thread_unsafe_return_value = xmalloc (tree_sitter_scan_characters + 1);
      else
	thread_unsafe_return_value = xrealloc (thread_unsafe_return_value,
					       tree_sitter_scan_characters + 1);
    }

  if (! BUFFER_LIVE_P (bp))
    error ("Selecting deleted buffer");

  sprintf (thread_unsafe_return_value, "%s",
	   SSDATA (Fbuffer_substring_no_properties
		   (make_fixnum (start),
		    make_fixnum (min (start + tree_sitter_scan_characters,
				      BUF_Z (bp) - BUF_BEG (bp) + 1)))));
  return thread_unsafe_return_value;
}

DEFUN ("tree-sitter-create",
       Ftree_sitter_create, Stree_sitter_create,
       2, 2, 0,
       doc: /* Create and return a tree-sitter in BUFFER for PROGMODE. */)
  (Lisp_Object progmode)
{
  Lisp_Object tree_sitter = Qnil;
  TSLanguageFunctor fn;

  CHECK_SYMBOL (progmode);

  fn = tree_sitter_language_functor (progmode);
  if (fn != NULL)
    {
      TSParser *ts_parser = ts_parser_new ();
      ts_parser_set_language (ts_parser, fn ());
      tree_sitter = make_tree_sitter (ts_parser, NULL, progmode);
    }
  return tree_sitter;
}

DEFUN ("tree-sitter-expand-pattern",
       Ftree_sitter_expand_pattern,
       Stree_sitter_expand_pattern, 1, 1, 0,
       doc: /* Expand PATTERN to its string form.

PATTERN can be

    :anchor
    :?
    :*
    :+
    (TYPE PATTERN...)
    [PATTERN...]
    FIELD-NAME:
    @CAPTURE-NAME
    (_)
    _
    \"TYPE\"  */)
  (Lisp_Object pattern)
{
  if (EQ (pattern, intern_c_string (":anchor")))
    return build_pure_c_string(".");
  if (EQ (pattern, intern_c_string (":?")))
    return build_pure_c_string("?");
  if (EQ (pattern, intern_c_string (":*")))
    return build_pure_c_string("*");
  if (EQ (pattern, intern_c_string (":+")))
    return build_pure_c_string("+");
  if (VECTORP (pattern) || CONSP (pattern))
    return concat3 (build_pure_c_string (VECTORP (pattern) ? "[" : "("),
		    Fmapconcat (intern_c_string
				("tree-sitter-expand-pattern"),
				pattern,
				build_pure_c_string (" ")),
		    build_pure_c_string (VECTORP (pattern) ? "]" : ")"));
  return CALLN (Fformat, build_pure_c_string("%S"), pattern);
}

static void
char_to_byte0 (EMACS_INT start, EMACS_INT end,
	       uint32_t *start_byte0, uint32_t *end_byte0)
{
  *start_byte0 = start;
  *end_byte0 = end;
}

DEFUN ("tree-sitter-query-capture",
       Ftree_sitter_query_capture,
       Stree_sitter_query_capture, 4, 4, 0,
       doc: /* Query NODE with PATTERN.

Returns a list of (CAPTURE_NAME . NODE).  CAPTURE_NAME is the name
assigned to the node in PATTERN.  NODE is the captured node.

PATTERN is either a string containing one or more matching patterns,
or a list containing one or more s-expression matching patterns.

BEG and END specify the query range.

Raise an tree-sitter-query-error if PATTERN is malformed.  See the
info node for how to read the error message.  */)
  (Lisp_Object node, Lisp_Object pattern,
   Lisp_Object beg, Lisp_Object end)
{
  TSNode *ts_node = (TSNode *) xmint_pointer (node);
  TSQueryError error_type;
  TSQuery *query;
  TSQueryCursor *cursor;
  TSQueryMatch match;
  uint32_t error_offset, start_byte0, end_byte0;
  Lisp_Object result = Qnil;
  TSLanguageFunctor fn =
    tree_sitter_language_functor (BVAR (current_buffer, major_mode));

  validate_region (&beg, &end);

  if (CONSP (pattern))
    pattern = Ftree_sitter_expand_pattern (pattern);
  CHECK_STRING (pattern);

  query = ts_query_new (fn (), SSDATA (pattern), strlen (SSDATA (pattern)),
			&error_offset, &error_type);

  if (query == NULL)
    xsignal1 (Qtree_sitter_query_error, make_fixnum (error_type));

  cursor = ts_query_cursor_new ();
  char_to_byte0 (XFIXNUM (beg), XFIXNUM (end), &start_byte0, &end_byte0);
  ts_query_cursor_set_byte_range (cursor, start_byte0, end_byte0);
  ts_query_cursor_exec (cursor, query, *ts_node);

  while (ts_query_cursor_next_match (cursor, &match))
    {
      const TSQueryCapture *captures = match.captures;
      for (int idx = 0; idx < match.capture_count; idx++)
	{
	  TSQueryCapture capture;
	  Lisp_Object captured_node;
	  const char *capture_name;
	  Lisp_Object entry;
	  uint32_t capture_name_len;

	  capture = captures[idx];
	  /* captured_node = make_tree_sitter_node (lisp_parser, capture.node); */
	  captured_node = Qnil;
	  capture_name = ts_query_capture_name_for_id
	    (query, capture.index, &capture_name_len);
	  entry = Fcons (intern_c_string (capture_name), captured_node);
	  result = Fcons (entry, result);
	}
    }
  ts_query_delete (query);
  ts_query_cursor_delete (cursor);
  return Freverse (result);
}

void
syms_of_tree_sitter (void)
{
  DEFSYM (Qtree_sitter_error, "tree-sitter-error");
  DEFSYM (Qtree_sitter_query_error, "tree-sitter-query-error");
  DEFSYM (Qtree_sitter_language_error, "tree-sitter-language-error");

  define_error (Qtree_sitter_error, "Generic tree-sitter exception", Qerror);
  define_error (Qtree_sitter_query_error, "Query pattern is malformed",
		Qtree_sitter_error);
  define_error (Qtree_sitter_language_error, "Cannot load language",
		Qtree_sitter_error);

  DEFVAR_INT ("tree-sitter-scan-characters", tree_sitter_scan_characters,
	      doc: /* Number of characters to read per tree sitter scan.  */);
  tree_sitter_scan_characters = 1024;

  DEFSYM (Qtree_sitter_mode_alist, "tree-sitter-mode-alist");

  DEFSYM (Qtree_sitter_buffer_state, "tree-sitter-buffer-state");
  Fmake_variable_buffer_local (Qtree_sitter_buffer_state);

  defsubr (&Stree_sitter_create);

  /* defsubr (&Stree_sitter_node_type); */
  /* defsubr (&Stree_sitter_node_start); */
  /* defsubr (&Stree_sitter_node_end); */
  /* defsubr (&Stree_sitter_node_string); */
  /* defsubr (&Stree_sitter_node_parent); */
  /* defsubr (&Stree_sitter_node_child); */
  /* defsubr (&Stree_sitter_node_check); */
  /* defsubr (&Stree_sitter_node_child_by_field_name); */
  /* defsubr (&Stree_sitter_node_next_sibling); */
  /* defsubr (&Stree_sitter_node_prev_sibling); */
  /* defsubr (&Stree_sitter_node_first_child_for_pos); */
  /* defsubr (&Stree_sitter_node_eq); */
  defsubr (&Stree_sitter_expand_pattern);
  defsubr (&Stree_sitter_query_capture);
}
