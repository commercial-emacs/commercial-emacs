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
#include <tree_sitter/highlight.h>

#define BUFFER_TO_SITTER(byte) ((uint32_t) CHAR_TO_BYTE (byte) - 1)
#define SITTER_TO_BUFFER(byte) (BYTE_TO_CHAR ((EMACS_INT) byte) + 1)

typedef TSLanguage *(*TSLanguageFunctor) (void);

static Lisp_Object
make_tree_sitter (TSParser *parser, TSTree *tree, Lisp_Object progmode_arg)
{
  struct Lisp_Tree_Sitter *sitter
    = ALLOCATE_PSEUDOVECTOR
    (struct Lisp_Tree_Sitter, progmode, PVEC_TREE_SITTER);

  CHECK_SYMBOL (progmode_arg);

  sitter->progmode = progmode_arg;
  sitter->parser = parser;
  sitter->prev_tree = NULL;
  sitter->tree = tree;
  return make_lisp_ptr (sitter, Lisp_Vectorlike);
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

static Lisp_Object
tree_sitter_create (Lisp_Object progmode)
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


DEFUN ("tree-sitter-highlights",
       Ftree_sitter_highlights, Stree_sitter_highlights,
       2, 2, "r",
       doc: /* Highlight BEG to END. */)
  (Lisp_Object beg, Lisp_Object end)
{
  Lisp_Object retval = Qnil, sitter,
    alist = Fsymbol_value (Qtree_sitter_highlight_alist);
  const EMACS_INT count = XFIXNUM (Flength (alist));
  const char **highlight_names = xmalloc(sizeof (char *) * count);
  intptr_t i = 0;
  FOR_EACH_TAIL (alist)
    {
      CHECK_STRING (XCAR (XCAR (alist)));
      highlight_names[i++] = SSDATA (XCAR (XCAR (alist)));
    }
  alist = Fsymbol_value (Qtree_sitter_highlight_alist);

  CHECK_FIXNUM (beg);
  CHECK_FIXNUM (end);
  sitter = Ftree_sitter (Fcurrent_buffer ());
  if (! NILP (sitter))
    {
      char *scope;
      TSHighlightError ts_highlight_error = TSHighlightOk;
      const char *error = NULL;
      TSHighlighter *ts_highlighter =
	ts_highlighter_new (highlight_names, highlight_names, (uint32_t) count);
      Lisp_Object suberror = Qnil, language =
	Fcdr_safe (Fassq (XTREE_SITTER (sitter)->progmode,
			  Fsymbol_value (Qtree_sitter_mode_alist))),
	highlights_scm;

      eassert (! NILP (language));

      USE_SAFE_ALLOCA;
      scope = SAFE_ALLOCA (strlen ("scope.") + SCHARS (language) + 1);
      sprintf (scope, "scope.%s", SSDATA (language));

      highlights_scm =
	Flocate_file_internal (concat3 (build_string ("queries/"), language, build_string ("/highlights.scm")),
			       Vload_path, Qnil, Qnil);
      if (! NILP (highlights_scm))
	{
	  char *highlights_query;
	  long highlights_query_length;
	  FILE *fp = fopen (SSDATA (highlights_scm), "rb");
	  if (!fp) {
	      suberror = highlights_scm;
	      error = "Cannot fopen";
	      goto finally;
	  }
	  fseek (fp, 0L, SEEK_END);
	  highlights_query_length = ftell (fp);
	  rewind(fp);

	  highlights_query = xzalloc(highlights_query_length + 1);

	  if (1 != fread (highlights_query, highlights_query_length, 1, fp))
	    {
	      suberror = highlights_scm;
	      error = "Cannot fread";
	    }
	  else
	    {
	      ts_highlight_error =
		ts_highlighter_add_language
		(ts_highlighter,
		 scope,
		 NULL,
		 ts_parser_language (XTREE_SITTER (sitter)->parser),
		 highlights_query,
		 "",
		 "",
		 strlen (highlights_query),
		 0,
		 0);
	      if (ts_highlight_error != TSHighlightOk)
		{
		  suberror = make_fixnum (ts_highlight_error);
		  error = "ts_highlighter_add_language non-Ok return";
		}
	    }
	  xfree (highlights_query);
	  fclose (fp);
	  if (error != NULL)
	    goto finally;
	}
      else
	{
	  suberror = language;
	  error = "Could not locate highlights scm";
	  goto finally;
	}

      for (TSNode node = ts_node_first_child_for_byte
	     (ts_tree_root_node (XTREE_SITTER (sitter)->tree),
	      BUFFER_TO_SITTER (XFIXNUM (beg)));
	   (! ts_node_is_null(node)
	    && ts_node_start_byte (node) < BUFFER_TO_SITTER (XFIXNUM (end)));
	   node = ts_node_next_sibling (node))
	{
	  Lisp_Object
	    node_start = make_fixnum (SITTER_TO_BUFFER (ts_node_start_byte (node))),
	    node_end = make_fixnum (SITTER_TO_BUFFER (ts_node_end_byte (node))),
	    source_code = Fbuffer_substring_no_properties (node_start, node_end);
	  TSHighlightEventSlice ts_highlight_event_slice =
	    (TSHighlightEventSlice) { NULL, 0 };
	  TSHighlightBuffer *ts_highlight_buffer = ts_highlight_buffer_new ();

	  /* source code is relative coords */
	  uint32_t restore_start = node.context[0];
	  node.context[0] = 0;

	  ts_highlight_event_slice =
	    ts_highlighter_return_highlights (ts_highlighter, scope,
					      SSDATA (source_code),
					      (uint32_t) SBYTES (source_code),
					      XTREE_SITTER (sitter)->tree,
					      &node,
					      ts_highlight_buffer);

	  /* restore to absolute coords */
	  node.context[0] = restore_start;

	  for (int i=ts_highlight_event_slice.len-1; i>=0; --i)
	    {
	      const TSHighlightEvent *ev = &ts_highlight_event_slice.arr[i];
	      eassert (ev->index < count);
	      if (ev->index >= TSHighlightEventTypeStartMin) {
		Lisp_Object name = make_string (highlight_names[ev->index],
						strlen (highlight_names[ev->index]));
		retval = Fcons (Fcdr (Fassoc (name, alist, Qnil)), retval);
	      } else if (ev->index == TSHighlightEventTypeSource) {
		uint32_t offset = ts_node_start_byte (node);
		retval =
		  Fcons (Fcons
			 (make_fixnum (SITTER_TO_BUFFER (ev->start + offset)),
			  make_fixnum (SITTER_TO_BUFFER (ev->end + offset))),
			 retval);
	      } else if (ev->index == TSHighlightEventTypeEnd) {
		retval = Fcons (Qnil, retval);
	      }
	    }

	  ts_highlighter_free_highlights (ts_highlight_event_slice);
	  ts_highlight_buffer_delete (ts_highlight_buffer);
	}

    finally:
      SAFE_FREE ();
      if (ts_highlighter != NULL)
	ts_highlighter_delete (ts_highlighter);
      if (error != NULL)
	xsignal2 (Qtree_sitter_error,
		  build_string (error),
		  suberror);
    }
  xfree (highlight_names);
  return retval;
}

DEFUN ("tree-sitter-root-node",
       Ftree_sitter_root_node, Stree_sitter_root_node,
       0, 1, 0,
       doc: /* Return TSNode stash from BUFFER's sitter. */)
  (Lisp_Object buffer)
{
  Lisp_Object retval = Qnil, sitter;

  if (NILP (buffer))
    buffer = Fcurrent_buffer ();

  CHECK_BUFFER (buffer);
  sitter = Ftree_sitter (buffer);

  if (! NILP (sitter))
    {
      const TSTree *tree = XTREE_SITTER (sitter)->tree;
      if (tree != NULL)
	retval = build_string (ts_node_string (ts_tree_root_node (tree)));
    }
  return retval;
}

DEFUN ("tree-sitter-changed-range",
       Ftree_sitter_changed_range, Stree_sitter_changed_range,
       0, 1, 0,
       doc: /* Return list of BEG and END of TSRange. */)
  (Lisp_Object buffer)
{
  Lisp_Object retval = Qnil, sitter;

  if (NILP (buffer))
    buffer = Fcurrent_buffer ();

  CHECK_BUFFER (buffer);
  sitter = Ftree_sitter (buffer);

  if (! NILP (sitter))
    {
      const TSTree *tree = XTREE_SITTER (sitter)->tree,
	*prev_tree = XTREE_SITTER (sitter)->prev_tree;
      if (tree != NULL && prev_tree != NULL)
	{
	  uint32_t count;
	  TSRange *range = ts_tree_get_changed_ranges (prev_tree, tree, &count);
	  retval = list2 (make_fixnum (SITTER_TO_BUFFER (range->start_byte)),
			  make_fixnum (SITTER_TO_BUFFER (range->end_byte)));
	  free (range);
	}
    }
  return retval;
}

static const char*
tree_sitter_read_buffer (void *payload, uint32_t byte_index,
                         TSPoint position, uint32_t *bytes_read)
{
  static int thread_unsafe_last_scan_characters = -1;
  static char *thread_unsafe_return_value = NULL;
  EMACS_INT start = SITTER_TO_BUFFER (byte_index);
  struct buffer *bp = (struct buffer *) payload;
  ptrdiff_t pdl_count = SPECPDL_INDEX ();

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

  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  Fwiden ();
  sprintf (thread_unsafe_return_value, "%s",
           SSDATA (Fbuffer_substring_no_properties
                   (make_fixnum (start),
                    make_fixnum (min (start + tree_sitter_scan_characters,
                                      BUF_Z (bp) - BUF_BEG (bp) + 1)))));
  unbind_to (pdl_count, Qnil);
  if (bytes_read)
    *bytes_read = strlen (thread_unsafe_return_value);
  return thread_unsafe_return_value;
}

DEFUN ("tree-sitter",
       Ftree_sitter, Stree_sitter,
       0, 1, 0,
       doc: /* Return BUFFER's Lisp_Tree_Sitter. */)
  (Lisp_Object buffer)
{
  Lisp_Object sitter;
  if (NILP (buffer))
    buffer = Fcurrent_buffer ();

  CHECK_BUFFER (buffer);

  sitter = Fbuffer_local_value (Qtree_sitter_sitter, buffer);
  if (NILP (sitter)
      || ! EQ (XTREE_SITTER (sitter)->progmode, BVAR (XBUFFER (buffer), major_mode)))
    {
      sitter = Fset (Qtree_sitter_sitter,
		     tree_sitter_create (BVAR (XBUFFER (buffer), major_mode)));
    }

  if (! NILP (sitter))
    {
      if (XTREE_SITTER (sitter)->tree == NULL)
	{
	  XTREE_SITTER (sitter)->tree =
	    ts_parser_parse (XTREE_SITTER (sitter)->parser,
			     XTREE_SITTER (sitter)->tree,
			     (TSInput) {
			       XBUFFER (buffer),
			       tree_sitter_read_buffer,
			       TSInputEncodingUTF8
			     });
	  if (XTREE_SITTER (sitter)->tree == NULL)
	    xsignal1 (Qtree_sitter_parse_error, BVAR (XBUFFER (buffer), name));
	}
    }
  return sitter;
}

/* TODO buffers not utf-8-clean. */
void
tree_sitter_record_change (ptrdiff_t start_char, ptrdiff_t old_end_char,
			   ptrdiff_t new_end_char)
{
  Lisp_Object sitter = Fbuffer_local_value (Qtree_sitter_sitter, Fcurrent_buffer ());
  if (! NILP (sitter))
    {
      TSTree *tree = XTREE_SITTER (sitter)->tree;
      if (tree != NULL)
	{
	  static const TSPoint dummy_point = { 0, 0 };
	  TSInputEdit edit = {
	    BUFFER_TO_SITTER (start_char),
	    BUFFER_TO_SITTER (old_end_char),
	    BUFFER_TO_SITTER (new_end_char),
	    dummy_point,
	    dummy_point,
	    dummy_point
	  };

	  if (XTREE_SITTER (sitter)->prev_tree != NULL)
	    ts_tree_delete (XTREE_SITTER (sitter)->prev_tree);
	  XTREE_SITTER (sitter)->prev_tree = ts_tree_copy (tree);
	  ts_tree_edit (tree, &edit);
	  XTREE_SITTER (sitter)->tree =
	    ts_parser_parse (XTREE_SITTER (sitter)->parser,
			     tree,
			     (TSInput) {
			       current_buffer,
			       tree_sitter_read_buffer,
			       TSInputEncodingUTF8
			     });
	  ts_tree_delete (tree);
	}
      else
	{
	  xsignal1 (Qtree_sitter_error, BVAR (XBUFFER (Fcurrent_buffer ()), name));
	}
    }
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
  start_byte0 = XFIXNUM (beg);
  end_byte0 = XFIXNUM (end);

  if (CONSP (pattern))
    pattern = Ftree_sitter_expand_pattern (pattern);
  CHECK_STRING (pattern);

  query = ts_query_new (fn (), SSDATA (pattern), strlen (SSDATA (pattern)),
			&error_offset, &error_type);

  if (query == NULL)
    xsignal1 (Qtree_sitter_query_error, make_fixnum (error_type));

  cursor = ts_query_cursor_new ();

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
  DEFSYM (Qtree_sitter_parse_error, "tree-sitter-parse-error");
  DEFSYM (Qtree_sitter_query_error, "tree-sitter-query-error");
  DEFSYM (Qtree_sitter_language_error, "tree-sitter-language-error");
  DEFSYM (Qtree_sitterp, "tree-sitterp");

  define_error (Qtree_sitter_error, "Generic tree-sitter exception", Qerror);
  define_error (Qtree_sitter_parse_error, "Parse error",
		Qtree_sitter_error);
  define_error (Qtree_sitter_query_error, "Query pattern is malformed",
		Qtree_sitter_error);
  define_error (Qtree_sitter_language_error, "Cannot load language",
		Qtree_sitter_error);

  DEFVAR_INT ("tree-sitter-scan-characters", tree_sitter_scan_characters,
	      doc: /* Number of characters to read per tree sitter scan.  */);
  tree_sitter_scan_characters = 1024;

  DEFSYM (Qtree_sitter_mode_alist, "tree-sitter-mode-alist");
  DEFSYM (Qtree_sitter_highlight_alist, "tree-sitter-highlight-alist");
  DEFSYM (Qtree_sitter_sitter, "tree-sitter-sitter");
  Fmake_variable_buffer_local (Qtree_sitter_sitter);

  defsubr (&Stree_sitter);
  defsubr (&Stree_sitter_root_node);
  defsubr (&Stree_sitter_highlights);
  defsubr (&Stree_sitter_changed_range);

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
