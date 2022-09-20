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
#include <ctype.h>

#include "lisp.h"
#include "tree-sitter.h"

typedef TSLanguage *(*TSLanguageFunctor) (void);
typedef Lisp_Object (*HighlightsFunctor) (const TSHighlightEventSlice *, TSNode, const char **);

static Lisp_Object
make_sitter (TSParser *parser, TSTree *tree, Lisp_Object progmode_arg)
{
  struct Lisp_Tree_Sitter *ptr =
    ALLOCATE_PSEUDOVECTOR (struct Lisp_Tree_Sitter, progmode, PVEC_TREE_SITTER);

  CHECK_SYMBOL (progmode_arg);

  ptr->progmode = progmode_arg;
  ptr->parser = parser;
  ptr->prev_tree = NULL;
  ptr->tree = tree;
  ptr->highlighter = NULL;
  ptr->highlight_names = NULL;
  ptr->highlights_query = NULL;
  ptr->indents_query = NULL;
  return make_lisp_ptr (ptr, Lisp_Vectorlike);
}

/* I would make this a Lisp_Misc_Ptr or PVEC_OTHER but TSNode are
   stack-allocated structs and I need CHECK_TREE_SITTER_NODE.  */

static Lisp_Object
make_node (TSNode node)
{
  struct Lisp_Tree_Sitter_Node *ptr =
    ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Tree_Sitter_Node, PVEC_TREE_SITTER_NODE);
  ptr->node = node;
  return make_lisp_ptr (ptr, Lisp_Vectorlike);
}

/* I would make this a Lisp_Misc_Ptr or PVEC_OTHER but TSTreeCursor
   needs bespoke deletion in free_by_pvectype ().  */

static Lisp_Object
make_cursor (TSTreeCursor cursor)
{
  struct Lisp_Tree_Sitter_Cursor *ptr =
    ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_Tree_Sitter_Cursor, PVEC_TREE_SITTER_CURSOR);
  ptr->cursor = cursor;
  return make_lisp_ptr (ptr, Lisp_Vectorlike);
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
	  Lisp_Object module = concat2 (Ffile_name_as_directory (Fsymbol_value (Qtree_sitter_resources_dir)), concat3 (build_string ("lib/"), language, Vmodule_file_suffix));
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
      else
	xsignal1 (Qtree_sitter_language_error,
		  concat2 (build_string ("No entry for "),
			   (concat3 (Fsymbol_name (progmode),
				     build_string (" in "),
				     Fsymbol_name (Qtree_sitter_mode_alist)))));
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
      tree_sitter = make_sitter (ts_parser, NULL, progmode);
    }
  return tree_sitter;
}

static Lisp_Object
list_highlights (const TSHighlightEventSlice *slice, TSNode node,
		 const char **highlight_names)
{
  Lisp_Object
    retval = Qnil,
    alist = Fsymbol_value (Qtree_sitter_highlight_alist);
  const EMACS_INT count = XFIXNUM (Flength (alist));
  const uint32_t offset = ts_node_start_byte (node);

  for (int i = 0; i < slice->len; ++i)
    {
      const TSHighlightEvent *ev = &(slice->arr[i]);
      eassert (ev->index < count);
      if (ev->index >= TSHighlightEventTypeStartMin) {
	Lisp_Object name = make_string (highlight_names[ev->index],
					strlen (highlight_names[ev->index]));
	retval = Fcons (Fcdr (Fassoc (name, alist, Qnil)), retval);
      } else if (ev->index == TSHighlightEventTypeSource) {
	retval =
	  Fcons (Fcons
		 (make_fixnum (SITTER_TO_BUFFER (ev->start + offset)),
		  make_fixnum (SITTER_TO_BUFFER (ev->end + offset))),
		 retval);
      } else if (ev->index == TSHighlightEventTypeEnd) {
	retval = Fcons (Qnil, retval);
      }
    }
  return retval;
}

static Lisp_Object
highlight_region (const TSHighlightEventSlice *slice,
		  const TSNode node, const char **highlight_names)
{
  Lisp_Object
    alist = Fsymbol_value (Qtree_sitter_highlight_alist),
    prevailing = Qnil;
  const EMACS_INT count = XFIXNUM (Flength (alist));
  const uint32_t offset = ts_node_start_byte (node);
  ptrdiff_t smallest = PTRDIFF_MAX, biggest = PTRDIFF_MIN;

  for (int i=0; i<slice->len; ++i)
    {
      const TSHighlightEvent *ev = &(slice->arr[i]);
      eassert (ev->index < count);
      if (ev->index >= TSHighlightEventTypeStartMin) {
	Lisp_Object name = make_string (highlight_names[ev->index],
					strlen (highlight_names[ev->index]));
	prevailing = Fcdr (Fassoc (name, alist, Qnil));
      } else if (ev->index == TSHighlightEventTypeSource) {
	ptrdiff_t beg = SITTER_TO_BUFFER (ev->start + offset),
	  end = SITTER_TO_BUFFER (ev->end + offset);
	smallest = min (smallest, beg);
	biggest = max (biggest, end);
	if (NILP (prevailing))
	  Fremove_text_properties (make_fixnum (beg), make_fixnum (end),
				   list2 (Qface, Qt), Qnil);
	else
	  Fput_text_property (make_fixnum (beg), make_fixnum (end),
			      Qface, prevailing, Qnil);

      } else if (ev->index == TSHighlightEventTypeEnd) {
	prevailing = Qnil;
      }
    }
  return smallest > biggest
    ? Qnil : list2 (make_fixnum (smallest), make_fixnum (biggest));
}

static TSHighlighter *
ensure_highlighter(Lisp_Object sitter)
{
  TSHighlighter *ret = XTREE_SITTER (sitter)->highlighter;
  if (ret == NULL)
    {
      char *scope;
      const char *error = NULL;
      Lisp_Object
	suberror = Qnil, highlights_scm,
	alist = Fsymbol_value (Qtree_sitter_highlight_alist),
	language = Fcdr_safe (Fassq (XTREE_SITTER (sitter)->progmode,
				     Fsymbol_value (Qtree_sitter_mode_alist)));
      const EMACS_INT count = XFIXNUM (Flength (alist));
      XTREE_SITTER (sitter)->highlight_names = xmalloc(sizeof (char *) * count);
      intptr_t i = 0;
      FILE *fp = NULL;

      eassert (! NILP (language)); /* by tree_sitter_create() */

      FOR_EACH_TAIL (alist)
	{
	  CHECK_STRING (XCAR (XCAR (alist)));
	  XTREE_SITTER (sitter)->highlight_names[i++] =
	    SSDATA (XCAR (XCAR (alist)));
	}
      alist = Fsymbol_value (Qtree_sitter_highlight_alist); /* FOR_EACH_TAIL mucks */

      USE_SAFE_ALLOCA;
      scope = SAFE_ALLOCA (strlen ("scope.") + SCHARS (language) + 1);
      sprintf (scope, "scope.%s", SSDATA (language));

      ret = (XTREE_SITTER (sitter)->highlighter =
	     ts_highlighter_new (XTREE_SITTER (sitter)->highlight_names,
				 XTREE_SITTER (sitter)->highlight_names,
				 (uint32_t) count));
      highlights_scm =
	concat2 (Ffile_name_as_directory (Fsymbol_value (Qtree_sitter_resources_dir)),
		 concat3 (build_string ("queries/"), language,
			  build_string ("/highlights.scm")));

      if (NILP (highlights_scm))
	{
	  suberror = language;
	  error = "Could not locate highlights scm";
	}
      else
	{
	  long highlights_query_length;
	  fp = fopen (SSDATA (highlights_scm), "rb");
	  if (! fp) {
	      suberror = highlights_scm;
	      error = "Cannot fopen";
	      goto finally;
	  }
	  fseek (fp, 0L, SEEK_END);
	  highlights_query_length = ftell (fp);
	  rewind (fp);

	  XTREE_SITTER (sitter)->highlights_query =
	    xzalloc(highlights_query_length + 1);

	  if (1 != fread (XTREE_SITTER (sitter)->highlights_query,
			  highlights_query_length, 1, fp))
	    {
	      suberror = highlights_scm;
	      error = "Cannot fread";
	    }
	  else
	    {
	      TSHighlightError ts_highlight_error =
		ts_highlighter_add_language
		(ret,
		 scope,
		 NULL,
		 ts_parser_language (XTREE_SITTER (sitter)->parser),
		 XTREE_SITTER (sitter)->highlights_query,
		 "",
		 "",
		 strlen (XTREE_SITTER (sitter)->highlights_query),
		 0,
		 0);
	      if (ts_highlight_error != TSHighlightOk)
		{
		  suberror = make_fixnum (ts_highlight_error);
		  error = "ts_highlighter_add_language non-Ok return";
		}
	    }
	}

    finally:
      SAFE_FREE ();
      if (fp != NULL)
	fclose (fp);
      if (error != NULL)
	xsignal2 (Qtree_sitter_error, build_string (error), suberror);
    }
  return ret;
}

static Lisp_Object
do_highlights (Lisp_Object beg, Lisp_Object end, HighlightsFunctor fn)
{
  Lisp_Object retval = Qnil, sitter;

  CHECK_FIXNUM (beg);
  CHECK_FIXNUM (end);
  sitter = Ftree_sitter (Fcurrent_buffer ());
  if (! NILP (sitter))
    {
      TSHighlighter *ts_highlighter = ensure_highlighter (sitter);
      Lisp_Object language = Fcdr_safe (Fassq (XTREE_SITTER (sitter)->progmode,
					       Fsymbol_value (Qtree_sitter_mode_alist)));
      Lisp_Object max_bytes = Fsymbol_value (Qjit_lock_chunk_size);
      char *scope;

      USE_SAFE_ALLOCA;
      scope = SAFE_ALLOCA (strlen ("scope.") + SCHARS (language) + 1);
      sprintf (scope, "scope.%s", SSDATA (language));

      if (ts_highlighter)
	{
	  TSNode node = ts_node_first_child_for_byte
	    (ts_tree_root_node (XTREE_SITTER (sitter)->tree),
	     BUFFER_TO_SITTER (XFIXNUM (beg)));
	  while (! ts_node_is_null (node)
		 && ts_node_start_byte (node) < BUFFER_TO_SITTER (XFIXNUM (end)))
	    {
	      Lisp_Object node_start, node_end, source_code;
	      TSHighlightEventSlice ts_highlight_event_slice =
		(TSHighlightEventSlice) { NULL, 0 };
	      TSHighlightBuffer *ts_highlight_buffer;
	      uint32_t restore_start;

	      if (FIXNUMP (max_bytes)
		  && (XFIXNUM (max_bytes) <
		      ts_node_end_byte (node) - ts_node_start_byte (node)))
		{
		  TSNode prosp = ts_node_first_child_for_byte
		    (node, BUFFER_TO_SITTER (XFIXNUM (beg)));
		  if (! ts_node_is_null (prosp)
		      && ts_node_start_byte (prosp) <= BUFFER_TO_SITTER (XFIXNUM (beg)))
		    {
		      node = prosp;
		      continue;
		    }
		}

	      node_start = make_fixnum (SITTER_TO_BUFFER (ts_node_start_byte (node)));
	      node_end = make_fixnum (SITTER_TO_BUFFER (ts_node_end_byte (node)));
	      source_code = Fbuffer_substring_no_properties (node_start, node_end);
	      ts_highlight_buffer = ts_highlight_buffer_new ();

	      /* source code is relative coords */
	      restore_start = node.context[0];
	      node.context[0] = 0;

	      ts_highlight_event_slice =
		ts_highlighter_return_highlights (ts_highlighter, scope,
						  SSDATA (source_code),
						  (uint32_t) SBYTES (source_code),
						  node,
						  ts_highlight_buffer);

	      /* restore to absolute coords */
	      node.context[0] = restore_start;
	      retval = nconc2 (fn (&ts_highlight_event_slice, node,
				   XTREE_SITTER (sitter)->highlight_names),
			       retval);
	      ts_highlighter_free_highlights (ts_highlight_event_slice);
	      ts_highlight_buffer_delete (ts_highlight_buffer);
	      node = ts_node_next_sibling (node);
	    }
	}

      SAFE_FREE ();
    }
  return retval;
}

DEFUN ("tree-sitter--testable",
       Ftree_sitter__testable, Stree_sitter__testable,
       1, 1, 0,
       doc: /* Is the bundled dynamic library readable by my OS. */)
  (Lisp_Object file)
{
  CHECK_STRING (file);
  return dynlib_open (SSDATA (file)) ? Qt : Qnil;
}

DEFUN ("tree-sitter-node-sexp",
       Ftree_sitter_node_sexp, Stree_sitter_node_sexp,
       1, 1, 0,
       doc: /* Return s-expression of NODE. */)
  (Lisp_Object node)
{
  Lisp_Object retval = Qnil;
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  char *sexp = ts_node_string (XTREE_SITTER_NODE (node)->node);
  retval = build_string (sexp);
  free (sexp);
  return retval;
}

DEFUN ("tree-sitter-root-node",
       Ftree_sitter_root_node, Stree_sitter_root_node,
       0, 1, 0,
       doc: /* Return TSNode from BUFFER's sitter. */)
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
	{
	  TSNode root_node = ts_tree_root_node (tree);
	  if (! ts_node_is_null (root_node))
	    retval = make_node (root_node);
	}
    }
  return retval;
}

DEFUN ("tree-sitter-ppss",
       Ftree_sitter_ppss, Stree_sitter_ppss,
       0, 1, 0,
       doc: /* Return ppss at POS. */)
  (Lisp_Object pos)
{
  uint32_t depth = 0;
  const TSTree *tree;
  TSNode node, parent;
  Lisp_Object retval =
    list (Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil, Qnil),
    sitter = Ftree_sitter (Fcurrent_buffer ());

  if (NILP (pos))
    pos = Fpoint ();

  CHECK_FIXNUM (pos);

  if (NILP (sitter))
    return retval;

  tree = XTREE_SITTER (sitter)->tree;
  if (tree == NULL)
    return retval;

  node = ts_tree_node_at (tree, BUFFER_TO_SITTER (XFIXNUM (pos)));
  if (ts_node_is_null (node))
    return retval;

  parent = ts_node_parent (node);
  if (ts_node_is_null (parent))
    return retval;

  Fsetcar (Fnthcdr (make_fixnum (1), retval),
	   make_fixnum (SITTER_TO_BUFFER (ts_node_start_byte (parent))));

  for (TSNode j = node;
       ! ts_node_is_null (j);
       j = ts_node_parent (j), depth++)
    {
      if (NILP (Fnth (make_fixnum (2), retval)))
	{
	  TSNode sibling = ts_node_prev_sibling (j);
	  if (! ts_node_is_null (sibling))
	    Fsetcar (Fnthcdr (make_fixnum (2), retval),
		     make_fixnum (SITTER_TO_BUFFER (ts_node_start_byte (sibling))));
	}

      if (! NILP (Fstring_match (build_string ("\\bstring"),
				 build_string (ts_node_type (j)), Qnil, Qnil)))
	/* best efforts regex for strings */
	Fsetcar (Fnthcdr (make_fixnum (3), retval),
		 make_fixnum (FETCH_CHAR (SITTER_TO_BUFFER
					  (ts_node_start_byte (j)))));

      if (! NILP (Fstring_match (build_string ("\\bcomment"),
				 build_string (ts_node_type (j)), Qnil, Qnil)))
	/* best efforts regex for comments */
	Fsetcar (Fnthcdr (make_fixnum (4), retval), Qt);

      if (! NILP (Fstring_match (build_string ("\\bescape"),
				 build_string (ts_node_type (j)), Qnil, Qnil))
	  && (BUFFER_TO_SITTER (XFIXNUM (pos)) == ts_node_start_byte (j) + 1))
	/* best efforts regex for escapes */
	Fsetcar (Fnthcdr (make_fixnum (5), retval), Qt);

      if (0 == strcmp ("source_file", ts_node_type (j)))
	{
	  depth--;
	  break;
	}
    }
  Fsetcar (retval, make_fixnum (max (depth, 0)));
  return retval;
}

DEFUN ("tree-sitter-node-of",
       Ftree_sitter_node_of, Stree_sitter_node_of,
       1, 1, 0,
       doc: /* Return node of CURSOR. */)
  (Lisp_Object cursor)
{
  if (NILP (cursor))
    return Qnil;

  CHECK_TREE_SITTER_CURSOR (cursor);

  TSNode node = ts_tree_cursor_current_node
    (&XTREE_SITTER_CURSOR (cursor)->cursor);
  return ts_node_is_null (node)
    ? Qnil
    : make_node (node);
}

DEFUN ("tree-sitter-cursor-at",
       Ftree_sitter_cursor_at, Stree_sitter_cursor_at,
       0, 1, 0,
       doc: /* Return TSTreeCursor at or after POS. */)
  (Lisp_Object pos)
{
  uint32_t byte;
  Lisp_Object root_node;
  TSTreeCursor cursor;

  if (NILP (pos))
    pos = Fpoint ();

  CHECK_FIXNUM (pos);

  byte = BUFFER_TO_SITTER (XFIXNUM (pos));
  root_node = Ftree_sitter_root_node (Fcurrent_buffer ());
  cursor = ts_tree_cursor_new (ts_node_first_child_for_byte
			       (XTREE_SITTER_NODE (root_node)->node,
				byte));
  for (TSNode node = ts_tree_cursor_current_node (&cursor);
       ! ts_node_is_null (node);
       (void) node)
    {
      if (byte < ts_node_start_byte (node))
	break;
      else if (byte >= ts_node_end_byte(node))
	{
	  if (! ts_tree_cursor_goto_next_sibling (&cursor))
	    break;
	  node = ts_tree_cursor_current_node (&cursor);
	}
      else if (! ts_tree_cursor_goto_first_child (&cursor))
	break;
      else
	node = ts_tree_cursor_current_node (&cursor);
    }
  return make_cursor (cursor);
}

DEFUN ("tree-sitter-goto-first-child",
       Ftree_sitter_goto_first_child, Stree_sitter_goto_first_child,
       1, 1, 0,
       doc: /* Move CURSOR to its first child.
This is a no-op if CURSOR is at a leaf node.  */)
  (Lisp_Object cursor)
{
  if (NILP (cursor))
    return Qnil;

  CHECK_TREE_SITTER_CURSOR (cursor);

  ts_tree_cursor_goto_first_child (&XTREE_SITTER_CURSOR (cursor)->cursor);
  return cursor;
}

DEFUN ("tree-sitter-goto-next-sibling",
       Ftree_sitter_goto_next_sibling, Stree_sitter_goto_next_sibling,
       1, 1, 0,
       doc: /* Move CURSOR to the next sibling of its current node.
This is a no-op if no sibling follows.  */)
  (Lisp_Object cursor)
{
  if (NILP (cursor))
    return Qnil;

  CHECK_TREE_SITTER_CURSOR (cursor);

  ts_tree_cursor_goto_next_sibling (&XTREE_SITTER_CURSOR (cursor)->cursor);
  return cursor;
}

DEFUN ("tree-sitter-goto-parent",
       Ftree_sitter_goto_parent, Stree_sitter_goto_parent,
       1, 1, 0,
       doc: /* Return CURSOR after moving to the parent of its current node.
This is a no-op if CURSOR was already at the root node.  */)
  (Lisp_Object cursor)
{
  if (NILP (cursor))
    return Qnil;

  CHECK_TREE_SITTER_CURSOR (cursor);

  ts_tree_cursor_goto_parent (&XTREE_SITTER_CURSOR (cursor)->cursor);
  return cursor;
}

DEFUN ("tree-sitter-node-at",
       Ftree_sitter_node_at, Stree_sitter_node_at,
       0, 2, 0,
       doc: /* Return nearest node at or after POS.
If PRECISE is non-nil, return nil if POS falls outside any node's range.  */)
  (Lisp_Object pos, Lisp_Object precise)
{
  TSNode node;
  const TSTree *tree;
  Lisp_Object sitter = Ftree_sitter (Fcurrent_buffer ());

  if (NILP (pos))
    pos = Fpoint ();

  CHECK_FIXNUM (pos);

  if (NILP (sitter))
    return Qnil;

  tree = XTREE_SITTER (sitter)->tree;
  if (tree == NULL)
    return Qnil;

  node = ts_tree_node_at (tree, BUFFER_TO_SITTER (XFIXNUM (pos)));
  return (ts_node_is_null (node)
	  || (! NILP (precise)
	      && (XFIXNUM (pos) < SITTER_TO_BUFFER (ts_node_start_byte (node))
		  || XFIXNUM (pos) >= SITTER_TO_BUFFER (ts_node_end_byte (node)))))
    ? Qnil : make_node (node);
}

DEFUN ("tree-sitter-node-parent",
       Ftree_sitter_node_parent, Stree_sitter_node_parent,
       1, 1, 0,
       doc: /* Return parent of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  TSNode parent = ts_node_parent (XTREE_SITTER_NODE (node)->node);
  return ts_node_is_null (parent)
    ? Qnil
    : make_node (parent);
}

DEFUN ("tree-sitter-node-child",
       Ftree_sitter_node_child, Stree_sitter_node_child,
       2, 2, 0,
       doc: /* Return Nth child of NODE. */)
  (Lisp_Object node, Lisp_Object n)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  CHECK_FIXNAT (n);

  TSNode child = ts_node_child (XTREE_SITTER_NODE (node)->node,
				(uint32_t) XFIXNAT (n));
  return ts_node_is_null (child)
    ? Qnil
    : make_node (child);
}

DEFUN ("tree-sitter-node-named-child",
       Ftree_sitter_node_named_child, Stree_sitter_node_named_child,
       2, 2, 0,
       doc: /* Return Nth named child of NODE. */)
  (Lisp_Object node, Lisp_Object n)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  CHECK_FIXNAT (n);

  TSNode child = ts_node_named_child (XTREE_SITTER_NODE (node)->node,
				      (uint32_t) XFIXNAT (n));
  return ts_node_is_null (child)
    ? Qnil
    : make_node (child);
}

DEFUN ("tree-sitter-node-child-by-field-name",
       Ftree_sitter_node_child_by_field_name,
       Stree_sitter_node_child_by_field_name,
       2, 2, 0,
       doc: /* Return child of NODE by FIELD_NAME. */)
  (Lisp_Object node, Lisp_Object field_name)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  CHECK_STRING (field_name);

  TSNode child = ts_node_child_by_field_name
    (XTREE_SITTER_NODE (node)->node,
     SSDATA (field_name),
     strlen (SSDATA (field_name)));

  return ts_node_is_null (child)
    ? Qnil
    : make_node (child);
}

DEFUN ("tree-sitter-node-child-by-field-id",
       Ftree_sitter_node_child_by_field_id,
       Stree_sitter_node_child_by_field_id,
       2, 2, 0,
       doc: /* Return child of NODE by FIELD_ID. */)
  (Lisp_Object node, Lisp_Object field_id)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  CHECK_FIXNAT (field_id);

  TSNode child = ts_node_child_by_field_id
    (XTREE_SITTER_NODE (node)->node,
     XFIXNAT (field_id));

  return ts_node_is_null (child)
    ? Qnil
    : make_node (child);
}

DEFUN ("tree-sitter-node-next-sibling",
       Ftree_sitter_node_next_sibling,
       Stree_sitter_node_next_sibling,
       1, 1, 0,
       doc: /* Return next sibling of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  TSNode sibling = ts_node_next_sibling (XTREE_SITTER_NODE (node)->node);

  return ts_node_is_null (sibling)
    ? Qnil
    : make_node (sibling);
}

DEFUN ("tree-sitter-node-prev-sibling",
       Ftree_sitter_node_prev_sibling,
       Stree_sitter_node_prev_sibling,
       1, 1, 0,
       doc: /* Return previous sibling of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  TSNode sibling = ts_node_prev_sibling (XTREE_SITTER_NODE (node)->node);

  return ts_node_is_null (sibling)
    ? Qnil
    : make_node (sibling);
}

DEFUN ("tree-sitter-node-next-named-sibling",
       Ftree_sitter_node_next_named_sibling,
       Stree_sitter_node_next_named_sibling,
       1, 1, 0,
       doc: /* Return next named sibling of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  TSNode sibling = ts_node_next_named_sibling (XTREE_SITTER_NODE (node)->node);

  return ts_node_is_null (sibling)
    ? Qnil
    : make_node (sibling);
}

DEFUN ("tree-sitter-node-prev-named-sibling",
       Ftree_sitter_node_prev_named_sibling,
       Stree_sitter_node_prev_named_sibling,
       1, 1, 0,
       doc: /* Return previous named sibling of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  TSNode sibling = ts_node_prev_named_sibling (XTREE_SITTER_NODE (node)->node);

  return ts_node_is_null (sibling)
    ? Qnil
    : make_node (sibling);
}

DEFUN ("tree-sitter-node-first-child-for-byte",
       Ftree_sitter_node_first_child_for_byte,
       Stree_sitter_node_first_child_for_byte,
       2, 2, 0,
       doc: /* Return first child of NODE extending beyond BEG.  */)
  (Lisp_Object node, Lisp_Object beg)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  CHECK_FIXNAT (beg);

  TSNode child = ts_node_first_child_for_byte (XTREE_SITTER_NODE (node)->node,
					       BUFFER_TO_SITTER (XFIXNAT (beg)));

  return ts_node_is_null (child)
    ? Qnil
    : make_node (child);
}

DEFUN ("tree-sitter-node-first-named-child-for-byte",
       Ftree_sitter_node_first_named_child_for_byte,
       Stree_sitter_node_first_named_child_for_byte,
       2, 2, 0,
       doc: /* Return first named child of NODE extending beyond BEG.  */)
  (Lisp_Object node, Lisp_Object beg)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  CHECK_FIXNAT (beg);

  TSNode child = ts_node_first_named_child_for_byte
    (XTREE_SITTER_NODE (node)->node, BUFFER_TO_SITTER (XFIXNAT (beg)));

  return ts_node_is_null (child)
    ? Qnil
    : make_node (child);
}

DEFUN ("tree-sitter-node-descendant-for-byte-range",
       Ftree_sitter_node_descendant_for_byte_range,
       Stree_sitter_node_descendant_for_byte_range,
       3, 3, 0,
       doc: /* Return deepest node under NODE spanning BEG to END. */)
  (Lisp_Object node, Lisp_Object beg, Lisp_Object end)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  CHECK_FIXNAT (beg);
  CHECK_FIXNAT (end);

  TSNode child = ts_node_descendant_for_byte_range
    (XTREE_SITTER_NODE (node)->node,
     BUFFER_TO_SITTER (XFIXNAT (beg)),
     BUFFER_TO_SITTER (XFIXNAT (end)));

  return ts_node_is_null (child)
    ? Qnil
    : make_node (child);
}

DEFUN ("tree-sitter-node-named-descendant-for-byte-range",
       Ftree_sitter_node_named_descendant_for_byte_range,
       Stree_sitter_node_named_descendant_for_byte_range,
       3, 3, 0,
       doc: /* Return deepest named node under NODE spanning BEG to END. */)
  (Lisp_Object node, Lisp_Object beg, Lisp_Object end)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  CHECK_FIXNAT (beg);
  CHECK_FIXNAT (end);

  TSNode child = ts_node_named_descendant_for_byte_range
    (XTREE_SITTER_NODE (node)->node,
     BUFFER_TO_SITTER (XFIXNAT (beg)),
     BUFFER_TO_SITTER (XFIXNAT (end)));

  return ts_node_is_null (child)
    ? Qnil
    : make_node (child);
}

DEFUN ("tree-sitter-node-equal",
       Ftree_sitter_node_equal, Stree_sitter_node_equal,
       2, 2, 0,
       doc: /* Test for node equality. */)
  (Lisp_Object node0, Lisp_Object node1)
{
  if (NILP (node0) || NILP (node1))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node0);
  CHECK_TREE_SITTER_NODE (node1);

  return (ts_node_eq (XTREE_SITTER_NODE (node0)->node,
		      XTREE_SITTER_NODE (node1)->node)
	  ? Qt : Qnil);
}

DEFUN ("tree-sitter-node-type",
       Ftree_sitter_node_type, Stree_sitter_node_type,
       1, 1, 0,
       doc: /* Return type of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  return ts_node_is_null (XTREE_SITTER_NODE (node)->node)
    ? Qnil
    : build_string (ts_node_type (XTREE_SITTER_NODE (node)->node));
}

DEFUN ("tree-sitter-node-field-name-for-child",
       Ftree_sitter_node_field_name_for_child,
       Stree_sitter_node_field_name_for_child,
       2, 2, 0,
       doc: /* Return field name for Nth child of NODE. */)
  (Lisp_Object node, Lisp_Object n)
{
  Lisp_Object name = Qnil;
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);
  CHECK_FIXNAT (n);

  if (! ts_node_is_null (XTREE_SITTER_NODE (node)->node))
    {
      const char *ret = ts_node_field_name_for_child
	(XTREE_SITTER_NODE (node)->node, (uint32_t) XFIXNAT (n));
      if (ret)
	name = build_string (ret);
    }
  return name;
}

DEFUN ("tree-sitter-node-symbol",
       Ftree_sitter_node_symbol, Stree_sitter_node_symbol,
       1, 1, 0,
       doc: /* Return symbol of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  return ts_node_is_null (XTREE_SITTER_NODE (node)->node)
    ? Qnil
    : make_uint (ts_node_symbol (XTREE_SITTER_NODE (node)->node));
}

DEFUN ("tree-sitter-node-child-count",
       Ftree_sitter_node_child_count, Stree_sitter_node_child_count,
       1, 1, 0,
       doc: /* Return number of children of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  return ts_node_is_null (XTREE_SITTER_NODE (node)->node)
    ? Qnil
    : make_uint (ts_node_child_count (XTREE_SITTER_NODE (node)->node));
}

DEFUN ("tree-sitter-node-named-child-count",
       Ftree_sitter_node_named_child_count, Stree_sitter_node_named_child_count,
       1, 1, 0,
       doc: /* Return number of named children of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  return ts_node_is_null (XTREE_SITTER_NODE (node)->node)
    ? Qnil
    : make_uint (ts_node_named_child_count (XTREE_SITTER_NODE (node)->node));
}

DEFUN ("tree-sitter-node-string",
       Ftree_sitter_node_string, Stree_sitter_node_string,
       1, 1, 0,
       doc: /* Return string representation of NODE. */)
  (Lisp_Object node)
{
  Lisp_Object sexp = Qnil;
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  if (! ts_node_is_null (XTREE_SITTER_NODE (node)->node))
    {
      char *ret = ts_node_string (XTREE_SITTER_NODE (node)->node);
      sexp = build_string (ret);
      free (ret);
    }
  return sexp;
}

DEFUN ("tree-sitter-node-start",
       Ftree_sitter_node_start, Stree_sitter_node_start,
       1, 1, 0,
       doc: /* Return beginning charpos of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  return ts_node_is_null (XTREE_SITTER_NODE (node)->node)
    ? Qnil
    : make_fixnum (SITTER_TO_BUFFER
		   (ts_node_start_byte (XTREE_SITTER_NODE (node)->node)));
}

DEFUN ("tree-sitter-node-end",
       Ftree_sitter_node_end, Stree_sitter_node_end,
       1, 1, 0,
       doc: /* Return one-past charpos of NODE. */)
  (Lisp_Object node)
{
  if (NILP (node))
    return Qnil;

  CHECK_TREE_SITTER_NODE (node);

  return ts_node_is_null (XTREE_SITTER_NODE (node)->node)
    ? Qnil
    : make_fixnum (SITTER_TO_BUFFER
		   (ts_node_end_byte (XTREE_SITTER_NODE (node)->node)));
}

DEFUN ("tree-sitter-calculate-indent",
       Ftree_sitter_calculate_indent, Stree_sitter_calculate_indent,
       0, 0, 0,
       doc: /* Return number of spaces for current line.  */)
  (void)
{
  Lisp_Object sitter = Ftree_sitter (Fcurrent_buffer ());
  if (NILP (sitter))
    return Qnil;

  /* target_node is the immediate node of current line (innermost).
     enclosing_node is the child of root_node that contains target_node (outermost) */
  const size_t indent_nspaces = 2;
  size_t result = 0;
  Lisp_Object target_node = Ftree_sitter_node_at (Fline_beginning_position (Qnil), Qnil);
  Lisp_Object enclosing_node = target_node;
  for (Lisp_Object root_node = Ftree_sitter_root_node (Fcurrent_buffer ());
       ! NILP (enclosing_node)
	 && NILP (Ftree_sitter_node_equal
		  (root_node, Ftree_sitter_node_parent (enclosing_node)));
       enclosing_node = Ftree_sitter_node_parent (enclosing_node));
  TSLanguageFunctor fn = tree_sitter_language_functor (XTREE_SITTER (sitter)->progmode);
  Lisp_Object language = Fcdr_safe (Fassq (XTREE_SITTER (sitter)->progmode,
					   Fsymbol_value (Qtree_sitter_mode_alist)));
  if (! NILP (enclosing_node) && fn != NULL && ! NILP (language))
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
      h = XHASH_TABLE (cache);
      i = hash_lookup (h, XTREE_SITTER (sitter)->progmode, &hash);
      if (i < 0)
	{
	  Lisp_Object filename = Fexpand_file_name
	    (build_string ("indents.scm"),
	     concat3 (Ffile_name_as_directory
		      (Fsymbol_value (Qtree_sitter_resources_dir)),
		      build_string ("queries/"),
		      language));
	  FILE *fp = fopen (SSDATA (filename), "rb");
	  if (! fp)
	    {
	      Lisp_Object args[2];
	      args[0] = build_string ("%s not found");
	      args[1] = filename;
	      xsignal1 (Qtree_sitter_language_error, Fformat (4, args));
	    }
	  else
	    {
	      char *query_buf;
	      long query_length;
	      fseek (fp, 0L, SEEK_END);
	      query_length = ftell (fp);
	      rewind (fp);
	      query_buf = xzalloc(query_length + 1);
	      if (1 == fread (query_buf, query_length, 1, fp))
		{
		  uint32_t error_offset;
		  TSQueryError error_type;
		  TSQuery *query = ts_query_new (fn (), query_buf, strlen (query_buf),
						 &error_offset, &error_type);
		  if (query == NULL)
		    {
		      Lisp_Object args[4];
		      args[0] = build_string
			("ts_query_new() of %s returned %d at offset %d");
		      args[1] = filename;
		      args[2] = make_fixnum ((EMACS_INT) error_type);
		      args[3] = make_fixnum ((EMACS_INT) error_offset);
		      xfree (query_buf);
		      xsignal1 (Qtree_sitter_language_error, Fformat (4, args));
		    }
		  else
		    {
		      if (XTREE_SITTER (sitter)->indents_query != NULL)
			ts_query_delete (XTREE_SITTER (sitter)->indents_query);
		      XTREE_SITTER (sitter)->indents_query = query;
		      i = hash_put (h, XTREE_SITTER (sitter)->progmode,
				    build_string (query_buf), hash);
		    }
		}
	      xfree (query_buf);
	    }
	}
      if (i >= 0)
	{
	  Lisp_Object source_query = HASH_VALUE (h, i);
	  const uint32_t sitter_beg =
	    ts_node_start_byte (XTREE_SITTER_NODE (enclosing_node)->node);
	  const uint32_t sitter_end =
	    ts_node_end_byte (XTREE_SITTER_NODE (enclosing_node)->node);
	  const ptrdiff_t beg = SITTER_TO_BUFFER (sitter_beg);
	  const ptrdiff_t end = SITTER_TO_BUFFER (sitter_end);

	  Lisp_Object source_code =
	    Fbuffer_substring_no_properties (make_fixnum (beg), make_fixnum (end));
	  // ts_captures_new() recreates TSQuery from source_query
	  // string, which is dumb since we already have indents_query.
	  TSQueryCaptureSlice slice =
	    ts_captures_new (fn (),
			     SSDATA (source_query),
			     (uint32_t) SBYTES (source_query),
			     SSDATA (source_code),
			     (uint32_t) SBYTES (source_code),
			     XTREE_SITTER_NODE (enclosing_node)->node,
			     sitter_beg,
			     sitter_end);

	  int slice_index = slice.len - 1;
	  const ptrdiff_t line_target_pt = count_lines (CHAR_TO_BYTE (beg), PT_BYTE);
	  ptrdiff_t line_node_beg;
	  for (Lisp_Object node = target_node,
		 root_node = Ftree_sitter_root_node (Fcurrent_buffer ());
	       slice_index >= 0
		 && ! NILP (node)
		 && NILP (Ftree_sitter_node_equal (root_node, node));
	       (void) node)
	    {
	      /* Find enclosing QueryCapture of NODE.  */
	      ptrdiff_t node_beg =
		SITTER_TO_BUFFER (ts_node_start_byte
				  (XTREE_SITTER_NODE (node)->node));
	      line_node_beg = count_lines (CHAR_TO_BYTE (beg), CHAR_TO_BYTE (node_beg));
	      for (;;)
		{
		  ptrdiff_t slice_beg =
		    SITTER_TO_BUFFER (ts_node_start_byte
				      (slice.captures[slice_index].node));
		  ptrdiff_t line_slice_beg =
		    count_lines (CHAR_TO_BYTE (beg), CHAR_TO_BYTE (slice_beg));
		  if (line_slice_beg <= line_node_beg)
		    break;
		  if (--slice_index < 0)
		    goto done;
		}

	      /* Process all QueryCaptures for the same line.  */
	      bool ignored_line = false, dented_line = false;
	      Lisp_Object capture_name_to_tsnode =
		make_hash_table (hashtest_eq, DEFAULT_HASH_SIZE,
				 DEFAULT_REHASH_SIZE, DEFAULT_REHASH_THRESHOLD,
				 Qnil, false);
	      Lisp_Object obarray = initialize_vector (10, make_fixnum (0));
	      for ((void) slice_index; slice_index >= 0; --slice_index)
		{
		  ptrdiff_t capture_beg =
		    SITTER_TO_BUFFER (ts_node_start_byte (slice.captures[slice_index].node));
		  ptrdiff_t capture_end =
		    SITTER_TO_BUFFER (ts_node_end_byte (slice.captures[slice_index].node));
		  ptrdiff_t line_capture_beg =
		    count_lines (CHAR_TO_BYTE (beg), CHAR_TO_BYTE (capture_beg));
		  ptrdiff_t line_capture_end =
		    count_lines (CHAR_TO_BYTE (beg), CHAR_TO_BYTE (capture_end));

		  if (slice.captures[slice_index].index == UINT32_MAX)
		    {
		      /* line's captures should not have been */
		      continue;
		    }

		  if (line_capture_beg != line_node_beg)
		    goto next_parent; /* !!! */

		  uint32_t capture_name_length = 0;
		  const char *capture_name =
		    ts_query_capture_name_for_id (XTREE_SITTER (sitter)->indents_query,
						  slice.captures[slice_index].index,
						  &capture_name_length);
		  Fputhash (Fintern (build_string (capture_name), obarray),
			    make_node (slice.captures[slice_index].node),
			    capture_name_to_tsnode);
		  uint32_t predicates_length = 0;
		  const TSQueryPredicateStep *predicates =
		    ts_query_predicates_for_pattern (XTREE_SITTER (sitter)->indents_query,
						     slice.pattern_indices[slice_index],
						     &predicates_length);
		  char delimiter_open = '\0';
		  if (predicates_length
		      && predicates[0].type == TSQueryPredicateStepTypeString)
		    {
		      uint32_t length;
		      const char *what =
			ts_query_string_value_for_id
			(XTREE_SITTER (sitter)->indents_query,
			 predicates[0].value_id, &length);
		      if (0 == strcmp (what, "set!")
			  && 0 == strcmp (ts_query_string_value_for_id
					  (XTREE_SITTER (sitter)->indents_query,
					   predicates[1].value_id, &length),
					  "delimiter"))
			{
			  delimiter_open = ts_query_string_value_for_id
			    (XTREE_SITTER (sitter)->indents_query,
			     predicates[2].value_id, &length)[0];
			}
		      else if (strstr (what, "has-type?")
			       && predicates[1].type == TSQueryPredicateStepTypeCapture)
			{
			  bool negate = (0 == strncmp (what, "not", 3));
			  const char *capture_name = ts_query_capture_name_for_id
			    (XTREE_SITTER (sitter)->indents_query,
			     predicates[1].value_id, &length);
			  Lisp_Object captured =
			    Fgethash (Fintern_soft (build_string (capture_name),
						    obarray),
				      capture_name_to_tsnode,
				      Qnil);
			  if (! NILP (captured))
			    {
			      CHECK_TREE_SITTER_NODE (captured);
			      const char *node_type =
				ts_node_type (XTREE_SITTER_NODE (captured)->node);
			      bool match =
				(0 == strcmp (node_type, ts_query_string_value_for_id
					      (XTREE_SITTER (sitter)->indents_query,
					       predicates[2].value_id, &length)));
			      /* Exclude capture when:
				 has-type? (!negate) and no match, or,
				 not-has-type? (negate) and match.  */
			      if (negate == match)
				for (int i = slice_index - 1; i >= 0; --i)
				  {
				    /* annul up-tree captures of this pattern.  */
				    if (slice.pattern_indices[i] ==
					slice.pattern_indices[slice_index])
				      slice.captures[i].index = UINT32_MAX;
				  }
			    }
			}
		    }

		  if (node_beg < capture_beg)
		    continue; /* next slice_index; no indents apply. */

		  if (0 == strcmp (capture_name, "zero_indent"))
		    {
		      result = 0;
		      goto next_parent; /* !!! */
		    }
		  else if (0 == strcmp (capture_name, "ignore"))
		    {
		      ignored_line = true;
		    }
		  else if (0 == strcmp (capture_name, "branch")
			   && line_node_beg == line_target_pt)
		    {
		      if (! dented_line)
			result -= indent_nspaces;
		      dented_line = true;
		    }
		  else if (0 == strcmp (capture_name, "dedent")
			   && line_node_beg != line_target_pt)
		    {
		      if (! dented_line)
			result -= indent_nspaces;
		      dented_line = true;
		    }
		  else if (line_capture_beg != line_capture_end
			   && line_node_beg != line_target_pt)
		    {
		      if (0 == strcmp (capture_name, "indent"))
			{
			  if (! dented_line)
			    result += indent_nspaces;
			  dented_line = true;
			}
		      else if (0 == strcmp (capture_name, "aligned_indent"))
			{
			  bool hanging_indent_p = false;
			  TSNode delimiter_node;
			  for (uint32_t count = ts_node_child_count
				 (slice.captures[slice_index].node), i = 0;
			       i < count;
			       ++i)
			    {
			      delimiter_node =
				ts_node_child (slice.captures[slice_index].node, i);
			      if (strlen (ts_node_type (delimiter_node)) == 1
				  && ts_node_type (delimiter_node)[0] == delimiter_open)
				{
				  hanging_indent_p = true;
				  ptrdiff_t delimiter_end =
				    SITTER_TO_BUFFER (ts_node_end_byte (delimiter_node));
				  for (ptrdiff_t bytepos = delimiter_end;
				       bytepos != ZV && FETCH_CHAR (bytepos) != '\n';
				       ++bytepos)
				    {
				      if (! isspace (FETCH_CHAR (bytepos)))
					{
					  hanging_indent_p = false;
					  break;
					}
				    }
				}
			    }
			  if (hanging_indent_p)
			    {
			      /* previous line ended with left paren. */
			      result += indent_nspaces;
			    }
			  else
			    {
			      /* align to left paren on previous line.  */
			      ptrdiff_t delimiter_beg =
				SITTER_TO_BUFFER (ts_node_start_byte (delimiter_node));
			      result += delimiter_beg;
			      goto done; /* since delimiter_beg is absolute.  */
			    }
			}
		    }
		}

	    next_parent:
	      if (ignored_line && ! dented_line)
		{
		  // unclear
		  result = 0;
		  goto done;
		}
	      for (;;)
		{
		  node = Ftree_sitter_node_parent (node);
		  if (NILP (node))
		    break;
		  ptrdiff_t node_beg =
		    SITTER_TO_BUFFER (ts_node_start_byte
				      (XTREE_SITTER_NODE (node)->node));
		  if (count_lines (CHAR_TO_BYTE (beg), CHAR_TO_BYTE (node_beg))
		      < line_node_beg)
		    break;
		}
	    }
	done:
	  ts_captures_free (slice);
	}
    }
  return make_fixnum (result);
}

DEFUN ("tree-sitter-highlights",
       Ftree_sitter_highlights, Stree_sitter_highlights,
       2, 2, "r",
       doc: /* Return list of highlights from BEG to END. */)
  (Lisp_Object beg, Lisp_Object end)
{
  return Fnreverse (do_highlights (beg, end, &list_highlights));
}

DEFUN ("tree-sitter-highlight-region",
       Ftree_sitter_highlight_region, Stree_sitter_highlight_region,
       2, 2, "r",
       doc: /* Highlight BEG to END. */)
  (Lisp_Object beg, Lisp_Object end)
{
  Lisp_Object bounds = do_highlights (beg, end, &highlight_region);
  return NILP (bounds)
    ? bounds : Fcons (apply1 (intern ("min"), bounds),
		      apply1 (intern ("max"), bounds));
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
	  if (count)
	    {
	      uint32_t sitter_end_byte = min (ZV_BYTE-1, range->end_byte);
	      retval = list2 (make_fixnum (SITTER_TO_BUFFER (range->start_byte)),
			      make_fixnum (SITTER_TO_BUFFER (sitter_end_byte)));
	      free (range);
	    }
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
  specpdl_ref pdl_count = SPECPDL_INDEX ();

  if (thread_unsafe_last_scan_characters < tree_sitter_scan_characters)
    {
      thread_unsafe_last_scan_characters = tree_sitter_scan_characters;
      /* hyper-conservative estimate of 4 bytes per character. */
      if (thread_unsafe_return_value == NULL)
	thread_unsafe_return_value = xmalloc (tree_sitter_scan_characters * 4 + 1);
      else
	thread_unsafe_return_value = xrealloc (thread_unsafe_return_value,
					       tree_sitter_scan_characters * 4 + 1);
    }

  if (! BUFFER_LIVE_P (bp))
    error ("Selecting deleted buffer");

  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  Fwiden ();
  /* Fbuffer_substring_no_properties is smart about multibyte and gap. */
  sprintf (thread_unsafe_return_value, "%s",
           SSDATA (Fbuffer_substring_no_properties
                   (make_fixnum (start),
                    make_fixnum (min (start + tree_sitter_scan_characters,
                                      BUF_Z (bp))))));
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
  else
    xsignal1 (Qtree_sitter_error, BVAR (XBUFFER (Fcurrent_buffer ()), name));
  return sitter;
}

/* TODO buffers not utf-8-clean. */
void
tree_sitter_record_change (ptrdiff_t start_char,
			   ptrdiff_t old_end_char,
			   uint32_t old_end_byte,
			   ptrdiff_t new_end_char)
{
  Lisp_Object sitter = Fbuffer_local_value (Qtree_sitter_sitter, Fcurrent_buffer ());
  if (! NILP (sitter))
    {
      TSTree *tree = XTREE_SITTER (sitter)->tree;
      if (tree != NULL)
	{
	  TSInputEdit edit = {
	    BUFFER_TO_SITTER (start_char),
	    old_end_byte,
	    BUFFER_TO_SITTER (new_end_char),
	    (TSPoint) { 0, 0 },
	    (TSPoint) { 0, 0 },
	    (TSPoint) { 0, max (1, new_end_char - start_char) } /* black magic */
	  };

	  if (XTREE_SITTER (sitter)->prev_tree != NULL)
	    {
	      ts_tree_delete (XTREE_SITTER (sitter)->prev_tree);
	      XTREE_SITTER (sitter)->prev_tree = NULL;
	    }

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

void
syms_of_tree_sitter (void)
{
  DEFSYM (Qtree_sitter_error, "tree-sitter-error");
  DEFSYM (Qtree_sitter_parse_error, "tree-sitter-parse-error");
  DEFSYM (Qtree_sitter_language_error, "tree-sitter-language-error");
  DEFSYM (Qtree_sitterp, "tree-sitterp");
  DEFSYM (Qtree_sitter_nodep, "tree-sitter-nodep");
  DEFSYM (Qtree_sitter_cursorp, "tree-sitter-cursorp");
  DEFSYM (Qjit_lock_chunk_size, "jit-lock-chunk-size");

  define_error (Qtree_sitter_error, "Generic tree-sitter exception", Qerror);
  define_error (Qtree_sitter_parse_error, "Parse error",
		Qtree_sitter_error);
  define_error (Qtree_sitter_language_error, "Cannot load language",
		Qtree_sitter_error);

  DEFVAR_INT ("tree-sitter-scan-characters", tree_sitter_scan_characters,
	      doc: /* Number of characters to read per tree sitter scan.  */);
  tree_sitter_scan_characters = 2048;

  DEFSYM (Qtree_sitter_mode_alist, "tree-sitter-mode-alist");
  DEFSYM (Qtree_sitter_resources_dir, "tree-sitter-resources-dir");
  DEFSYM (Qtree_sitter_highlight_alist, "tree-sitter-highlight-alist");
  DEFSYM (Qtree_sitter_sitter, "tree-sitter-sitter");
  Fmake_variable_buffer_local (Qtree_sitter_sitter);

  defsubr (&Stree_sitter);
  defsubr (&Stree_sitter_root_node);
  defsubr (&Stree_sitter_node_sexp);
  defsubr (&Stree_sitter_node_at);
  defsubr (&Stree_sitter_node_type);
  defsubr (&Stree_sitter_node_symbol);
  defsubr (&Stree_sitter_node_string);
  defsubr (&Stree_sitter_node_parent);
  defsubr (&Stree_sitter_node_child);
  defsubr (&Stree_sitter_node_field_name_for_child);
  defsubr (&Stree_sitter_cursor_at);
  defsubr (&Stree_sitter_node_child_count);
  defsubr (&Stree_sitter_node_named_child_count);
  defsubr (&Stree_sitter_node_child_by_field_name);
  defsubr (&Stree_sitter_node_child_by_field_id);
  defsubr (&Stree_sitter_node_named_child);
  defsubr (&Stree_sitter_node_next_sibling);
  defsubr (&Stree_sitter_node_prev_sibling);
  defsubr (&Stree_sitter_node_next_named_sibling);
  defsubr (&Stree_sitter_node_prev_named_sibling);
  defsubr (&Stree_sitter_node_first_child_for_byte);
  defsubr (&Stree_sitter_node_first_named_child_for_byte);
  defsubr (&Stree_sitter_goto_first_child);
  defsubr (&Stree_sitter_goto_next_sibling);
  defsubr (&Stree_sitter_goto_parent);
  defsubr (&Stree_sitter_node_descendant_for_byte_range);
  defsubr (&Stree_sitter_node_named_descendant_for_byte_range);
  defsubr (&Stree_sitter_node_of);
  defsubr (&Stree_sitter_node_equal);
  defsubr (&Stree_sitter_node_start);
  defsubr (&Stree_sitter_node_end);
  defsubr (&Stree_sitter_ppss);
  defsubr (&Stree_sitter_calculate_indent);
  defsubr (&Stree_sitter_highlights);
  defsubr (&Stree_sitter_highlight_region);
  defsubr (&Stree_sitter_changed_range);
  defsubr (&Stree_sitter__testable);
}
