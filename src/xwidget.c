/* Support for embedding graphical components in a buffer.

Copyright (C) 2011-2021 Free Software Foundation, Inc.

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

#include "buffer.h"
#include "xwidget.h"

#include "lisp.h"
#include "blockinput.h"
#include "dispextern.h"
#include "frame.h"
#include "keyboard.h"
#include "gtkutil.h"
#include "sysstdio.h"
#include "termhooks.h"
#include "window.h"

/* Include xwidget bottom end headers.  */
#ifdef USE_GTK
#include <webkit2/webkit2.h>
#include <JavaScriptCore/JavaScript.h>
#include <cairo.h>
#include <X11/Xlib.h>
#elif defined NS_IMPL_COCOA
#include "nsxwidget.h"
#endif

static Lisp_Object id_to_xwidget_map;
static uint32_t xwidget_counter = 0;

#ifdef USE_GTK
static Lisp_Object x_window_to_xwv_map;
static gboolean offscreen_damage_event (GtkWidget *, GdkEvent *, gpointer);
static void synthesize_focus_in_event (GtkWidget *);
static GdkDevice *find_suitable_keyboard (struct frame *);
#endif

static struct xwidget *
allocate_xwidget (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget, script_callbacks, PVEC_XWIDGET);
}

static struct xwidget_view *
allocate_xwidget_view (void)
{
  return ALLOCATE_PSEUDOVECTOR (struct xwidget_view, w, PVEC_XWIDGET_VIEW);
}

#define XSETXWIDGET(a, b) XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET)
#define XSETXWIDGET_VIEW(a, b) XSETPSEUDOVECTOR (a, b, PVEC_XWIDGET_VIEW)

static struct xwidget_view *xwidget_view_lookup (struct xwidget *,
						 struct window *);
#ifdef USE_GTK
static void webkit_view_load_changed_cb (WebKitWebView *,
                                         WebKitLoadEvent,
                                         gpointer);
static void webkit_javascript_finished_cb (GObject *,
                                           GAsyncResult *,
                                           gpointer);
static gboolean webkit_download_cb (WebKitWebContext *, WebKitDownload *, gpointer);
static GtkWidget *webkit_create_cb (WebKitWebView *, WebKitNavigationAction *, gpointer);
static gboolean
webkit_decide_policy_cb (WebKitWebView *,
                         WebKitPolicyDecision *,
                         WebKitPolicyDecisionType,
                         gpointer);
static GtkWidget *find_widget_at_pos (GtkWidget *, int, int, int *, int *);

struct widget_search_data
{
  int x;
  int y;
  bool foundp;
  bool first;
  GtkWidget *data;
};

static void find_widget (GtkWidget *t, struct widget_search_data *);
static void mouse_target_changed (WebKitWebView *, WebKitHitTestResult *, guint,
				  gpointer);
#endif


DEFUN ("make-xwidget",
       Fmake_xwidget, Smake_xwidget,
       5, 7, 0,
       doc: /* Make an xwidget of TYPE.
If BUFFER is nil, use the current buffer.
If BUFFER is a string and no such buffer exists, create it.
TYPE is a symbol which can take one of the following values:

- webkit

RELATED is nil, or an xwidget.  When constructing a WebKit widget, it
will share the same settings and internal subprocess as RELATED.
Returns the newly constructed xwidget, or nil if construction
fails.  */)
  (Lisp_Object type,
   Lisp_Object title, Lisp_Object width, Lisp_Object height,
   Lisp_Object arguments, Lisp_Object buffer, Lisp_Object related)
{
#ifdef USE_GTK
  if (!xg_gtk_initialized)
    error ("make-xwidget: GTK has not been initialized");
#endif
  CHECK_SYMBOL (type);
  CHECK_FIXNAT (width);
  CHECK_FIXNAT (height);

  struct xwidget *xw = allocate_xwidget ();
  Lisp_Object val;
  xw->type = type;
  xw->title = title;
  xw->buffer = (NILP (buffer) ? Fcurrent_buffer ()
		: Fget_buffer_create (buffer, Qnil));
  xw->height = XFIXNAT (height);
  xw->width = XFIXNAT (width);
  xw->kill_without_query = false;
  XSETXWIDGET (val, xw);
  Vxwidget_list = Fcons (val, Vxwidget_list);
  xw->plist = Qnil;
  xw->xwidget_id = ++xwidget_counter;
  xw->find_text = NULL;

  Fputhash (make_fixnum (xw->xwidget_id), val, id_to_xwidget_map);

#ifdef USE_GTK
  xw->widgetwindow_osr = NULL;
  xw->widget_osr = NULL;
  xw->hit_result = 0;
  if (EQ (xw->type, Qwebkit))
    {
      block_input ();
      WebKitSettings *settings;
      WebKitWebContext *webkit_context = webkit_web_context_get_default ();

# if WEBKIT_CHECK_VERSION (2, 26, 0)
      if (!webkit_web_context_get_sandbox_enabled (webkit_context))
	webkit_web_context_set_sandbox_enabled (webkit_context, TRUE);
# endif

      xw->widgetwindow_osr = gtk_offscreen_window_new ();
      gtk_window_resize (GTK_WINDOW (xw->widgetwindow_osr), xw->width,
                         xw->height);

      if (EQ (xw->type, Qwebkit))
        {
	  WebKitWebView *related_view;

	  if (NILP (related)
	      || !XWIDGETP (related)
	      || !EQ (XXWIDGET (related)->type, Qwebkit))
	    {
	      xw->widget_osr = webkit_web_view_new ();

	      /* webkitgtk uses GSubprocess which sets sigaction causing
		 Emacs to not catch SIGCHLD with its usual handle setup in
		 'catch_child_signal'.  This resets the SIGCHLD sigaction.  */
	      struct sigaction old_action;
	      sigaction (SIGCHLD, NULL, &old_action);
	      webkit_web_view_load_uri (WEBKIT_WEB_VIEW (xw->widget_osr),
					"about:blank");
	      sigaction (SIGCHLD, &old_action, NULL);
	    }
	  else
	    {
	      related_view = WEBKIT_WEB_VIEW (XXWIDGET (related)->widget_osr);
	      xw->widget_osr = webkit_web_view_new_with_related_view (related_view);
	    }

	  /* Enable the developer extras.  */
	  settings = webkit_web_view_get_settings (WEBKIT_WEB_VIEW (xw->widget_osr));
	  g_object_set (G_OBJECT (settings), "enable-developer-extras", TRUE, NULL);
	}

      gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width,
                                   xw->height);

      if (EQ (xw->type, Qwebkit))
        {
          gtk_container_add (GTK_CONTAINER (xw->widgetwindow_osr),
                             GTK_WIDGET (WEBKIT_WEB_VIEW (xw->widget_osr)));
        }
      else
        {
          gtk_container_add (GTK_CONTAINER (xw->widgetwindow_osr),
                             xw->widget_osr);
        }

      gtk_widget_show (xw->widget_osr);
      gtk_widget_show (xw->widgetwindow_osr);
      synthesize_focus_in_event (xw->widgetwindow_osr);

      /* Store some xwidget data in the gtk widgets for convenient
         retrieval in the event handlers.  */
      g_object_set_data (G_OBJECT (xw->widget_osr), XG_XWIDGET, xw);
      g_object_set_data (G_OBJECT (xw->widgetwindow_osr), XG_XWIDGET, xw);

      /* signals */
      if (EQ (xw->type, Qwebkit))
        {
          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "load-changed",
                            G_CALLBACK (webkit_view_load_changed_cb), xw);

          g_signal_connect (G_OBJECT (webkit_context),
                            "download-started",
                            G_CALLBACK (webkit_download_cb), xw);

          g_signal_connect (G_OBJECT (xw->widget_osr),
                            "decide-policy",
                            G_CALLBACK
                            (webkit_decide_policy_cb),
                            xw);

	  g_signal_connect (G_OBJECT (xw->widget_osr),
			    "mouse-target-changed",
			    G_CALLBACK (mouse_target_changed),
			    xw);
	  g_signal_connect (G_OBJECT (xw->widget_osr),
			    "create",
			    G_CALLBACK (webkit_create_cb),
			    xw);
        }

      g_signal_connect (G_OBJECT (xw->widgetwindow_osr), "damage-event",
			G_CALLBACK (offscreen_damage_event), xw);

      unblock_input ();
    }
#elif defined NS_IMPL_COCOA
  nsxwidget_init (xw);
#endif

  return val;
}

#ifdef USE_GTK
static void
set_widget_if_text_view (GtkWidget *widget, void *data)
{
  GtkWidget **pointer = data;

  if (GTK_IS_TEXT_VIEW (widget))
    *pointer = widget;
}
#endif

DEFUN ("xwidget-perform-lispy-event",
       Fxwidget_perform_lispy_event, Sxwidget_perform_lispy_event,
       2, 3, 0, doc: /* Send a lispy event to XWIDGET.
EVENT should be the event that will be sent.  FRAME should be the
frame which generated the event, and defaults to the selected frame.
On X11, modifier keys will not be processed if FRAME is nil and the
selected frame is not an X-Windows frame.  */)
  (Lisp_Object xwidget, Lisp_Object event, Lisp_Object frame)
{
  struct xwidget *xw;
  struct frame *f = NULL;
  int character = -1, keycode = -1;
  int modifiers = 0;

#ifdef USE_GTK
  GdkEvent *xg_event;
  GtkContainerClass *klass;
  GtkWidget *widget;
  GtkWidget *temp = NULL;
#endif

  CHECK_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);

  if (!NILP (frame))
    f = decode_window_system_frame (frame);
  else if (FRAME_X_P (SELECTED_FRAME ()))
    f = SELECTED_FRAME ();

#ifdef USE_GTK
  widget = gtk_window_get_focus (GTK_WINDOW (xw->widgetwindow_osr));

  if (!widget)
    widget = xw->widget_osr;

  if (RANGED_FIXNUMP (0, event, INT_MAX))
    {
      character = XFIXNUM (event);

      if (character < 32)
	modifiers |= ctrl_modifier;

      modifiers |= character & meta_modifier;
      modifiers |= character & hyper_modifier;
      modifiers |= character & super_modifier;
      modifiers |= character & shift_modifier;
      modifiers |= character & ctrl_modifier;

      character = character & ~(1 << 21);

      if (character < 32)
	character += '_';

      if (f)
	modifiers = x_emacs_to_x_modifiers (FRAME_DISPLAY_INFO (f), modifiers);
      else
	modifiers = 0;
    }
  else if (SYMBOLP (event))
    {
      Lisp_Object decoded = parse_modifiers (event);
      Lisp_Object decoded_name = SYMBOL_NAME (XCAR (decoded));

      int off = 0;
      bool found = false;

      while (off < 256)
	{
	  if (lispy_function_keys[off]
	      && !strcmp (lispy_function_keys[off],
			  SSDATA (decoded_name)))
	    {
	      found = true;
	      break;
	    }
	  ++off;
	}

      if (f)
	modifiers = x_emacs_to_x_modifiers (FRAME_DISPLAY_INFO (f),
					    XFIXNUM (XCAR (XCDR (decoded))));
      else
	modifiers = 0;

      if (found)
	keycode = off + 0xff00;
    }

  if (character == -1 && keycode == -1)
    return Qnil;

  block_input ();
  xg_event = gdk_event_new (GDK_KEY_PRESS);
  xg_event->any.window = gtk_widget_get_window (xw->widget_osr);
  g_object_ref (xg_event->any.window);

  if (character > -1)
    keycode = gdk_unicode_to_keyval (character);

  xg_event->key.keyval = keycode;
  xg_event->key.state = modifiers;

  if (keycode > -1)
    {
      /* WebKitGTK internals abuse follows.  */
      if (WEBKIT_IS_WEB_VIEW (widget))
	{
	  /* WebKitGTK relies on an internal GtkTextView object to
	     "translate" keys such as backspace.  We must find that
	     widget and activate its binding to this key if any.  */
	  klass = GTK_CONTAINER_CLASS (G_OBJECT_GET_CLASS (widget));

	  klass->forall (GTK_CONTAINER (xw->widget_osr), TRUE,
			 set_widget_if_text_view, &temp);

	  if (GTK_IS_WIDGET (temp))
	    {
	      if (!gtk_widget_get_realized (temp))
		gtk_widget_realize (temp);

	      gtk_bindings_activate (G_OBJECT (temp), keycode, modifiers);
	    }
	}
    }

  if (f)
    gdk_event_set_device (xg_event,
			  find_suitable_keyboard (SELECTED_FRAME ()));

  gtk_main_do_event (xg_event);
  xg_event->type = GDK_KEY_RELEASE;
  gtk_main_do_event (xg_event);
  gdk_event_free (xg_event);
  unblock_input ();
#endif

  return Qnil;
}

DEFUN ("get-buffer-xwidgets", Fget_buffer_xwidgets, Sget_buffer_xwidgets,
       1, 1, 0,
       doc: /* Return a list of xwidgets associated with BUFFER.
BUFFER may be a buffer or the name of one.  */)
  (Lisp_Object buffer)
{
  Lisp_Object xw, tail, xw_list;

  if (NILP (buffer))
    return Qnil;
  buffer = Fget_buffer (buffer);
  if (NILP (buffer))
    return Qnil;

  xw_list = Qnil;

  for (tail = Vxwidget_list; CONSP (tail); tail = XCDR (tail))
    {
      xw = XCAR (tail);
      if (XWIDGETP (xw) && EQ (Fxwidget_buffer (xw), buffer))
        xw_list = Fcons (xw, xw_list);
    }
  return xw_list;
}

static bool
xwidget_hidden (struct xwidget_view *xv)
{
  return xv->hidden;
}

struct xwidget *
xwidget_from_id (uint32_t id)
{
  Lisp_Object key = make_fixnum (id);
  Lisp_Object xwidget = Fgethash (key, id_to_xwidget_map, Qnil);

  if (NILP (xwidget))
    emacs_abort ();

  return XXWIDGET (xwidget);
}

#ifdef USE_GTK

static GdkDevice *
find_suitable_pointer (struct frame *f)
{
  GdkSeat *seat = gdk_display_get_default_seat
    (gtk_widget_get_display (FRAME_GTK_WIDGET (f)));

  if (!seat)
    return NULL;

  return gdk_seat_get_pointer (seat);
}

static GdkDevice *
find_suitable_keyboard (struct frame *f)
{
  GdkSeat *seat = gdk_display_get_default_seat
    (gtk_widget_get_display (FRAME_GTK_WIDGET (f)));

  if (!seat)
    return NULL;

  return gdk_seat_get_keyboard (seat);
}

static void
find_widget_cb (GtkWidget *widget, void *user)
{
  find_widget (widget, user);
}

static void
find_widget (GtkWidget *widget,
	     struct widget_search_data *data)
{
  GtkAllocation new_allocation;
  GdkWindow *window;
  int x_offset = 0;
  int y_offset = 0;

  gtk_widget_get_allocation (widget, &new_allocation);

  if (gtk_widget_get_has_window (widget))
    {
      new_allocation.x = 0;
      new_allocation.y = 0;
    }

  if (gtk_widget_get_parent (widget) && !data->first)
    {
      window = gtk_widget_get_window (widget);
      while (window != gtk_widget_get_window (gtk_widget_get_parent (widget)))
	{
	  gint tx, ty, twidth, theight;

	  if (!window)
	    return;

	  twidth = gdk_window_get_width (window);
	  theight = gdk_window_get_height (window);

	  if (new_allocation.x < 0)
	    {
	      new_allocation.width += new_allocation.x;
	      new_allocation.x = 0;
	    }

	  if (new_allocation.y < 0)
	    {
	      new_allocation.height += new_allocation.y;
	      new_allocation.y = 0;
	    }

	  if (new_allocation.x + new_allocation.width > twidth)
	    new_allocation.width = twidth - new_allocation.x;
	  if (new_allocation.y + new_allocation.height > theight)
	    new_allocation.height = theight - new_allocation.y;

	  gdk_window_get_position (window, &tx, &ty);
	  new_allocation.x += tx;
	  x_offset += tx;
	  new_allocation.y += ty;
	  y_offset += ty;

	  window = gdk_window_get_parent (window);
	}
    }

  if ((data->x >= new_allocation.x) && (data->y >= new_allocation.y) &&
      (data->x < new_allocation.x + new_allocation.width) &&
      (data->y < new_allocation.y + new_allocation.height))
    {
      /* First, check if the drag is in a valid drop site in one of
	 our children.	*/
      if (GTK_IS_CONTAINER (widget))
	{
	  struct widget_search_data new_data = *data;

	  new_data.x -= x_offset;
	  new_data.y -= y_offset;
	  new_data.foundp = false;
	  new_data.first = false;

	  gtk_container_forall (GTK_CONTAINER (widget),
				find_widget_cb, &new_data);

	  data->foundp = new_data.foundp;
	  if (data->foundp)
	    data->data = new_data.data;
	}

      /* If not, and this widget is registered as a drop site, check
	 to emit "drag_motion" to check if we are actually in a drop
	 site.	*/
      if (!data->foundp)
	{
	  data->foundp = true;
	  data->data = widget;
	}
    }
}

static GtkWidget *
find_widget_at_pos (GtkWidget *w, int x, int y,
		    int *new_x, int *new_y)
{
  struct widget_search_data data;

  data.x = x;
  data.y = y;
  data.foundp = false;
  data.first = true;

  find_widget (w, &data);

  if (data.foundp)
    {
      gtk_widget_translate_coordinates (w, data.data, x,
					y, new_x, new_y);
      return data.data;
    }

  *new_x = x;
  *new_y = y;

  return NULL;
}

static Emacs_Cursor
cursor_for_hit (guint result, struct frame *frame)
{
  Emacs_Cursor cursor = FRAME_OUTPUT_DATA (frame)->nontext_cursor;

  if ((result & WEBKIT_HIT_TEST_RESULT_CONTEXT_EDITABLE)
      || (result & WEBKIT_HIT_TEST_RESULT_CONTEXT_SELECTION)
      || (result & WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT))
    cursor = FRAME_X_OUTPUT (frame)->text_cursor;

  if (result & WEBKIT_HIT_TEST_RESULT_CONTEXT_SCROLLBAR)
    cursor = FRAME_X_OUTPUT (frame)->vertical_drag_cursor;

  if (result & WEBKIT_HIT_TEST_RESULT_CONTEXT_LINK)
    cursor = FRAME_X_OUTPUT (frame)->hand_cursor;

  return cursor;
}

static void
define_cursors (struct xwidget *xw, WebKitHitTestResult *res)
{
  struct xwidget_view *xvw;

  xw->hit_result = webkit_hit_test_result_get_context (res);

  for (Lisp_Object tem = Vxwidget_view_list; CONSP (tem);
       tem = XCDR (tem))
    {
      if (XWIDGET_VIEW_P (XCAR (tem)))
	{
	  xvw = XXWIDGET_VIEW (XCAR (tem));

	  if (XXWIDGET (xvw->model) == xw)
	    {
	      xvw->cursor = cursor_for_hit (xw->hit_result, xvw->frame);
	      if (xvw->wdesc != None)
		XDefineCursor (xvw->dpy, xvw->wdesc, xvw->cursor);
	    }
	}
    }
}

static void
mouse_target_changed (WebKitWebView *webview,
		      WebKitHitTestResult *hitresult,
		      guint modifiers, gpointer xw)
{
  define_cursors (xw, hitresult);
}


static void
xwidget_button_1 (struct xwidget_view *view,
		  bool down_p, int x, int y, int button,
		  int modifier_state, Time time)
{
  GdkEvent *xg_event = gdk_event_new (down_p ? GDK_BUTTON_PRESS : GDK_BUTTON_RELEASE);
  struct xwidget *model = XXWIDGET (view->model);
  GtkWidget *target;

  /* X and Y should be relative to the origin of view->wdesc.  */
  x += view->clip_left;
  y += view->clip_top;

  target = find_widget_at_pos (model->widgetwindow_osr, x, y, &x, &y);

  if (!target)
    target = model->widget_osr;

  xg_event->any.window = gtk_widget_get_window (target);
  g_object_ref (xg_event->any.window); /* The window will be unrefed
					  later by gdk_event_free.  */

  xg_event->button.x = x;
  xg_event->button.x_root = x;
  xg_event->button.y = y;
  xg_event->button.y_root = y;
  xg_event->button.button = button;
  xg_event->button.state = modifier_state;
  xg_event->button.time = time;
  xg_event->button.device = find_suitable_pointer (view->frame);

  gtk_main_do_event (xg_event);
  gdk_event_free (xg_event);
}

void
xwidget_button (struct xwidget_view *view,
		bool down_p, int x, int y, int button,
		int modifier_state, Time time)
{
  if (button < 4 || button > 8)
    xwidget_button_1 (view, down_p, x, y, button, modifier_state, time);
  else
    {
      GdkEvent *xg_event = gdk_event_new (GDK_SCROLL);
      struct xwidget *model = XXWIDGET (view->model);
      GtkWidget *target;

      x += view->clip_left;
      y += view->clip_top;

      target = find_widget_at_pos (model->widgetwindow_osr, x, y, &x, &y);

      if (!target)
	target = model->widget_osr;

      xg_event->any.window = gtk_widget_get_window (target);
      g_object_ref (xg_event->any.window); /* The window will be unrefed
					      later by gdk_event_free.  */
      if (button == 4)
	xg_event->scroll.direction = GDK_SCROLL_UP;
      else if (button == 5)
	xg_event->scroll.direction = GDK_SCROLL_DOWN;
      else if (button == 6)
	xg_event->scroll.direction = GDK_SCROLL_LEFT;
      else
	xg_event->scroll.direction = GDK_SCROLL_RIGHT;

      xg_event->scroll.device = find_suitable_pointer (view->frame);

      xg_event->scroll.x = x;
      xg_event->scroll.x_root = x;
      xg_event->scroll.y = y;
      xg_event->scroll.y_root = y;
      xg_event->scroll.state = modifier_state;
      xg_event->scroll.time = time;

      xg_event->scroll.delta_x = 0;
      xg_event->scroll.delta_y = 0;

      gtk_main_do_event (xg_event);
      gdk_event_free (xg_event);
    }
}

void
xwidget_motion_or_crossing (struct xwidget_view *view, const XEvent *event)
{
  GdkEvent *xg_event = gdk_event_new (event->type == MotionNotify
				      ? GDK_MOTION_NOTIFY
				      : (event->type == LeaveNotify
					 ? GDK_LEAVE_NOTIFY
					 : GDK_ENTER_NOTIFY));
  struct xwidget *model = XXWIDGET (view->model);
  int x;
  int y;
  GtkWidget *target = find_widget_at_pos (model->widgetwindow_osr,
					  (event->type == MotionNotify
					   ? event->xmotion.x + view->clip_left
					   : event->xmotion.y + view->clip_top),
					  (event->type == MotionNotify
					   ? event->xmotion.y + view->clip_left
					   : event->xcrossing.y + view->clip_top),
					  &x, &y);

  if (!target)
    target = model->widgetwindow_osr;

  xg_event->any.window = gtk_widget_get_window (target);
  g_object_ref (xg_event->any.window); /* The window will be unrefed
					  later by gdk_event_free.  */

  if (event->type == MotionNotify)
    {
      xg_event->motion.x = x;
      xg_event->motion.y = y;
      xg_event->motion.x_root = event->xmotion.x_root;
      xg_event->motion.y_root = event->xmotion.y_root;
      xg_event->motion.time = event->xmotion.time;
      xg_event->motion.state = event->xmotion.state;
      xg_event->motion.device = find_suitable_pointer (view->frame);
    }
  else
    {
      xg_event->crossing.detail = min (5, event->xcrossing.detail);
      xg_event->crossing.time = event->xcrossing.time;
      xg_event->crossing.x = x;
      xg_event->crossing.y = y;
      xg_event->crossing.x_root = event->xcrossing.x_root;
      xg_event->crossing.y_root = event->xcrossing.y_root;
      gdk_event_set_device (xg_event, find_suitable_pointer (view->frame));
    }

  gtk_main_do_event (xg_event);
  gdk_event_free (xg_event);
}

static void
synthesize_focus_in_event (GtkWidget *offscreen_window)
{
  GdkWindow *wnd;
  GdkEvent *focus_event;

  if (!gtk_widget_get_realized (offscreen_window))
    gtk_widget_realize (offscreen_window);

  wnd = gtk_widget_get_window (offscreen_window);

  focus_event = gdk_event_new (GDK_FOCUS_CHANGE);
  focus_event->any.window = wnd;
  focus_event->focus_change.in = TRUE;
  g_object_ref (wnd);

  gtk_main_do_event (focus_event);
  gdk_event_free (focus_event);
}

struct xwidget_view *
xwidget_view_from_window (Window wdesc)
{
  Lisp_Object key = make_fixnum (wdesc);
  Lisp_Object xwv = Fgethash (key, x_window_to_xwv_map, Qnil);

  if (NILP (xwv))
    return NULL;

  return XXWIDGET_VIEW (xwv);
}

static void
xwidget_show_view (struct xwidget_view *xv)
{
  xv->hidden = false;
  XMoveWindow (xv->dpy, xv->wdesc,
	       xv->x + xv->clip_left,
	       xv->y + xv->clip_top);
  XMapWindow (xv->dpy, xv->wdesc);
  XFlush (xv->dpy);
}

/* Hide an xwidget view.  */
static void
xwidget_hide_view (struct xwidget_view *xv)
{
  xv->hidden = true;
  XUnmapWindow (xv->dpy, xv->wdesc);
  XFlush (xv->dpy);
}

static void
xv_do_draw (struct xwidget_view *xw, struct xwidget *w)
{
  GtkOffscreenWindow *wnd;
  cairo_surface_t *surface;
  block_input ();
  wnd = GTK_OFFSCREEN_WINDOW (w->widgetwindow_osr);
  surface = gtk_offscreen_window_get_surface (wnd);

  cairo_save (xw->cr_context);
  if (surface)
    {
      cairo_set_source_surface (xw->cr_context, surface, xw->clip_left,
				xw->clip_top);
      cairo_set_operator (xw->cr_context, CAIRO_OPERATOR_SOURCE);
      cairo_paint (xw->cr_context);
    }
  cairo_restore (xw->cr_context);

  unblock_input ();
}

/* When the off-screen webkit master view changes this signal is called.
   It copies the bitmap from the off-screen instance.  */
static gboolean
offscreen_damage_event (GtkWidget *widget, GdkEvent *event,
                        gpointer xwidget)
{
  block_input ();

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
	{
	  struct xwidget_view *view = XXWIDGET_VIEW (XCAR (tail));

	  if (view->wdesc && XXWIDGET (view->model) == xwidget)
	    xv_do_draw (view, XXWIDGET (view->model));
	}
    }

  unblock_input ();

  return FALSE;
}

void
xwidget_expose (struct xwidget_view *xv)
{
  struct xwidget *xw = XXWIDGET (xv->model);

  xv_do_draw (xv, xw);
}
#endif /* USE_GTK */

void
store_xwidget_event_string (struct xwidget *xw, const char *eventname,
                            const char *eventstr)
{
  struct input_event event;
  Lisp_Object xwl;
  XSETXWIDGET (xwl, xw);
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;
  event.arg = list3 (intern (eventname), xwl, build_string (eventstr));
  kbd_buffer_store_event (&event);
}

void
store_xwidget_download_callback_event (struct xwidget *xw,
                                       const char *url,
                                       const char *mimetype,
                                       const char *filename)
{
  struct input_event event;
  Lisp_Object xwl;
  XSETXWIDGET (xwl, xw);
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;
  event.arg = list5 (intern ("download-callback"),
                     xwl,
                     build_string (url),
                     build_string (mimetype),
                     build_string (filename));
  kbd_buffer_store_event (&event);
}

void
store_xwidget_js_callback_event (struct xwidget *xw,
                                 Lisp_Object proc,
                                 Lisp_Object argument)
{
  struct input_event event;
  Lisp_Object xwl;
  XSETXWIDGET (xwl, xw);
  EVENT_INIT (event);
  event.kind = XWIDGET_EVENT;
  event.frame_or_window = Qnil;
  event.arg = list4 (intern ("javascript-callback"), xwl, proc, argument);
  kbd_buffer_store_event (&event);
}


#ifdef USE_GTK
static void
store_xwidget_display_event (struct xwidget *xw)
{
  struct input_event evt;
  Lisp_Object val;

  XSETXWIDGET (val, xw);
  EVENT_INIT (evt);
  evt.kind = XWIDGET_DISPLAY_EVENT;
  evt.frame_or_window = Qnil;
  evt.arg = val;
  kbd_buffer_store_event (&evt);
}

static void
webkit_ready_to_show (WebKitWebView *new_view,
		      gpointer user_data)
{
  Lisp_Object tem;
  struct xwidget *xw;

  for (tem = Vxwidget_list; CONSP (tem); tem = XCDR (tem))
    {
      if (XWIDGETP (XCAR (tem)))
	{
	  xw = XXWIDGET (XCAR (tem));

	  if (EQ (xw->type, Qwebkit)
	      && WEBKIT_WEB_VIEW (xw->widget_osr) == new_view)
	    store_xwidget_display_event (xw);
	}
    }
}

static GtkWidget *
webkit_create_cb_1 (WebKitWebView *webview,
		    struct xwidget_view *xv)
{
  Lisp_Object related;
  Lisp_Object xwidget;
  GtkWidget *widget;

  XSETXWIDGET (related, xv);
  xwidget = Fmake_xwidget (Qwebkit, Qnil, make_fixnum (0),
			   make_fixnum (0), Qnil,
			   build_string (" *detached xwidget buffer*"),
			   related);

  if (NILP (xwidget))
    return NULL;

  widget = XXWIDGET (xwidget)->widget_osr;

  g_signal_connect (G_OBJECT (widget), "ready-to-show",
		    G_CALLBACK (webkit_ready_to_show), NULL);

  return widget;
}

static GtkWidget *
webkit_create_cb (WebKitWebView *webview,
		  WebKitNavigationAction *nav_action,
		  gpointer user_data)
{
  switch (webkit_navigation_action_get_navigation_type (nav_action))
    {
    case WEBKIT_NAVIGATION_TYPE_OTHER:
      return webkit_create_cb_1 (webview, user_data);

    case WEBKIT_NAVIGATION_TYPE_BACK_FORWARD:
    case WEBKIT_NAVIGATION_TYPE_RELOAD:
    case WEBKIT_NAVIGATION_TYPE_FORM_SUBMITTED:
    case WEBKIT_NAVIGATION_TYPE_FORM_RESUBMITTED:
    case WEBKIT_NAVIGATION_TYPE_LINK_CLICKED:
    default:
      return NULL;
    }
}

void
webkit_view_load_changed_cb (WebKitWebView *webkitwebview,
                             WebKitLoadEvent load_event,
                             gpointer data)
{
  struct xwidget *xw = g_object_get_data (G_OBJECT (webkitwebview),
					  XG_XWIDGET);

  switch (load_event)
    {
    case WEBKIT_LOAD_FINISHED:
      store_xwidget_event_string (xw, "load-changed", "load-finished");
      break;
    case WEBKIT_LOAD_STARTED:
      store_xwidget_event_string (xw, "load-changed", "load-started");
      break;
    case WEBKIT_LOAD_REDIRECTED:
      store_xwidget_event_string (xw, "load-changed", "load-redirected");
      break;
    case WEBKIT_LOAD_COMMITTED:
      store_xwidget_event_string (xw, "load-changed", "load-committed");
      break;
    }
}

/* Recursively convert a JavaScript value to a Lisp value. */
static Lisp_Object
webkit_js_to_lisp (JSCValue *value)
{
  if (jsc_value_is_string (value))
    {
      gchar *str_value = jsc_value_to_string (value);
      Lisp_Object ret = build_string (str_value);
      g_free (str_value);

      return ret;
    }
  else if (jsc_value_is_boolean (value))
    {
      return (jsc_value_to_boolean (value)) ? Qt : Qnil;
    }
  else if (jsc_value_is_number (value))
    {
      return make_fixnum (jsc_value_to_int32 (value));
    }
  else if (jsc_value_is_array (value))
    {
      JSCValue *len = jsc_value_object_get_property (value, "length");
      const gint32 dlen = jsc_value_to_int32 (len);

      Lisp_Object obj;
      if (! (0 <= dlen && dlen < PTRDIFF_MAX + 1.0))
	memory_full (SIZE_MAX);

      ptrdiff_t n = dlen;
      struct Lisp_Vector *p = allocate_nil_vector (n);

      for (ptrdiff_t i = 0; i < n; ++i)
	{
	  p->contents[i] =
	    webkit_js_to_lisp (jsc_value_object_get_property_at_index (value, i));
	}
      XSETVECTOR (obj, p);
      return obj;
    }
  else if (jsc_value_is_object (value))
    {
      char **properties_names = jsc_value_object_enumerate_properties (value);
      guint n = g_strv_length (properties_names);

      Lisp_Object obj;
      if (PTRDIFF_MAX < n)
	memory_full (n);
      struct Lisp_Vector *p = allocate_nil_vector (n);

      for (ptrdiff_t i = 0; i < n; ++i)
	{
	  const char *name = properties_names[i];
	  JSCValue *property = jsc_value_object_get_property (value, name);

	  p->contents[i] =
	    Fcons (build_string (name), webkit_js_to_lisp (property));
	}

      g_strfreev (properties_names);

      XSETVECTOR (obj, p);
      return obj;
    }

  return Qnil;
}

static void
webkit_javascript_finished_cb (GObject      *webview,
                               GAsyncResult *result,
                               gpointer      arg)
{
  GError *error = NULL;
  struct xwidget *xw = g_object_get_data (G_OBJECT (webview), XG_XWIDGET);

  ptrdiff_t script_idx = (intptr_t) arg;
  Lisp_Object script_callback = AREF (xw->script_callbacks, script_idx);
  ASET (xw->script_callbacks, script_idx, Qnil);
  if (!NILP (script_callback))
    xfree (xmint_pointer (XCAR (script_callback)));

  WebKitJavascriptResult *js_result =
    webkit_web_view_run_javascript_finish
    (WEBKIT_WEB_VIEW (webview), result, &error);

  if (!js_result)
    {
      g_warning ("Error running javascript: %s", error->message);
      g_error_free (error);
      return;
    }

  if (!NILP (script_callback) && !NILP (XCDR (script_callback)))
    {
      JSCValue *value = webkit_javascript_result_get_js_value (js_result);

      Lisp_Object lisp_value = webkit_js_to_lisp (value);

      /* Register an xwidget event here, which then runs the callback.
	 This ensures that the callback runs in sync with the Emacs
	 event loop.  */
      store_xwidget_js_callback_event (xw, XCDR (script_callback), lisp_value);
    }

  webkit_javascript_result_unref (js_result);
}


gboolean
webkit_download_cb (WebKitWebContext *webkitwebcontext,
                    WebKitDownload *arg1,
                    gpointer data)
{
  WebKitWebView *view = webkit_download_get_web_view(arg1);
  WebKitURIRequest *request = webkit_download_get_request(arg1);
  struct xwidget *xw = g_object_get_data (G_OBJECT (view),
                                          XG_XWIDGET);

  store_xwidget_event_string (xw, "download-started",
                              webkit_uri_request_get_uri(request));
  return FALSE;
}

static gboolean
webkit_decide_policy_cb (WebKitWebView *webView,
                         WebKitPolicyDecision *decision,
                         WebKitPolicyDecisionType type,
                         gpointer user_data)
{
  switch (type) {
  case WEBKIT_POLICY_DECISION_TYPE_RESPONSE:
    /* This function makes webkit send a download signal for all unknown
       mime types.  TODO: Defer the decision to Lisp, so that it's
       possible to make Emacs handle mime text for instance.  */
    {
      WebKitResponsePolicyDecision *response =
        WEBKIT_RESPONSE_POLICY_DECISION (decision);
      if (!webkit_response_policy_decision_is_mime_type_supported (response))
        {
          webkit_policy_decision_download (decision);
          return TRUE;
        }
      else
        return FALSE;
      break;
    }
  case WEBKIT_POLICY_DECISION_TYPE_NEW_WINDOW_ACTION:
    {
      WebKitNavigationPolicyDecision *navigation_decision =
        WEBKIT_NAVIGATION_POLICY_DECISION (decision);
      WebKitNavigationAction *navigation_action =
        webkit_navigation_policy_decision_get_navigation_action (navigation_decision);
      WebKitURIRequest *request =
        webkit_navigation_action_get_request (navigation_action);
      WebKitWebView *newview;
      struct xwidget *xw = g_object_get_data (G_OBJECT (webView), XG_XWIDGET);
      Lisp_Object val, new_xwidget;

      XSETXWIDGET (val, xw);

      new_xwidget = Fmake_xwidget (Qwebkit, Qnil, make_fixnum (0),
				   make_fixnum (0), Qnil,
				   build_string (" *detached xwidget buffer*"),
				   val);

      if (NILP (new_xwidget))
	return FALSE;

      newview = WEBKIT_WEB_VIEW (XXWIDGET (new_xwidget)->widget_osr);
      webkit_web_view_load_request (newview, request);

      store_xwidget_display_event (XXWIDGET (new_xwidget));
      return TRUE;
    }
  case WEBKIT_POLICY_DECISION_TYPE_NAVIGATION_ACTION:
    {
      WebKitNavigationPolicyDecision *navigation_decision =
        WEBKIT_NAVIGATION_POLICY_DECISION (decision);
      WebKitNavigationAction *navigation_action =
        webkit_navigation_policy_decision_get_navigation_action (navigation_decision);
      WebKitURIRequest *request =
        webkit_navigation_action_get_request (navigation_action);

      struct xwidget *xw = g_object_get_data (G_OBJECT (webView), XG_XWIDGET);
      store_xwidget_event_string (xw, "decide-policy",
                                  webkit_uri_request_get_uri (request));
      return FALSE;
      break;
    }
  default:
    return FALSE;
  }
}
#endif /* USE_GTK */


/* Initializes and does initial placement of an xwidget view on screen.  */
static struct xwidget_view *
xwidget_init_view (struct xwidget *xww,
                   struct glyph_string *s,
                   int x, int y)
{

#ifdef USE_GTK
  if (!xg_gtk_initialized)
    error ("xwidget_init_view: GTK has not been initialized");
#endif

  struct xwidget_view *xv = allocate_xwidget_view ();
  Lisp_Object val;

  XSETXWIDGET_VIEW (val, xv);
  Vxwidget_view_list = Fcons (val, Vxwidget_view_list);

  XSETWINDOW (xv->w, s->w);
  XSETXWIDGET (xv->model, xww);

#ifdef USE_GTK
  xv->dpy = FRAME_X_DISPLAY (s->f);

  xv->x = x;
  xv->y = y;

  xv->clip_left = 0;
  xv->clip_right = xww->width;
  xv->clip_top = 0;
  xv->clip_bottom = xww->height;

  xv->wdesc = None;
  xv->frame = s->f;
  xv->cursor = cursor_for_hit (xww->hit_result, s->f);
#elif defined NS_IMPL_COCOA
  nsxwidget_init_view (xv, xww, s, x, y);
  nsxwidget_resize_view(xv, xww->width, xww->height);
#endif

  return xv;
}

void
x_draw_xwidget_glyph_string (struct glyph_string *s)
{
  /* This method is called by the redisplay engine and places the
     xwidget on screen.  Moving and clipping is done here.  Also view
     initialization.  */
  struct xwidget *xww = s->xwidget;
  struct xwidget_view *xv = xwidget_view_lookup (xww, s->w);
  int text_area_x, text_area_y, text_area_width, text_area_height;
  int clip_right;
  int clip_bottom;
  int clip_top;
  int clip_left;

  int x = s->x;
  int y = s->y + (s->height / 2) - (xww->height / 2);

  /* Do initialization here in the display loop because there is no
     other time to know things like window placement etc.  Do not
     create a new view if we have found one that is usable.  */
#ifdef USE_GTK
  if (!xv)
    xv = xwidget_init_view (xww, s, x, y);
#elif defined NS_IMPL_COCOA
  if (!xv)
    {
      /* Enforce 1 to 1, model and view for macOS Cocoa webkit2.  */
      if (xww->xv)
        {
          if (xwidget_hidden (xww->xv))
            {
              Lisp_Object xvl;
              XSETXWIDGET_VIEW (xvl, xww->xv);
              Fdelete_xwidget_view (xvl);
            }
          else
            {
              message ("You can't share an xwidget (webkit2) among windows.");
              return;
            }
        }
      xv = xwidget_init_view (xww, s, x, y);
    }
#endif

  window_box (s->w, TEXT_AREA, &text_area_x, &text_area_y,
              &text_area_width, &text_area_height);

  /* On X11, this keeps generating expose events.  */
#ifndef USE_GTK
  /* Resize xwidget webkit if its container window size is changed in
     some ways, for example, a buffer became hidden in small split
     window, then it can appear front in merged whole window.  */
  if (EQ (xww->type, Qwebkit)
      && (xww->width != text_area_width || xww->height != text_area_height))
    {
      Lisp_Object xwl;
      XSETXWIDGET (xwl, xww);
      Fxwidget_resize (xwl,
                       make_int (text_area_width),
                       make_int (text_area_height));
    }
#endif

  clip_left = max (0, text_area_x - x);
  clip_right = max (clip_left,
		    min (xww->width, text_area_x + text_area_width - x));
  clip_top = max (0, text_area_y - y);
  clip_bottom = max (clip_top,
		     min (xww->height, text_area_y + text_area_height - y));

  /* We are concerned with movement of the onscreen area.  The area
     might sit still when the widget actually moves.  This happens
     when an Emacs window border moves across a widget window.  So, if
     any corner of the outer widget clipping window moves, that counts
     as movement here, even if it looks like no movement happens
     because the widget sits still inside the clipping area.  The
     widget can also move inside the clipping area, which happens
     later.  */
  bool moved = (xv->x + xv->clip_left != x + clip_left
		|| xv->y + xv->clip_top != y + clip_top);

#ifdef USE_GTK
  bool wdesc_was_none = xv->wdesc == None;
#endif
  xv->x = x;
  xv->y = y;

#ifdef USE_GTK
  block_input ();
  if (xv->wdesc == None)
    {
      Lisp_Object xvw;
      XSETXWIDGET_VIEW (xvw, xv);
      XSetWindowAttributes a;
      a.event_mask = (ExposureMask | ButtonPressMask | ButtonReleaseMask
		      | PointerMotionMask | EnterWindowMask | LeaveWindowMask);

      xv->wdesc = XCreateWindow (xv->dpy, FRAME_X_WINDOW (s->f),
				 x + clip_left, y + clip_top,
				 clip_right - clip_left,
				 clip_bottom - clip_top, 0,
				 CopyFromParent, CopyFromParent,
				 CopyFromParent, CWEventMask, &a);
      XDefineCursor (xv->dpy, xv->wdesc, xv->cursor);
      xv->cr_surface = cairo_xlib_surface_create (xv->dpy,
						  xv->wdesc,
						  FRAME_DISPLAY_INFO (s->f)->visual,
						  clip_right - clip_left,
						  clip_bottom - clip_top);
      xv->cr_context = cairo_create (xv->cr_surface);
      Fputhash (make_fixnum (xv->wdesc), xvw, x_window_to_xwv_map);

      moved = false;
    }
#endif

  /* Has it moved?  */
  if (moved)
    {
#ifdef USE_GTK
      XMoveResizeWindow (xv->dpy, xv->wdesc, x + clip_left, y + clip_top,
			 clip_right - clip_left, clip_bottom - clip_top);
      XFlush (xv->dpy);
      cairo_xlib_surface_set_size (xv->cr_surface, clip_right - clip_left,
				   clip_bottom - clip_top);
#elif defined NS_IMPL_COCOA
      nsxwidget_move_view (xv, x + clip_left, y + clip_top);
#endif
    }

  /* Clip the widget window if some parts happen to be outside
     drawable area.  An Emacs window is not a gtk window.  A gtk window
     covers the entire frame.  Clipping might have changed even if we
     haven't actually moved; try to figure out when we need to reclip
     for real.  */
  if (xv->clip_right != clip_right
      || xv->clip_bottom != clip_bottom
      || xv->clip_top != clip_top || xv->clip_left != clip_left)
    {
#ifdef USE_GTK
      if (!wdesc_was_none && !moved)
	{
	  XResizeWindow (xv->dpy, xv->wdesc, clip_right - clip_left,
			 clip_bottom - clip_top);
	  XFlush (xv->dpy);
	  cairo_xlib_surface_set_size (xv->cr_surface, clip_right - clip_left,
				       clip_bottom - clip_top);
	}
#elif defined NS_IMPL_COCOA
      nsxwidget_resize_view (xv, clip_right - clip_left,
                             clip_bottom - clip_top);
      nsxwidget_move_widget_in_view (xv, -clip_left, -clip_top);
#endif

      xv->clip_right = clip_right;
      xv->clip_bottom = clip_bottom;
      xv->clip_top = clip_top;
      xv->clip_left = clip_left;
    }

  /* If emacs wants to repaint the area where the widget lives, queue
     a redraw.  It seems its possible to get out of sync with emacs
     redraws so emacs background sometimes shows up instead of the
     xwidgets background.  It's just a visual glitch though.  */
  if (!xwidget_hidden (xv))
    {
#ifdef USE_GTK
      gtk_widget_queue_draw (xww->widget_osr);
#elif defined NS_IMPL_COCOA
      nsxwidget_set_needsdisplay (xv);
#endif
    }

#ifdef USE_GTK
  unblock_input ();
#endif
}

static bool
xwidget_is_web_view (struct xwidget *xw)
{
#ifdef USE_GTK
  return xw->widget_osr != NULL && WEBKIT_IS_WEB_VIEW (xw->widget_osr);
#elif defined NS_IMPL_COCOA
  return nsxwidget_is_web_view (xw);
#endif
}

/* Macro that checks xwidget hold webkit web view first.  */
#define WEBKIT_FN_INIT()						\
  CHECK_XWIDGET (xwidget);						\
  struct xwidget *xw = XXWIDGET (xwidget);				\
  if (!xwidget_is_web_view (xw))					\
    {									\
      fputs ("ERROR xw->widget_osr does not hold a webkit instance\n",	\
	     stdout);							\
      return Qnil;							\
    }

DEFUN ("xwidget-webkit-uri",
       Fxwidget_webkit_uri, Sxwidget_webkit_uri,
       1, 1, 0,
       doc: /* Get the current URL of XWIDGET webkit.  */)
  (Lisp_Object xwidget)
{
  WEBKIT_FN_INIT ();
#ifdef USE_GTK
  WebKitWebView *wkwv = WEBKIT_WEB_VIEW (xw->widget_osr);
  return build_string (webkit_web_view_get_uri (wkwv));
#elif defined NS_IMPL_COCOA
  return nsxwidget_webkit_uri (xw);
#endif
}

DEFUN ("xwidget-webkit-title",
       Fxwidget_webkit_title, Sxwidget_webkit_title,
       1, 1, 0,
       doc: /* Get the current title of XWIDGET webkit.  */)
  (Lisp_Object xwidget)
{
  WEBKIT_FN_INIT ();
#ifdef USE_GTK
  WebKitWebView *wkwv = WEBKIT_WEB_VIEW (xw->widget_osr);
  const gchar *title = webkit_web_view_get_title (wkwv);

  return build_string (title ? title : "");
#elif defined NS_IMPL_COCOA
  return nsxwidget_webkit_title (xw);
#endif
}

DEFUN ("xwidget-webkit-goto-uri",
       Fxwidget_webkit_goto_uri, Sxwidget_webkit_goto_uri,
       2, 2, 0,
       doc: /* Make the xwidget webkit instance referenced by XWIDGET browse URI.  */)
  (Lisp_Object xwidget, Lisp_Object uri)
{
  WEBKIT_FN_INIT ();
  CHECK_STRING (uri);
  uri = ENCODE_FILE (uri);
#ifdef USE_GTK
  webkit_web_view_load_uri (WEBKIT_WEB_VIEW (xw->widget_osr), SSDATA (uri));
#elif defined NS_IMPL_COCOA
  nsxwidget_webkit_goto_uri (xw, SSDATA (uri));
#endif
  return Qnil;
}

DEFUN ("xwidget-webkit-goto-history",
       Fxwidget_webkit_goto_history, Sxwidget_webkit_goto_history,
       2, 2, 0,
       doc: /* Make the XWIDGET webkit the REL-POSth element in load history.

If REL-POS is 0, the widget will be just reload the current element in
history.  If REL-POS is more or less than 0, the widget will load the
REL-POSth element around the current spot in the load history. */)
  (Lisp_Object xwidget, Lisp_Object rel_pos)
{
  WEBKIT_FN_INIT ();
  /* Should be one of -1, 0, 1 */
  if (XFIXNUM (rel_pos) < -1 || XFIXNUM (rel_pos) > 1)
    args_out_of_range_3 (rel_pos, make_fixnum (-1), make_fixnum (1));

#ifdef USE_GTK
  WebKitWebView *wkwv = WEBKIT_WEB_VIEW (xw->widget_osr);
  WebKitBackForwardList *list;
  WebKitBackForwardListItem *it;

  if (XFIXNUM (rel_pos) == 0)
    webkit_web_view_reload (wkwv);
  else
    {
      list = webkit_web_view_get_back_forward_list (wkwv);
      it = webkit_back_forward_list_get_nth_item (list, XFIXNUM (rel_pos));

      if (!it)
	error ("There is no item at this index");

      webkit_web_view_go_to_back_forward_list_item (wkwv, it);
    }
#elif defined NS_IMPL_COCOA
  nsxwidget_webkit_goto_history (xw, XFIXNAT (rel_pos));
#endif
  return Qnil;
}

DEFUN ("xwidget-webkit-zoom",
       Fxwidget_webkit_zoom, Sxwidget_webkit_zoom,
       2, 2, 0,
       doc: /* Change the zoom factor of the xwidget webkit instance referenced by XWIDGET.  */)
  (Lisp_Object xwidget, Lisp_Object factor)
{
  WEBKIT_FN_INIT ();
  if (FLOATP (factor))
    {
      double zoom_change = XFLOAT_DATA (factor);
#ifdef USE_GTK
      webkit_web_view_set_zoom_level
        (WEBKIT_WEB_VIEW (xw->widget_osr),
         webkit_web_view_get_zoom_level
         (WEBKIT_WEB_VIEW (xw->widget_osr)) + zoom_change);
#elif defined NS_IMPL_COCOA
      nsxwidget_webkit_zoom (xw, zoom_change);
#endif
    }
  return Qnil;
}

#ifdef USE_GTK
/* Save script and fun in the script/callback save vector and return
   its index.  */
static ptrdiff_t
save_script_callback (struct xwidget *xw, Lisp_Object script, Lisp_Object fun)
{
  Lisp_Object cbs = xw->script_callbacks;
  if (NILP (cbs))
    xw->script_callbacks = cbs = make_nil_vector (32);

  /* Find first free index.  */
  ptrdiff_t idx;
  for (idx = 0; !NILP (AREF (cbs, idx)); idx++)
    if (idx + 1 == ASIZE (cbs))
      {
	xw->script_callbacks = cbs = larger_vector (cbs, 1, -1);
	break;
      }

  ASET (cbs, idx, Fcons (make_mint_ptr (xlispstrdup (script)), fun));
  return idx;
}
#endif

DEFUN ("xwidget-webkit-execute-script",
       Fxwidget_webkit_execute_script, Sxwidget_webkit_execute_script,
       2, 3, 0,
       doc: /* Make the Webkit XWIDGET execute JavaScript SCRIPT.
If FUN is provided, feed the JavaScript return value to the single
argument procedure FUN.*/)
  (Lisp_Object xwidget, Lisp_Object script, Lisp_Object fun)
{
  WEBKIT_FN_INIT ();
  CHECK_STRING (script);
  if (!NILP (fun) && !FUNCTIONP (fun))
    wrong_type_argument (Qinvalid_function, fun);

  script = ENCODE_SYSTEM (script);

#ifdef USE_GTK
  /* Protect script and fun during GC.  */
  intptr_t idx = save_script_callback (xw, script, fun);

  /* JavaScript execution happens asynchronously.  If an elisp
     callback function is provided we pass it to the C callback
     procedure that retrieves the return value.  */
  gchar *script_string
    = xmint_pointer (XCAR (AREF (xw->script_callbacks, idx)));
  webkit_web_view_run_javascript (WEBKIT_WEB_VIEW (xw->widget_osr),
				  script_string,
                                  NULL, /* cancelable */
                                  webkit_javascript_finished_cb,
				  (gpointer) idx);
#elif defined NS_IMPL_COCOA
  nsxwidget_webkit_execute_script (xw, SSDATA (script), fun);
#endif
  return Qnil;
}

DEFUN ("xwidget-resize", Fxwidget_resize, Sxwidget_resize, 3, 3, 0,
       doc: /* Resize XWIDGET to NEW_WIDTH, NEW_HEIGHT.  */ )
  (Lisp_Object xwidget, Lisp_Object new_width, Lisp_Object new_height)
{
  CHECK_XWIDGET (xwidget);
  int w = check_integer_range (new_width, 0, INT_MAX);
  int h = check_integer_range (new_height, 0, INT_MAX);
  struct xwidget *xw = XXWIDGET (xwidget);

  xw->width = w;
  xw->height = h;

  /* If there is an offscreen widget resize it first.  */
#ifdef USE_GTK
  if (xw->widget_osr)
    {
      gtk_window_resize (GTK_WINDOW (xw->widgetwindow_osr), xw->width,
                         xw->height);
      gtk_container_resize_children (GTK_CONTAINER (xw->widgetwindow_osr));
      gtk_widget_set_size_request (GTK_WIDGET (xw->widget_osr), xw->width,
                                   xw->height);
    }
#elif defined NS_IMPL_COCOA
  nsxwidget_resize (xw);
#endif

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail); tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        {
          struct xwidget_view *xv = XXWIDGET_VIEW (XCAR (tail));
          if (XXWIDGET (xv->model) == xw)
            {
	      wset_redisplay (XWINDOW (xv->w));
            }
        }
    }

  redisplay ();

  return Qnil;
}




DEFUN ("xwidget-size-request",
       Fxwidget_size_request, Sxwidget_size_request,
       1, 1, 0,
       doc: /* Return the desired size of the XWIDGET.
This can be used to read the xwidget desired size, and resizes the
Emacs allocated area accordingly.  */)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
#ifdef USE_GTK
  GtkRequisition requisition;
  gtk_widget_size_request (XXWIDGET (xwidget)->widget_osr, &requisition);
  return list2i (requisition.width, requisition.height);
#elif defined NS_IMPL_COCOA
  return nsxwidget_get_size (XXWIDGET (xwidget));
#endif
}

DEFUN ("xwidgetp",
       Fxwidgetp, Sxwidgetp,
       1, 1, 0,
       doc: /* Return t if OBJECT is an xwidget.  */)
  (Lisp_Object object)
{
  return XWIDGETP (object) ? Qt : Qnil;
}

DEFUN ("xwidget-view-p",
       Fxwidget_view_p, Sxwidget_view_p,
       1, 1, 0,
       doc: /* Return t if OBJECT is an xwidget-view.  */)
  (Lisp_Object object)
{
  return XWIDGET_VIEW_P (object) ? Qt : Qnil;
}

DEFUN ("xwidget-info",
       Fxwidget_info, Sxwidget_info,
       1, 1, 0,
       doc: /* Return XWIDGET properties in a vector.
Currently [TYPE TITLE WIDTH HEIGHT].  */)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  struct xwidget *xw = XXWIDGET (xwidget);
  return CALLN (Fvector, xw->type, xw->title,
		make_fixed_natnum (xw->width), make_fixed_natnum (xw->height));
}

DEFUN ("xwidget-view-info",
       Fxwidget_view_info, Sxwidget_view_info,
       1, 1, 0,
       doc: /* Return properties of XWIDGET-VIEW in a vector.
Currently [X Y CLIP_RIGHT CLIP_BOTTOM CLIP_TOP CLIP_LEFT].  */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  struct xwidget_view *xv = XXWIDGET_VIEW (xwidget_view);
  return CALLN (Fvector, make_fixnum (xv->x), make_fixnum (xv->y),
		make_fixnum (xv->clip_right), make_fixnum (xv->clip_bottom),
		make_fixnum (xv->clip_top), make_fixnum (xv->clip_left));
}

DEFUN ("xwidget-view-model",
       Fxwidget_view_model, Sxwidget_view_model,
       1, 1, 0,
       doc:  /* Return the model associated with XWIDGET-VIEW.  */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  return XXWIDGET_VIEW (xwidget_view)->model;
}

DEFUN ("xwidget-view-window",
       Fxwidget_view_window, Sxwidget_view_window,
       1, 1, 0,
       doc:  /* Return the window of XWIDGET-VIEW.  */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  return XXWIDGET_VIEW (xwidget_view)->w;
}


DEFUN ("delete-xwidget-view",
       Fdelete_xwidget_view, Sdelete_xwidget_view,
       1, 1, 0,
       doc:  /* Delete the XWIDGET-VIEW.  */)
  (Lisp_Object xwidget_view)
{
  CHECK_XWIDGET_VIEW (xwidget_view);
  struct xwidget_view *xv = XXWIDGET_VIEW (xwidget_view);
#ifdef USE_GTK
  if (xv->wdesc != None)
    {
      block_input ();
      cairo_destroy (xv->cr_context);
      cairo_surface_destroy (xv->cr_surface);
      XDestroyWindow (xv->dpy, xv->wdesc);
      Fremhash (make_fixnum (xv->wdesc), x_window_to_xwv_map);
      unblock_input ();
    }
#elif defined NS_IMPL_COCOA
  nsxwidget_delete_view (xv);
#endif

  Vxwidget_view_list = Fdelq (xwidget_view, Vxwidget_view_list);
  return Qnil;
}

DEFUN ("xwidget-view-lookup",
       Fxwidget_view_lookup, Sxwidget_view_lookup,
       1, 2, 0,
       doc: /* Return the xwidget-view associated with XWIDGET in WINDOW.
If WINDOW is unspecified or nil, use the selected window.
Return nil if no association is found.  */)
  (Lisp_Object xwidget, Lisp_Object window)
{
  CHECK_XWIDGET (xwidget);

  if (NILP (window))
    window = Fselected_window ();
  CHECK_WINDOW (window);

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      Lisp_Object xwidget_view = XCAR (tail);
      if (EQ (Fxwidget_view_model (xwidget_view), xwidget)
          && EQ (Fxwidget_view_window (xwidget_view), window))
        return xwidget_view;
    }

  return Qnil;
}

DEFUN ("xwidget-plist",
       Fxwidget_plist, Sxwidget_plist,
       1, 1, 0,
       doc: /* Return the plist of XWIDGET.  */)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  return XXWIDGET (xwidget)->plist;
}

DEFUN ("xwidget-buffer",
       Fxwidget_buffer, Sxwidget_buffer,
       1, 1, 0,
       doc: /* Return the buffer of XWIDGET.  */)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  return XXWIDGET (xwidget)->buffer;
}

DEFUN ("set-xwidget-buffer",
       Fset_xwidget_buffer, Sset_xwidget_buffer,
       2, 2, 0,
       doc: /* Set XWIDGET's buffer to BUFFER.  */)
  (Lisp_Object xwidget, Lisp_Object buffer)
{
  CHECK_XWIDGET (xwidget);
  CHECK_BUFFER (buffer);

  XXWIDGET (xwidget)->buffer = buffer;
  return Qnil;
}

DEFUN ("set-xwidget-plist",
       Fset_xwidget_plist, Sset_xwidget_plist,
       2, 2, 0,
       doc: /* Replace the plist of XWIDGET with PLIST.
Returns PLIST.  */)
  (Lisp_Object xwidget, Lisp_Object plist)
{
  CHECK_XWIDGET (xwidget);
  CHECK_LIST (plist);

  XXWIDGET (xwidget)->plist = plist;
  return plist;
}

DEFUN ("set-xwidget-query-on-exit-flag",
       Fset_xwidget_query_on_exit_flag, Sset_xwidget_query_on_exit_flag,
       2, 2, 0,
       doc: /* Specify if query is needed for XWIDGET when Emacs is exited.
If the second argument FLAG is non-nil, Emacs will query the user before
exiting or killing a buffer if XWIDGET is running.
This function returns FLAG.  */)
  (Lisp_Object xwidget, Lisp_Object flag)
{
  CHECK_XWIDGET (xwidget);
  XXWIDGET (xwidget)->kill_without_query = NILP (flag);
  return flag;
}

DEFUN ("xwidget-query-on-exit-flag",
       Fxwidget_query_on_exit_flag, Sxwidget_query_on_exit_flag,
       1, 1, 0,
       doc: /* Return the current value of the query-on-exit flag for XWIDGET.  */)
  (Lisp_Object xwidget)
{
  CHECK_XWIDGET (xwidget);
  return (XXWIDGET (xwidget)->kill_without_query ? Qnil : Qt);
}

DEFUN ("xwidget-webkit-search", Fxwidget_webkit_search, Sxwidget_webkit_search,
       2, 5, 0,
       doc: /* Begin an incremental search operation in an xwidget.
QUERY should be a string containing the text to search for.  XWIDGET
should be a WebKit xwidget where the search will take place.  When the
search operation is complete, callers should also call
`xwidget-webkit-finish-search' to complete the search operation.

CASE-INSENSITIVE, when non-nil, will cause the search to ignore the
case of characters inside QUERY.  BACKWARDS, when non-nil, will cause
the search to proceed towards the beginning of the widget's contents.
WRAP-AROUND, when nil, will cause the search to stop upon hitting the
end of the widget's contents.

It is OK to call this function even when a search is already in
progress.  In that case, the previous search query will be replaced
with QUERY.  */)
  (Lisp_Object query, Lisp_Object xwidget, Lisp_Object case_insensitive,
   Lisp_Object backwards, Lisp_Object wrap_around)
{
#ifdef USE_GTK
  WebKitWebView *webview;
  WebKitFindController *controller;
  WebKitFindOptions opt;
  struct xwidget *xw;
  gchar *g_query;
#endif

  CHECK_STRING (query);
  CHECK_XWIDGET (xwidget);

#ifdef USE_GTK
  xw = XXWIDGET (xwidget);
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  query = ENCODE_UTF_8 (query);
  opt = WEBKIT_FIND_OPTIONS_NONE;
  g_query = xstrdup (SSDATA (query));

  if (!NILP (case_insensitive))
    opt |= WEBKIT_FIND_OPTIONS_CASE_INSENSITIVE;
  if (!NILP (backwards))
    opt |= WEBKIT_FIND_OPTIONS_BACKWARDS;
  if (!NILP (wrap_around))
    opt |= WEBKIT_FIND_OPTIONS_WRAP_AROUND;

  if (xw->find_text)
    xfree (xw->find_text);
  xw->find_text = g_query;

  block_input ();
  controller = webkit_web_view_get_find_controller (webview);
  webkit_find_controller_search (controller, g_query, opt, G_MAXUINT);
  unblock_input ();
#endif

  return Qnil;
}

DEFUN ("xwidget-webkit-next-result", Fxwidget_webkit_next_result,
       Sxwidget_webkit_next_result, 1, 1, 0,
       doc: /* Show the next result matching the current search query.

XWIDGET should be an xwidget that currently has a search query.
Before calling this function, you should start a search operation
using `xwidget-webkit-search'.  */)
  (Lisp_Object xwidget)
{
  struct xwidget *xw;
#ifdef USE_GTK
  WebKitWebView *webview;
  WebKitFindController *controller;
#endif

  CHECK_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);

  if (!xw->find_text)
    error ("Widget has no ongoing search operation");

#ifdef USE_GTK
  block_input ();
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  controller = webkit_web_view_get_find_controller (webview);
  webkit_find_controller_search_next (controller);
  unblock_input ();
#endif

  return Qnil;
}

DEFUN ("xwidget-webkit-previous-result", Fxwidget_webkit_previous_result,
       Sxwidget_webkit_previous_result, 1, 1, 0,
       doc: /* Show the previous result matching the current search query.

XWIDGET should be an xwidget that currently has a search query.
Before calling this function, you should start a search operation
using `xwidget-webkit-search'.  */)
  (Lisp_Object xwidget)
{
  struct xwidget *xw;
#ifdef USE_GTK
  WebKitWebView *webview;
  WebKitFindController *controller;
#endif

  CHECK_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);

  if (!xw->find_text)
    error ("Widget has no ongoing search operation");

#ifdef USE_GTK
  block_input ();
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  controller = webkit_web_view_get_find_controller (webview);
  webkit_find_controller_search_previous (controller);

  if (xw->find_text)
    {
      xfree (xw->find_text);
      xw->find_text = NULL;
    }
  unblock_input ();
#endif

  return Qnil;
}

DEFUN ("xwidget-webkit-finish-search", Fxwidget_webkit_finish_search,
       Sxwidget_webkit_finish_search, 1, 1, 0,
       doc: /* Finish XWIDGET's search operation.

XWIDGET should be an xwidget that currently has a search query.
Before calling this function, you should start a search operation
using `xwidget-webkit-search'.  */)
  (Lisp_Object xwidget)
{
  struct xwidget *xw;
#ifdef USE_GTK
  WebKitWebView *webview;
  WebKitFindController *controller;
#endif

  CHECK_XWIDGET (xwidget);
  xw = XXWIDGET (xwidget);

  if (!xw->find_text)
    error ("Widget has no ongoing search operation");

#ifdef USE_GTK
  block_input ();
  webview = WEBKIT_WEB_VIEW (xw->widget_osr);
  controller = webkit_web_view_get_find_controller (webview);
  webkit_find_controller_search_finish (controller);
  unblock_input ();
#endif

  return Qnil;
}

void
syms_of_xwidget (void)
{
  defsubr (&Smake_xwidget);
  defsubr (&Sxwidgetp);
  DEFSYM (Qxwidgetp, "xwidgetp");
  defsubr (&Sxwidget_view_p);
  DEFSYM (Qxwidget_view_p, "xwidget-view-p");
  defsubr (&Sxwidget_info);
  defsubr (&Sxwidget_view_info);
  defsubr (&Sxwidget_resize);
  defsubr (&Sget_buffer_xwidgets);
  defsubr (&Sxwidget_view_model);
  defsubr (&Sxwidget_view_window);
  defsubr (&Sxwidget_view_lookup);
  defsubr (&Sxwidget_query_on_exit_flag);
  defsubr (&Sset_xwidget_query_on_exit_flag);

  defsubr (&Sxwidget_webkit_uri);
  defsubr (&Sxwidget_webkit_title);
  defsubr (&Sxwidget_webkit_goto_uri);
  defsubr (&Sxwidget_webkit_goto_history);
  defsubr (&Sxwidget_webkit_zoom);
  defsubr (&Sxwidget_webkit_execute_script);
  DEFSYM (Qwebkit, "webkit");

  defsubr (&Sxwidget_size_request);
  defsubr (&Sdelete_xwidget_view);

  defsubr (&Sxwidget_plist);
  defsubr (&Sxwidget_buffer);
  defsubr (&Sset_xwidget_plist);
  defsubr (&Sxwidget_perform_lispy_event);
  defsubr (&Sxwidget_webkit_search);
  defsubr (&Sxwidget_webkit_finish_search);
  defsubr (&Sxwidget_webkit_next_result);
  defsubr (&Sxwidget_webkit_previous_result);
  defsubr (&Sset_xwidget_buffer);

  DEFSYM (QCxwidget, ":xwidget");
  DEFSYM (QCtitle, ":title");

  /* Do not forget to update the docstring of make-xwidget if you add
     new types.  */

  DEFSYM (Qvertical, "vertical");
  DEFSYM (Qhorizontal, "horizontal");

  DEFSYM (QCplist, ":plist");

  DEFVAR_LISP ("xwidget-list", Vxwidget_list,
               doc:	/* xwidgets list.  */);
  Vxwidget_list = Qnil;

  DEFVAR_LISP ("xwidget-view-list", Vxwidget_view_list,
             doc:	/* xwidget views list.  */);
  Vxwidget_view_list = Qnil;

  Fprovide (intern ("xwidget-internal"), Qnil);

  id_to_xwidget_map = CALLN (Fmake_hash_table, QCtest, Qeq);
  staticpro (&id_to_xwidget_map);

#ifdef USE_GTK
  x_window_to_xwv_map = CALLN (Fmake_hash_table, QCtest, Qeq);

  staticpro (&x_window_to_xwv_map);
#endif
}


/* Value is non-zero if OBJECT is a valid Lisp xwidget specification.  A
   valid xwidget specification is a list whose car is the symbol
   `xwidget', and whose rest is a property list.  The property list must
   contain a value for key `:type'.  That value must be the name of a
   supported xwidget type.  The rest of the property list depends on the
   xwidget type.  */

bool
valid_xwidget_spec_p (Lisp_Object object)
{
  return CONSP (object) && EQ (XCAR (object), Qxwidget);
}


/* Find a value associated with key in spec.  */
static Lisp_Object
xwidget_spec_value (Lisp_Object spec, Lisp_Object key)
{
  Lisp_Object tail;

  eassert (valid_xwidget_spec_p (spec));

  for (tail = XCDR (spec);
       CONSP (tail) && CONSP (XCDR (tail)); tail = XCDR (XCDR (tail)))
    {
      if (EQ (XCAR (tail), key))
	return XCAR (XCDR (tail));
    }

  return Qnil;
}


void
xwidget_view_delete_all_in_window (struct window *w)
{
  struct xwidget_view *xv = NULL;
  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        {
          xv = XXWIDGET_VIEW (XCAR (tail));
          if (XWINDOW (xv->w) == w)
            {
              Fdelete_xwidget_view (XCAR (tail));
            }
        }
    }
}

static struct xwidget_view *
xwidget_view_lookup (struct xwidget *xw, struct window *w)
{
  Lisp_Object xwidget, window, ret;
  XSETXWIDGET (xwidget, xw);
  XSETWINDOW (window, w);

  ret = Fxwidget_view_lookup (xwidget, window);

  return NILP (ret) ? NULL : XXWIDGET_VIEW (ret);
}

struct xwidget *
lookup_xwidget (Lisp_Object spec)
{
  /* When a xwidget lisp spec is found initialize the C struct that is
     used in the C code.  This is done by redisplay so values change
     if the spec changes.  So, take special care of one-shot events.  */
  Lisp_Object value;
  struct xwidget *xw;

  value = xwidget_spec_value (spec, QCxwidget);
  xw = XXWIDGET (value);

  return xw;
}

/* Set up detection of touched xwidget.  */
static void
xwidget_start_redisplay (void)
{
  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        XXWIDGET_VIEW (XCAR (tail))->redisplayed = false;
    }
}

/* The xwidget was touched during redisplay, so it isn't a candidate
   for hiding.  */
static void
xwidget_touch (struct xwidget_view *xv)
{
  xv->redisplayed = true;
}

static bool
xwidget_touched (struct xwidget_view *xv)
{
  return xv->redisplayed;
}

/* Redisplay has ended, now we should hide untouched xwidgets.  */
void
xwidget_end_redisplay (struct window *w, struct glyph_matrix *matrix)
{
  int i;
  int area;

  xwidget_start_redisplay ();
  /* Iterate desired glyph matrix of window here, hide gtk widgets
     not in the desired matrix.

     This only takes care of xwidgets in active windows.  If a window
     goes away from the screen, xwidget views must be deleted.

     dump_glyph_matrix (matrix, 2);  */
  for (i = 0; i < matrix->nrows; ++i)
    {
      /* dump_glyph_row (MATRIX_ROW (matrix, i), i, glyphs); */
      struct glyph_row *row;
      row = MATRIX_ROW (matrix, i);
      if (row->enabled_p)
	for (area = LEFT_MARGIN_AREA; area < LAST_AREA; ++area)
	  {
	    struct glyph *glyph = row->glyphs[area];
	    struct glyph *glyph_end = glyph + row->used[area];
	    for (; glyph < glyph_end; ++glyph)
	      if (glyph->type == XWIDGET_GLYPH)
		{
		  /* The only call to xwidget_end_redisplay is in dispnew.
		     xwidget_end_redisplay (w->current_matrix);  */
		  struct xwidget_view *xv
		    = xwidget_view_lookup (xwidget_from_id (glyph->u.xwidget), w);
#ifdef USE_GTK
		  /* FIXME: Is it safe to assume xwidget_view_lookup
		     always succeeds here?  If so, this comment can be removed.
		     If not, the code probably needs fixing.  */
		  eassume (xv);
		  xwidget_touch (xv);
#elif defined NS_IMPL_COCOA
                  /* In NS xwidget, xv can be NULL for the second or
                     later views for a model, the result of 1 to 1
                     model view relation enforcement.  */
                  if (xv)
                    xwidget_touch (xv);
#endif
		}
	  }
    }

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XWIDGET_VIEW_P (XCAR (tail)))
        {
          struct xwidget_view *xv = XXWIDGET_VIEW (XCAR (tail));

          /* "touched" is only meaningful for the current window, so
             disregard other views.  */
          if (XWINDOW (xv->w) == w)
            {
              if (xwidget_touched (xv))
                {
#ifdef USE_GTK
                  xwidget_show_view (xv);
#elif defined NS_IMPL_COCOA
                  nsxwidget_show_view (xv);
#endif
                }
              else
                {
#ifdef USE_GTK
                  xwidget_hide_view (xv);
#elif defined NS_IMPL_COCOA
                  nsxwidget_hide_view (xv);
#endif
                }
            }
        }
    }
}

#ifdef USE_GTK
void
kill_frame_xwidget_views (struct frame *f)
{
  Lisp_Object rem = Qnil;

  for (Lisp_Object tail = Vxwidget_view_list; CONSP (tail);
       tail = XCDR (tail))
    {
      if (XXWIDGET_VIEW (XCAR (tail))->frame == f)
	rem = Fcons (XCAR (tail), rem);
    }

  for (; CONSP (rem); rem = XCDR (rem))
    Fdelete_xwidget_view (XCAR (rem));
}
#endif

/* Kill all xwidget in BUFFER.  */
void
kill_buffer_xwidgets (Lisp_Object buffer)
{
  Lisp_Object tail, xwidget;
  for (tail = Fget_buffer_xwidgets (buffer); CONSP (tail); tail = XCDR (tail))
    {
      xwidget = XCAR (tail);
      Vxwidget_list = Fdelq (xwidget, Vxwidget_list);
      /* TODO free the GTK things in xw.  */
      {
        CHECK_XWIDGET (xwidget);
        struct xwidget *xw = XXWIDGET (xwidget);
	Fremhash (make_fixnum (xw->xwidget_id), id_to_xwidget_map);
#ifdef USE_GTK
        if (xw->widget_osr && xw->widgetwindow_osr)
          {
            gtk_widget_destroy (xw->widget_osr);
            gtk_widget_destroy (xw->widgetwindow_osr);
          }
	if (xw->find_text)
	  xfree (xw->find_text);
	if (!NILP (xw->script_callbacks))
	  for (ptrdiff_t idx = 0; idx < ASIZE (xw->script_callbacks); idx++)
	    {
	      Lisp_Object cb = AREF (xw->script_callbacks, idx);
	      if (!NILP (cb))
		xfree (xmint_pointer (XCAR (cb)));
	      ASET (xw->script_callbacks, idx, Qnil);
	    }
#elif defined NS_IMPL_COCOA
        nsxwidget_kill (xw);
#endif
      }
    }
}
