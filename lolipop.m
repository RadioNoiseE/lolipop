#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <emacs-module.h>

#define epsilon 6

int plugin_is_GPL_compatible;

typedef struct {
  NSWindow *window;
  NSView   *view;
  NSColor  *color;
  NSPoint   last_pos;
  NSSize    last_size;
  BOOL      last_bool;
} lolipop_state;

static lolipop_state lolipop;

static NSColor *lolipop_color (const char *buffer) {
  char         *end;
  unsigned long color = strtoul (++buffer, &end, 16);
  CGFloat       model[3];

  for (int i = 0; i < 3; i++)
    model[i] = ((color >> (16 - 8 * i)) & 0xFF) / (CGFloat) 255;

  return [NSColor colorWithRed:model[0] green:model[1] blue:model[2] alpha:0.6];
}

void lolipop_at (NSPoint new_pos, NSSize new_size, NSView *view) {
  CGFloat (^cubic_bezier) (CGFloat) = ^CGFloat (CGFloat time) {
    if (time < 0.5)
      return 4 * time * time * time;
    time = 2 * time - 2;
    return 0.5 * time * time * time + 1;
  };

  CGFloat (^clamp) (CGFloat) = ^CGFloat (CGFloat x) {
    return x < 0 ? 0 : (x > 1 ? 1 : x);
  };

  CGPoint (^interpolate) (CGPoint, CGPoint, CGFloat) =
      ^CGPoint (CGPoint from, CGPoint to, CGFloat time) {
        return CGPointMake (from.x + (to.x - from.x) * time,
                            from.y + (to.y - from.y) * time);
      };

  if (NSEqualPoints (new_pos, lolipop.last_pos))
    return;

  NSRect last_cursor =
      NSMakeRect (lolipop.last_pos.x, lolipop.last_pos.y,
                  lolipop.last_size.width, lolipop.last_size.height);
  NSRect new_cursor =
      NSMakeRect (new_pos.x, new_pos.y, new_size.width, new_size.height);

  CGFloat dx = new_pos.x - lolipop.last_pos.x;
  CGFloat dy = new_pos.y - lolipop.last_pos.y;

  CGFloat distance = hypot (dx, dy);
  CGFloat duration = 0.24 * tanh (distance / 100);

  int steps = duration * 120;

  CAShapeLayer *layer = [CAShapeLayer layer];
  layer.fillColor     = lolipop.color.CGColor;
  [view.layer addSublayer:layer];

  NSMutableArray *paths = [NSMutableArray arrayWithCapacity:steps + 1];

  for (int step = 0; step <= steps; step++) {
    CGFloat alpha = (CGFloat) step / steps;
    CGFloat fast  = cubic_bezier (clamp (1.6 * alpha));
    CGFloat norm  = cubic_bezier (clamp (1.6 * (alpha - 0.2)));
    CGFloat slow  = cubic_bezier (clamp (1.6 * (alpha - 0.4)));
    CGFloat tl_time, tr_time, br_time, bl_time;

    if (fabs (dx) < epsilon) {
      if (dy > 0) {
        tl_time = slow;
        tr_time = slow;
        br_time = fast;
        bl_time = fast;
      } else {
        tl_time = fast;
        tr_time = fast;
        br_time = slow;
        bl_time = slow;
      }
    } else if (fabs (dy) < epsilon) {
      if (dx > 0) {
        tl_time = slow;
        tr_time = fast;
        br_time = fast;
        bl_time = slow;
      } else {
        tl_time = fast;
        tr_time = slow;
        br_time = slow;
        bl_time = fast;
      }
    } else if (dx > 0) {
      if (dy > 0) {
        tl_time = slow;
        tr_time = norm;
        br_time = fast;
        bl_time = norm;
      } else {
        tl_time = norm;
        tr_time = fast;
        br_time = norm;
        bl_time = slow;
      }
    } else {
      if (dy > 0) {
        tl_time = norm;
        tr_time = slow;
        br_time = norm;
        bl_time = fast;
      } else {
        tl_time = fast;
        tr_time = norm;
        br_time = slow;
        bl_time = norm;
      }
    }

    CGPoint pos_tl =
        interpolate (last_cursor.origin, new_cursor.origin, tl_time);
    CGPoint pos_tr = interpolate (
        CGPointMake (NSMaxX (last_cursor), NSMinY (last_cursor)),
        CGPointMake (NSMaxX (new_cursor), NSMinY (new_cursor)), tr_time);
    CGPoint pos_br = interpolate (
        CGPointMake (NSMaxX (last_cursor), NSMaxY (last_cursor)),
        CGPointMake (NSMaxX (new_cursor), NSMaxY (new_cursor)), br_time);
    CGPoint pos_bl = interpolate (
        CGPointMake (NSMinX (last_cursor), NSMaxY (last_cursor)),
        CGPointMake (NSMinX (new_cursor), NSMaxY (new_cursor)), bl_time);

    CGMutablePathRef path = CGPathCreateMutable ();
    CGPathMoveToPoint (path, NULL, pos_tl.x, pos_tl.y);
    CGPathAddLineToPoint (path, NULL, pos_tr.x, pos_tr.y);
    CGPathAddLineToPoint (path, NULL, pos_br.x, pos_br.y);
    CGPathAddLineToPoint (path, NULL, pos_bl.x, pos_bl.y);
    CGPathCloseSubpath (path);

    [paths addObject:(__bridge id) path];
    CGPathRelease (path);
  }

  CAKeyframeAnimation *animation =
      [CAKeyframeAnimation animationWithKeyPath:@"path"];
  animation.values              = paths;
  animation.duration            = duration;
  animation.removedOnCompletion = YES;

  [CATransaction begin];
  [CATransaction setCompletionBlock:^{
    [layer removeFromSuperlayer];
  }];

  [layer addAnimation:animation forKey:nil];
  layer.path = (__bridge CGPathRef) paths.lastObject;

  [CATransaction commit];
}

static void lolipop_lick (CGFloat x, CGFloat y, CGFloat w, CGFloat h) {
  NSWindow *window   = [NSApp mainWindow];
  NSView   *view     = window.contentView;
  NSPoint   new_pos  = NSMakePoint (x, view.bounds.size.height - y - h);
  NSSize    new_size = NSMakeSize (w, h);

  if (!view.layer)
    view.wantsLayer = YES;

  if (lolipop.window != window) {
    lolipop.window    = window;
    lolipop.view      = view;
    lolipop.last_bool = NO;
  }

  if (lolipop.last_bool)
    lolipop_at (new_pos, new_size, view);

  lolipop.last_pos  = new_pos;
  lolipop.last_size = new_size;
  lolipop.last_bool = YES;
}

static emacs_value lolipop_chew (emacs_env *env, ptrdiff_t nargs,
                                 emacs_value *args, void *data) {
  if (nargs != 5)
    goto home;

  int x = env->extract_integer (env, args[0]);
  int y = env->extract_integer (env, args[1]);
  int w = env->extract_integer (env, args[2]);
  int h = env->extract_integer (env, args[3]);

  ptrdiff_t length;
  env->copy_string_contents (env, args[4], NULL, &length);
  char *buffer = malloc (length);
  env->copy_string_contents (env, args[4], buffer, &length);
  lolipop.color = lolipop_color (buffer);
  free (buffer);

  dispatch_async (dispatch_get_main_queue (), ^{
    lolipop_lick ((CGFloat) x, (CGFloat) y, (CGFloat) w, (CGFloat) h);
  });

home:
  return env->intern (env, "nil");
}

int emacs_module_init (struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment (runtime);

  emacs_value function =
      env->make_function (env, 5, 5, lolipop_chew, "Lolipop dissolves.", NULL);
  emacs_value symbol = env->intern (env, "lolipop-chew");
  emacs_value args[] = {symbol, function};

  env->funcall (env, env->intern (env, "defalias"), 2, args);

  return 0;
}
