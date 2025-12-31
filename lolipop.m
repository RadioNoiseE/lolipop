#include <AppKit/AppKit.h>
#include <QuartzCore/QuartzCore.h>
#include <emacs-module.h>

#define BEZIER(t)                                                              \
  ({                                                                           \
    CGFloat _t = (t);                                                          \
    _t < 0.5 ? 4 * _t *_t *_t : (0.5 * ((_t = 2 * _t - 2), _t * _t * _t) + 1); \
  })
#define CLAMP01(x)                                                             \
  ({                                                                           \
    CGFloat _x = (x);                                                          \
    _x < 0 ? 0 : (_x > 1 ? 1 : _x);                                            \
  })
#define LERP(a, b, t)                                                          \
  ({                                                                           \
    CGPoint _a = (a), _b = (b);                                                \
    CGFloat _t = (t);                                                          \
    CGPointMake (_a.x + (_b.x - _a.x) * _t, _a.y + (_b.y - _a.y) * _t);        \
  })

int plugin_is_GPL_compatible;

typedef struct {
  NSWindow *window;
  NSView   *view;
  NSColor  *color;
  BOOL      state;
  NSPoint   previous_position;
  NSSize    previous_geometry;
} lolipop_state;

static lolipop_state lolipop;

static void lolipop_crush (NSPoint current_position, NSSize current_geometry,
                           NSView *view) {
  if (NSEqualPoints (current_position, lolipop.previous_position))
    return;

  NSRect previous_cursor = NSMakeRect (
      lolipop.previous_position.x, lolipop.previous_position.y,
      lolipop.previous_geometry.width, lolipop.previous_geometry.height);
  NSRect current_cursor =
      NSMakeRect (current_position.x, current_position.y,
                  current_geometry.width, current_geometry.height);

  CGFloat dx = current_position.x - lolipop.previous_position.x;
  CGFloat dy = current_position.y - lolipop.previous_position.y;

  CGFloat distance = hypot (dx, dy);
  CGFloat duration = 0.6 * tanh (distance / 400);

  NSScreen *screen = view.window.screen ?: [NSScreen mainScreen];
  int       frames = ceil (duration * screen.maximumFramesPerSecond);

  CAShapeLayer *layer = [CAShapeLayer layer];
  layer.fillColor     = lolipop.color.CGColor;
  [view.layer addSublayer:layer];

  NSMutableArray *paths = [NSMutableArray arrayWithCapacity:frames + 1];

  for (int frame = 0; frame <= frames; frame++) {
    CGFloat alpha = (CGFloat) frame / frames;
    CGFloat fast  = BEZIER (CLAMP01 (1.6 * alpha));
    CGFloat norm  = BEZIER (CLAMP01 (1.6 * (alpha - 0.2)));
    CGFloat slow  = BEZIER (CLAMP01 (1.6 * (alpha - 0.4)));
    CGFloat tl_ease, tr_ease, br_ease, bl_ease;

    if (fabs (dx) < CGFLOAT_EPSILON) {
      if (dy > 0) {
        tl_ease = slow;
        tr_ease = slow;
        br_ease = fast;
        bl_ease = fast;
      } else {
        tl_ease = fast;
        tr_ease = fast;
        br_ease = slow;
        bl_ease = slow;
      }
    } else if (fabs (dy) < CGFLOAT_EPSILON) {
      if (dx > 0) {
        tl_ease = slow;
        tr_ease = fast;
        br_ease = fast;
        bl_ease = slow;
      } else {
        tl_ease = fast;
        tr_ease = slow;
        br_ease = slow;
        bl_ease = fast;
      }
    } else if (dx > 0) {
      if (dy > 0) {
        tl_ease = slow;
        tr_ease = norm;
        br_ease = fast;
        bl_ease = norm;
      } else {
        tl_ease = norm;
        tr_ease = fast;
        br_ease = norm;
        bl_ease = slow;
      }
    } else {
      if (dy > 0) {
        tl_ease = norm;
        tr_ease = slow;
        br_ease = norm;
        bl_ease = fast;
      } else {
        tl_ease = fast;
        tr_ease = norm;
        br_ease = slow;
        bl_ease = norm;
      }
    }

    CGPoint tl_position =
        LERP (CGPointMake (NSMinX (previous_cursor), NSMinY (previous_cursor)),
              CGPointMake (NSMinX (current_cursor), NSMinY (current_cursor)),
              tl_ease);
    CGPoint tr_position =
        LERP (CGPointMake (NSMaxX (previous_cursor), NSMinY (previous_cursor)),
              CGPointMake (NSMaxX (current_cursor), NSMinY (current_cursor)),
              tr_ease);
    CGPoint br_position =
        LERP (CGPointMake (NSMaxX (previous_cursor), NSMaxY (previous_cursor)),
              CGPointMake (NSMaxX (current_cursor), NSMaxY (current_cursor)),
              br_ease);
    CGPoint bl_position =
        LERP (CGPointMake (NSMinX (previous_cursor), NSMaxY (previous_cursor)),
              CGPointMake (NSMinX (current_cursor), NSMaxY (current_cursor)),
              bl_ease);

    CGMutablePathRef path = CGPathCreateMutable ();

    CGPathMoveToPoint (path, NULL, tl_position.x, tl_position.y);
    CGPathAddLineToPoint (path, NULL, tr_position.x, tr_position.y);
    CGPathAddLineToPoint (path, NULL, br_position.x, br_position.y);
    CGPathAddLineToPoint (path, NULL, bl_position.x, bl_position.y);
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

static void lolipop_chew (CGFloat x, CGFloat y, CGFloat width, CGFloat height,
                          BOOL render) {
  NSWindow *window   = [NSApp mainWindow];
  NSView   *view     = window.contentView;
  NSPoint   position = NSMakePoint (x, view.bounds.size.height - y - height);
  NSSize    geometry = NSMakeSize (width, height);

  if (!view.layer)
    view.wantsLayer = YES;

  if (lolipop.window != window) {
    lolipop.window = window;
    lolipop.view   = view;
    lolipop.state  = NO;
  }

  if (lolipop.state && render)
    lolipop_crush (position, geometry, view);

  lolipop.state             = YES;
  lolipop.previous_position = position;
  lolipop.previous_geometry = geometry;
}

static emacs_value lolipop_lick (emacs_env *env, ptrdiff_t nargs,
                                 emacs_value *args, void *data) {
  BOOL render = env->is_not_nil (env, args[0]);

  int x = env->extract_integer (env, args[1]);
  int y = env->extract_integer (env, args[2]);

  int width  = env->extract_integer (env, args[3]);
  int height = env->extract_integer (env, args[4]);

  double red   = env->extract_float (env, args[5]);
  double green = env->extract_float (env, args[6]);
  double blue  = env->extract_float (env, args[7]);

  lolipop.color = [NSColor colorWithRed:red green:green blue:blue alpha:1];

  dispatch_async (dispatch_get_main_queue (), ^{
    lolipop_chew ((CGFloat) x, (CGFloat) y, (CGFloat) width, (CGFloat) height,
                  render);
  });

  return env->intern (env, "nil");
}

int emacs_module_init (struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment (runtime);

  emacs_value function = env->make_function (
      env, 8, 8, lolipop_lick,
      R"(Render the cursor animation on a separate window layer.

RENDER controls whether the cursor animation is rendered.  X and Y
specify the cursor position in pixels.  WIDTH and HEIGHT specify the
cursor size.  RED, GREEN and BLUE specify the cursor color channels.

The animation is rendered on a dedicated layer attached to current
frame and does not participate in Emacs redisplay.

If RENDER is nil, only internal cursor state is updated.

(fn RENDER X Y WIDTH HEIGHT RED GREEN BLUE))",
      NULL);
  emacs_value symbol = env->intern (env, "lolipop-lick");
  emacs_value args[] = {symbol, function};

  env->funcall (env, env->intern (env, "defalias"), 2, args);

  return 0;
}
