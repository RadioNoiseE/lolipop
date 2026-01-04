+++ To use this package, you must use Emacs 31 built from a revision
    after commit 48b80a, which introduces Fwindow_cursor_info.

The installation process remains unchanged.  However, two new
customization variables have been added to disable cursor animation in
situations where it is undesirable.  For details, use
describe-function and look up filter-modes and filter-commands under
the lolipop customization group.

One significant change is the switch from debounce to throttling.
This introduces a design choice: either force redisplay every time the
function is called (window-cursor-info only returns reliable result
after a successful redisplay), or defer execution using idle timer,
ensuring that cursor data is only read after redisplay triggered by
cursor movement.  Because excessive redisplay interferes with
with-editor (notably magit), the latter approach was chosen.

The package is now expected to handle mixed-font contexts more
reliably.  This is achieved by comparing the cursor height with the
glyph height; when the glyph is shorter, the y-coordinate and cursor
height are patched accordingly.


--- The development will be paused for a while, before I finish
    exposing cursor related stuff.

Now it is mostly usable, and can handle text buffers (mixed font)
correctly.  Use make to build the shared library and the minor mode,
then put them in your load-path to install.

The patch required to obtain cursor coordinates and geometry at
finerer granularity has already been submitted and is currently under
review for inclusion in Emacs.  Until it is merged, the following
items remain on the project roadmap:

 -  Reduce flickering during high-frequency cursor motion (i.e.,
    middle and long distance movements).

 -  Replace the current duration function 0.6 * tanh (distance / 400)
    with a more robust and better-tuned alternative.

 -  Dynamically adjust trail (animation) transparency based on cursor
    animation characteristics.

 -  Elastic coefficients for the cursor's four corners derived from
    the cursor animation movement offset.

Patch status at https://debbugs.gnu.org/cgi/bugreport.cgi?bug=80023.
