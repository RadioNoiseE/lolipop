+++ In order to use this package, you will need an Emacs 31 after
    commit 48b80a1e2b98f22d8da21f7c89ecfd9861643408, with the addition
    of function Fwindow_cursor_info.

Installation instructions are the same, while there are two variables
you customize to prevent rendering of cursor animations.  Search for
filter-modes and commands in group lolipop with describe-function.

Changes are switching to debounce to throttling.  As a consequence, we
will either have to request for redisplay everytime the function is
invoked (since the value returned by window-cursor-info is only
guaranteed to be correct after a successful redisplay), or we will
have to wrap it in a run-with-idle-timer with 0 seconds delay.
Excessive redisplay breaks with-editor (at least magit) so the latter
one is implemented.

And I hope it now supports mixed font context well enough.  Since we
need to compare the cursor height with the glyph height, then if the
glyph is shorter, we patch the y-coordinate and the cursor height.


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
