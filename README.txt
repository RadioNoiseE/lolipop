The development will be paused for a while, before I finish exposing
cursor related stuff.

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
