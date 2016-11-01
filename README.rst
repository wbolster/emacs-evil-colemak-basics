======================
evil-colemak-basics.el
======================

This Emacs package provides basic key bindings for ``evil-mode`` for
use with the Colemak keyboard layout.


Basic idea
==========

The core design principle is that the ``hnei`` keys (``hjkl`` on
Qwerty) are used for navigation. This has a few implications:

* The ``h`` key stays at the same position. It stays as-is.

* Since ``nei`` are now used for navigation, the original
  functionality for ``n`` (next search match), ``e`` (end of word) and
  ``i`` (insert) needs a new place. Since ``j``, ``k``, and ``l``
  (``y``, ``n``, and ``u`` on Qwerty) are no longer needed for
  navigation, intelligently move these features to their positions
  while retaining as much muscle memory as possible.

* As a final tweak, ``u`` and ``l`` get swapped so that undo and
  insert have the same positions on both Colemak and Qwerty. Again,
  retain muscle memory.

The result is that you do not need to retrain your muscle memory
for navigation, insert, undo, and search match navigation.


Key bindings
============

The starting point is a plain Colemak layout with all keys in their
normal positions. On top of that the following keys are changed.

Motion, normal, visual and operator-pending state:

- ``hnei`` to navigate
  (``hjkl`` on Qwerty, same position)

- ``k`` and ``K`` to search next/previous
  (``n`` and ``N`` on Qwerty, same position)

- ``j`` and ``J`` to jump to end of word/WORD
  (``e`` and ``E`` on Qwerty, *different* position)

Normal and visual state:

- ``u`` and ``U`` to insert
  (``i`` and ``I`` on Qwerty, same position)

- ``l`` to undo
  (``u`` on Qwerty, same position)

- ``N`` to join lines
  (``J`` on Qwerty, same position)

- ``E`` for lookup
  (``K`` on Qwerty, same position)

Operator-pending state:

- ``u`` for choosing an inner text object
  (``i`` on Qwerty, same position),
  e.g. ``duw`` (``diw`` on Qwerty) deletes the inner word

In addition to the keys listed explicitly above, variations like
``gn`` and ``ge`` (``gj`` and ``gk`` on Qwerty) to navigate visual
lines instead of real lines also behave as expected.


Background
==========

Some other Colemak packages for Emacs/Evil (and Vim) redefine a big
part of the keyboard and hence significantly change the editing
experience. This is a no go for seasoned users who are used to the
default Emacs/Evil (and Vim) key bindings, and just want a new
keyboard layout, not a new editor.

The other extreme would be to not change anything at all. Since this
breaks muscle memory for commonly used navigation keys, it makes
switching to Colemak (from Qwerty) even harder. More importantly, this
also results in non-ergonomic navigation, which defeats the purpose of
using Colemak in the first place.

This package provides a sensible compromise. It changes just a few key
bindings, namely those used for basic navigation, and only makes a
minimal number of additional changes to deal sensibly with the
conflicts introduced by remapping the navigation keys.


Installation
============

This package is available from Melpa and can be installed with the
package manager (``package.el``) that comes bundled with Emacs 24+.
Simply run::

  M-x package-install RET evil-colemak-basics RET

Alternatively, put the Elisp file somewhere in your loading path and
load it explicitly::

  (require 'evil-colemak-basics)

Note that this ``(require)`` is not needed when installing from Melpa.


Usage
=====

To enable globally, use::

  M-x global-evil-colemak-basics-mode RET

To enable for just a single buffer, use::

  M-x evil-colemak-basics-mod RET

To enable permanently, put this in your ``init.el``::

  (global-evil-colemak-basics-mode)

When enabled, a lighter showing ``hnei`` will appear in your mode
line. If you don't like it, use ``rich-minority`` or ``diminish`` to
hide it.

Note that this package assumes that your operating system is properly
configured for the Colemak keyboard layout. It does not implement the
Colemak layout on top of a Qwerty layout.


Credits
=======

These bindings were inspired by a similar configuration for Vim and
other software by James Pike, available from
https://github.com/ohjames/colemak
