======================
evil-colemak-basics.el
======================

This Emacs package provides basic key bindings for ``evil-mode`` for
use with the Colemak keyboard layout.

It changes just a few key bindings, namely those used for basic
navigation, and only makes a minimal number of additional changes to
deal sensibly with the conflicts introduced by remapping the
navigation keys.


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

This package provides a sensible compromise. The core principle is
that ``h``/``n``/``e``/``i`` (``h``/``j``/``k``/``l`` on Qwerty) are
used for navigation. That means that the functionality for
``n``/``e``/``i`` (and ``N``/``E``/``I``) needs a new place. The ``h``
key stays at the same position, and hence needs no special handling.
Since ``k``/``l``/``j`` (``n``/``u``/``y`` on Qwerty) are now free,
simply move it there. As a final tweak, swap ``u`` and ``l`` so that
undo and insert have the same positions on both Colemak and Qwerty.

The end result is that you do not need to
retrain your muscle memory for navigation, insert, undo, and search
match navigation.


Key bindings
============

The starting point is a plain Colemak layout with all keys in their
normal positions. On top of that the following keys are changed.

Motion, normal, visual and operator-pending state:

- ``h``/``n``/``e``/``i`` to navigate
  (``h``/``j``/``k``/``l`` on Qwerty, same position)

- ``k``/``K`` to search next/previous
  (``n``/``N`` on Qwerty, same position)

- ``j``/``J`` to jump to end of word/WORD
  (``e``/``E`` on Qwerty, *not* same position)

Normal and visual state:

- ``u``/``U`` to insert
  (``i``/``I`` on Qwerty, same position)

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
``gn``/``ge`` (``gj``/``gk`` on Qwerty) to navigate visual lines
instead of real lines also behave as expected.


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


To do
=====

* allow enabling/disabling via custom

* add customisable next/previous line functions (visual-line vs regular line)
