======================
evil-colemak-basics.el
======================

This Emacs package provides basic key bindings for evil-mode_
optimized for the Colemak_ keyboard layout.

.. _evil-mode: https://bitbucket.org/lyro/evil/
.. _Colemak: https://colemak.com/

Designed as a smart hybrid between Colemak and Qwerty, it works
especially well for Colemak converts who have used Vim on a Qwerty
keyboard before they made the switch to Colemak. All keys (except one)
are in their Colemak or Qwerty positions, depending on what provides
the most ergonomic editing experience, and `muscle memory`_ is
retained for many frequently used commands.

.. _muscle memory: https://en.wikipedia.org/wiki/Muscle_memory>


Key bindings
============

The starting point is a plain Colemak layout with all keys in their
normal positions. On top of that the following keys are changed for
motion state (``m``), normal state (``n``), visual state (``v``), and
operator-pending state (``o``, inherits from motion state):

.. list-table::
   :header-rows: 1

   * - Colemak
     - Qwerty
     - Action
     - States
     - At Qwerty position?
     - Remarks

   * - ``h``, ``n``, ``e``, ``i``
     - ``h``, ``j``, ``k``, ``l``
     - navigate
     - ``mnvo``
     - yes
     -

   * - ``k``, ``K``
     - ``n``, ``N``
     - search next/previous
     - ``mnvo``
     - yes
     -

   * - ``u``, ``U``
     - ``i``, ``I``
     - insert
     - ``_nv_``
     - yes
     -

   * - ``l``
     - ``u``
     - undo
     - ``_nv_``
     - yes
     -

   * - ``N``
     - ``J``
     - join lines
     - ``_nv_``
     - yes
     -

   * - ``E``
     - ``K``
     - lookup
     - ``mnv_``
     - yes
     -

   * - ``u``
     - ``i``
     - inner text object keymap
     - ``___o``
     - yes
     -

   * - ``f``, ``F``
     - ``e``, ``E``
     - jump to end of word
     - ``mnvo``
     - yes
     - with ``t-f-j`` rotation

   * - ``t``, ``T``
     - ``f``, ``f``
     - jump to character
     - ``mnvo``
     - yes
     - with ``t-f-j`` rotation

   * - ``j``, ``J``
     - ``e``, ``E``
     - jump until character
     - ``mnvo``
     - no
     - with ``t-f-j`` rotation

   * - ``j``, ``J``
     - ``e``, ``E``
     - jump to end of word
     - ``mnvo``
     - no
     - without ``t-f-j`` rotation

In addition to the keys listed explicitly above, variations like
``gn`` and ``ge`` (``gj`` and ``gk`` on Qwerty) to navigate visual
lines instead of real lines also behave as expected.

The tables below indicate whether a key has its Colemak meaning, its
Qwerty meaning, the same meaning, or neither.

======= = = = = = = = = = =
colemak q w f p g j l u y ;
\       ↕ ↕ ↓ ↑ ↑ – ↓ ↓ ↑ ↑
qwerty  q w e r t y u i o p

======= = = = = = = = = = =
colemak a r s t d h n e i o
\       ↕ ↑ ↑ ↓ ↑ ↕ ↓ ↓ ↓ ↑
qwerty  a s d f g h j k l ;
======= = = = = = = = = = =

======= = = = = = = =
colemak z x c v b k m
\       ↕ ↕ ↕ ↕ ↕ ↓ ↕
qwerty  z x c v b n m
======= = = = = = = =


Design rationale
================

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

This package provides a sensible compromise. It changes a few key
bindings, namely those used for basic navigation (``hnei``), and only
makes a number of additional cascading changes to deal sensibly with
the implications of remapping the navigation keys. No functionality is
lost.

The design steps to arrive at the key bindings provided by this package are as follows:

* The starting point is a standard Colemak keyboard layout. This works
  well for many mnemonic keys like ``d`` (delete), ``p`` (put/paste),
  ``y`` (yank), and various others.

* The ``hnei`` (``hjkl`` on qwerty) keys are used for navigation. This
  is a must-have for ergonomic ‘arrow key’ navigation.

* This means ``n`` (next search match), ``e`` (end of word) and ``i``
  (insert) need a new home. All of these move to their Qwerty
  positions.

* As a consequence, ``u`` (undo), and ``f`` (jump to character) need a
  new home. Also move these to their Qwerty positions.

* At this point, all commands are either at their Colemak or Qwerty
  position, with one exception. The infrequently used ``t`` (jump
  until character) command has to be relocated to Colemak ``j``, the
  only remaining spot, which is not only hard to reach, but also
  matches neither its Colemak nor its Qwerty location.

While this may seem complex, the result is that you can happily think
and type in Colemak, while you can use muscle memory for many often
used commands:

* basic ‘arrow key’ navigation
* insert
* undo
* search match navigation
* end of word
* jump to character

…in addition to all the keys that already have the same position on
Colemak and Qwerty.

A lighter variation of the above scheme is also available by omitting
the ``t-f-j`` rotation, which will cause ``t`` (jump until character)
and ``f`` (jump to character) to live at their Colemak position, which
means that the ‘end of word’ command ends up at the ``j`` position,


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


Configuration
=============

The ``t-f-j`` rotation is enabled by default but can be disabled using::

  (setq evil-colemak-basics-rotate-t-f-j nil)

To use evil-snipe_ for the ‘jump to character’ and ‘jump until
character’ commands, use::

  (setq evil-colemak-basics-char-jump-commands 'evil-snipe)

.. _evil-snipe: https://github.com/hlissner/evil-snipe

You can also use the customize interface to get more information about
these settings::

  M-x customize-group RET evil-colemak-basics RET

However, since the settings *must* be set before loading the package
(since they influence how the keymap is constructed), the most
reliable way is to put ``(setq …)`` in your ``init.el`` file, before
using ``(require …)`` or invoking any of the autoloaded functions like
``(global-evil-colemak-basics-mode)``.


Credits
=======

These bindings were inspired by a similar configuration for Vim and
other software by James Pike, available from
https://github.com/ohjames/colemak
