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

.. _muscle memory: https://en.wikipedia.org/wiki/Muscle_memory


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
     - ``f``, ``F``
     - jump to character
     - ``mnvo``
     - yes
     - with ``t-f-j`` rotation

   * - ``j``, ``J``
     - ``t``, ``T``
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

Some other Colemak configurations for Emacs/Evil (and Vim) redefine
big parts of the non-insert states (normal, visual, and so on) by
changing or even completely removing standard Vim commands. Such
changes significantly change the editing experience. This is a no go
for seasoned users who are used to the default Emacs/Evil (and Vim)
key bindings, and just want a new keyboard layout, not a new editor.

The other extreme is to not change anything at all. While a ‘no
configuration’ approach may work fine for some, others consider it
simply unacceptable to not have ‘arrow navigation’ keys (which are not
mnemonic commands) at their usual ergonomic home row positions,
because it completely breaks their muscle memory, making the switch to
Colemak (from Qwerty) even harder than it already is.

This package provides a sensible compromise between ‘change
everything’ and ‘change nothing’. It changes a few key bindings,
namely those used for basic navigation (``hnei``), and only makes a
number of additional cascading changes to deal sensibly with the
implications of remapping the navigation keys. No functionality is
lost.

The design steps to arrive at the key bindings provided by this
package are as follows:

* The starting point is a standard Colemak keyboard layout. This works
  well for many mnemonic keys like ``d`` (delete), ``p`` (put/paste),
  ``y`` (yank), and various others.

* The ``hnei`` (``hjkl`` on Qwerty) keys are used for the familiar
  ‘arrow key’ navigation.

* This means ``n`` (next search match), ``e`` (end of word) and ``i``
  (insert) need a new home. All of these move to their Qwerty
  positions.

* As a consequence, ``u`` (undo), and ``f`` (jump to character) need a
  new home. Also move these to their Qwerty positions.

* At this point only ``t`` (jump until character) needs a new home. It
  cannot go to its Qwerty position, since that would cause a cascade
  of follow-up changes that would break many mnemonic keys. Therefore
  it moves to Colemak ``j``, the only remaining unused spot. This
  position is hard to reach, but since this command is not frequently
  used, this is an acceptable trade-off.

* Now all commands are either at their Colemak or Qwerty position,
  except for ‘jump until character‘, which is at neither.

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
Colemak and Qwerty, such as ``b`` (previous word), ``c`` (change),
``w`` (next word), and various others.

As an alternative, a lighter variation of the above scheme is also
available by omitting the ``t-f-j`` rotation. Without that rotation,
``t`` (jump until character) and ``f`` (jump to character) stay at
their Colemak position, which some may prefer. The downside is that
the ‘end of word’ command ends up at the hard to reach ``j`` position.


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

Note that this package will load ``evil-snipe``, so if you have any
configuration that should be set before ``evil-snipe`` is loaded, such
as ``evil-snipe-auto-disable-substitute``, make sure to configure
``evil-snipe`` before this package is loaded.

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
