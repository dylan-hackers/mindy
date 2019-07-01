mindy
=====

.. image:: https://travis-ci.org/dylan-hackers/mindy.svg?branch=master
   :target: https://travis-ci.org/dylan-hackers/mindy

Mindy is an implementation of a language that is very much like the
language described in the `Dylan Reference Manual`_ (DRM). Mindy was
named for "Mindy Is Not Dylan Yet", and it still is and will probably
always remain a Dylan subset, primarily because ``mindycomp`` doesn’t
do enough semantic analysis to implement macros or to implement the
precise semantics of Dylan naming.

What is Dylan?
==============

... Put more info here in the near future ...

Building and Installing Mindy
=============================

Mindy has very few dependencies:

* ``flex``
* ``bison``
* ``cmake``
* ``ninja`` or ``make`` on all but Windows.
* A C compiler that ``cmake`` and ``ninja`` or ``make`` know how to drive.

Packages for these are typically available on all platforms or may already
be installed by default.

Mac OS X with homebrew (and Xcode already installed)::

    brew install cmake ninja

Mindy comes with a build system that uses `cmake`_.

We recommend doing your build with `ninja`_ rather than ``make``
as it is significantly faster and therefore more enjoyable.

We also recommend using a separate directory for your build
rather than doing it directly in the source directory. Fortunately,
this is very easy to do.

::

    mkdir build
    cd build
    cmake .. -G Ninja
    ninja
    ninja test

Building Documentation
======================

If you want to build the documentation, make sure that you've installed
`Sphinx`_ and then pass ``-DMINDY_BUILD_DOCS=ON`` when invoking ``cmake``::

    cmake .. -G Ninja -DMINDY_BUILD_DOCS=ON

Also, make sure that you've gotten the Git submodules along with the
sources for this repository.  You can do this by cloning recursively::

    git clone --recursive https://github.com/project-mindy/mindy.git

If you've already cloned the repository, then you can::

    git submodule update --init --recursive

The documentation will be present in your build directory in
``documentation/html``.

Learning More
=============

We'll link here when the documentation has been revived and placed
online.

What Can You Do?
================

Apart from just using mindy, you're more than welcome to help us
develop it further and improve it.

* You can talk to us on `Gitter`_ (similar to Slack).
* You can also talk to us on IRC in ``#dylan`` on ``irc.freenode.net``
  if you prefer.
* Join our `mailing list`_.
* Check out our list of `open issues` or our list of `open easy issues`.

Why Mindy?
==========

Since we already have `Open Dylan`_, why is Mindy useful?

* It is very easy to compile and use.
* It is very fast to compile code (despite running it more slowly).
* It is a very small and self-contained code base, so it is easy
  to hack on and experiment with.
* The built-in debugger is pretty powerful and convenient.
* It may be useful as an embedded scripting engine.

Why not Mindy?
==============

* It doesn't implement the full Dylan language. Notably, macros and
  limited collections are not implemented.
* Mindy is less functional and less complete than Open Dylan.
* The libraries are less complete and less up to date.
* The interpreter isn't terribly fast (although it may be fast enough).
* The compiler doesn't do much analysis or modeling, so it isn't
  capable of doing much error checking, reporting or other diagnostics.

.. _Dylan Reference Manual: http://opendylan.org/books/drm/
.. _cmake: http://www.cmake.org/
.. _ninja: https://martine.github.io/ninja/
.. _Sphinx: http://www.sphinx-doc.org/
.. _Gitter: https://gitter.im/project-mindy/mindy
.. _mailing list: https://lists.opendylan.org/mailman/listinfo/hackers
.. _open issues: https://github.com/project-mindy/mindy/issues
.. _open easy issues: https://github.com/project-mindy/mindy/issues?q=is%3Aopen+is%3Aissue+label%3AE-easy
.. _Open Dylan: https://github.com/dylan-lang/opendylan
