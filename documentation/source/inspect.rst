The Mindy Inspector
===================

Introduction
------------

This document describes the use of the Inspector libraries for Mindy.
The Inspector libraries are used to supplement the current Mindy
debugger, and provide programmers with a tool for interactively
inspecting their data structures. The Inspector has two separate
interfaces, a text-based interface and a graphical interface. The text
interface is similar to that of Mindy debugger. The graphical interface
offers a pleasant if uninspiring way of inspecting objects; it also
offers a rudimentary class diagram viewer.

The Inspector is divided into three libraries, Inspector-Base,
Text-Inspector, and X-Inspector. The first of these provides facilities
that are common to the other two libraries. The Text-Inspector library
implements a text interface to the Inspector and the function
:func:`inspect`. The X-Inspector library implements a graphical interface to
the Inspector and the function :func:`xinspect`. Use of the X-Inspector
requires Dylan/TK, which may not available on all systems.

The Inspector is most often invoked via the ``inspect`` and ``xinspect``
debugger commands. These debugger commands evaluate their arguments and
invoke the function :func:`inspect` or :func:`xinspect` with the appropriate
arguments. The debugger commands will automatically load the appropriate
libraries if they have not already been loaded.

Inspector-Base
--------------

The Inspector-base library exists solely to support the Text-Inspector
and X-Inspector libraries. Normal users will not find it useful.

Text-Inspector
--------------

.. current-library:: text-inspector
.. current-module:: text-inspector

Exported Names
~~~~~~~~~~~~~~

.. function:: inspect

.. constant:: $all-libraries

.. function:: display-object-info

Text-Inspector Commands
~~~~~~~~~~~~~~~~~~~~~~~

When the text inspector starts, it will display the object you have
passed to it, along with all of the information you requested using the
keyword flags. Each item will be numbered, and to inspect a sub-object
you simply type the number corresponding to the desired choice (in this
case, even a superclass is considered a sub-object, because it was
inspected via the main object). This lets you move around the object
heterarchy, but it is not very convenient. There are additional commands
that are designed to allow easier use of the inspector:

* ``1``, ``2``, ``3``, ...
* ``history``
* ``up``
* ``print``
* ``store``
* ``view``
* ``?`` or ``help``
* ``quit`` or ``exit``

With the exception of history, all commands may be abbreviated by their
first letter.

The History Command
~~~~~~~~~~~~~~~~~~~

The history command lists all of the previously visited objects in the
current session. A sample output may look like this::

    Instance of <my-class>              <--- Initial object, the one given in the argument
    Instance of <sequence>
    Instance of <fixed-integer>
    Class <fixed-integer>
    Class <integer>              <--- Last object seen, the one that "up" goes to
    Class <rational>              <--- The current object

The Up Command
~~~~~~~~~~~~~~

The up command moves you "up" the history, and lets you see the
previously visited object. In the above example, if you moved up twice,
the history would look like this::

    Instance of <my-class>
    Instance of <sequence>
    Instance of <fixed-integer>
    Class <fixed-integer>

And you would be inspecting the class ``<fixed-integer>``.

The Print Command
~~~~~~~~~~~~~~~~~

The print command pretty-prints the current object (using the Gwydion
Print library), but does not number the sub-objects. It is useful only
for displaying the objects in a different, possible more informative
manner.

The Store Command
~~~~~~~~~~~~~~~~~

The store command stores the current object in the next unused debugger
variable. See mindy/debug.txt for information about debugger variables.

The View Command
~~~~~~~~~~~~~~~~

The view command will allow you to redisplay the object that you are
currently inspecting. This is mainly useful if previous commands such as
history or view have moved the object off of the screen.

The Help Command
~~~~~~~~~~~~~~~~

The help command will display a short help page listing all of the
commands and a brief one line description of how they work.

The Quit and Exit Command
~~~~~~~~~~~~~~~~~~~~~~~~~

This command (both words mean the same thing) will allow you to leave
the inspector and continue with the execution of your program. The next
time inspect is called, it will not have any memory of the past session.
This means that the history will initially show only the object that you
called inspector with. If **inspect** was invoked from the debugger, you
will return to the debugger.

X-Inspector
-----------

.. current-library:: x-inspector
.. current-module:: x-inspector

The X-Inspector's graphical interface is pretty self explanatory. In
addition to offering all the features of the text interface, it also
offers a rudimentary class diagram viewer. (The class diagrams are
non-interactive--you can click all you want on them, but they won't do
anything) The Close button will close a single window; the Quit button
will close all the X-Inspector windows. When there are no longer any
open X-Inspector windows, you will be returned to the Mindy debugger
prompt.

The name X-Inspector is something of a misnomer, since it does not
require X-Windows to work.

The X-Inspector library exports the following objects:

.. function:: xinspect

.. constant:: $all-libraries

*Caveat*: Both Inspectors differentiate between a binding (like a
variable or a constant) and the object bound to that binding. If the
binding is read-only, inspecting the binding is almost identical to
inspecting the object it refers to. It is not exactly the same, because
if the binding refers to a class, the X-Inspector will not offer you the
option of a class hierarchy diagram.
