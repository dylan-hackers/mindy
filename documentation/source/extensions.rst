Gwydion Extensions to Dylan
===========================

.. current-library:: dylan

*Copyright (c) 1994, 1995, 1996, 1997 Carnegie Mellon University All
rights reserved.*

Introduction
------------

In the process of working with Dylan, the Gwydion Project has come up
with numerous extensions to the Dylan language. Some of them form entire
libraries, like the Collection-Extensions and String-Extensions
libraries. Others have been added to the Dylan library, in such modules
as Extensions and System.

We continue to make no claims about future support for our extensions.
However, some extensions are more likely than others to make it into our
future compilers. This file documents those extensions which we think
will be included in our compiler's Dylan library. Extensions which go in
separate libraries are documented in their own files; extensions which
are part of the Mindy Dylan library but which have a less certain future
are documented in the Mindy documentation.

For the remainder of this document, we shall refer to "Gwydion
compilers" as a shorthand for "Mindy and other Dylan compilers that the
Gwydion Project may write." It is not meant as a guarantee that all
future Gwydion releases will support these extensions.

Specific Gwydion compilers may support extensions not listed here; see
their documentation for details.

Dylan Language Issues
---------------------

Whenever possible, we have tried to keep the Dylan module pristine and
unextended, preferring to add our extensions to separate modules or
libraries. However, this is not always possible, particularly when it
involves extending the behavior of a function or macro that is exported
from the Dylan module. Currently, Gwydion compilers support these
extensions to the Dylan module as described below:

- Gwydion compilers support *keyed-by* clauses in for statements. The
  format of such a clause is:

  .. code-block:: dylan

     var KEYED-BY key IN collection

  *Var* is bound to each element in *collection*, and *key* is bound to
  the element's key value.

- Gwydion compilers supports *using* clauses in for statements. The
  format of such a clause is:

  .. code-block:: dylan

     var IN collection USING protocol

  *protocol* will be used instead of :drm:`forward-iteration-protocol`.
  *protocol* must be a variable name, not an expression. Using clauses may
  be used together with ``keyed-by``:

  .. code-block:: dylan

     var KEYED-BY key IN collection USING protocol

- Gwydion compilers have an additional type of top level definition,
  define function, which creates a constant binding in the current
  module and initializes it to a new function. Define function's usage
  is similar to define method. The following is an example:

  .. code-block:: dylan

     define function cube (x)
       x * x * x;
     end function cube;

  A similar result might be had by writing

  .. code-block:: dylan

     define constant cube = method (x)
                             x * x * x;
                           end method;

  or

  .. code-block:: dylan

     define method cube (x)
       x * x * x;
     end method cube;

- Gwydion compilers supports subclass specializers via the ``limited``
  function. A subclass specializer causes a method to be invoked
  whenever the generic function was called on a value that is the
  specified class or any subclass of the specified class. The method is
  never invoked on a value that is an instance (direct or indirect) of
  the specified class, only when the value is a subclass of the
  specified class. The following is an example:

  .. code-block:: dylan

     define method make
         (result-class :: limited(<class>, subclass-of: <my-class>));
       let x = next-method();
       do-special-logging-or-something(x);
       x;
     end method;

Conditional Compilation
-----------------------

Gwydion compilers supports conditional compilation. The syntax is:

  .. code-block:: dylan

     #if (feature-expression)
       dylan-code
     #elseif (feature-expression)
       dylan-code
     #else
       dylan-code
      #endif

(The ``#elseif`` and ``#else`` clauses are optional) A *feature-expression*
is composed of features and the ``~``, ``&``, and ``\|`` operators, and
may be parenthesized as usual. Features are not case sensitive. Mindy
uses the ``-D`` and ``-U`` command line switches to define new features.
Some features currently supported include:

* ``mindy``
* ``compiled-for-x86``
* ``compiled-for-win32``
* ``compiled-for-linux``
* ``compiled-for-unix``
* ``newlines-are-CRLF``

Modules of the Dylan Library
----------------------------

In addition to containing the Dylan module, the Dylan library contains a
variety of modules which provide extensions. Gwydion compilers export
the following modules from the Dylan library:

* `The Extensions Module`_
* `The System Module`_
* `The Introspection Module`_
* `The Cheap-io Module`_

.. _the-extensions-module:

The Extensions Module
---------------------

.. current-module:: extensions

Ultimately, there will be several, more logically separate libraries
that extend Dylan or provide an application framework for users. For
now, we put any commonly used utilities in the Extensions module.

Generally Useful Definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Extensions module exports the following generally useful
functionality:

.. function:: assert

.. class:: <byte-vector>

   This class is a subclass of :drm:`<vector>` that can only hold integers
   between 0 and 255 inclusively. This class is a temporary addition to
   Mindy to support the requirement that the Streams library export a
   ``<byte-vector>`` definition. When Mindy supports limited collections,
   this may be defined within the Streams library.

.. class:: <byte-character>

   This class is a subclass of :drm:`<character>`. Characters of this type
   represent the ASCII character set (or extensions to ASCII). Note, in
   Mindy the :drm:`<character>` class is equivalent to unicode characters.

.. generic-function:: main

   :param invocation-name: An instance of :drm:`<byte-string>`.
   :param #rest arguments:

   :description:

     Has no methods, but is called by Mindy when it starts up. To make a
     standalone program, you define a method on main that does whatever you
     want it to do. *Invocation-name* is the first token on the command line
     that invoked Mindy. *Arguments* is a sequence of strings. There is a
     string in *arguments* for every argument on the command line that
     invoked Mindy, except all -f switches and the argument following each -f
     switch (that is, the file to load) is missing. Remember that any module
     that adds a method to main must use the Extensions module from the Dylan
     library.

.. function:: one-of

   :param #rest objects: A sequence of :drm:`<object>`.
   :value type: An instance of :drm:`<type>`.

   :description:

     This function takes any number of objects, and returns the type that
     is the type-union of the singletons of those objects. For example, the
     expression

     .. code-block:: dylan

        one-of(#"foo", #"bar", #"baz")

     is equivalent to

     .. code-block:: dylan

         type-union(singleton(#"foo"), singleton(#"bar"), singleton(#"baz"))

.. function:: false-or

   :param #rest objects: A sequence of :drm:`<object>`.
   :value type: An instance of :drm:`<type>`.

   :description:

     This function is useful in type expressions. It captures the common
     idiom of returning an instance of a particular type or the value #f. The
     expression

     .. code-block:: dylan

                false-or(<integer>)

     is equivalent to the expression

     .. code-block:: dylan

                type-union(<integer>, singleton(#f))

.. function:: load

   :param name: An instance of :drm:`<byte-string>`.

   :description:

     This function takes the name of a ``.dbc`` file and loads the code in the
     file into Mindy as if the file had been specified on the command line to
     Mindy. There is one exception: you can only load code that defines new
     variables or adds methods. You cannot redefine existing definitions.

.. function:: load-library

   :param name: An instance of :drm:`<symbol>`.

   :description:

     This function takes a library name and loads the code for the library
     into Mindy as if the library was used by another library.

.. _debugger-customizations:

Debugger Customizations
~~~~~~~~~~~~~~~~~~~~~~~

(A note on terminology: We use the term "debugger" here in the loose,
Dylan sense of anything that handles an uncaught error. In Mindy, this
debugger is indeed a full fledged debugger, but in other Gwydion
compilers it may not be)

The debugger uses the function :gf:`report-condition` to print conditions
as error messages to users; for example, this is the function that
implements the ``%S`` format-string directive for conditions. The debugger
also uses the :func:`format` function exported from the ``Cheap-io`` module to
process format strings, and it prints directly to the Unix ``stdout``. If
any library that is used itself uses the ``Debugger-format`` library, then
the debugger uses :func:`format` from the ``Format`` library, which is shipped
with Gwydion compilers. You can extend how the debugger prints
conditions, change what formatting function it uses, and direct where
debugger output goes with the following:

.. generic-function:: report-condition

   :param condition: An instance of :drm:`<condition>`.
   :param stream: An instance of :drm:`<stream>`.

   :description:

     This is the function Mindy uses to print condition variables
     as error messages to users. The internal :func:`format`
     function used by Mindy uses :gf:`report-condition` for
     condition arguments to the ``%S`` format directive.
     The ``Format`` library's :gf:`print-message` method for
     conditions calls :gf:`report-condition`.

     If you are writing a module that does no output but still
     provides :gf:`report-condition` methods, you should use
     :gf:`condition-format` to format output. Using
     :gf:`condition-format` makes your module more flexible for
     users of your module. If you call Mindy's internal
     :func:`format`, you'll be forced to write to only one destination,
     Mindy's ``stdout``, ignoring the *stream* argument. If you call
     the ``Format`` library's :func:`format` function, then your
     module will require the ``Format``, ``Print``, and ``Streams``
     libraries; therefore, users of your module may ultimately
     load these other libraries needlessly. Of course, if you want
     to make use of the extended functionality of the ``Format``
     library's format control strings, then you only have one
     choice anyway, and there's no reason to use :gf:`condition-format`.

.. generic-function:: condition-format

   :param stream: An instance of :drm:`<object>`.
   :param control-string: An instance of :drm:`<string>`.
   :param #rest arguments: Additional arguments.

   :description:

     This function serves as a firewall between the condition system
     and the ``Streams`` and ``Format`` libraries. Methods on
     :gf:`report-condition` should use :gf:`condition-format` to do
     their formatting. Users will generally use :var:`*debug-output*``
     or :var:`*warning-output*` for the *stream* argument, but this
     is not required.

     Mindy supplies a method for when *stream* is ``#"Cheap-IO"``. The
     Gwydion ``Format`` library supplies a method for when *stream*
     is a subclass of :class:`<stream>`. If you are implementing your
     own streams or format libraries, you will need to define a method
     on :gf:`condition-format` for your type of stream.

.. generic-function:: condition-force-output

   :param stream: An instance of :drm:`<object>`.

   :description:

     :gf:`condition-force-output` forces any pending output from
     *stream*'s buffer to *stream*'s destination. This function
     is invoked by the debugger after a condition has been reported
     and before it pauses for user input. Unless you are writing
     a debugger, you do not need to call :gf:`condition-force-output`
     yourself.

     Mindy supplies a method for when *stream* is ``#"Cheap-IO"``. The
     Gwydion ``Format`` library supplies a method for when *stream*
     is a subclass of :class:`<stream>`. If you are implementing your
     own streams or format libraries, you will need to define a method on
     :gf:`condition-force-output` for your type of stream.

.. variable:: *debug-output*
.. variable:: *warning-output*

   The debugger uses the value of :var:`*debug-output*` when performing
   output. :drm:`default-handler` for :drm:`<warning>` uses
   :var:`*warning-output*` to print warning messages. Both variables
   must be either a :class:`<stream>` from the ``Streams`` library,
   or ``#"Cheap-IO"`` (the default). When these variables are
   ``#"Cheap-IO"``, the output goes to ``stderr``.

   Using the ``Debugger-format`` module in the ``Format`` library
   will set both variables to :var:`*standard-output*` (see
   ``Streams`` Library).

Tables
~~~~~~

The Extensions module exports the following **<table>** subclasses:

.. class:: <equal-table>

   :description:

     This class is a subclass of :drm:`<table>` that uses the :drm:`\=`
     function to compare keys and the :gf:`equal-hash` function to
     generate hash codes. If you define your own classes and ``\=``
     methods specialized on those classes, then you should define
     a method for the :gf:`equal-hash` function specialized to your
     classes (see :gf:`equal-hash`).

.. class:: <value-table>
   :abstract:

   :description:

     This class is a subclass of :drm:`<table>``. Users can define
     subclasses of this class and provide a method for
     :gf:`tableprotocol` that is specialized to their new subclass.
     Any subclass of :class:`<value-table>` must use a hash function
     that never uses an object's identity (that is, its location in
     the heap) as a means of computing a hash ID. These tables are
     specifically designed to save overhead in testing hash states
     and whether the table needs to be rehashed after garbage collections.
     The second value of the hash function should always be
     :drm:`$permanent-hash-state`. For example:

     .. code-block:: dylan

        define class <my-table> (<value-table>)
        end class;

        define method table-protocol (table :: <my-table>)
          values(\=, string-hash);
        end method;

.. class:: <string-table>

   :description:

     This class is a subclass of :class:`<value-table>`. It is a table
     that has instances of :drm:`<string>`' for keys (compared with
     :drm:`\=`) and :drm:`<object>` for elements. It is an error to
     use a key that is not a :drm:`<string>`.

The Extensions module exports the following functions to make it easier
for users to use :class:`<equal-table>` and :class:`<value-table>`:

.. generic-function:: equal-hash

   :param key: An instance of :drm:`<object>`.
   :value hash-id: An instance of :drm:`<integer>`.
   :value hash-state: An instance of :drm:`<object>`.

   :description:

     This function returns a hash ID and hash state for use with
     :class:`<equal-table>`. If you define your own classes and
     :drm:`\=` methods specialized on those classes, then you
     should define a method for the :gf:`equal-hash` function
     specialized to your classes. Specialized methods exist for
     :drm:`<number>`, :drm:`<character>`, :drm:`<function>`,
     :drm:`<symbol>`, and :drm:`<collection>`. The method for
     :drm:`<object>` returns the integer ``42`` and
     :drm:`$permanent-hash-state`. This function may use an object's
     identity (that is, its location in the heap) to produce a hash ID.

.. function:: collection-hash

   :param collection: An instance of :drm:`<collection>`.
   :param key-hash-function: An instance of :drm:`<function>`.
   :param elt-hash-function: An instance of :drm:`<function>`.
   :value hash-id: An instance of :drm:`<integer>`.
   :value hash-state: An instance of :drm:`<object>`.

   :description:

     This function hashes every element of *collection* using
     *key-hash-function* on the keys and *element-hash-function*
     on the elements. Note, though two sequences may be equal
     according to the :drm:`\=` function, :func:`sequence-hash`
     and :gf:`collection-hash` may return different hash codes
     for the sequences.

.. function:: sequence-hash

   :param sequence: An instance of :drm:`<sequence>`.
   :param elt-hash-function: An instance of :drm:`<function>`.
   :value hash-id: An instance of :drm:`<integer>`.
   :value hash-state: An instance of :drm:`<object>`.

   :description:

     This function hashes every element of *sequence* using
     *elt-hash-function*, merging the resulting hash codes
     in order. Note, though two sequences may be equal
     according to the ``\=`` function, :func:`sequence-hash`
     and :func:`collection-hash` may return different hash
     codes for the sequences.

.. function:: string-hash

   :param string: An instance of :drm:`<string>`.
   :value hash-id: An instance of :drm:`<integer>`.
   :value hash-state: An instance of :drm:`<object>`.

   :description:

     This function calls produces hash codes for strings
     without using the strings' identities. This function
     is suitable for use with :class:`<value-table>`.

.. generic-function:: value-hash

   :param object: An instance of :drm:`<object>`.
   :value hash-id: An instance of :drm:`<integer>`.
   :value hash-state: An instance of :drm:`<object>`.

   :description:

     This function produces hash codes for objects without
     using the objects' identities. This function is suitable for
     use with :class:`<value-table>`. Mindy provides methods
     specialized for the following types: :drm:`<string>`,
     :drm:`<integer>`, :drm:`<float>`, :drm:`<character>`,
     :drm:`<symbol>`, ``singleton(#t)``, and ``singleton(#f)``.

Exiting Applications
~~~~~~~~~~~~~~~~~~~~

The Extensions module exports the following functionality for
controlling the exiting of applications:

.. function:: exit

   :param #key exit-code: An instance of :drm:`<integer>`. Default: ``0``.

   :description:

     Causes the process to exit. Mindy calls this function when there is no
     code left to execute. Mindy will exit with the return code *exit-code*.

.. function:: on-exit

   :param function: An instance of :drm:`<function>`.
   :value meaningless: An instance of ``singleton(#f)``.

   :description:

     Arranges for the :func:`exit` function to call the argument *function*.
     The argument *function* must take no required arguments. Users may call
     :func:`on-exit` multiple times to install more than one function for
     :func:`exit` to call, but the order in which :func:`exit` invokes the
     functions is undefined. Calling :func:`on-exit` on the same function
     repeatedly, installs that function multiple times.

Weak Pointers
~~~~~~~~~~~~~

The Extensions module exports the following weak-pointer functionality:

.. class:: <weak-pointer>

   :description:

     This class is a subclass of :drm:`<object>`. The :drm:`make`
     method for this class takes the keyword parameter ``object:``.
     Instances of :class:`<weak-pointer>` refer to the object
     passed to the :drm:`make` method as long as some other
     reference to the object exists. Whenever an instance
     of :class:`<weak-pointer>` is the only reference to an object, and
     a garbage collection occurs, then Mindy considers the object to
     be garbage. When Mindy garbage collects an object referred to
     by a weak pointer, then Mindy marks the weak pointer as being
     *broken* (see :func:`weak-pointer-object`).

.. function:: weak-pointer-object

   :param wp: An instance of :class:`<weak-pointer>`.
   :value object: An instance of :drm:`<object>`.
   :value broken?: An instance of :drm:`<boolean>`.

   :description:

     Returns the object referred to by the weak pointer and whether
     the weak pointer is *broken*. A weak pointer is broken when
     it contains the only reference to an object, and in this
     situation, :func:`weak-pointer-object` returns the values
     ``#f`` and ``#t``.

Collections
~~~~~~~~~~~

The Extensions module exports the following :drm:`<collection>`
functionality:

.. generic-function:: key-exists?

   :param collection: An instance of :drm:`<collection>`.
   :param key: An instance of :drm:`<object>`.
   :value win?: An instance of :drm:`<boolean>`.
   :value ele: An instance of :drm:`<object>`.

   :description:

     Return whether *key* is in *collection*. If the key is in the
     collection, then the second value is the element associated
     with *key*; otherwise, the second return value is ``#f``.

Integers
~~~~~~~~

Mindy has an abstract class :class:`<general-integer>` which
has two concrete classes, :drm:`<integer>` and :class:`<extended-integer>`.
Both concrete classes are direct subclasses of :class:`<general-integer>`.
Instances of :drm:`<integer>` have a limited range of values, and
:drm:`<integer>` arithmetic uses the computer's underlying integer
facilities.  Instances of :class:`<extended-integer>` can take on
any value, and are similar to Common Lisp "big-nums." Expressions
involving :class:`<extended-integer>` produce :class:`<extended-integer>`
results because :class:`<extended-integer>` are contagious. If an
expression involving only :drm:`<integer>` values would
produce a result that does not fit in an :drm:`<integer>`, then Mindy
signals an overflow error. You can use the :drm:`as` function to convert
back and forth between :drm:`<integer>` and :class:`<extended-integer>`.
:drm:`as` signals an error when converting an :class:`<extended-integer>`
to a :drm:`<integer>`, and the value does not fit in a :drm:`<integer>`.

Mindycomp parses all integer literals as :drm:`<integer>`. If a literal
does not fit in a :drm:`<integer>`, then mindycomp issues a compiler error.
Though the compiler supports no literal syntax for
:class:`<extended-integer>`, the Mindy debugger prints them in a
``#eDDD...`` format where each ``D`` is a decimal digit.

The Extension module exports the following integer functionality:

.. class:: <general-integer>
   :abstract:

   :description:

     :class:`<general-integer>` is a subclass of :drm:`<rational>`,
     and is the superclass of :drm:`<integer>` and
     :class:`<extended-integer>`.

.. class:: <extended-integer>

.. constant:: $maximum-integer

   :description:

     This constant holds the largest positive :drm:`<integer>`.

.. constant:: $minimum-integer

   :description:

     This constant holds the largest negative :drm:`<integer>`.

Ratios
~~~~~~

The Extensions module exports the following:

.. class:: <ratio>

   :description:

     This class is a subclass of :drm:`<rational>`. The ratio
     is normalized so that it has a positive denominator, and
     the greatest common divisor of the numerator and the
     denominator is one. Ratios are never automatically
     converted to integers. For example, ``ratio(4, 2)`` would
     return ``2/1``.

     A numeric operation involving two ratios produces a
     normalized ratio result. A numeric operation involving a
     ratio and an integer produced a normalized ratio result.
     A numeric operation involving a ratio and a float produces
     a float result.

.. function:: ratio

   :param numerator: An instance of :class:`<general-integer>`.
   :param denominator: An instance of :class:`<general-integer>`.
   :value ratio: An instance of :class:`<ratio>`.

   :description:

     This function makes a ratio from the two integers.

.. function:: numerator

   :param ratio: An instance of :class:`<ratio>`.
   :value the-numerator: An instance of :class:`<general-integer>`.

   :description:

     This function returns the numerator part of *ratio*.

.. function:: denominator

   :param ratio: An instance of :class:`<ratio>`.
   :value the-numerator: An instance of :class:`<general-integer>`.

   :description:

     This function returns the denominator part of *ratio*.

The System Module
-----------------

.. current-module:: system

The System module exports the following:

.. class:: <buffer>

.. function:: copy-bytes

.. function:: getenv

.. function:: get-time-of-day

.. function:: system

The Introspection Module
------------------------

.. current-module:: introspection

The Introspection module exports reflective operations for examining
classes, functions, and types.

Functions
~~~~~~~~~

Dylan provides some reflective operations for functions, such as
:drm:`function-specializers` and :drm:`instance?`. With the latter, you can
determine if a function is a :drm:`<generic-function>` or :drm:`<method>`, but
neither Dylan nor Gwydion compilers provide exported class identifiers
for other types of functions (such as ``block`` exit functions). The
Subsection `Types`_ describes definitions that are also useful when
inspecting methods because you can get detailed information about method
specializer types.

The Introspection module exports the following functions:

.. function:: function-name

Classes
~~~~~~~

The Introspection module exports the following for class objects:

.. function:: class-name

Types
~~~~~

The Introspection module exports the following for inspecting types (and
therefore, method specializers):

.. function:: singleton-object

.. class:: <subclass>

.. function:: subclass-of

.. class:: <limited-integer>

.. function:: limited-integer-base-class

.. function:: limited-integer-minimum

.. function:: limited-integer-maximum

.. class:: <union>

.. function:: union-members

Miscellaneous
~~~~~~~~~~~~~

The Introspection module exports the following miscellaneous
functionality:

.. function:: object-address

The Cheap-io Module
-------------------

.. current-module:: cheap-io

The ``Cheap-io`` module exports some basic, unextendable I/O
functionality.  Gwydion compilers use the ``Cheap-io`` functions
internally. The Gwydion Project also provides the ``Streams``,
``Print``, and ``Format`` libraries (see the $INSTALL/doc/libraries/
directory for documentation). It is an error to use both ``Cheap-IO``
functions and ``Streams`` / ``Print`` / ``Format`` functions on a
single stream. (For example, if you are using the ``Streams`` library
``*standard-input*``, do not also use the ``Cheap-io`` input functions)
For this reason, if any library that you load into Mindy uses the
``Debugger-format`` library, the debugger will use :func:`format` from the
``Format`` library.

.. function:: format

.. function:: print

Extensions to the Streams Library
---------------------------------

.. current-module:: piped-exec

The ``Piped-exec`` module is a Gwydion extension to the ``Streams``
library. It exports the following function:

.. function:: piped-exec

Copyright and Terms of Use
--------------------------

Copyright (c) 1994, 1995, 1996, 1997 Carnegie Mellon University All
rights reserved.

Use and copying of this software and preparation of derivative works
based on this software are permitted, including commercial use, provided
that the following conditions are observed:

- This copyright notice must be retained in full on any copies and on
  appropriate parts of any derivative works.
- Documentation (paper or online) accompanying any system that
  incorporates this software, or any part of it, must acknowledge the
  contribution of the Gwydion Project at Carnegie Mellon University.

This software is made available *as is*. Neither the authors nor
Carnegie Mellon University make any warranty about the software, its
performance, or its conformity to any specification.

Bug reports, questions, comments, and suggestions should be sent by
E-mail to the Internet address gwydion-bugs@cs.cmu.edu.
