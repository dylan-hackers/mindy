The Mindy Compiler and Interpreter
==================================

.. current-library:: dylan

Copyright (c) 1994, 1995, 1996 Carnegie Mellon University All rights
reserved. Refer to the end of this document for precise terms of use.

The Gwydion Project would like to thank those on the net that have
contributed code patches and bug reports for Mindy:

Adam Alpern, Steve Strassman, Scott Collins, Ed Gamble, Bruno Haible,
John Shen, Galen Hunt, Richard Lynch, Dan Ratner, Court Demas, Miles
Bader, Kelly Murray, Nick Thompson, Brent Benson, Brian Rogoff, Alain
Rogister, Mark Chu-Carroll, Enrico Colombini, Dave Dyer, Jonathan
Bachrach, Michael Binz, Jonathan Sobel, Eric Kidd, John Shen, Carl Gay,
Patrick Premont, Eric Gouriou

Special thanks for major efforts to Roger Critchlow, Patrick Beard, Gary
Palter, and Jim Studt for enhancements to Mindy.

Introduction
------------

Mindy is an implementation of a language that is very much like the
language described in the *Dylan Reference Manual* (DRM). The name
*Mindy* is derived from "Mindy Is Not Dylan Yet", and as the name
implies, Mindy is incomplete. Mindy is incomplete for the following
reasons:

- We do not implement everything in the DRM.
- The DRM does not specify all that Apple intends Dylan to be.
- There's no way to validate what a Dylan implementation is, even if we
  had a full specification.

However, Mindy does implement most of what we believe Dylan will be.

Mindy was developed by the Gwydion Project at Carnegie Mellon University
for our own internal use as a development tool while we work on our
*real* high-performance Dylan implementation. We have decided to make
Mindy available for other people who want to learn about Dylan. However,
the amount of effort that we can put into maintaining Mindy is strictly
limited.

Mindy will never be an industrial-strength implementation, and nobody
should depend on it for real work. We will make future releases from
time to time as we add new features and fix bugs, but this is strictly a
sideshow for us. We would appreciate receiving bug reports (especially
those accompanied by code patches) and suggestions for improvements, but
we may not fix every bug reported in a timely manner, or fix it at all.
Our work on development of the *real* Gwydion/Dylan must take
precedence.

We hope that nobody will draw any conclusions about the performance of
our future Gwydion/Dylan compiler or the performance attainable in Dylan
from experience using Mindy. It was not designed to be fast.

Mindy comprises two C programs, a compiler that produces byte-codes and
a byte-code interpreter. Instructions for compiling and installing Mindy
can be found in the file INSTALL at the top level of the Mindy release.
Instructions for obtaining the different versions of Mindy can be found
in the file README at the top level of the Mindy release.

Hello, World
------------

Well, the first program anyone should endeavor to write in a new
language is, of course, Hello World. Type this into a file
called ``hw-exports.dylan``:

.. code-block:: dylan

  Module: dylan-user

  define library hello-world
    use Dylan;
  end library;

  define module hello-world
    use dylan;
    use cheap-io;
    use extensions;
  end module;

And put this in ``hw.dylan``:

.. code-block:: dylan

  module: Hello-World

  // This is the canonical "hello, world" demo.
  define method main (argv0 :: <byte-string>, #rest noise)
    puts("Hello, World.\n");
  end;

To compile your program invoke $INSTALL/bin/mindycomp, for example::

    % $INSTALL/bin/mindycomp -lhello-world hw-exports.dylan
    % $INSTALL/bin/mindycomp -lhello-world hw.dylan

This produces files named ``hw.dbc`` and ``hw-exports.dbc``. The ``.dbc``
stands for **"Dylan Byte Code"**. To run the program, say::

    % $INSTALL/bin/mindy -f hw-exports.dbc -f hw.dbc

It should print "Hello, World." to standard output and then exit.

Note that even the minimal Dylan program has at least two files.
This is because in order to do anything much at all, you must
access additional libraries, and the only way to set up your
namespace is to define your own library and module. But the only
way to place code in a module is to use the ``Module:`` file
header, so two files are required.

Note also that a blank line is required after the ``Module:`` file header.

The Main Routine
----------------

After loading your program, Mindy invokes the generic function :gf:`main`
from the Extensions module of the Dylan library. Your program must
define a method for :gf:`main`, or Mindy will signal a **no applicable
methods** error and put you in the debugger. For more information on the
:gf:`main` function, see :ref:`The Extensions Module <the-extensions-module>`.

It can be useful to load code into Mindy with no main method. Once you
land in the debugger, you can call any function manually. This provides
a way to test any library.

Multiple Files
--------------

When working with a larger program, you will probably have more than one
``.dylan`` file. In which case, you just compile them each independently,
and then run Mindy with multiple ``-f`` switches::

    % mindy -f foo.dbc -f bar.dbc -f baz.dbc

Mindy loads the files specified with the ``-f`` switches in the order you
specify the files on the command line. This becomes important when you
define your own modules (see `Using Libraries and Modules`_).

If you typically load several ``.dbc`` files as part of a single program,
you can combine them into one file for convenience. The mechanism for
combining ``.dbc`` files is the Unix ``cat`` utility::

    % cat foo.dbc bar.dbc baz.dbc > big.dbc
    % mindy -f big.dbc

Return Codes
------------

If Mindy encounters an unrecoverable error, or if Mindy is exited via
the debugger quit command, Mindy exits with a return code of 1. If the
Dylan program ends with a call to :func:`exit` and a return code is
specified, Mindy exits with that return code.  Otherwise, the return
code is ``0``.

Command Line Switches and Environment Variables
-----------------------------------------------

Mindycomp recognizes the following command line switches:

- -D\ *feature* : This tells the conditional compilation system that
  *feature* is present (ie, "define"). Notice that there is no space
  between -D and *feature*.
- -U\ *feature* : This tells the conditional compilation system that
  *feature* is not present (ie, "undefine"). Notice that there is no
  space between -U and *feature*.
- -fcolor-diagnostics : This tells mindycomp to always enable color
  diagnostics.

Mindy recognizes the following command line switches:

- -f *filename* : This tells Mindy to load file *filename*. See
  `Multiple Files`_.
- -x *filename* : This tells Mindy to load file *filename* like -f
  does, but the rest of the command line is then left uninterpretted by
  Mindy. This can be useful if your Dylan program also has a -f option.

Mindy recognizes the following environment variables:

- BYTES\_CONSED\_BETWEEN\_GCS : This is the number of bytes Mindy will
  allocate before invoking a garbage collection. For maximum speed,
  this should be set to the largest value possible that won't cause
  thrashing. See also **collect-garbage** and **\*print-GC-messages\***
  in `The System Module`_.
- MINDYPATH : This controls where Mindy searches for Dylan libraries.
  See `Using Libraries and Modules`_.

Errors and Warnings
-------------------

7.1. Syntax Errors
~~~~~~~~~~~~~~~~~~

If there are any syntax errors in your program, mindycomp will report
them to stderr while compiling. For example, if you had left off the
closing parenthesis in the call to **puts** in the above example,
mindycomp would have reported::

    hw.dylan:4: parse error at or before `;'

Because the line introduction, hw.dylan:4:, has the same format that the
C compiler uses, gnu-emacs's compile package can parse the error
messages from mindycomp.

Mindycomp's error recovery is not the best in the world. Often, it has
to completely punt, telling you only about the first few errors it
found. You have to fix what it reports and try again.

A hint to getting slightly tighter error recovery is to end all method
and class definitions with "end method;" or "end class;". For example,
if you forget an end token for a statement inside a method definition,
the mindycomp parser goes all the way to the end of the file and then
reports a syntax error at the EOF position. You do not get any more
clues. If you use "end method;", then the parser can recover at the end
of the method containing the bad syntax and reports the syntax error
there. This gives you a lot tighter recovery and more information in
this situation.

7.2. Runtime Errors
~~~~~~~~~~~~~~~~~~~

Much more common than syntax errors are runtime errors. And given the
simplistic model of compilation mindycomp uses, most semantic errors are
not detected until runtime. When Mindy hits a runtime error that is not
handled via the condition system, it drops you into a debugger. From
this debugger you can look at variables, examine the stack, and invoke
functions. For example, if you had assumed that **puts** would be named
something more reasonable, like **put-string**, you would have gotten
the following when you tried to run your *Hello World* program::

    % mindy -f hw.dbc

    Warning: the following variables are undefined:
      in library Dylan-user:
        in module Dylan-user:
          put-string[hw.dylan, line 9]

    thread [0] D   main
    fp 0x1003009c: invoke-debugger({<simple-error> 0x101a24c9})
    mindy>

Typing help at the ``mindy>`` prompt will list the various commands you can
use. See the document debug.ps for more information.

7.3. Warnings
~~~~~~~~~~~~~

Mindycomp issues warnings at compile time when:

- While or until is used inside a for loop instead of while: or until:
- The obsolete "keyword: (default)" syntax is used instead of the
  "keyword = default" syntax.
- Function return values have names but not types
- Next-method is referenced inside a method that does not declare #next
  in the method header

Mindy issues warnings at runtime when:

- A variable is undefined (this becomes an error if the code
  referencing the undefined variable is executed)
- A method's return types do not match the generic function's return
  types because return types were not specified for the method.

7.4. Internal Lossage
~~~~~~~~~~~~~~~~~~~~~

Sometimes mindycomp or Mindy will get an internal error. When this
happens, it will print a message to stderr and then abort. This results
in the process dying due to some kind of signal. On the pmax, this
signal is SIGILL, or Illegal Instruction. When this happens, send
*gwydion-bugs@cs.cmu.edu* a piece of mail containing the error message
and information on what it was you did that triggered the problem.

Dylan vs. Mindy Language Issues
-------------------------------

The Dylan language is still changing slightly. Mindy implements most of
the *Dylan Reference Manual*, as well as some features we would like to
see in Dylan. Currently, the Mindy diverges from the DRM as described
below:

Additions:

Mindy supports multiple value binding in the =/then clauses of for
statements. The format of such a clause is

::

              (var1, var2, ...) = expr1 THEN expr2

Mindy supports *keyed-by* clauses in for statements. The format of
such a clause is

::

              var KEYED-BY key IN collection

Var is bound to each element in collection, and key is bound to the
element's key value.

Mindy supports *using* clauses in for statements. The format of such a
clause is

::

              var IN collection USING protocol

protocol will be used instead of forward-iteration-protocol. protocol
must be a variable name, not an expression. Using clauses may be used
together with keyed-by:

::

              var KEYED-BY key IN collection USING protocol

Keyword parameters may have type information, and you can specify
default values with either DRM syntax (which uses "= ...") or with the
obsolete syntax (which uses "(...)"). Using the second syntax will
generate a compiler warning. Mindy does not enforce any congruence rules
for keyword parameter types, so effectively, keyword type information in
generic function declarations serves as documentation only.

Mindy has an additional type of top level definition, define function,
which creates a constant binding in the current module and initializes
it to a new function. Define function's usage is similar to define
method. The following is an example:

::

              define function cube (x)
                x * x * x;
              end function cube;

A similar result might be had by writing

::

              define constant cube = method (x)
                                      x * x * x;
                                    end method;

or

::

              define method cube (x)
                x * x * x;
              end method cube;

Mindy supports conditional compilation. The syntax is

::

              #if (feature-expression)
              dylan-code;
              #else
              more-dylan-code;
              #endif

(The #else clause is optional) A *feature-expression* is composed of
features and the ~, &, and \operators, and may be parenthesiszed as
usual. Features are not case sensitive. See `Command Line
Switches and Environment Variables`_ for
information on defining and undefining features.

Mindy allows you to place library and module definitions in the same
file as the actual code. See `Using Libraries and Modules`_ for details.

Mindy supports subclass specializers via the **limited** function. A
subclass specializer causes a method to be invoked whenever the generic
function was called on a value that is the specified class or any
subclass of the specified class. The method is never invoked on a value
that is an instance (direct or indirect) of the specified class, only
when the value is a subclass of the specified class. The following is an
example:

::

              define method make
                  (result-class :: limited(<class>, subclass-of: <my-class>));
                let x = next-method();
                do-special-logging-or-something(x);
                x;
              end method;

Deficiencies:

Mindy does not implement macros. The DRM built-in macros (such as if
and method-definer) can not be manipulated via the module system.

Mindy does not have limited collections. (It does have limited
integers, though)

Sealed/open and primary keywords are parsed where allowed, but Mindy
ignores this information about your program.

Mindy parses the seal generic and define sealed domain forms, but does
not enforce them.

Define method does not automatically insert #next next-method in
parameter lists. You have to explicitly add it yourself.

Many of the DRM built-in macros allow the bodies to be empty. Mindy
does not. For example, in Mindy the following is not legal:

::

              if (foo)
              end if;

- Make(<class>, ...) is unsupported.

Built-in Libraries and Modules
------------------------------

Mindy has full support for modules and libraries. Mindy provides two
built-in libraries, Dylan and Dylan-user. The Dylan library contains the
Dylan language implementation and the following exported modules:

Dylan
  This module contains the Dylan language implementation and exports all
  the built-in Dylan definitions.

Extensions
  This module exports useful extensions to the Dylan language (see
  :ref:`The Extensions Module <the-extensions-module>`).
  Ultimately, there will be several, more logically separate libraries
  that extend Dylan or provide an
  application framework for users. For now, we put any commonly used
  utilities in the Extensions
  module.

System
  This module exports an interface to operating system calls and
  special, low-level functionality (see `The System Module`_).

Introspection
  This module exports reflective operations for examining classes,
  functions, and so on.

File-descriptors
  This module exports an interface to most standard C system calls that
  operate on file descriptors.

Cheap-io
  This module exports some basic, unextendable input and output
  functionality.

Threads
  This module exports an interface to threads, locks, and objects that
  behave similarly to cthreads.h
  condition variables.

Transcendental
  This module exports some transcendental functions and the constants
  **$pi** and **$e**.

Debugger-format
  See :ref:`Debugger Customizations <debugger-customizations>`.

The ``Dylan-user`` library is the default library in which ``mindycomp``
compiles user code. Mindy provides this library for user convenience
when whipping up play code or small applications for which the
programmer does not want to bother to create a library. You cannot
redefine the ``Dylan-user`` library. This library contains one module,
``Dylan-user``, and you cannot redefine this module.

The Dylan language requires every library to contain a ``Dylan-user``
module, and this module must use the ``Dylan`` module from the ``Dylan`` library
regardless of any user specifications. This module provides a starting
point in every library where users can begin to define modules; without
an initial module in the library, you would be unable to write any code,
including module definitions. Each ``Dylan-user`` module in Mindy also
automatically uses the modules from the ``Dylan`` library described above.
You cannot redefine the ``Dylan-user`` module, so if your code requires
module other than those described above, then you must define your own
library and module.

Mindy comes bundled with several other libraries. Documentation for
these libraries can be found in $INSTALL/doc/libraries.

Using Libraries and Modules
---------------------------

To compile code into a particular library use the ``-l`` switch to
``mindycomp``::

        % mindycomp -lmy-lib foo.dylan

If there is no -l switch, then mindycomp compiles the code into the
Dylan-user library. When loading a .dbc file into Mindy that was
compiled into a particular library, one of the following conditions must
be satisfied to avoid errors:

- The library must be the Dylan-user library. Technically, you could
  put code in the Dylan library, but do not do this.
- You must have defined the library in a file previously loaded (see
  `Multiple Files`_ for information on
  loading multiple files).
- The first piece of code in the source file that produced the .dbc
  file must be the library definition.

While loading a file, if Mindy processes a library definition that uses
an undefined library, then Mindy stops loading the current file,
searches for the undefined library, and loads it. After loading the
undefined library, Mindy continues loading the current file and
processing the original library definition. Mindy searches for the
undefined library in the directories listed in the MINDYPATH environment
variable. If MINDYPATH is undefined, then Mindy uses the pathname
$INSTALL/lib. In each directory, Mindy first looks for the file
<library>-lib.dbc, where <library> is the name of the undefined library,
and if this file does not exist, then Mindy looks for <library>.dbc.

Mindy loads the Dylan library when it first sees a reference to it. A
reference to the Dylan library occurs when loading a file compiled to be
in the Dylan library, or when loading a file with a library definition
that uses the Dylan library. Mindy loads the Dylan library by looking
for the file dylan.dbc on MINDYPATH.

To make a single compiled file for a library which has multiple source
files, compile all the files that constitute the library with the -l
switch set to the library's name. Then cat all the resulting .dbc files
together (see `Multiple Files`_),
making sure the file that defines the library is first. Then install the
combined .dbc file in one of the directories in your MINDYPATH.

To compile code into a particular module, use the module: file header.
Whenever a source file lacks a module: file header, mindycomp issues a
compiler warning and compiles the code into the Dylan-user module. This
is the Dylan-user module of the library specified with the -l switch,
and if there was no -l switch, it is the Dylan-user module of the
Dylan-user library. If a file contains no file headers, it still must
contain a leading blank line to conform to Dylan syntax.

When loading a .dbc file into Mindy that was compiled into a particular
module, one of the following conditions must be satisfied to avoid
errors:

- The module must be the Dylan-user module.
- You must have defined the module in a file previously loaded (see
  `Multiple Files`_ for information on
  loading multiple files).
- The first code in the source file that produced the .dbc file must be
  library and module definitions, and one of the module definitions
  must be the module in question.

The System Module
-----------------

The System module exports the following:

**<buffer>** [Class]

This class is a subclass of :drm:`<vector>`. It is the built-in class in
Mindy that the Streams module supports.

**copy-bytes** [Function]

Arguments

dst :: type\_or(<buffer>, <byte-vector>, <byte-string>)

dst-offset :: <integer>

src :: type\_or(<buffer>, <byte-vector>, <byte-string>)

src-offset :: <integer>

count :: <integer>)

Values

dst :: type\_or(<buffer, <byte-vector>, <byte-string>)

Description

Copies *count* bytes from *src* to *dst*, starting at *src-offset* and
*dst-offset*, respectively. This function returns *dst*. This function
does no bounds checking. *Dst* and *src* may be the same (**\\==**)
object; this function ensures that it copies bytes from to the
destination portion correctly, regardless of overlap.

**\*print-GC-messages\***\ [Variable]

Default value

#f

Description

This variable controls whether Mindy prints garbage collection
information whenever the garbage collector runs.

**collect-garbage** [Function]

Arguments

#key purify :: <boolean> = #f

Values

meaningless :: singleton(#f)

Description

If *purify* is true, collect-garbage does a purifying garbage
collection. Otherwise, it does a normal garbage collection. A purifying
collection is just like a normal collection except that everything left
over after the collection is rendered permanent and is never again
considered for collection. This can be quite useful in reducing the
memory demands of your programs, as the total heap you need is:

overhead + permanent + newspace + oldspace

with overhead being about five megabytes. Without purify, permanent
will be 0, newspace is the amount of live data, and oldspace is the
amount of live data plus BYTES\_CONSED\_BETWEEN\_GCS. Note that the live
data is being counted twice: once in newspace and once in oldspace. But
if you purify, that live data gets moved over to permanent, and then
only gets counted once.

**getcwd** [Function]

Arguments

none

Values

current-directory :: <string>

Description

Returns the current working directory.

**getenv** [Function]

Arguments

environment-variable-name :: <string>

Values

environment-variable-value :: false-or(<string>)

Description

Returns the value of the environment variable
*environment-variable-name*. If *environment-variable-name* is
undefined, **getenv** returns #f.

**get-time-of-day**\ [Function]

Arguments

none

Values

time-in-seconds :: <general-integer>

Description

Returns the number of seconds since midnight, January 1, 1970.

**system** [Function]

Arguments

command-line :: <string>

Values

return-code :: <integer>

Description

System causes *command-line* to be given to your shell as input as if
the string had been typed as a command. If environment variable SHELL is
found, its value will be used as the command interpreter (shell);
otherwise sh(1) is used.

Mindy will wait until the command terminates. Upon termination of the
sub-process, system will return a negative value if the command couldn't
be executed, or the command's return code if it was executed.

The Introspection Module
------------------------

The Introspection module exports reflective operations for examining
classes, functions, and types.

Functions
~~~~~~~~~

Dylan provides some reflective operations for functions, such as
**function-specializers** and **instance?**. With the latter, you can
determine if a function is a **<generic-function>** or **<method>**, but
neither Dylan nor Mindy provides exports class identifiers for other
types of functions (such as **block** exit functions). The Subsection
*Types* describes definitions that are also useful when inspecting
methods because you can get detailed information about method
specializer types.

The Introspection module exports the following functions:

**function-name** [Function]

Arguments

function :: <function>

Values

result :: false-or(<symbol>)

Description

Returns the name of *function* as a **<symbol>** if *function* has a
name; otherwise **function-name** returns #f. All functions defined with
define generic or define method have names, and some other functions
have names.

Classes and Instances
~~~~~~~~~~~~~~~~~~~~~

The Introspection module exports the following for class objects, slot
descriptions, and fetching and modifying the slot values of general
objects:

**abstract?**\ [Function]

Arguments

class :: <class>

Values

result :: <boolean>

Description

Returns #t if *class* is an abstract class; otherwise
returns #f.

**class-name** [Function]

Arguments

class :: <class>

Values

result :: false-or(<symbol>)

Description

Returns the name of *class* as a **<symbol>** if class has a name;
otherwise, this function returns #f. Mindy can always determine the name
of classes defined with define class.

**<slot-descriptor>** [Class]

This class is a subclass of **<object>**. The **slot-descriptors**
function returns instances of this class to describe the slots of a
class object.

**slot-descriptors** [Function]

Arguments

class :: <class>

Values

descriptors :: <list>

Description

Returns a list of **<slot-descriptor>**\ s for *class*. The result may
be the empty list.

**slot-name** [Function]

Arguments

slot :: <slot-descriptor>

Values

name :: <symbol>

Description

Returns the name of *slot* as a **<symbol>**.

**slot-allocation** [Function]

Arguments

slot :: <slot-descriptor>

Values

allocation :: one-of(#"instance", #"class", #"each-subclass",
#"virtual")

Description

Returns the allocation type for *slot* as a **<symbol>**.

**slot-type** [Function]

Arguments

slot :: <slot-descriptor>

Values

type :: <type>

Description

Returns the type of values permitted for *slot*.

**slot-getter** [Function]

Arguments

slot :: <slot-descriptor>

Values

gf :: <generic-function>

Description

Returns the generic function that accesses *slot*.

**slot-setter** [Function]

Arguments

slot :: <slot-descriptor>

Values

gf :: <generic-function>

Description

Returns the generic function that stores into *slot*.

**slot-value** [Function]

Arguments

slot :: <slot-descriptor>

object :: <object>

Values

value :: <object>

initialized? :: <boolean>

Description

Returns the value for *slot* in *object* and #t. If the slot in the
object is uninitialized, then this function returns #f and #f. Note,
this function does not go through generic function dispatch, and it
calls no user methods; this function uses an internal primitive to fetch
the slot's value.

**slot-value-setter** [Function]

Arguments

value :: <object>

slot :: <slot-descriptor>

object :: <object>

Values

value :: <object>

Description

Stores *value* into *slot* of *object* and returns *value*. This
function performs whatever type checking is necessary to ensure *value*
is safe for *slot*.

**init-keyword** [Function]

Arguments

slot :: <slot-descriptor>

Values

keyword :: false-or(<symbol>)

Description

Returns the init-keyword associated with *slot*, or #f if there is
none.

**keyword-required?** [Function]

Arguments

slot :: <slot-descriptor>

Values

answer :: <boolean>

Description

Returns #t if there is a required-init-keyword for *slot*, otherwise
returns #f.

Types
~~~~~

The Introspection module exports the following for inspecting types (and
therefore, method specializers):

**singleton-object** [Function]

Arguments

specializer :: <singleton>

Values

object :: <object>

Description

This function returns the object of the singleton value type.

**<subclass>** [Class]

This class is a subclass of :drm:`<type>`. Instances of this class
represent subclass specializers. A subclass specializer causes a method
to be invoked whenever the generic function was called on a value that
is the specified class or any subclass of the specified class (see
`Dylan vs. Mindy Language Issues`_ for
more information). The function **subclass-of** returns the class
specified for the subclass specializer.

**subclass-of** [Function]

Arguments

specializer :: <subclass>

Values

class :: <class>

Description

Returns the class specified for the subclass specializer.

**<limited-integer>** [Class]

This class is a subclass of :drm:`<type>`. Instances of this class
represent limited integer types. See the functions
**limited-integer-base-class**, **limited-integer-minimum**, and
**limited-integer-maximum**.

**limited-integer-base-class** [Function]

Arguments

specializer :: <limited-integer>

Values

class :: one-of(<integer>, <extended-integer>)

Description

Returns the class specified for the limited-integer specializer,
either :drm:`<integer>` or **<extended-integer>**.

**limited-integer-minimum** [Function]

**limited-integer-maximum** [Function]

Arguments

specializer :: <limited-integer>

Values

class :: false-or(<integer>)

Description

Return the inclusive bounds of the limited-integer specializer. If the
minimum or maximum is unbounded, then the appropriate function returns
#f.

**<union>** [Class]

This class is a subclass of :drm:`<type>`. Instances of this class
represent union types. The function **union-members** returns a list of
the member types in the union.

**union-members** [Function]

Arguments

specializer :: <union>

Values

types :: <list>

Description

Returns the member types of the union type. The result may contain
more than two elements. This function collapses nested union types to a
flat list.

Miscellaneous
~~~~~~~~~~~~~

The Introspection module exports the following miscellaneous
functionality:

**object-address** [Function]

Arguments

object :: <object>

Values

address :: <integer>

Description

Returns an integer for *object*. If the object is represented
internally represented as immediate data, then the integer returned is
only unique to the value of the object. If the object is represented on
the dynamic heap, then the integer uniquely identifies the object from
all other objects.

The File-descriptor Module
--------------------------

A cleaner interface to most of these functions is available from the
Streams library (see the document
$INSTALL/doc/libraries/streams.{ps,txt}). You probably do not need to
use the File-descriptor module, unless you are using **fd-exec** or need
an obscure file mode.

The File-descriptor module exports the following functions and
constants:

**fd-exec** [Function]

Arguments

command-line :: <string>

Values

in-fd :: false-or(<integer>)

out-fd :: false-or(<integer>)

Description

This function provides a facility for running programs and scripts
from within Mindy. The *command-line* argument should contain the name
of the program and all of the command line arguments for that program.
This function returns the file descriptors for the new process's
standard input and output. If **fd-exec** is unable to start the
process, then it returns #f and #f.

This function does not work when running on a WindowsNT platform.

**fd-open** [Function]

Arguments

path :: <byte-string>

flags :: <integer>

Values

fd :: false-or(<integer>)

errno :: false-or(<integer>)

Description

This function calls the C **open** system call and returns the file
descriptor and #f, if successful. If the first value is #f, then the
second value is the error number. You can convert the error number to a
string using the **fderrorstring** function.

**fd-close** [Function]

Arguments

fd :: <integer>

Values

win? :: <boolean>

errno :: false-or(<integer>)

Description

This function calls the C **close** system call and returns #t and #f,
if successful. If the first value is #f, then the second value is the
error number. You can convert the error number to a string using the
**fd-error-string** function.

**fd-read** [Function]

Arguments

fd :: <integer>

buffer :: <buffer>

offset :: <integer>

count :: <integer>

Values

count :: false-or( <integer>)

errno :: false-or(<integer>)

Description

This function calls the C **read** system call and returns the number
of bytes read and #f, if successful. *Offset* is an index into *buffer*,
and it the index at which **fd-read** should start writing into the
buffer. All other arguments are the same as those described by the Unix
man page.

If the first value is #f, then the second value is the error number.
You can convert the error number to a string using the
**fd-error-string** function.

This function does no bounds checking.

**fd-write** [Function]

Arguments

fd :: <integer>

buffer :: <buffer>

offset :: <integer>

count :: <integer>

Values

count :: false-or( <integer>)

errno :: false-or(<integer>)

Description

This function calls the C **write** system call and returns the number
of bytes written and #f, if successful. *Offset* is an index into
*buffer*, and it is the index at which **fd-write** should start reading
from the buffer. All other arguments are the same as those described by
the Unix man page.

If the first value is #f, then the second value is the error number.
You can convert the error number to a string using the
**fd-error-string** function.

This function does no bounds checking.

**fd-input-available?** [Function]

Arguments

fd :: <integer>

Values

input? :: <boolean>

errno :: false-or(<integer>)

Description

This function returns whether there is any input available on the file
descriptor. The second return value is #f if **fd-input-available?**
could determine whether input was available. If there is an error, the
second return value is the error number. You can convert the error
number to a string using the **fd-error-string** function.

**fd-sync-output** [Function]

Arguments

fd :: <integer>

Values

win? :: <boolean>

errno :: false-or(<integer>)

Description

This function calls the C **fsync** system call and returns #t and #f,
if successful. If the first value is #f, then the second value is the
error number. You can convert the error number to a string using the
**fd-error-string** function.

**fd-seek** [Function]

Arguments

fd :: <integer>

offset :: <integer>

whence :: <integer>

Values

new-pos :: false-or(<integer>)

errno :: false-or(<integer>)

Description

This function calls the C **lseek** system call and returns the new
absolute position in the file and #f, if successful. If the first value
is #f, then the second value is the error number. You can convert the
error number to a string using the **fd-error-string** function.

**fd-error-string** [Function]

Arguments

errno :: <integer>

Values

msg :: false-or(<byte-string>)

Description

This function calls the C **strerror** system call and returns the
string that describes the given error number. If the error number is
unknown, then **fd-error-string** return #f.

**SEEK\_SET** [Constant]

**SEEK\_CUR** [Constant]

**SEEK\_END** [Constant]

**O\_RDONLY** [Constant]

**O\_WRONLY** [Constant]

**O\_RDWR** [Constant]

**O\_APPEND** [Constant]

**O\_CREAT** [Constant]

**O\_TRUNC** [Constant]

**O\_EXCL** [Constant]

**ENOENT** [Constant]

**EIO** [Constant]

**ENXIO** [Constant]

**EACCES** [Constant]

**EFAULT** [Constant]

**EEXIST** [Constant]

**ENOTDIR** [Constant]

**EISDIR** [Constant]

**EINVAL** [Constant]

**ENFILE** [Constant]

**EMFILE** [Constant]

**ENOSPC** [Constant]

**EROFS** [Constant]

**ENAMETOOLONG** [Constant]

**EBADF** [Constant]

**EINTR** [Constant]

**EPIPE** [Constant]

**EFBIG** [Constant]

These constants are mostly the same constants from the standard C
libraries, file.h and errno.h, but a few names have been changed. Those
names that have changed should be obvious. The Filedescriptors module
exports all the constants users need to call the functions in the
module, or test the functions' return values.

The Cheap-io Module
-------------------

The Cheap-io module exports some basic, unextendable I/O functionality.
Mindy uses the Cheap-io functions internally. The Gwydion Project also
provides the Streams, Print, and Format libraries (see the
$INSTALL/doc/libraries/ directory for documentation). If any library
that you load into Mindy uses the Debugger-format library, then the
debugger uses **format** from the Format library.

**format** [Function]

Arguments

control-string :: <byte-string>

#rest arguments

Values

meaningless :: singleton(#f)

Description

This **format** adheres to the format strings described in the *Dylan
Interim Reference Manual* with one exception. Mindy incorrectly prints
instances of **<condition>** supplied to the %S directive. The Format
library provides a correct **format** function that supports an
upward-compatible extension to the format control strings described in
the DRM.

**print** [Function]

**prin1** [Function]

Arguments

object :: <object>

Values

meaningless :: singleton(#f)

Description

Prints thing to stdout. **Print** follows thing with a newline. You
cannot extend or specialize how objects are printed because these
function's are written in C code, within Mindy's implementation.

**puts** [Function]

Arguments

string :: <byte-string>

Values

meaningless :: singleton(#f)

Description

Prints the contents of *string* to stdout.

**putc** [Function]

Arguments

char :: <byte-character>

Values

meaningless :: singleton(#f)

Description

Prints *char* to stdout.

**getc** [Function]

Arguments

none

Values

char :: <byte-character>

Description

Read and return the next character from stdin. Returns #f at EOF.

**fflush** [Function]

Arguments

none

Values

meaningless :: singleton(#f)

Description

Forces out any pending output generated by **format**, **print**,
**prin1**, **puts**, and **putc**.

The Threads Module
------------------

This module is in the Dylan library and exports an interface to
**<thread>**\ s, **<lock>**\ s, and **<event>**\ s (objects on which
threads can wait until a signalling thread indicates the events have
occurred).

Classes and Functions
~~~~~~~~~~~~~~~~~~~~~

The Threads module exports the following classes and functions:

**<thread>** [Class]

This class is a subclass of **<object>**. Instances of this class are
the handles by which programs manipulate threads.

**spawn-thread** [Function]

Arguments

debug-name :: <byte-string>

init-function :: <function>

Values

thread :: <thread>

Description

Spawns a concurrent asynchronous thread and invokes *init-function* in
that thread. The dynamic context of the thread is the same as if it were
the main thread of a program at the beginning of the program's
execution.

**kill-thread** [Function]

Arguments

thread :: <thread>

Values

thread :: <thread>

Description

Kills *thread* immediately. After calling this function, the argument
*thread* never executes again.

**current-thread** [Function]

Arguments

none

Values

thread :: <thread>

Description

Returns the thread handle of the current thread.

**<lock>** [Abstract Class]

This class is a subclass of **<object>**. Instances of this class
provide logical locks. A lock is locked when a thread successfully
*grabs* a lock, and we say the thread *holds* the lock. Holding a lock
in no way prohibits access to a resource. It is purely the convention of
various threads to access a shared resource only after successfully
grabbing a lock. If **<lock>** is passed to **make**, **make** returns a
**<spinlock>**.

**<spinlock>** [Sealed Class]

This class is a subclass of **<lock>**. Instances of this class
provide a single-locking model. Whenever a **<spinlock>** is locked, any
thread that tries to grab it will block. Whenever a **<spinlock>** is
locked, any thread may release it. Whenever a **<spinlock>** is
unlocked, any thread may grab it.

**<spinlock>**\ s are designed to be held for a very short period of
time, several machine instructions at most. Threads should only hold a
**<spinlock>** for a very short period of time because other threads
that are waiting for the lock are blocked and could be wasting CPU
cycles by busy looping; that is, waiting for a **<spinlock>** does not
necessarily use anything as heavy weight as a system call to sleep the
thread waiting for the lock. If only a couple threads are sharing a
resource, it may be more efficient to actually hold a **<spinlock>** for
a moderate amount of time while performing a high-level operation,
rather than use a lock to build a more heavy-weight mutual exclusion
mechanism (such as a semaphore) to isolate access to the shared
resource.

Unlocking a **<spinlock>** when it is already unlocked signals an
error.

**<multilock>** [Sealed Class]

This class is a subclass of **<lock>**. Instances of this class
provide a multilocking model. Whenever a **<multilock>** is unlocked,
any thread may grab it. A thread that holds a **<multilock>** may grab
the lock repeatedly without releasing the lock. Each grab effectively
increments a counter, and each release effectively decrements a counter.
A **<multilock>** is available to be grabbed by any thread when the
counter returns to zero; therefore, a thread must release the lock for
each grabbing of the lock. This behavior is useful for implementing a
high-level operation that needs to isolate access to a resource while
calling a few lower-level operations that lock the resource; in this
way, the high-level operation effectively calls all the lower-level
operations atomically with no other threads affecting the state of the
resource between the calls.

Whenever a **<multilock>** is locked, only the thread that holds the
lock may release it.

**<multilock>**\ s are designed to be held for as long as a thread
requires. When other threads call the **grab-lock** function and block
because a **<multilock>** is locked, the other threads are guaranteed to
sleep until the lock is available.

Unlocking a **<multilock>** when it is already unlocked signals an
error.

**<semaphore>** [Sealed Class]

This class is a subclass of **<lock>**. Instances of this class
provide a single-locking model.

Whenever a **<semaphore>** is unlocked, any thread may grab it.
Whenever a **<semaphore>** is locked, any thread that tries to grab it
will block. Whenever a d is locked, any thread may release it.

**<semaphore>**\ s are designed to be held for as long as a thread
requires. When other threads call the **grab-lock** function and block
because a **<semaphore>** is locked, the other threads are guaranteed to
sleep until the lock is available.

Unlocking a **<semaphore>** when it is already unlocked signals an
error.

**locked?** [Function]

Arguments

lock :: <lock>

Values

locked? :: <boolean>

Description

Returns whether the lock is held by any thread.

**grab-lock** [Generic Function]

Arguments

lock :: <lock>

Values

meaningless :: singleton(#f)

Description

Returns after successfully grabbing the lock. If the lock is not
immediately available, this function waits for the lock to become
available.

**grab-lock** [G.F. Method]

Arguments

lock :: <spinlock>

Values

meaningless :: singleton(#f)

Description

Returns after successfully grabbing the lock. This method can only
grab *lock* when it is unlocked. When the lock is held, this method may
busy-loop until the lock is unlocked.

**grab-lock** [G.F. Method]

Arguments

lock :: <semaphore>

Values

meaningless :: singleton(#f)

Description

Returns after successfully grabbing the lock. This method can only
grab *lock* when it is unlocked. When the lock is held, this method puts
the calling thread to sleep until the lock is available.

**grab-lock** [G.F. Method]

Arguments

lock :: <multilock>

Values

meaningless :: singleton(#f)

Description

Returns after successfully grabbing the lock. A single thread may
successfully call this method repeatedly, but the thread must call
**release-lock** once for each call to **grab-lock**. If the thread
calls **release-lock** fewer times than **grab-lock**, the lock remains
locked, and any threads waiting for the lock will continue to wait. When
a thread that does not hold the lock calls this method, the method puts
the calling thread to sleep until the lock is available.

**release-lock** [Generic Function]

Arguments

lock :: <lock>

Values

meaningless :: singleton(#f)

Description

Releases the lock. If *lock* is unlocked, this function signals an
error.

**release-lock** [G.F. Method]

Arguments

lock :: union(<spinlock>, <semaphore>)

Values

meaningless :: singleton(#f)

Description

Releases the lock. If *lock* is unlocked, this function signals an
error. Any thread may unlock a **<spinlock>** or **<semaphore>**,
regardless of whether it is the thread that successfully grabbed the
lock.

**release-lock** [G.F. Method]

Arguments

lock :: <multilock>

Values

meaningless :: singleton(#f)

Description

Releases the lock. If *lock* is unlocked, this function signals an
error. Only the thread that holds *lock* may call this function, and if
another thread tries to release the lock, this method signals an error.
When this function returns, *lock* may still be locked. A thread that
has repeatedly grabbed a **<multilock>** must call **release-lock** once
for each call to **grab-lock**.

**<event>** [Class]

This class is a subclass of **<object>**. Threads use events to block
without busy looping and to communicate to other threads that they
should wake up.

**wait-for-event** [Generic Function]

Arguments

event :: <event>

lock :: <lock>

Values

meaningless :: singleton(#f)

Description

Releases the lock and puts the calling thread to sleep until some
other thread signals *event*. After this function returns, the lock is
unheld, and the calling thread must try to grab the lock before
accessing any shared resources. Due to implementation details, this
function may return even when the lock is unavailable, or the event has
not truly occurred; because of this, programs need to loop over
**wait-for-event** and **grab-lock**, testing that the event actually
occurred. Methods exist for both **<spinlock>**\ s and
**<semaphore>**\ s.

**signal-event** [Function]

Arguments

event :: <event>

Values

meaningless :: singleton(#f)

Description

Signals that the event occurred, indicating that Mindy should wake up
a thread that is waiting on this event.

**broadcast-event** [Function]

Arguments

<event>

Values

meaningless :: singleton(#f)

Description

Signals that the event occurred and causes Mindy to wake up every
thread that is waiting on this event.

Examples
~~~~~~~~

The following code shows how to use locks and events to isolate access
to a queue:

::

        // This example shows two routines, get-queue and release-queue.  Code
        // that accesses the queue should call get-queue before doing so and call
        // release-queue when done.  Any code failing to isolate access to the
        // queue in this way has undefined behavior and is incorrectly written.
        //    // This variable is #t if and only if the queue is generally available.
        //
        define variable queue-available? = #t;    // This constant holds an event object used to signal when the queue
        // becomes generally available again.
        //
        define constant queue-available = make(<event>);    // This constant holds a lock object used to isolate access to
        // queue-available? for testing and setting purposes.
        //
        define constant queue-lock = make(<lock>);    // When this function returns, the caller has exclusive access to the
        // queue.  If necessary, this function waits for the queue to become
        // available, but it does not busy loop.  This function returns #f as
        // a meaningless return value.
        //
        define method get-queue ()
          grab-lock(queue-lock);
          while (~ queue-available?)
            wait-for-event(queue-available, queue-lock);
            grab-lock(queue-lock);
          end;
          queue-available? := #f;
          lock-release(queue-lock);
          #f;
        end;
        // This function releases the queue and signals that it is released so
        // that someone waiting on the queue will be woken up.  This function
        // returns #f as a meaningless return value.
        //
        define method release-queue ()
          grab-lock(queue-lock);
          queue-available? := #t;
          release-lock(queue-lock);
          signal-event(queue-available);
          #f;
        end;

The following example shows how to use a lock to isolate queue access in
a different way than the previous example:

.. code-block:: dylan

    // This constant holds an event object used to signal when an element
    // exists in the queue.
    //
    define constant something-available = make(<event>);    // This constant holds a lock that is held whenever a thread is accessing
    // queue.
    //
    define constant lock = make(<lock>);    // This constant holds a queue object.
    //
    define constant queue = make(<deque>);    // This function returns an element from queue.  If no element is
    // immediately available, then this function blocks until it can return
    // an element.  This function assumes only one or two other threads are
    // ever waiting for the queue, and it assumes pop is a fast high-level
    // operation.
    //
    define method get-something()
      grab-lock(lock);
      while (empty?(queue))
        wait-for-event(something-available, lock);
        grab-lock(lock);
      end;
      let result = pop(queue);
      lock-release(lock);
      result;
    end;    // This function adds thing to queue.  It assumes only one or two other
    // threads are ever waiting for the queue, and it assumes push is a fast
    // high-level operation.
    //
    define method put-something(thing)
      grab-lock(lock);
      push(queue, thing);
      release-lock(lock);
      signal-event(something-available);
    end;

The Transcendental Module
-------------------------

This module is in the Dylan library and provides some common
transcendental functions.

**sin** [Function]
**cos** [Function]
**tan** [Function]

Arguments

radians :: <float>

Values

answer :: <float>

Description

**sin** returns the sine of the argument, **cos** the cosine, and
**tan** the tangent. The argument is in radians. The return value is the
same type of float as the input value.

**asin** [Function]
**acos** [Function]
**atan** [Function]

Arguments

number :: <float>

Values

answer :: <float>

Description

**asin** returns the inverse sine of the argument. **acos** returns
the inverse cosine of the argument. **atan** returns the inverse tangent
of the argument. The return value is the same type of float as the input
value.

**atan2** [Function]

Arguments

x :: <float>

y :: <float>

Values

answer :: <float>

Description

**atan2**\ returns the two argument inverse tangent of *x* / *y*. If
*x* and *y* are both **<single-float>**\ s, the return value is a
**<single-float>**; otherwise, the return value is a **<double-float>**.

**sinh** [Function]
**cosh** [Function]
**tanh** [Function]

Arguments

number :: <float>

Values

answer :: <float>

Description

**sinh** returns the hyperbolic sine of the argument. **cosh** returns
the hyperbolic cosine of the argument. **tanh** returns the hyperbolic
tangent of the argument. The return value is the same type of float as
the input value.

**exp** [Function]
**log** [Function]
**sqrt** [Function]

Arguments

number :: <float>

Values

answer :: <float>

Description

**exp**\ returns **$e** raised to the power *number*, where **$e** is
the base of the natural logarithm. **log** returns the natural logarithm
of *number*. **sqrt** returns the square root of *number*. The return
value is the same type of float as the input value.

**^** [Method]

Arguments

base-number :: <float>

power-number :: <float>

Values

answer :: <float>

Description

**expt**\ returns *base-number* raised to the power *power-number*. If
both *base-number* and *power-number* are **<single-float>**\ s, the
return value is a <single-float>; otherwise, the return value is a
**<double-float>**.

**$pi** [Constant]
**$e** [Constant]

**$pi** is the best approximation to the mathematical constant pi, in
**<double-float>** format. **$e** is the best approximation to the
mathematical constant e, the base of the natural logarithm, in
**<double-float>** format.

Streams Library
---------------

There is a Streams library that adheres to the Gwydion streams
specification. For documentation on the stream specification, see the
file $INSTALL/doc/libraries/streams.{ps,txt}.

The Streams library exports two modules, Streams and Standard-io. The
Streams module exports all identifiers from the streams specification.
The Streams module also exports **<fd-stream>**:

**<fd-stream>** [Class]

This class is a subclass of **<stream>**. These streams are based on C
file descriptors, and they do not adhere to the Random Access Protocol
described in the Gwydion streams specification. The **make** method
accepts the following keywords:

*direction:*

This keyword is optional and defaults to #"input". When supplied, it
must be either #"input" or #"output".

*fd:*

This keyword is required and should be an open file-descriptor.

*size:*

This keyword is optional and is the size of the buffer. See the
Streams specification for details.

The Standard-io module exports the following:

**\*standard-input\*** [Constant]

**\*standard-output\*** [Constant]

**\*standard-error\*** [Constant]

These have the following values respectively:

::

                make(<fd-stream>, fd: 0)
                make(<fd-stream>, fd: 1, direction: #"output")
                make(<fd-stream>, fd: 2, direction: #"output")

Miscellaneous Implementation Choices
------------------------------------

The **error**\ method specialized on **<byte-string>** applies the
**format** function to the arguments passed to **error**. See
:ref:`The Extensions Module <the-extensions-module>` for the details of
**format** from the Cheap-io module of the Dylan library. See
$INSTALL/doc/libraries/format.{ps,txt} for the details of **format**
from the Format library.

Rest arguments in Mindy are **<sequence>**\ s. You cannot use any
functions on the rest argument that assumes the collection is an
instance of any class more specific than **<sequence>**; for example,
you cannot use the **head** or **tail** functions because they operate
on instances of **<pair>**.

Mindy's :drm:`<character>` implementation is equivalent to unicode
characters. The **<byte-character>** class exported from the Extensions
module of the Dylan library is a subclass of :drm:`<character>`.

Copyright and Terms of Use
--------------------------

Copyright (c) 1994, 1995, 1996 Carnegie Mellon University All rights
reserved.

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
E-mail to the Internet address gwydionbugs@cs.cmu.edu.
