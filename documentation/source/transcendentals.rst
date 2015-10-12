Transcendental Library
======================


.. current-library:: transcendental
.. current-module:: transcendental

Introduction
------------

The ``Transcendental`` library implements some common mathematical functions
and constants, such as sine and cosine. All functions in the
``Transcendental`` library signal errors when given invalid arguments, and
Dylan floating point precision contagion rules are obeyed. Precise
contagion rules are given for each function below.

Exported Names
--------------

The following names are exported from the ``Transcendental`` module of the
``Transcendental`` library:

.. constant:: $single-pi

   The value of *π* as a single precision floating point number.

   :type: <single-float>

   :superclasses: :drm:`<float>`

   :description:

     The value of *π* as a single precision floating point number.

   :seealso:

     - :const:`$double-pi`

.. constant:: $double-pi

   The value of *π* as a double precision floating point number.

   :type: <double-float>

   :superclasses: :drm:`<float>`

   :description:

     The value of *π* as a double precision floating point number.

   :seealso:

     - :const:`$single-pi`

.. constant:: $single-e

   The value of *e*, the base of natural logarithms, as a single precision
   floating point number.

   :type: <single-float>

   :superclasses: :drm:`<float>`

   :description:

     The value of *e*, the base of natural logarithms, as a single precision
     floating point number.

   :seealso:

     - :const:`$double-e`

.. constant:: $double-e

    The value of *e*, the base of natural logarithms, as a double precision
    floating point number.

    :type: <double-float>

    :superclasses: :drm:`<float>`

    :description:

      The value of *e*, the base of natural logarithms, as a double precision
      floating point number.

   :seealso:

     - :const:`$single-e`

.. generic-function:: sin

   Returns the sine of its argument.

   :signature: sin x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the sine of its argument. The floating point precision of the
     result is given by the precision of ``x``. The result is a single-float
     if ``x`` is an integer.

   :seealso:

     - :gf:`cos`
     - :gf:`tan`

.. generic-function:: cos

   Returns the cosine of its argument.

   :signature: cos x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the cosine of its argument. The floating point precision of the
     result is given by the precision of ``x``. The result is a single-float
     if ``x`` is an integer.

   :seealso:

     - :gf:`sin`
     - :gf:`tan`

.. generic-function:: tan

   Returns the tangent of its argument.

   :signature: tan x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the tangent of its argument. The floating point precision of the
     result is given by the precision of ``x``. The result is a single-float
     if ``x`` is an integer.

   :seealso:

     - :gf:`cos`
     - :gf:`sin`

.. generic-function:: asin

   Returns the arc sine of its argument.

   :signature: asin x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
                 If ``x`` is not in the range `[-1,+1]`, an error is signalled.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the arc sine of its argument. The floating point precision of
     the result is given by the precision of ``x``. The result is a
     single-float if ``x`` is an integer.

   :seealso:

     - :gf:`acos`
     - :gf:`atan`

.. generic-function:: acos

   Returns the arc cosine of its argument.

   :signature: acos x => y

   :parameter x: an instance of type :drm:`<real>`. The angle, in radians.
                 If ``x`` is not in the range ``[-1,+1]``, an error is signalled.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the arc cosine of its argument. The floating point precision of
     the result is given by the precision of ``x``. The result is a
     single-float if ``x`` is an integer.

   :seealso:

     - :gf:`asin`
     - :gf:`atan`

.. generic-function:: atan

   Returns the arc tangent of its argument.

   :signature: atan x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
                 If ``x`` is not in the range `[-1,+1]`, an error is signalled.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the arc tangent of its argument. The floating point precision of
     the result is given by the precision of *x*. The result is a
     single-float if *x* is an integer.

   :seealso:

     - :gf:`acos`
     - :gf:`asin`

.. generic-function:: atan2

   Returns the arc tangent of one angle divided by another.

   :signature: atan2 x y => z

   :parameter x: An instance of type :drm:`<real>`. The first angle, in radians.
   :parameter y: An instance of type :drm:`<real>`. The second angle, in radians.
   :value z: An instance of type :drm:`<float>`.

   :description:

     Returns the arc tangent of ``x`` divided by ``y``. ``x`` may be zero if ``y``
     is not zero. The signs of ``x`` and ``y`` are used to derive what quadrant
     the angle falls in.

     The floating point precision of the result is given by the precision of
     ``x``/``y``. The result is a single-float if ``x/y`` is an integer.


.. generic-function:: sinh

   Returns the hyperbolic sine of its argument.

   :signature: sinh x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the hyperbolic sine of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :gf:`cosh`
     - :gf:`tanh`

.. generic-function:: cosh

   Returns the hyperbolic cosine of its argument.

   :signature: cosh x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the hyperbolic cosine of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :gf:`sinh`
     - :gf:`tanh`

.. generic-function:: tanh

   Returns the hyperbolic tangent of its argument.

   :signature: tanh x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
   :parameter y: An instance of type :drm:`<float>`.

   :description:

     Returns the hyperbolic tangent of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :gf:`cosh`
     - :gf:`sinh`

.. generic-function:: log

   Returns the natural logarithm of its argument.

   :signature: log x => y

   :parameter x: An instance of type :drm:`<real>`.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the natural logarithm of ``x`` to the base e. If x <= 0 <= 1, an
     error is signalled. The floating point precision of the result is given
     by the precision of ``x``. The result is a single-float if ``x`` is an
     integer.

   :seealso:

     - :gf:`exp`

.. generic-function:: exp

   Returns *e*, the base of natural logarithms, raised to the power
   indicated by its argument.

   :signature: exp x => y

   :parameter x: An instance of type :drm:`<real>`.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns *e*, the base of natural logarithms, raised to the power ``x``.
     The floating point precision is given by the precision of ``x``.

   :seealso:

     - :gf:`^`
     - :gf:`log`

.. generic-function:: ^

   Returns its first argument, raised to the power indicated by its second
   argument.

   :signature: ^ b x => y

   :parameter b: An instance of type :drm:`<real>`.
   :parameter x: An instance of type :drm:`<real>`.
   :value y: An instance of type :drm:`<real>`.

   :description:

     Returns ``b`` raised to the power ``x``. If ``b`` is ``0`` and ``x`` is not
     positive, an error is signalled. If ``b`` is negative and ``x`` is not an
     integer, an error is signalled.

     If ``b`` and ``x`` are both integers, the result is an integer. If ``x`` is
     negative, an error is signalled.

     The floating point precision is given by the precision of ``b``. The
     result is a single-float if ``b`` is an integer.

   :seealso:

     - :gf:`exp`

.. method:: ^
   :specializer: <integer>, <integer>

.. generic-function:: sqrt

   Returns the square root of its argument.

   :signature: sqrt x => y

   :parameter x: An instance of type :drm:`<real>`.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the square root of x. If x is less than zero an error is
     signalled. The floating point precision of the result is given by the
     precision of ``x``. The result is a single-float if ``x`` is an integer.

   :seealso:

     - :gf:`isqrt`

.. generic-function:: isqrt

   Returns the integer square root of its argument.

   :signature: isqrt x => y

   :parameter x: An instance of type :drm:`<integer>`.
   :value y: An instance of type :drm:`<integer>`.

   :description:

     Returns the integer square root of ``x``, that is the greatest integer
     less than or equal to the exact positive square root of ``x``. If ``x`` <
     ``0``, an error is signalled.

   :seealso:

     - :gf:`sqrt`

Unimplemented Functions
-----------------------

We intend to someday implement the following functions, but haven't done
so yet:

.. generic-function:: asinh

   Returns the hyperbolic arc sine of its argument.

   :signature: asinh x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the hyperbolic arc sine of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :gf:`acosh`
     - :gf:`atanh`

.. generic-function:: acosh

   Returns the hyperbolic arc cosine of its argument.

   :signature: acosh x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the hyperbolic arc cosine of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :gf:`asinh`
     - :gf:`atanh`

.. generic-function:: atanh

   Returns the hyperbolic arc tangent of its argument.

   :signature: atanh x => y

   :parameter x: An instance of type :drm:`<real>`. The angle, in radians.
   :value y: An instance of type :drm:`<float>`.

   :description:

     Returns the hyperbolic arc tangent of its argument. The floating point
     precision of the result is given by the precision of ``x``. The result is
     a single-float if ``x`` is an integer.

   :seealso:

     - :gf:`acosh`
     - :gf:`asinh`