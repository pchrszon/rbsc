.. _sec-functions:

Functions
=========

RBL allows the definition of functions to reduce code duplication and enable
abstraction. Functions can be used in any expression, including constant
definitions and the definition of other functions.


Function definitions
--------------------

A function is defined using the ``function`` keyword::

   function add_one(x : int) : int = x + 1;

Every parameter of the function must be annotated with a
:ref:`type <sec-type-system>`. The return type is stated after the parameter
list. It is also possible to define functions without any parameters, which is
useful to define commonly used expressions which are not constant.

A function can also be recursive::

   function fact(n : int) : int =
       if n <= 1 then 1 else n * fact(n - 1);

Note that there is a limit on recursive calls to prevent infinite recursion. The
default limit is 100, but can be changed using the ``--recursion-depth`` option.


Associated functions
--------------------

It is also possible to define functions associated to a component type.
Associated functions are defined by preceding the function name with a type
name. In the body of an associated function, the ``self`` keyword refers to the
component on which the function was invoked. Associated functions are useful
for providing a common interface to structurally different components, as
illustrated by the following example::

   type Cell {
       cell : [0 .. 1] init 0;
   }

   function Cell.is_empty() : bool = self.cell = 0;

   type Buffer {
       cells : array 3 of [0 .. 1] init 0;
   }

   function Buffer.is_empty() : bool = forall i : [0..3]. self.cells[i] = 0;

   label "all_empty" = forall c : {Cell, Buffer}. c.is_empty();

Here, the ``is_empty`` function is defined for both the ``Cell`` type and the
``Buffer`` type. This allows us to call this function on all instances of these
types in the label definition.
