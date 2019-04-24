.. _sec-constants:

Constants
=========

RBL allows the definition of *constants*. A constant can be defined in terms
of literals or expressions including other constants. They are defined using the
``const`` keyword::

   const x : int = 5;
   const b : bool = true;
   const squares : array 4 of int = [1, 4, 9, 16];

A constant can have any type listed on the :ref:`type system <sec-type-system>`
page. The type annotation is optional. If no type is given, the type of the
constant is inferred from the expression.

Constants can be left undefined in the model::

   const x : int;

In this case, the constant's value must be provided on the command line, for
example::

   rbsc my_model.rbl -c x=2


Enumerations
------------

Constants are sometimes used to give names to locations, for instance::

   const INIT = 0;
   const SEND = 1;
   const RECEIVE = 2;

   module sensor {
       loc : [0 .. 2] init INIT;
   }

In order to define such constants with increasing values, an ``enum`` block
can be used::

   enum {
       INIT,
       SEND,
       RECEIVE
   }

This enumeration is equivalent to the constant definitions above. If the
enumeration is only used by a single module, it can also be inlined::

   module sensor {
       loc : enum { INIT, SEND, RECEIVE} init INIT;
   }

Note that the constants defined this way are still global.
