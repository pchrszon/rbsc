.. _sec-type-system:

Type system
===========

RBL is statically typed, i.e., every constant, variable, parameter and
expression has a fixed type. During translation, rbsc type checks the whole
model, such that all generated PRISM models are free from type errors.


Types
-----

The following types are available in RBL:

``bool``
   Values of type ``bool`` can be either ``true`` or ``false``.

``int``
   Signed integers.

``double``
    Double-precision floating point values.

``action``
    The type of action labels which are used for synchronization between
    components.

component types
   Each :ref:`type declaration <sec-type-decls>` creates a type. Multiple
   component types can be combined into a type set, for instance
   ``{Type1, Type2}``. The predefined type sets ``natural``, ``role`` and
   ``compartment`` contain all natural types, role types and compartment types
   defined in the model, respectively. The type set ``component`` comprises all
   component types defined in the model.

``array <n> of <type>``
   An array of ``type`` with size ``n``. The size ``n`` is part of the type,
   i.e., ``array 2 of int`` and ``array 3 of int`` are distinct types. Note
   that ``type`` can also be an ``array`` type itself, which allows the
   definition of multi-dimensional arrays::

      const squares : array 3 of array 2 of int = [ [1, 1], [2, 4], [3, 9] ];

   Component arrays defined in the :ref:`system <sec-system>` block also have
   an array type::

      system {
         w[3] : Worker;
      }

   Here, ``w`` has the type ``array 3 of Worker``.

function types
    A one-argument function has type ``<arg-type> -> <return-type>``.
    Functions with multiple arguments are represented by multiple applications
    of the ``->`` type operator (``->`` is left-associative). For example, the
    ``min`` function has type ``int -> int -> int``.


.. _sec-variable-types:

Variable types
--------------

The type of local and global variables is limited to:

* ``bool``
* bounded integers: ``[lower .. upper]``
* arrays of ``bool`` or bounded integers, possibly multi-dimensional

Note that the bounded integer type is equivalent to the ``int`` type.


Implicit conversions
--------------------

Values of some types can be automatically converted into values with a different
type. The following list contains all implicit conversions allowed in RBL:

* ``int`` to ``double``
* ``array <n> of int`` to ``array <n> of double``
* ``<type>`` to ``array <n> of <type>``

The last conversion can be used to initialize all elements of an array with the
same value, for example::

   const ones : array 3 of int = 1;

This definition is equivalent to::

   const ones : array 3 of int = [1, 1, 1];
