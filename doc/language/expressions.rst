.. _sec-expressions:

Expressions
===========

Expressions can contain literal values, identifiers (constants, components,
local and global variables) and operators.  Literals for Booleans, integers and
doubles are written in the standard way: ``true``, ``false``, ``4``, ``3.14``.


Operators
---------

The following operators can be used in expressions, ordered by their precedence
(most strongly binding operators first):

* ``[...]``, ``.``, ``(...)`` (array indexing, member access, function call)
* ``-`` (unary minus)
* ``boundto``, ``in``, ``:`` (role binding, compartment membership, has type,
  see :ref:`system instantiation <sec-system>` for details)
* infix functions (binary functions written in infix notation)
* ``*``, ``/`` (multiplication, division)
* ``+``, ``-`` (addition, subtraction)
* ``>``, ``>=``, ``<``, ``<=`` (relational operators)
* ``=``, ``!=`` (equality operators)
* ``!`` (unary negation)
* ``&`` (logical and)
* ``|`` (logical or)
* ``=>`` (implication)
* ``if ... then ... else ...``

Infix functions are written in between their operators without parentheses,
for instance ``plus(1, 2)`` is equivalent to ``1 plus 2``.

The has-type operator ``:`` also accepts type sets on the right-hand side,
including the built-in sets ``natural``, ``role``, and ``compartment``.


Arrays
------

Arrays are written using brackets, e.g., ``[1, 2, 3]`` represents an array with
the elements ``1``, ``2`` and ``3``. Note that an array can contain any
expression and is not limited to literals. Array values can also be constructed
using comprehensions. The following two constant definitions are equivalent::

   const xs = [2, 4, 6];
   const ys = [ i * 2 || i : [1 .. 3] ];


.. _sec-quantification:

Quantification
--------------

The ``forall`` and ``exists`` quantifiers can be used to write expressions
ranging over all or some components in the model. Consider the following
example::

   type N {
       s : [0..1] init 0;
       // ...
   }

   system {
       n[2] : N;
       m : Monitor;
   }

   type Monitor {
       done : bool init false;

       [] forall c : N. c.s = 1 -> (done' = true);
   }

The ``forall`` quantifier ranges over all components in the model that have
type ``N``, namely ``n[0]`` and ``n[1]``. Thus, the guard of the monitor's
command expands to ``n[0].s = 1 & n[1].s = 1``. The type following the
identifier can also be a type set (see :ref:`type system <sec-type-system>` for
details). If no type is given, then the quantifier ranges over all components
in the model.

In addition to quantification over components, RBL also supports quantification
over bounded integers, so the above guard can also be written as follows::

   type Monitor {
       done : bool init false;

       [] forall i : [0 .. 1]. n[i].s = 1 -> (done' = true);
   }

Nested quantifiers of the same kind, e.g., ``forall x : N. forall y : M.``, can
be written using the shorthand notation ``forall x : N, y : M.``

In addition to ``forall`` and ``exists``, there are also the keywords ``sum``
and ``product`` that are used exactly the same::

   function fact(n : int) : int = product i : [1 .. n]. i;


Built-in functions
------------------

The following built-in functions are provided:

* ``min(x, y)`` and ``max(x, y)`` which select the minimum or maximum of two
  integers, respectively, and their variants ``minf`` and ``maxf`` for doubles.
* ``floor(x)`` and ``ceil(x)`` which round ``x`` up and down, respectively, to
  the nearest integer.
* ``pow(x, y)`` which computes ``x`` to the power of ``y``, and its variant
  ``powf`` for doubles.
* ``mod(i, n)`` for integer modulo.
* ``log(x, b)`` which computes the logarithm of ``x`` to base ``b``.
* ``count(R, c)`` which counts the number of role instances with type ``R``
  contained in the compartment instance ``c``.
* ``length(arr)`` which returns the length of an array.
* ``player(r)`` returns the component to which ``r`` is bound. Calling this
  function with a component that is not a role will throw an error during the
  translation of the model. Use ``r : role`` to check if ``r`` actually is a
  role.
* ``playable(r [, act])`` returns ``true`` if the role ``r`` can be played in
  the current system state, i.e., there is an outgoing transition labeled with
  ``r``. If an optional action ``act`` is given, then ``playable(r, act)``
  returns ``true`` only if ``r`` can be played on action ``act``.
* ``index(c)`` returns the index of a component contained in a component array.
  For example, ``index(workers[2])`` will return ``2``, where ``workers`` is
  a component array.

The ``player`` function can also be used as a keyword without an argument. This
is equivalent to writing ``player(self)``.


.. _sec-expression-contexts:

Expression contexts
-------------------

Expressions can appear in different *contexts* which influences their semantics.

action context
   The action context applies to all expressions that appear in the action-label
   position of a command. All identifiers of an expression in this context which
   are not defined elsewhere are interpreted as actions. Consider the following
   example::

      const b = true;

      module example {
          [if b then foo else bar] true -> true;
      }

   Here, the ``if`` expression is interpreted in the action context. The
   identifiers ``foo`` and ``bar`` are actions, since they are not defined
   elsewhere. However, the identifier ``b`` is not an action, since it was
   defined as a constant.

   Accessing an otherwise undefined local identifier of a component also creates
   an action::

      type N {
          [self.a] true -> true;
      }

      system {
          n : N;
      }

   Here, the component ``n`` has no local variable named ``a``, thus ``self.a``
   evaluates to the action ``n.a`` upon instantiation of ``N``.

   Furthermore, actions are automatically converted into action arrays when
   using the index operator on them.

   The action context can be entered explicitly by using the ``action`` keyword,
   for example::

      const act_arr : array 2 of action = [action a, action b];

   Here, an array containing two actions ``a`` and ``b`` is defined. An explicit
   action context can also be used to return actions from
   :ref:`functions <sec-functions>`::

      function send(from : int, to : int) : action = action snd[from][to];

      module example {
         [send(1, 2)] true -> true;
      }

   In the above example, the ``send`` function returns an action derived from
   its two parameters. The result of the function application is then used as
   action in a command.

constraint context
   The constraint context applies to all expressions that appear as a role guard
   in a coordinator command. All identifiers refering to role components are
   automatically converted to Boolean expressions. Consider the following
   example::

      natural type N;
      role type R(N);

      system {
          n : N;
          a : R; a boundto n;
          b : R; b boundto n;
      }

      coordinator {
          [] [a & !b] true -> true;
      }

   Here, the identifiers ``a`` and ``b`` refer to role components. But since
   they are used in the constraint context in the coordinator command, they
   are used as Booleans.

   Sometimes it is necessary to explicitly force the conversion of a role
   component to a ``bool``. For this, the following function can be used::

      function played(r : bool) : bool = r;

      coordinator {
          [] [if cond then played(a) else !played(a)] true -> true;
      }

   The constraint context can be entered explicitly by using the ``constraint``
   keyword. This is especially useful for extracting role-playing constraints
   into functions::

      function a_played(cond : bool) : bool =
         constraint if cond then played(a) else !a;

      coordinator {
          [] [a_played(true)] true -> true;
      }
