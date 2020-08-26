.. _sec-system:

System instantiation
====================

A role-based system consists of one or more component instances, which are
created by instantiating :ref:`component types <sec-type-decls>`. These
instances are then connected by binding roles to players and by assigning roles
to compartments.


.. _sec-component-instantiation:

Component instantiation
-----------------------

Components are instantiated within ``system`` blocks. There may be multiple
``system`` blocks in an RBL model, but there must be at least one. A component
instance is created by specifying its name followed by its type::

   system {
       w : Worker;
   }

In the above example, an instance of the natural type ``Worker`` with name ``w``
is created.

Multiple components with the same component type can be instantiated by
creating a component array. In the following example, three workers are created
which are all contained in the ``workers`` array::

   system {
       workers[3] : Worker;
   }


Role binding
------------

For binding roles to players, the operator ``boundto`` is used. In the following
example, the ``c`` role is bound to the ``w`` component, making ``w`` the player
of ``c``::

   system {
       w : Worker;
       c : Consumer;
       c boundto w;
   }

Note that only role components can appear on the left hand side of the
``boundto`` operator. Furthermore, a role can only be bound once, i.e., it must
have exactly one player.


Compartment membership
----------------------

The ``in`` operator states that a role instance is part of a compartment
instance::

   system {
       c : Consumer;
       pc : ProducerConsumerProtocol;
       c in pc;
   }

In the system specified above, the ``c`` role is part of the ``pc`` compartment.
Only role instances can appear on the left hand side of an ``in`` operator and
the component on the right hand side must be a compartment.


Multiple system instances
-------------------------

A system instance is fully specified if the following constraints are fulfilled:

1. All roles are bound.
2. All compartments are saturated, i.e., the set of contained role instances
   fulfills the occurrence constraints of at least one alternative (see
   :ref:`compartment types <sec-compartment-types>` for details).

It is possible to define multiple system instances by means of
underspecification. Consider the following example::

   natural type Worker;
   role type Consumer(Worker);
   role type Producer(Worker);
   compartment type ProducerConsumerProtocol(Producer, Consumer);

   system {
       w : Worker;
       p : Producer;
       pc : ProducerConsumerProtocol;
       p in pc;
   }

This system is underspecified. There is a missing role of type ``Consumer`` in
the ``pc`` compartment and the role ``p`` is not bound to a player.

Generally speaking, rbsc completes such underspecified system instances by
creating new instances, by binding roles to players and by assigning roles to
compartments. In case of unbound roles, rbsc either creates a new instance
as the player (chosen among the possible player types in the corresponding role
type declaration) or it binds an existing instance that has the right type.
The same approach is used to add missing roles to compartments. Then, a system
instance for each possible combination of these choices is created.

In the example above, the ``p`` role may either be bound to the existing ``w``
natural or a new instance of ``Worker`` may be created. Next, the ``pc``
compartment is missing a role of type ``Consumer``. Since there is no existing
role of that type, a new instance is created. Again, this role instance is
unbound. It is then bound analogously to ``p``. Combining all possible choices,
the following system instances are created, where ``c`` is the name of the
newly created ``Consumer`` instance and ``w2`` and ``w3`` are new instances of
``Worker``:

1. ``p`` is bound to ``w2`` and ``c`` is bound to ``w3``
2. ``p`` is bound to ``w2`` and ``c`` is bound to ``w``
3. both ``p`` and ``c`` are bound to ``w2``
4. ``p`` is bound to ``w`` and ``c`` is bound to ``w2``
5. both ``p`` and ``c`` are bound to ``w``


Constraints
-----------

Constraints can be specified to filter out system instances. A constraint is a
Boolean expression over the component instances of the system. Within
constraints, the ``:``, ``boundto`` and ``in`` operators can be used to
describe the structure of the system. If a system instance created by the
completion described in the previous section violates one or more constraints,
no output file for that instance will be generated.

In the example above, it might be desired that the natural ``w`` plays some role
in the ``pc`` compartment. Then, only the system instances 2, 4 and 5 are
allowed. This can be achieved by extending the ``system`` block with a
constraint::

   system {
       w : Worker;
       p : Producer;
       pc : ProducerConsumerProtocol;
       p in pc;

       exists r : role. r boundto w & r in pc;
   }

Additionally, we can specify that a ``Worker`` plays at most one role by adding
the following constraint::

   forall worker : Worker, r1 : role, r2 : role.
       r1 boundto worker & r2 boundto worker => r1 = r2;

Note that constraints are ordinary Boolean expression, which means that also
:ref:`functions <sec-functions>` can be used within or as constraints. This
allows us to abstract commonly used constraints into functions. For example,
the constraint that ``w`` should play a role in ``pc`` can be generalized::

   function player_in(p : component, c : compartment) : bool =
       exists r : role. r boundto p & r in c;

Then, we can use this function to rewrite the constraint to::

   system {
       // ...

       player_in(w, pc);
   }
