.. _sec-type-decls:

Type declarations
=================

Each component (natural, role or compartment) in an RBL model has a component
type. This allows us to instantiate multiple components of the same type and to
specify constraints on the level of types (see
:ref:`system instantiation <sec-system>`).


Natural types
-------------

The simplest type is the natural type, which only has a name::

   natural type Worker;

The keyword ``natural`` is optional, but can be added to clearly state that
instances of this type are meant to be role players. In case the type is only
needed for technical reasons, e.g., to define a counter, omitting the
``natural`` keyword is more appropriate.


Role types
----------

A role type is declared similarly. However, a role type declaration must also
specify the possible player types. Note that players are not necessarily only
naturals, but other roles and compartments are allowed as well. In the following
example, the role type ``Consumer`` is defined. Instances of this role type can
be played by components of type ``Worker``. ::

   role type Consumer(Worker);


.. _sec-compartment-types:

Compartment types
-----------------

A compartment type declaration must include a list of role types that comprise
the compartment::

   compartment type ConsumerProducerProtocol(Producer, Consumer);

This type declaration states that an instance of ``ConsumerProducerProtocol``
must contain exactly one role instance of type ``Producer`` and one role
instance of type ``Consumer``.

It is also possible to define alternatives for the set of roles in a
compartment. The following declaration specifies that either a ``Producer`` or
a ``Consumer``, or both must be contained in the compartment::

   compartment type ConsumerProducerProtocol(Producer | Consumer | Producer, Consumer);

Furthermore, lower and upper bounds on the occurrence of roles can be
specified::

   compartment type ConsumerProducerProtocol(Producer, Consumer[1..3]);

Here, the compartment must contain at least one and at most three instances with
role type ``Consumer``. If lower and upper bounds are identical, the shorthand
notation ``Consumer[3]`` can be used.


Type set definitions
--------------------

If a certain set of component types is often used together, e.g., in
:ref:`function <sec-functions>` parameter types or in
:ref:`quantification <sec-quantification>` over components, it can be useful to
define an alias for the set of types. For instance, if there are component types
``SlowWorker`` and ``FastWorker``, a type name comprising both of them can be
defined as follows::

   typedef Worker = { SlowWorker, FastWorker };

Then, this type can be used, e.g., in a ``forall`` quantifier::

   forall w : Worker. w.work = 0
