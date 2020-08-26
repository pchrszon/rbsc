.. _sec-behavior:

Behavior definitions
====================

In RBL, the behavior of components is defined using an extension of PRISM's
guarded command language and is contained in *modules*. Modules are linked to
a component types, i.e., they provide the "implementation" for component types.
The behavior of a component instance is derived from this implementation upon
instantiation of the component type.


Linking modules and component types
-----------------------------------

A module is linked to a component type using the ``impl`` keyword. In the
following example, the module ``worker_impl`` is defined as the implementation
of the natural type ``Worker``::

   natural type Worker;

   impl Worker(worker_impl);

   module worker_impl {
       // module body
   }

A type can be implemented by multiple modules, which are combined using parallel
composition. For instance, we may split the functionality of the ``Worker`` into
the modules ``fetch`` and ``process``::

   impl Worker(fetch, process);

Note that modules can be reused in different ``impl`` definitions, i.e., a
module may be linked to multiple component types.

Usually, a component type is only implemented by a single module which is not
used anywhere else in the model. In this case, the module definition can be
directly combined with the ``impl`` definition. This also frees us from giving
the module a name. ::

   impl Worker {
       // module body
   }

In case there is no need to separate the type definition and the behavior
definition, an ever shorter notation can be used, where the type definition
is immediately followed by the module body::

   natural type Worker {
       // module body
   }


Module definitions
------------------

A module consists of two parts. The *variables* define its possible states and
*commands* specify the behavior by transitions between these states.

For example, the ``Consumer`` may have a variable that stores the amount of
remaining work to be processed::

   module consumer_impl {
       work : [0 .. 3] init 0;
   }

For a list of allowed variable types, see
:ref:`variable types <sec-variable-types>`.

A command comprises a *guard* and an *update*. If the guard evaluates to
``true`` in the current state the command may be executed, which updates the
local variables of the module. In the following example, the command processes
the remaining work by decreasing the ``work`` variable::

   module consumer_impl {
       work : [0 .. 3] init 0;

       [] work > 0 -> (work' = work - 1);
   }

Expressions within a module definition can refer to all
:ref:`constants <sec-constants>`, :ref:`global variables <sec-globals>`,
:ref:`functions <sec-functions>`,
:ref:`component instances <sec-component-instantiation>`, and local variables
of other components defined in the model. Local variables of components can be
accessed using the familiar dot-notation. For instance, the local variable of a
``Consumer`` component ``c`` can be read using the expression ``c.work``.

Each time a component type in instantiated, a new instance of its associated
modules is created with it. The keyword ``self`` refers to the corresponding
component instance of the module instance. Intuitively, the ``self`` keyword
is replaced by the component instance upon instantiation of the module.


Synchronization
---------------

Commands can be labeled with *actions*. Actions can be used to force two or
more modules to take their transitions simultaneously, i.e., to *synchronize*.
Actions are placed in the square brackets before a command::

   module consumer_impl {
       work : [0 .. 3] init 0;

       [dequeue] work = 0 -> (work' = 3);
   }

In the above example, the ``Consumer`` synchronizes with the buffer over the
``dequeue`` action to fetch the next work package.

There are some key differences between RBL and the PRISM language concerning
actions. In RBL, action names must be distinct from constant and variable names.
Similar to local variables, actions can also be qualified with a component
name::

   [self.dequeue] work = 0 -> (work' = 3);

In the above example, we have qualified the ``dequeue`` action with the
``Consumer`` instance. This allows us to have multiple ``Consumer`` instances in
the model that can ``dequeue`` work packages independently from each other. Note
that the instances will not synchronize with each other, since their ``dequeue``
actions are distinct.

Actions can be indexed similar to arrays::

   [self.dequeue[1]] work = 0 -> (work' = 1);
   [self.dequeue[2]] work = 0 -> (work' = 2);
   [self.dequeue[3]] work = 0 -> (work' = 3);

Here, we created actions for different work package sizes. Indexing of actions
is especially useful in conjunction with
:ref:`quantification <sec-quantification>` over integers, which
allows us to write the above example more concisely::

   forall w : [1 .. 3] {
       [self.dequeue[w]] work = 0 -> (work' = w);
   }

An action may have multiple indices, which are written using chained indexing
operators, e.g., ``act[2][5]``.

Actions are first-class objects in RBL, which means they can be assigned, used
as :ref:`function <sec-functions>` parameters and return values (see
:ref:`expression contexts <sec-expression-contexts>` for details). Furthermore,
the action in front of a command can actually be any expression which evaluates
to an action::

   forall i : [0 .. 3] {
       [if mod(i, 2) = 0 then tick else tock] s = i -> (s' = i + 1);
   }

Here, the ``tick`` action can be executed whenever ``s`` is even, and ``tock``
when ``s`` is odd.


.. _sec-meta-programming:

Meta-programming
----------------

Commands, updates and assignments can be generated using the ``forall``
:ref:`quantifier <sec-quantification>`. Consider the following excerpt from the
implementation of a FIFO buffer::

   type Buffer {
       cell : array SIZE of [0 .. 3] init -1;

       forall w : [1 .. 3] {
           forall c : Consumer {
               [c.dequeue[w]] cell[0] > 0 ->
                   forall i : [0 .. SIZE - 2] {
                       (cell[i]' = cell[i + 1])
                   } & (cell[SIZE - 1]' = 0);
           }
       }
   }

Two nested ``forall`` blocks are used to generate a command for each work
package size and ``Consumer`` role in the system. The inner ``forall`` block
generates the variable updates that shift the buffer contents.

RBL provides an ``if`` statement to conditionally generate commands, updates
and assignments. For example, if the FIFO buffer should only provide commands
for the ``Consumer`` instances contained in the ``pc`` compartments, the inner
``forall`` block can be extended as follows::

   forall c : Consumer {
       if c in pc {
           [c.dequeue[w]] // ...
       }
   }

There is a shorthand notation for a ``forall`` block which contains only an
``if`` block. The above example can be shortened to::

   forall c : Consumer. c in pc {
       [c.dequeue[w]] // ...
   }


Role behavior
-------------

Role components are special components that can be *played* by another
component. A role is played if it actively takes one of its transitions.
Binding a role to a player always preserves the base behavior of the player.
That is, if the role is not played the player behaves as before the binding. In
addition to synchronization with its player, a role may also *override* actions
of its player. Both possibilities are illustrated in the following example::

   natural type N {
       s : [0..2] init 0;

       [a] s = 0 -> (s' = 1);
       [b] s = 0 -> (s' = 2);
   }

   role type R(N) {
       t : [0..2] init 0;

       [a]          t = 0 -> (t' = 1);
       [override b] t = 0 -> (t' = 2);
   }

   system {
      n : N;
      r : R;
      r boundto n;
   }

In the example above, the role ``r`` and its player ``n`` may synchronize over
the ``a`` action. In this case, role ``r`` is played. If the role is not played,
``n`` may take action ``a`` alone without synchronizing with ``r``.

The action ``b`` is marked with the ``override`` keyword in the role's behavior
definition. When the role is played, it can execute the ``b`` action without
synchronizing with the ``b`` action of the player ``n``. Again, the player's
behavior is preserved in case the role is not played, which means ``n`` can
execute its ``b`` action if ``r`` is not played.

The following table lists the successor states of the initial state for actions
``a`` and ``b`` for the cases where role ``r`` is played and ``r`` is not
played.

=======  ==============  ================
Action   ``r`` played    ``r`` not played
=======  ==============  ================
a        s' = 1, t' = 1  s' = 1, t' = 0
b        s' = 0, t' = 2  s' = 2, t' = 0
=======  ==============  ================

Some actions may only be used for the synchronization between the player and
its role, i.e., no other "external" component synchronizes with this action.
It is often desirable that the player does not execute this action alone in
case the role is not played. To achieve this behavior, the keyword ``internal``
can be added in the player definition, for example::

   natural type N {
       [internal a] s = 0 -> (s' = 1);
       [b] s = 0 -> (s' = 2);
   }

Here, the player ``n`` can only execute action ``a`` together with ``r``, but
not on its own. Therefore, the transition to the successor state
(s' = 1, t' = 0) does not exist in this amended model.
