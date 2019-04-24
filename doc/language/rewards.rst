.. _sec-rewards:

Costs and rewards
=================

In order to reason about quantitative measures, such as expected energy
consumption, expected time or expected number of lost messages, an RBL model can
be augmented with costs and rewards. Rewards are defined in reward structures
and can be assigned to states, transitions or role playing.

Consider the following example::

   rewards {
       x = 2 := 1;
   }

Here, a reward of 1 is assigned to all states where ``x`` is 2. The reward does
not need to be constant, but can also depend on the model state::

   rewards {
       x = 2 := pow(y, 2);
   }

Rewards can be assigned to transitions by adding a (possibly empty) action
label, as shown in the following example::

   rewards {
       [act] true := 1;
       [] true    := 2;
   }

The first reward item assigns a reward of 1 to all transitions labeled with the
action ``act``. The second item specifies a reward of 2 for all transitions.

Rewards can also be assigned to certain role-playing. The syntax used here is
similar to that of :ref:`coordinator <sec-coordination>` commands::

   rewards {
       [] [a & b] true := 1;
       [act] [a] true  := 2;
   }

In this example, a reward of 1 is assigned to all transitions where the roles
``a`` and ``b`` are played. Role guards can also be combined with actions, as
shown in the second reward item.

The :ref:`meta-programming <sec-meta-programming>` constructs can also be used
in reward structures::

   rewards {
       forall w : Worker {
           [w.work] true := 1;
       }
   }

Here, a reward is assigned to the ``work`` actions of all ``Worker``
components.

Reward structure can be labeled with a name to define different cost measures::

   rewards "time" {
       [tick] true := 1;
   }

   rewards "energy" {
       [work] true := 2;
   }
