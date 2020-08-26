.. _sec-coordination:

Coordination
============

Usually not all possible role-playing is actually allowed. There may be some
static constraints, e.g., "role ``a`` and role ``b`` must not be played at the
same time", or temporal constraints, such as "role ``a`` must be played before
role ``b`` can be played". In order to monitor and constrain role-playing in the
model, one or more role-playing coordinators can be defined. A coordinator is
basically a special component, which can not only synchronize over actions, but
also over role-playing.


Role guards
-----------

Consider the following example containing roles ``a`` and ``b``::

   natural type N {
       s : [0..2] init 0;

       [act] s < 2 -> (s' = s + 1);
   }

   role type R(N) {
       t : [0..1] init 0;

       [act] t = 0 -> (t' = 1);
   }

   system {
       n : N;
       a : R; a boundto n;
       b : R; b boundto n;
   }

In order to enforce the first constraint (``a`` and ``b`` must not be played
together), the following coordinator can be added::

   coordinator {
       [act] [!(a & b)] true -> true;
   }

A coordinator command can contain an additional *role guard* after the action
label. A role guard is a Boolean expression over role instances defined in the
model. Intuitively, the command synchronizes both with the action ``act`` and
all role-playings satisfying the role guard. Therefore, the transition where
both ``a`` and ``b`` are played at the same time is blocked.

The following table lists all forms a coordinator command can have.

==========================  ==============================================
Command                     Effect
==========================  ==============================================
``[act] [role-guard] ...``  Synchronize both with ``act`` and role-playing
``[] [role-guard] ...``     Synchronize with role-playing for all actions
``[act] ...``               Synchronize only with ``act``
``[] ...``                  Internal action, no synchronization
==========================  ==============================================

Similar to regular components, a coordinator can have local state. This allows
us to specify the second constraint (role ``a`` must be played before role ``b``
can be played)::

   coordinator {
       played_a : bool init false;

       [] [a  & !b] true     -> (played_a' = true);
       [] [!a & !b] true     -> true;
       [] [a & b]   played_a -> true;
       [] [!a & b]  played_a -> true;
   }

Here, we set the variable ``played_a`` to ``true`` once the role ``a`` has been
played. Then, we only allow playing of ``b`` if ``played_a`` has been set. The
second command is necessary to allow transitions where neither role is played.


Explicit role alphabets
-----------------------

The alphabet of a role guard is implicitly defined by the roles it contains. It
is possible to make the alphabet explicit, which allows us to collapse the last
two commands into one::

   [] [b over [a, b]] played_a -> true;

The alphabet is defined as an array of roles after the ``over`` keyword. The
role guard shown above is evaluated over the roles ``a`` and ``b``. Since it
only states that ``b`` must be ``true``, ``a`` can be both ``true`` or
``false``. Hence, this command has the same effect as the last two commands in
the original coordinator.

If all commands of the coordinator should have the same alphabet, the alphabet
for the whole coordinator can be defined as follows::

   coordinator over [a, b] {
       // ...
   }


Coordinator partitioning
------------------------

There is an important difference in the semantics of coordinators and regular
modules. A coordinator is automatically partitioned into independent parts. Two
coordinator commands are independent if both their role alphabets and their
updated variables are disjunct. Independent commands can be executed together,
i.e., they can synchronize. Consider the following example::

   coordinator {
       [act] [a] true -> true;
       [act] [b] true -> true;
   }

Note that the commands of this coordinator are independent. Their role guards
have disjunct alphabets and none updates any variables. Since both are labeled
with the ``act`` action, they will synchronize. Thus, the above coordinator has
the same effect as the following coordinator::

   coordinator {
       [act] [a & b] true -> true;
   }
