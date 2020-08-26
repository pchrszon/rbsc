.. _sec-labels:

Labels
======

Like PRISM models, RBL models can also contain labels to identify certain sets
of states. Labels are defined using the ``label`` keyword and must have type
``bool``, as illustrated in the following example::

   label "idle" = forall w : Worker. w.work = 0;
