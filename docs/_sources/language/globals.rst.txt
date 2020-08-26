.. _sec-globals:

Global variables
================

In addition to local variables in modules, an RBL model can also contain global
variables. The same :ref:`type <sec-variable-types>` restrictions as for local
variables apply. Globals are defined using the ``global`` keyword::

   global x : [0 .. 2];
   global b : bool init true;
