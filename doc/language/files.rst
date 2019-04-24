Model files
===========

By convention, the file extension ``.rbl`` is used for role-based models.

A model description may include comments. Comments are started by ``//`` and
last until the end of the line.

The modeling language provides an ``include`` directive for including the
contents of another model file in the current file, for example::

   include "constants.rbl"

The path to the include file is relative to the path of the file containing the
``include`` directive.
