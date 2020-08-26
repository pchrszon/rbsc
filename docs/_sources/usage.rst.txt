Running rbsc
============

Basic usage
-----------

rbsc can be run from the command line and must be invoked with the path to
a role-based model file::

   rbsc my_model.rbl

The model will then be translated into a standard PRISM model. By default, the
PRISM file will be written to ``out.prism``. The ``-o`` option can be used to
specify a different path::

   rbsc my_model.rbl -o my_model.prism

If the model defines more than one system instance, one PRISM file for each
instance is generated and an index is added to each output file. For instance,
if ``my_model.rbl`` defines two system instances, the command shown above will
generate the files ``my_model_0.prism`` and ``my_model_1.prism``.


Options
-------

rbsc provides a number of command line options listed below.

``-m`` or ``--multi-actions``
   Translates the model into a PRISM model with multi-actions.

``-c`` or ``--const``
   Defines the value of a constant defined in the model, for example::

      rbsc my_model.rbl -c x=2

   Multiple constant value can be specified by repeating the ``-c`` option::

      rbsc my_model.rbl -c x=2 -c y=5

   Any valid RBL :ref:`expression <sec-expressions>` can be specified after
   the ``=`` sign. However, it may be necessary to quote the expression, since
   some of the contained symbols may have a special meaning in the shell::

      rbsc my_model.rbl -c 'x=[5*y, z+2]'

``--export-systems <FILE>``
   Exports the completed :ref:`system definition <sec-system>` to the given
   file. If multiple systems have been instantiated, an index is added to the
   file name automatically.

``--export-diagrams <FILE>``
   Exports a diagram that shows all component instances defined in the model,
   with arrows for the role bindings. Roles belonging to a compartment are drawn
   in the same box as the compartment. The diagram is exported in the Graphviz
   DOT format. If multiple systems have been instantiated, an index is added to
   the file name automatically.

``--print-consts``
   Prints out the values of all constants defined in the model.

``--recursion-depth <INT>``
   Sets the maximum number of recursive calls when evaluating
   :ref:`functions <sec-functions>`. The default limit is 100.

``--page-width <INT>``
   Sets the number of columns that is used for pretty printing the PRISM model.
   The pretty printer will try to stay below the specified page width by
   inserting newlines into the source code. The default is 120.

``--no-color``
   Disables the use of color in error messages and warnings.

``--no-warn``
   Silences all warnings.

``--warn-no-sync``
   Shows a warning for all actions in the model that are only used in a single
   module, i.e., actions that never synchronize.

``-v`` or ``--verbose``
   Enables verbose output.
