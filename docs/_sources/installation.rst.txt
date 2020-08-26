Installation
============

rbsc runs on Windows, Linux and MacOS.


Prerequisites
-------------

For building rbsc from source, the tool ``stack`` must be installed. For
installation instructions, visit the
`stack website <https://www.haskellstack.org>`_.


Building from source
--------------------

In the projects root directory, which contains the ``package.yaml`` file, run
the following command to build the tool::

   stack build

The tools needed for building and all required dependencies will be downloaded
automatically.

Optionally, the rbsc binary can be copied to the local bin path by using the
command::

   stack install
