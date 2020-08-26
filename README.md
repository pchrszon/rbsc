rbsc
====

rbsc is a tool for the verification and quantitative analysis of role-based
systems by means of probabilistic model checking. It provides a modeling
language that extends the input language of the probabilistic model checker
[PRISM](http://www.prismmodelchecker.org) with role-related concepts.


Installation
------------

### Download

Binaries for Linux and macOS are provided on the
[Releases](https://github.com/pchrszon/rbsc/releases) page. Extract the binary
from the archive and move it to a suitable location (preferably some location
that is contained in your `PATH` variable). No further installation steps are
necessary.

Note that the macOS-binary is not signed. Therefore, you may have to explicitly
allow the program to run in the security settings.


### Building from source

For building rbsc from source, the tool `stack` must be installed. For
installation instructions, visit the
[stack website](https://www.haskellstack.org).

**(optional)** To generate the user documentation,
[Sphinx](https://www.sphinx-doc.org) must be installed.

In the projects root directory, which contains the `package.yaml` file, run
the following command to build the tool:

    stack build

The tools needed for building and all required dependencies will be downloaded
automatically.

Optionally, the rbsc binary can be copied to the local bin path by using the
command:

    stack install

### Generating the documentation

The developer documentation can be built using the command:

    stack haddock

To build the user documentation, go to the `doc` directory and run the following
command:

    make html

For other documentation formats, consult the Makefile or the Sphinx
documentation.
