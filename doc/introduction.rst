Introduction
============

rbsc is a tool for the verification and quantitative analysis of role-based
systems by means of probabilistic model checking. It provides a modeling
language that extends the input language of the probabilistic model checker
`PRISM <http://www.prismmodelchecker.org>`_ with role-related concepts. The
rbsc tool is not a probabilistic model checker. Rather, it translates a
role-based model into the PRISM language. Then, PRISM can be used for carrying
out the quantitative analysis.

A role-based model described in the role-based modeling language RBL consists of
a set of *components*. A component can either be a *natural*, a *role* or a
*compartment*. Naturals are the atomic components that serve as players for
roles. A compartment represents a context, in which a set of roles is acting in.
