Constraints
===========

- based on first-order logic: quantification over components (instances)

Syntax
------

* relations
    - has type: `<instance> : <type>`
    - bound to: `<instance> boundto <instance>`
    - role contained in: `<instance> in <instance>`
* quantification
    - `forall <instance>. <constraint>` and `exists <instance>. <constraint>`
    - optional type annotation: quantification only over instances of given
      type: `forall <instance> : <type>. <constraint>`
* functions:
    - count role instances in compartment: `count(<type>, <instance>)`
