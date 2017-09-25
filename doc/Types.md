Type System
===========

- each type of the source language has a corresponding Haskell type
- each Haskell type has a value-level representation that is only used for
  type-checking
- values of the Haskell types are used for evaluation of source language
  expressions


-----------------------------------------------------------------------
Source Language Type    Haskell Type        Haskell Type Representation
--------------------    ------------        ---------------------------
`bool`                  `Bool`              `TBool`

`int`                   `Integer`           `TInteger`

`double`                `Double`            `TDouble`

`array [l..u] of t`     `[t]` or `Array t`  `TArray t`

component type          `Component`         `TComponent`
- `natural type`        - type              - type name
- `role type`           - bound to          - supertype (natural,
- `compartment type`    - contained in        role, compartment)
                                            - local variable types
                                            - (players, contained)
-----------------------------------------------------------------------

