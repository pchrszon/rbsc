System Instantiation
====================

- role model defines component types, role types and compartment types
- system is constructed by creating instances of these types
- under-specified instantiations should generate all possible instantiations

Syntax
------

- the top level block is `system` (acts like the "global" compartment)
- each compartment definition is followed by an instantiation block that may
  reuse instances from outer blocks
- within each block, each name can be used only once
- normal form: all instantiations and bindings are on the top level,
  compartments only reference instance names

Example role model
------------------

- component types: `ComponentA`, `ComponentB`
- role types:
    - `Role1` can be played by `ComponentA` and `ComponentB`
    - `Role2` can be played by `Role1` and `ComponentB`
- compartment types: `CompartmentX` contains roles `Role1` and `Role2`

Example instantiations
----------------------

### Components

```
ca1 : ComponentA,
cb1 : ComponentB
```

- syntax: `instance : type`

### Roles

```
ca1 : ComponentA,

r1 : Role1,         // instantiate unbound roles
r2 : Role2,

r1 <- r2            // bind role r2 to r1
```

- syntax for binding: `player <- role`
- `r1` is not explicitly bound, therefore instantiations for all possible
  players (including `ca1`) are generated
- if a role should remain unbound, the syntax `_ <- r1` is used

### Compartments

```
ca1 : ComponentA,
cb1 : ComponentB,

r1 : Role1,
r2 : Role2,

ca1 <- r1,
cb1 <- r2,

cx : CompartmentX { r1, r2 }
```

- syntax for compartment construction: `instance : type { contained roles }`

### Inline instantiation

```
cx : CompartmentX {
    ca1 : ComponentA <- r1 : Role1,
    cb1 : ComponentB <- r2 : Role2
}
```

- instances can be defined and used at once
- definition of `cx` here is equivalent to definition above

### Anonymous instantiation

```
CompartmentX {
    ComponentA <- Role1,
    ComponentB <- Role2
}
```

- if instance name is not needed, instantiations can be nameless
- if type is used in position where instance name is expected, then a new
  instance is generated
- definition of the compartment here is equivalent to definition above (but
  without explicit instance names)

### Incomplete instantiations

```
cb1: ComponentB,

cx: CompartmentX { cb1 }
```

- no role instances are given within definition of `cx`
- since player `cb1` is referenced, it must be used in each instance
- what happens if referenced player cannot be used by any role?
- possible instantiations of `cx`:
    - `cb1 <- (Role1, Role2)`
    - `cx 
