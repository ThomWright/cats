# cats

Coupling/cohesion Analysis for TypeScript

## Developing

```bash
# build
stack build

# run
stack exec -- cats --help

# format source files
hfmt -w
```

## TODO

- given a directory
- recursively, for each .js(x)/.ts(x) file
  - read each import/require
- make a map from filepath to list of imports

## Background

[Package by feature, not layer](http://www.javapractices.com/topic/TopicAction.do?Id=205)

[Measuring Coupling and Cohesion: An Information-Theory Approach](http://www.sdml.cs.kent.edu/library/Allen99.pdf)

- nodes=files, modules=directories
- analyse intramodule coupling
- analyse cohesion of a module
- analyse intermodule coupling

- > Regular patterns are easy for designers to remember, but counts do not reflect this. According to information theory, regular patterns have low information content, because the symbolic content can be compactly described [14].
- > Entropy is the average information per node
- > Intramodule coupling and cohesion are measures of different attributes of relationships within modules; intramodule coupling is a quantity of bits, but cohesion is a normalized fraction.

Coupling `C` of a subgraph `S` with `n + 1` nodes, where all nodes are fully connected:

`C(S(n)) = (n - 1)log(n + 1) for n > 2`
