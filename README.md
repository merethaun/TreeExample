# Parsing Randomness
**Seminar Probabilistic Programming** RWTH, summersemester 2023

Author: Meret Unbehaun, Advisor: Phillip Schroer

This repository is supposed to demonstrate the ideas from the paper [TODO](Add original paper) on the example of binary Boolean trees.

## Binary Boolean trees
The trees will have the following elements:
- `N` stands for a node
  - A Node has 2 children (other tree)
  - and a Boolean value assigned (`0` or `1`)
- `L` stands for leaf

An example could look like this:
```haskell
     N: 0 
   /     \
  L      N: 1
       /     \
      N: 0    L
    /     \
   L       L
```

## Implementations
First of all, the concept of a Free Generator (the correct data type implementation) is programmed according to [TODO].
- This also includes the interpretations, here as a Generator and as a Parser

This data structure is then concretely implemented for the tree example.


## Execution
The whole project is represented in a Jupyter notebook with the following Haskell kernel implementation utilised [TODO].

Either look at the already compiled file, or follow the setup in the linked repository and execute the script yourseld locally.