# java 8 parser

This project finds occurences of methods, classes and packages in java-files.

There are two stack-projects, stack.yaml is used for development and has only the lib-module. Just run `stack setup` and `stack install` to use it. Stack-GHCJS.yaml uses the ghcjs compiler and compiles to javascript. The interface to interact with Javascript is defined in the client module. Prepend the commands with `STACK_YAML=stack-GHCJS.yaml`, then run `stack setup` and `stack install`. If `stack install` fails you have to run `stack build alex` first.