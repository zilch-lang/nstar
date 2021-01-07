# N\*

[![stars](https://img.shields.io/github/stars/zilch-lang/nsc?color=%23fdaa33&style=for-the-badge)](https://github.com/zilch-lang/nsc/stargazers)    [![forks](https://img.shields.io/github/forks/zilch-lang/nsc?color=%23654321&label=Forks&style=for-the-badge)](https://github.com/zilch-lang/nsc/network/members)

N\* is a low-level typed assembly language meant to be used as a compiler target language.
It is very low-level, and features zero-cost abstractions such as structures, unions or even types.

## Compiling

Make sure that you have [stack](https://docs.haskellstack.org/en/stable/README/) in your path.

Then type `stack build` and `stack exec -- <command-line arguments>`.

The first command will build nsc, the compiler for N\*, and the second one will run it with the given command-line arguments.

## Hacking on the source code

The codebase is written almost entirely in Haskell at the moment, but a rewriting in Zilch is planned for when Zilch will be production-ready and complete enough to be used on its own.

Make sure to conform to the license before requesting any change!

## Code examples

N\* is not yet production-ready, thus example codes are lacking.
It is also pretty unstable, so examples in the [./examples](directory) will change at some point.

## License

&copy; Ghilain Bergeron (Mesabloo) and collaborators.
This work is licensed under the BSD-3 license.
