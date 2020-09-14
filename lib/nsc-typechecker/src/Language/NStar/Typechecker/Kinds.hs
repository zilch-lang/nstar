{-|
  Module: Language.NStar.Typechecker.Kinds
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Kinds where

{-

    Because there is close to nothing about kind inference in N*,
    there probably won't be anything in this module.

    One thing that belongs here is the kind checking.
    So:
    - `a::s` needs `a: T8|Ta` and `s: Ts` and returns `Ts`
    - `{%rax: s}` needs `a: T8`
    - `sptr s` needs `s: Ts` and returns `T8`
    - `*a` needs `a: T8|Ta` and returns `T8` (on x64)

-}
