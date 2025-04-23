---
layout: default
title: About
rank: 1
---

# About

The [Happy Haskell Programming (HHP) package](http://hackage.haskell.org/package/happy-haskell-programming) includes the `hhpc` command, the `hhpi` command, the HHP library, and Emacs front-end.

The `hhpc` command and `hhpi` command are backend commands to enrich Haskell programming on editors.
`hhpc` and `hhpi` are based on the [HHP library](http://hackage.haskell.org/packages/archive/hhp/latest/doc/html/Hhp.html)
which is a wrapper of [GHC API](https://downloads.haskell.org/~ghc/latest/docs/html/) and [Cabal](http://hackage.haskell.org/package/Cabal).

Emacs front-end is an extension of [Haskell mode](https://github.com/haskell/haskell-mode). It enables to complete Haskell symbols and to browse documents of modules. Syntax error highlighting with GHC/Hlint is also integrated. Moreover, you are free from _import hell_.

## Supported GHC and Cabal

Version 1.x.x supports:

- GHC 9.4 - Cabal 3.8
- GHC 9.6 - Cabal 3.10
- GHC 9.8 - Cabal 3.10
- GHC 9.10 - Cabal 3.12
- GHC 9.12 - Cabal 3.14

Version 0.x.x supports:

- GHC 8.0 - Cabal 1.24
- GHC 8.2 - Cabal 2.0
- GHC 8.4 - Cabal 2.2
- GHC 8.6 - Cabal 2.4
- GHC 8.8 - Cabal 3.0
- GHC 8.10 - Cabal 3.2

## Copyright and license

Copyright of this package belongs to [IIJ Innovation Institute Inc](http://www.iij-ii.co.jp/en/).
This package is available under [BSD3 license](LICENSE).
