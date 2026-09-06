# ChangeLog for HHP(Happy Haskell Programming)

## 2026-09-07 v1.0.4

- Bug fix for reading symbols from Emacs Lisp.
- Properly specifying hs-source-dir: Previously, all hs-source-dirs
  were specified whenever there were multiple ones. If modules
  happened to share the same name and the paths were specified in the
  wrong order, syntax checks would fail. So, updating the
  configuration to specify only the minimum necessary hs-source-dirs.

## 2026-03-11 v1.0.4

- Updating Emacs Lisp.

## 2025-03-31 v1.0.3

- Supporting GHC 9.12

## 2023-03-17 v1.0.2

- Supporting GHC 9.6.

## 2022-09-15 v1.0.1

- Supporting GHC 9.4.

## 2022-05-30 v1.0.0

- Supporting GHC 9.x only.

## 2022-05-30 v0.0.4

- Injecting cabal flags via environment variable. [#13](https://github.com/kazu-yamamoto/hhp/pull/13)
- Update to handle sub-libraries in .cabal file. [#14](https://github.com/kazu-yamamoto/hhp/pull/14)

## 2020-07-21 v0.0.3

- fixing test due to hlint change.
- supporting GHC 8.10

## 2019-10-03 v0.0.2

- supporting Cabal 3.0 and GHC 8.8.

## 2019-06-21 v0.0.1

- setting PartialTypeSignatures and NamedWildCards for check
- Remap save-buffer to hhp-save-buffer [#6](https://github.com/kazu-yamamoto/hhp/pull/6)
- using "-fdefer-typed-holes" for check.

## 2018-12-18 v0.0.0

- First release
