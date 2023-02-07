## 0.5.0

_2023-02-08, Chris Martin_

The behavior of `(<>)` for the `Ini` type has changed
[#2](https://github.com/andreasabel/ini/issues/2)

- `<>` previously discarded all `iniGlobals`. Now it concatenates
  the globals from the two `Ini` values.

- When two `Ini` values contained `iniSections` with the same name,
  `<>` previously returned the section from the left value and
  discarded the section of the same name from the right value.
  Now it concatenates the sections of the same name.

Tested with GHC 7.0 - ghc-9.6.0.20230128.

## 0.4.2

_2022-07-26, Andreas Abel_

- Fail parsing if the input is not completely consumed [#30](https://github.com/chrisdone/ini/pull/30)
- Print global values as well [#28](https://github.com/chrisdone/ini/pull/28)

Tested with GHC 7.0 - 9.4.1 RC1.

## 0.4.1

_2019-01-02, Chris Done_

- Allow global section [#6](https://github.com/chrisdone/ini/issues/6)
