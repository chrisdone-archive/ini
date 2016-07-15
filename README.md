ini [![Hackage](https://img.shields.io/hackage/v/ini.svg?style=flat)](https://hackage.haskell.org/package/ini)
=====

Quick and easy configuration files in the INI format for Haskell.

Format rules and recommendations:

* `foo: bar` or `foo=bar` are allowed.
* The `:` syntax is space-sensitive.
* Keys are case-sensitive.
* Lower-case is recommended.
* Values can be empty.
* Keys cannot contain `:`, `=`, `[`, or `]`.
* Comments must start at the beginning of the line with `;` or `#`.

An example configuration file:

``` ini
# Some comment.
[SERVER]
port=6667
hostname=localhost
[AUTH]
user=hello
pass=world
# Salt can be an empty string.
salt=
```

Parsing example:

``` haskell
> parseIni "[SERVER]\nport: 6667\nhostname: localhost"
Right (Ini {unIni = fromList [("SERVER",fromList [("hostname","localhost")
                                                 ,("port","6667")])]})
```

Extracting values:

``` haskell
> parseIni "[SERVER]\nport: 6667\nhostname: localhost" >>=
  lookupValue "SERVER" "hostname"
Right "localhost"
```

Parsing:

``` haskell
> parseIni "[SERVER]\nport: 6667\nhostname: localhost" >>=
  readValue "SERVER" "port" decimal
Right 6667
```

Import `Data.Text.Read` to use `decimal`.

## Related packages

[`ini-qq`](https://hackage.haskell.org/package/ini-qq) provides a quasiquoter for INI.
