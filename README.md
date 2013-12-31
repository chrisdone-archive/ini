ini
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
> fmap (lookupValue "SERVER" "hostname")
       (parseIni "[SERVER]\nport: 6667\nhostname: localhost")
Right (Right "localhost")
```
