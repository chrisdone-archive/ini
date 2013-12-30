ini
=====

Quick and easy configuration files in the INI format for Haskell.

Format rules and recommendations:

* The `:` syntax is space-sensitive.
* Keys are case-sensitive.
* Lower-case is recommended.
* Values can be empty.
* Keys cannot contain `:`, `=`, `[`, or `]`.
* Comments are not supported at this time.

An example configuration file:

    [SERVER]
    port=6667
    hostname=localhost
    [AUTH]
    user: hello
    pass: world
    salt:

Parsing example:

``` haskell
> parseIni "[SERVER]\nport: 6667\nhostname: localhost"
Right (Ini {unIni = fromList [("SERVER",fromList [("hostname","localhost"),("port","6667")])]})
```
