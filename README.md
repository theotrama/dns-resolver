# Domain Name Resolver
A recursive DNS resolver written in Erlang.
## How to run
Go into src/ directory and start erl. Then run the resolver providing a domain.
```bash
cd apps/domain_name_resolver/src
erl
```

```bash
c(resolver).
resolver:run("google.com").
```

## Website
To run the browser resolver. Afterwards, go to http://localhost:8080/static.
```bash
rebar3 shell # from root directory
```
