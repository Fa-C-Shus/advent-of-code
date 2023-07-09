setup project

create 1 file dune-project
```ocaml
(lang dune 2.9)
(name lets-get-real)
```

create 1-many dune modules
```ocaml
(executable
 (name sum)
 (libraries base stdio))
```

ctrl-d => end of file from stdin