# TODO 

<!----------------------------------------------->

## `skeletor`

### 

naming: defaults.ini vs config.ini
"config" abbreviates configuration.
"defaults" implies everything can be overwritten (which should be true anyway).

### 

filepaths are expanded. i.e.:

* `~` — expands to `$HOME`
* `.` — expands to the *current file*'s directory (i.e. its `dirname`), NOT the present working directory (i.e. the `pwd`).
* `${...}` — environment variables are expanded. this includes `${SKELETOR_DATA}` and `${SKELETOR_CONFIG}`. all environment variables referenced in filepaths are accessed: simultaneously; near the start of the program invocation, after the configuration has been resolved (in particular, `${PWD}` SHOULD always be the directory from which the `skeletor` program was invoked). 

e.g. in this `defaults.ini`:

```
project=${SKELETOR_HASKELL_DATA}/${USER}-default
```

`${SKELETOR_HASKELL_DATA}/${USER}-default` might expand to the `~/.local/share/skeletor/haskell/sboo-default` directory.

<!----------------------------------------------->

## `skeletor-haskell`

### 

`skeletor-*` are subcommands of `skeletor`.

c.f. `git`, where `git-annex` is a third-party program, which can be invoked by `git` itself, i.e. as `git annex`, and which integrates (or should) with `git`'s completion.

### 

<!----------------------------------------------->

## 