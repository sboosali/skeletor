# `skeletor`

`skeletor` is a scaffolding program which supports *buildable scaffolds*.

## *What it does*

*Buildable Project Skeletons* (a.k.a. *Testable Templates*) means:

- 

## *How it works*

`skeletor` templates can have valid syntax for any format, and thus `skeletor` projects can be built and tested (like normal projects).



















## Usage

### Using `skeletor` templates


## Development

### Writing `skeletor` templates





## *Skeleton Files*

### Variables

there are *two kinds* of variables in the `skeletor` tempting language:

1. *Template* Variables: What the *author* of a template defines, and writes throughout the template files of a project template.
2. *Configuration* Variables: What the *user* of a template writes (e.g. on the command line, in a config file, under an environment variable, etc) to invoke `skeletor new`.

#### Configuration Variables

For example, let's assume there is a configuration variable:

* which is named `package`
* whose type is `String`.

This *`package`* configuration variable can be specified:


* on the command line, as the option `-p package=dictation-server`.
* within a config file, 
* in an environment variable, 
* 
* 


#### Template Variables

For example, let's define a template variable for the (aforementioned) `package` configuration variable.

aliases:

* `xxx-package-xxx`
* `__PACKAGE__`

#### 








FAQ

1. compiling templates?

2. context-free grammars for Configuration String Variables?

alternatives for Refinement Types of Configuration String Variables:

* arbitrary Haskell `Predicate`
* a Regular Expression (a `String`).
* a Context-free Grammar (in BNF, at some `FilePath`).

e.g. for Haskell Identifier:

* a Regular Expression (a `String`):

    `"[[:alpha:]_][[:alnum:]'_]*"`

* a Context-free Grammar (for *Identifiers and Operators*):

    ``` bnf
    varid	->	(small {small | large | digit | ' })<reservedid>
    
    conid	->	large {small | large | digit | ' }
    
    reservedid	->	case | class | data | default | deriving | do | else
    |	if | import | in | infix | infixl | infixr | instance
    |	let | module | newtype | of | then | type | where | _
    ```

* arbitrary Haskell `Predicate`:

    `(\s -> _)`

3. Builtin `(Predicate String)` for Haskell Syntax.

most interpolatable:

* *Haskell98* Identifier — e.g. `isn't_A1`.Regexp: `"[[:alpha:]_][[:alnum:]'_]*"`.
* Haskell98* Module (a *module path* of *module identifiers*) — e.g. `Example.Module`. Regexp: `"\\(?:[[:upper:]][[:alnum:]'_]*\\.\\)*[[:upper:]][[:alnum:]'_]*""`. 
* 
*Cabal* Package — e.g. `example-package`. Regexp: `"[-[:alnum:]]+"`. 

(less interpolatable):

* *Haskell98* Value Identifier — e.g. `x_`. Regexp: `"[[:lower:]_][[:alnum:]'_]*"`. 
* *Haskell98* Type/Constructor/Class Identifier — e.g. `T_`. Regexp: `"[[:upper:]][[:alnum:]'_]*"`. 

BNFs (*Haskell98* Specification)...

http://www.hck.sk/users/peter/HaskellEx.htm



Regexes (`haskell-mode`)..


https://github.com/haskell/haskell-mode/blob/master/haskell-lexeme.el

`haskell-lexeme.el`:

``` elisp

;;----------------------------------------------;;

(dolist (KEY (string-to-list "!#$%&*+./<=>?@^|~\\-:"))
    (modify-category-entry KEY ?P)))

;;----------------------------------------------;;

(defconst haskell-lexeme-modid


  "[[:upper:]][[:alnum:]'_]*"

  "Regexp matching a valid Haskell module identifier.

Note that GHC accepts Unicode category UppercaseLetter as a first
character. Following letters are from Unicode categories
UppercaseLetter, LowercaseLetter, OtherLetter, TitlecaseLetter,
ModifierLetter, DecimalNumber, OtherNumber, backslash or
underscore.")


;;----------------------------------------------;;

(defconst haskell-lexeme-id

  "[[:alpha:]_][[:alnum:]'_]*"

  "Regexp matching a valid Haskell identifier.

GHC accepts a string starting with any alphabetic character or
underscore followed by any alphanumeric character or underscore
or apostrophe.")

;;----------------------------------------------;;

(defconst haskell-lexeme-sym


  "\\cP+"

  "Regexp matching a valid Haskell variable or constructor symbol.

GHC accepts a string of chars from the set
[:!#$%&*+./<=>?@^|~\\-] or Unicode category Symbol for chars with
codes larger than 128 only.")

;;----------------------------------------------;;

(defconst haskell-lexeme-idsym-first-char


  "\\(?:[[:alpha:]_]\\|\\cP\\)"

  "Regexp matching first character of a qualified or unqualified
identifier or symbol.

Useful for `re-search-forward'.")

;;----------------------------------------------;;

(defconst haskell-lexeme-modid-opt-prefix

  (concat "\\(?:" haskell-lexeme-modid "\\.\\)*")

  "Regexp matching a valid Haskell module prefix, potentially empty.

Module path prefix is separated by dots and finishes with a
dot. For path component syntax see `haskell-lexeme-modid'.")

;;----------------------------------------------;;

(defconst haskell-lexeme-qid-or-qsym

  (rx-to-string `(: (regexp ,haskell-lexeme-modid-opt-prefix)
                    (group (| (regexp ,haskell-lexeme-id) (regexp ,haskell-lexeme-sym)
                              ))))

  "Regexp matching a valid qualified identifier or symbol.

Note that (match-string 1) returns the unqualified part.")

;;----------------------------------------------;
```

















## Keywords

- *Project scaffolding*
- *Project skeletons*
- *Templating language*
- **
- **
- **
- **
- *Haskell*

