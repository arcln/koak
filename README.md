# koak

Koak is a Kaleidoscope compiler. The Kaleidoscope programming language is a constraint programming language embedding constraints into an imperative object-oriented language. It adds keywords always, once, and assert..during (formerly while..assert) to make statements about relational invariants. Objects have constraint constructors, which are not methods, to enforce the meanings of user-defined datatypes.

## Dependencies

Before using it, you should install some dependencies:
- [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
```bash
curl -sSL https://get.haskellstack.org/ | sh
```
- [LLVM](https://llvm.org/)
With Linux, you have to compile the library, [here is the process](http://www.linuxfromscratch.org/blfs/view/cvs/general/llvm.html).

## Usage

```bash
make
```

```bash
./koak source.kk
```

You can also run it in command prompt mode:
```bash
./koak
```

| Option          | Effect        |
| --------------- | ------------- |
| `--asm`         | display the generated ASM code |
| `--ast`         | display the generated abstract syntax tree |

## Example



## Features



    | Call Name [Expr]
    | Function Name [Expr] Type Expr
    | Extern Name [Type] Type Bool
    | If Expr Expr Expr
    | While Expr Expr
    | For Expr Expr Expr Expr


### Langage features

#### Variables

We declare variable like this:
```C
int foo = 42;
```

| Type | Value |
| ---- | ----- |
| `int` | `42` |
| `double` | `42.0` or `42.` |
| `string` | `"Here is a string."` |

For reassigning a variable
```C
foo = 0;
```

#### Operations

Operations can be perform on variables or values.

##### Operations on two values

```C
10 + 5;
```
| Operator | Effect |
| -------- | ------ |
| `+` | add values |
| `-` | substract values |
| `*` | multiply values |
| `/` | divide values |
| `==` | check if values are equals and return boolean |
| `!=` | check if values are not equals and return boolean |
| `>` | check if first value is greater than the second and return boolean |
| `<` | check if first value is lower than the second and return boolean |
| `>=` | check if first value is greater or equal than the second and return boolean |
| `<=` | check if first value is lower or equal than the second and return boolean |

##### Operations on single value

```C
-42;
```

| Operator | Effect |
| -------- | ------ |
| `-` | switch the value sign |
| `!` | switch the boolean value |

#### Functions

```C
def avg(a: int b: int): int
  int x = a + b:
  x / 2;
```

Functions prototypes are formatted like this `def NAME (ARGS): RETURN_TYPE`

After, we can chain expressions with the `:` operator before end with a semicolon.

Functions always return the value of the last expression.

Also, we can import function from the C standard library:

```C
using printf(string...): int;
```

This seems like a regular function prototype but `using` replace `def` keyword.

> `...` is for var args

### JIT

### Type inference
