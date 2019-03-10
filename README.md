# Koak

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

##### Declaration

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

##### Call

```C
printf("Average of 10 & 20: %d\n", avg(10, 20));
```

Calling a function is like a lot of common langages.

#### Control Flow

##### If/else
```C
string s = if 1 == 2 then "equal" else "not equal";
```

The `if` expression is formatted like this `if CONDITION then RETURN_TRUE else RETURN_FALSE;`.

##### While
```C
int a = 10;
while a > 0 do a = a - 1 : printf("loop\n");
```

The `while` loop is formatted like this `while CONDITION do EXPRESSIONS;`.

##### For
```C
for int i = 0, i < 10, i = i + 1 in printf("loop %i\n", i);
```

The `for` loop is formatted like this `for INITIALIZATION, CONDITION, INCREMENTATION in EXPRESSIONS;`.

### JIT

Just-in-time (JIT) compilation is a way of executing computer code that involves compilation during execution of a program – at run time – rather than prior to execution. Most often, this consists of source code or more commonly bytecode translation to machine code, which is then executed directly.

JIT compilation is accessible via the command prompt interface.

### Type inference

Type inference refers to the automatic detection of the data type of an expression in a programming language.

Here are a few examples:
```C
using printf(string ...): int;

printf("%f\n", 42. * 42);
printf("%f\n", 42 * 42.);
printf("%d\n", (1 == 1) * 42);
printf("%d\n", 42 * (1 == 1));
printf("%f\n", (1 == 1) * 42.);
printf("%f\n", 42. * (1 == 1));
printf("%f\n", (1 == 1) * 42 * 42.);
printf("%f\n", (1 == 1) * 42. * 42);
printf("%f\n", 42 * (1 == 1) * 42.);
printf("%f\n", 42. * (1 == 1) * 42);
printf("%f\n", 42. * 42 * (1 == 1));
printf("%f\n", 42 * 42. * (1 == 1));
```

## Kaleidoscope Grammar

Our Kaleidoscope implementation use the following BNF grammar.

```
stmt <- kdefs * # eof
kdefs <- extern | 'def ' defs ';' | expressions ';'
extern <- 'using' identifier '(' type* ('...')? ')' ':' type ';'
defs <- prototype expressions
prototype <- identifier prototype_args
prototype_args <- '(' ( identifier ':' type ) * ')' ':' type
type <- 'int ' | 'double ' | 'void '
expressions <- for_expr | if_expr | while_expr | expression (':' expression ) *
for_expr <- 'for ' expression ',' expression ',' expression 'in ' expressions
if_expr <- 'if ' expression 'then ' expressions ('else ' expressions ) ?
while_expr <- 'while ' expression 'do ' expressions
expression <- var_decl | var_assign | const_str | unary (# binop ( unary | expression ) ) *
var_decl <- type var_assign
var_assign <- identifier '=' expression
const_str <- '"' (.!'"')* '"'
unary <- # unop unary | postfix
postfix <- primary call_expr ?
call_expr <- '(' ( expression (',' expression ) *) ? ')'
primary <- identifier | literal | '(' expressions ')
identifier <- [a - zA - Z ][ a - zA - Z0 -9]*
dot <- '.' !'.'
decimal_const <- [0 -9]+
double_const <- ( decimal_const dot [0 -9]* | dot [0 -9]+ )
literal <- decimal_const | double_const
```
