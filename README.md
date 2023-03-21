# The Little Language That Could

qdbp is small. Really, really small. The language has just 2 keywords and 7 core constructs(along with full type inference and a little bit of syntax sugar to sweeten the deal). In fact here is a small program, implementing a basic boolean object, that demonstrates *every single primitive* of the language:
```ocaml
true := {
  BoolVal [#True{}]
  If [then else|
    self BoolVal.
      True? [then]
      False? [else].
  ]
  Negate [
    self If
      then: { self BoolVal[#False{}] }
      else: { self BoolVal[#True{}] }.
  ]
}
false := (true Negate)
false Negate. Negate. Negate. Negate.
```
As a comparison, Go 1.20 has 25 keywords, Python 3.10 has 38, C++14 has 84, and Lua 5.4 has 22. Neither keyword nor feature count are sufficient to judge language simplicity, but they are good approximations, and the magnitude of the difference between qdbp and its closest competitor is striking. Though the code snippet above may seem unfamiliar, understanding those 15 lines of code, along with learning a little bit of syntax sugar, is all that is required to understand the *entire* language.

Of course, just being small and simple is not sufficient. If it were, the world would run on [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck). qdbp's beauty comes from its ability to compose its small set of primitives to express complex abstractions. [Here](#Demo) is a list and demonstration of features other languages have that qdbp can naturally emulate. The list includes

- Infinite lists
- All sorts of loops
- Inheritance
- Iterators
- Operators
- Functional programming Patterns(list, map, filter, etc)
- Interfaces
- Modules
- Domain specific language creation, despite not having macros

That qdbp's core feature set can be so naturally wielded to express complex abstractions is testament to the elegance of the language. The goal of qdbp is to be the distillation of programming into only its necessary components, and in doing so, become greater than the sum of its parts. This document demonstrates how we have achieved this goal.

# Table of Contents
- [Tutorial](#Tutorial)
- [Rationale](#Rationale)
- [Demo](#Demo)
- [Limitations](#Limitations)
- [Grammar](#Grammar)

# Tutorial
```ocaml
(*
  This is a multiline comment
  (* They can nest :) *)
*)
; And here is a single line comment

(* Variables, ints, and strings *)
three := 3
hello_world := "Hello, world!"
y := "some text"
(* variables are shadowed *)
y := 3 ; y is now 3

(* We can bundle data together using prototype objects
   A prototype is a collection of labeled methods *)
prototype := {
  Method1 [3] (* A method that just returns 3 *)
  (* This method takes an argument and returns it *)
  Method2[arg1 | arg1 ]
  (* All methods take in implicit self argument that is always set to the value of
     the prototype it is in. Method3's return is equivalent to `prototype`*)
  Method3[ self ]
}
empty_prototype := {}

(* We can invoke methods on prototypes. Note the period at the end *)
three := prototype Method1.
(* Methods with arguments must have the names specified *)
four := prototype Method2 arg1: 4.

(* qdbp has no concept of functions in the traditional sense. However,
   we can use a prototype with a single label to simulate a function. By
   convention, we use the label `!` *)
identity_fn := {![x| x]}
(* From now on, we will refer to anything with the label `!` as a function *)
(* We can invoke the function by calling the method `!` *)
five := identity_fn! x: 5.

(* Methods within a prototype can be overloaded by parameter names *)
print_args := {
  ![arg1 | arg1 Print.]
  ![arg1 arg2 | 
    ignore := arg1 Print.
    arg2 Print.
  ]
}
(* qdbp has no sequence expression. To evaluate multiple expressions, we just assign
   them to successive variables *)
ignore := print_args! arg1: "Hello World\n".
ignore := print_args! arg1: "Hello " arg2: "World".

(* qdbp has no type annotations. Instead, it infers types.
   For example, if `arg1` to `print_args` doesn't have a `Print` method, the 
   program will refuse to compile. Therefore, `print_args! arg1: {}.` will fail. *)

ignore := print_args! arg1: 3. (* `print_args` is generic *)

(* To be specific, variable names must start with a lowercase letter.
   Method names within a prototype must start with an uppercase letter or a symbol.
   We can use method names with symbols to emulate operators. For example, int
   objects have a `+` method with a single argument, `val` *)
five := 3 + val: 2.

(* Here is some syntax sugar that makes common prototype patterns less verbose *)
(* To make "functions" easier to define, this: *)
my_fn := { ![x| x] }
(* is equivalent to this: *)
my_fn := {x | x} 
(* In other words, to make an object with a single method `!`, we can omit the
   `[`, `]`, and `!` *)
(* Also, if we omit the name of the first argument, it is automatically assumed
   to be `val` *)
five := 3 + val: 2. (* is equivalent to *) five := 3 + 2.
(* Periods are optional when invocation is surrounded by parenthesis *)
three := identity_fn! x: 3. (* is the same as *) three := (identity_fn! x: 3)

(* Variables are all immutable. However, you can copy a prototype and change
   one or more of its methods *)
small_circle := {
  Radius[3]
  Diameter[(self Radius) * 2.]
}
(* Same as `small_circle` but `Radius` now returns 6 *)
big_circle := {
  small_circle
  (* `Radius` still needs to have the same type as the original `Radius`*)
  Radius[6]
}
twelve := big_circle Diameter.

(* Unfortunately we don't have doubles yet, but stay tuned! *)
pi := 3

(* Prototypes can also be extended with new methods *)
big_circle := {
  big_circle
  Area[(self Radius) * (self Radius). * pi.]
}
one_hundred_eight := big_circle Area.

(* Most of the time, the compiler can disambiguate between whether to extend
   or replace. The one exception is when the original's prototype's field list
   is dependent on a parameter. For example, *)
fn := {x | 
  {
    x
    Method[val | val]
  }
}
(* The compiler can't disambiguate whether or not to replace the `Method` field in `x`
   or to extend `x`. In situations like this, the compiler will always expect that
   the original prototype contains the field and so will replace it. *)

(* We also can tag an object *)
object := {}
tagged_object := #TagName object
(* In qdbp, everything is right associative *)
tagged_object := #Tag #Tag #Tag {} (* parsed as (#Tag (#Tag (#Tag {}))) *)
(* An example application of this is to represent booleans *)
true := #True{}
false := #False{}

(* We can pattern match on tagged objects. For example, this is a function that
   negates a bool. Like method invocation, notice the `.` at the end *)
negate := {val |
  val
    True? [#False{}]
    False? [#True{}].
}
(* The type system will ensure that the pattern matching is exhaustive. For example,
   if we don't handle the `#False` case for a value that could be `#False`, the program
   won't compile *)

(* Another application of tagged objects is error types. Here is a safe
   division by 0 function *)
safe_divide := {a b|
  b = 0.
    True? [#Error{}]
    False? [#Ok a / b.].
}
two := safe_divide! a: 4 b: 2.
ignore := 
  two
    Ok? [val| 
      val Print.
    ]
    Error? [
      "Error\n" Print.
    ].
(* Like any language, we want to be able to split our programs across multiple
   files. Each qdbp program is a single expression that returns a value. 
   `@filename`'s result is the result of evaluating `filename.qdbp`. Filenames
   can only have letters, `/`, `_`, and `.` *)

empty_list := @list
intlist := empty_list Append 3. Append 2.
(* qdbp does not support cyclic imports. For example, this file cannot import itself
   However, a check for that is a todo. Currently, the compiler will just
   loop infinitely *)

(* We can also call functions in the target language. See `./int.qdbp` for example *)
(* The return type of the function must be encoded in the function name.
   It can either end with `_int`, `_string`, or `_bool` *)

(* To end a program abruptly, the special keyword ABORT will kill it *)
(* The "return type" of the `ABORT` changes according to the context *)
get_val := {val|
  val
    Ok? [val| val]
    Error? [ABORT.].
}

(* The value of the literal 3 is actually the result of evaluating `@int`, 
   and then invoking `!` method of the result, passing in an object of type `int64`
   in the target language* to the `val` parameter. In other words,  *)
  three := 3
  (* expands to *)
  three := @int! val: 3.(* Where this `3` has type `int64` in the target language *)
(* This allows users to have complete control over every aspect of their code,
   down to the methods of literals.
   A similar strategy is used for strings, doubles, etc *)

(* If you were to import this file from another file, the result would be this string*)
" This concludes the language reference and tutorial "
```
# Rationale
qdbp has a lot of differences from other languages. This section explains some of the rationale behind the starker ones.
## Syntax
The syntax of qdbp is what immediately stands out first. Although initially unfamiliar, it is small enough that it can be picked up quickly.
### Method Invocation
The method invocation syntax, inspired by smalltalk, is qdbp's biggest syntactic variation compared to other languages. While most languages have
```c++
foo.Bar(a, b)
```
qdbp has
```ocaml
foo Bar arg1: a arg2: b.
```
There are a few reasons for this. 
#### Documentation
Forcing users to specify the names of arguments makes it easier to read and understand their code. Consider the following two snippets:
```ocaml
rectangle Make width: 3 height: 4.
```
versus
```c++
rectangle.Make(3, 4)
```
In the first example, we can clearly see what the `3` and `4` mean, whereas in the second example, we have to look at the definition of `rectangle.Make` to understand what the arguments mean.
#### Overloading
Overloading by type would be tricky to implement and reason about in a language like qdbp with a structural type system. Overloading by parameter name, which is only possible with named arguments, is much easier. Having and overloading on named arguments allows us to write code like:
```ocaml
circle1 := circle Make radius: 3.
circle2 := circle Make area: 15.
circle3 := circle Make diameter: 4.
```
#### Operators
Operators are notoriously hard to do well. In particular, they often lead to programmers having to memorize a large number of precedences, introducing a lot of mental overhead. As a result, qdbp does not have operators. However, operator-like syntax like below is natural and unambiguous in qdbp because of its method invocation syntax. 
```ocaml
(3 + 4) * 2. / 18.
```
#### "Extensibility"
qdbp is not extensible. It has no macros or facilities for compile-time metaprogramming. However, its method invocation syntax makes the language feel extensible. For example, we can easily add an equivalent to the `if` construct to the language
```ocaml
if := {val then else|
  val
    True? [then!.]
    False? [else!.].
}
```
and use it like so
```ocaml
if! condition: ...
  then: {
    ...
  }
  else: {
    ...
  }.
```
Similarly `while`, `for`, `switch`, monads, iterators, `defer`, etc can all be implemented as objects and used naturally. We provide some examples of this below and [here](#demo)

The variety of constructs that the syntax makes possible and natural to implement extends beyond normal general purpose language features. qdbp's syntax makes domain specific language(dsl) implementation easy. For example, there currently is a work in progress dsl for a build system using qdbp that looks like this:
```ocaml
my_project
  AddLibDirectory "./lib".
  AddTestDirectory "./test".
  AddExecutable "bin/main.qdbp".
  SetOptimizationLevels performance: 3 size: 2.
```
Users of this build system don't need to learn a new syntax and developers don't need to implement a parser or macros - it can all be done naturally in qdbp. 

### `.`, `@`, `#` and `?`
These symbols were all picked because their linguistic meaning corresponds to their meaning in qdbp.

- Much like periods end sentences in the English language, they end method invocation and pattern matching expressions
- Similarly, pattern matching can be thought of like asking questions about tagged objects. Hence the `?` symbol
- When we import a file, we get the expression at(`@`) the file.
- `#` is used for making tagged objects because `#` is the hash*tag* symbol

### `[]`
We chose
```ocaml
MethodName[arg1 arg2 ...| body]
```
as opposed to
```c++
MethodName(arg1, arg2, ...) {
  body
}
```
because the former is simpler and more concise.

The decision to make tagged object pattern matching have similar syntax to methods was intentional. Tagged objects are similar to prototypes except for, rather than expressing a "this *and* this" relationship, they express a "this *or* this" one. Their similarity in syntax reflects their similarity in semantics.
## Structural Type System
qdbp uses a simple yet powerful type system that is often referred to as "static duck typing." The type system(described in detail [here](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/scopedlabels.pdf)) is based on the concept that types of objects are defined by what they can do, not their name. This allows qdbp to be as easy to use as dynamically typed languages while retaining the safety of static typing
## Language Constructs
### The role of methods
Unlike other languages, objects in qdbp are only comprised of methods. Prototypes can't have "regular data" fields. This is because "regular data" fields are trivially emulatable with methods that just return the data. Thus, there was no need to add them to the language.
### Literals as Libraries
One of qdbp's most unique features is that its literals are implemented as libraries rather than built into the language. In practice for most users, this makes no difference because they can just use an existing library. However, this allows programmers to easily inspect the behavior of literals and change their behavior because there is no one size fits all, even for common operations like division(should division by `0` abort or return `#Error{}` or return `infinity`?). 
### The Lack Of
Many of qdbp's design decisions were made using the following principle: "If a construct can be emulated with another existing feature, it should not be added to the language." The converse, "A construct can be added to the language if it cannot be emulated with another existing feature," was also used to guide design decisions.
#### (switch/for/while/if/defer/...)
Because all of these constructs are naturally implementable as libraries as shown [here](demo.md), in keeping with the guiding principle above, they are not part of the language primitives. 
#### Classes
Classes can be emulated as functions that return prototypes, removing the need to add them separately
#### Inheritance
Inheritance introduces a *lot* of complexity to the type system. It requires subtyping, and inference of subtypes is notoriously difficult to theoretically impossible. Instead, prototype extension is much simpler and captures most of the uses of single inheritance. qdbp has no way of mimicking multiple inheritance, but multiple inheritance is controversial at best and often unnecessary.
#### Mutablity
Mutable variables introduce complexity, from the syntactic complexity of adding an assignment syntax to the semantic complexity of having to reason about variables whose values are constantly changing. qdbp avoids this by not having mutable variables. As an added bonus, this makes it impossible to form circular references, allowing qdbp to have predictable, reliable reference counting. This does have unavoidable performance implications, but for most applications, the performance hit is more than tolerable.
#### Macros
Macros are a powerful tool, but they are also a source of complexity. They often make programs impossible to reason about and can be a source of subtle bugs. qdbp does not have macros, but it does have a powerful, expressive syntax that makes it easy to use objects for many of the same purposes as macros.
#### Non-Local Control Flow(exceptions, coroutines, algebraic effects, etc)
Non-local control flow often makes programs much harder to reason about and adds a lot of complexity to languages. In addition, such constructs are much trickier to implement on the compiler side. As a result, qdbp does not have any non-local control flow(other than `ABORT.`). 

However, non-local control cannot be emulated with any existing feature. So, while qdbp doesn't have it right now, we don't rule out adding it in the future.
#### Purely Functional I/O
qdbp does not have any purely functional I/O. This is because qdbp is not a purely functional language. In fact, qdbp is arguably not even a functional language(it doesn't even have first class functions). Purely functional I/O, both in the form of algebraic effects and monads, is a powerful tool for reasoning about programs, but qdbp's core value of simplicity makes it a poor fit for the language.
#### Sequence Expressions
qdbp does not have explicit syntactic support for sequence expressions. Instead, they have to be done in the following manner
```
ignore := expr1
ignore := expr2
expr3
```
Admittedly, this is unsatisfying. The reason qdbp doesn't have sequence expressions yet is because we haven't found a good syntax for them. In particular, we want to avoid the `;` for expression separators because people find it annoying and it adds too much punctuation to the language(for example, `1+1+2..;` just looks ugly). If you have any ideas for this, please let us know.
#### Concurrency
qdbp is simple and concurrency is complex. It will be added in the future, but it is arguably the hardest feature to add while keeping with the ethos of the language, so we are taking our time with it.
# Demo
## A demo of qdbp
While the core language of qdbp is small, its features can be composed to express complex abstractions. This demonstrates some examples of this. Most of this code you wouldn't write regularly but would instead reuse from a library.

### Functions
```python
def add(a, b):
    return a + b
add(1, 2)
```
can be done in qdbp like so:
```ocaml
add := {a b | a + b.}
add! a: 1 b: 2.
```
`add` is technically not a function. It is a prototype with a single method `!`. For convenience, however, we refer to any prototype with `!` as a function.
### Generics
Methods are generic by default. Here is a generic `print` function:
```ocaml
print := {val | val Print.}
ignore := print! 3.
print! "hello".
```
### If/Then/Else
```ocaml
if := {val then else|
  val
    True? [then!.]
    False? [else!.].
}
if! 1 > 2.
  then: { 
    "true" Print. 
 }
  else: {
    "false" Print.
}.
```
Alternatively, `if` can be a method in a boolean object like so:
```ocaml
true := {
  BoolVal[#True{}]
  If[then else|
    self BoolVal.
      True? [then!.]
      False? [else!.].
  ]
}
true If
  then: { 
    "true" Print. 
 }
  else: {
    "false" Print.
}.
```
### Switch
There are a variety of ways to implement `switch`, depending on your needs. Here is one
```ocaml
switch := {val |
  {
    Val[val]
    Result[#None{}]
    Case[val then|
      self Val. = val.
        True? [
          result := then!.
          {self Result[#Some result]}]
        False? [self].
    ]
    Default[then|
      self Result.
        Some? [val| val]
        None? [then!.].
    ]
  }
}
str := switch! 5.
  Case 1 then: {"one"}.
  Case 2 then: {"two"}.
  Case 3 then: {"three"}.
  Case 4 then: {"four"}.
  Case 5 then: {"five"}.
  Default then: {"None of the above"}.
str Print.
```
### Data Structures
All of the data structures [here](https://en.wikipedia.org/wiki/Purely_functional_data_structure#Examples) can be implemented in qdbp. In addition, qdbp will have Perceus Reference Counting in the near future, allowing data structures to reuse memory when possible and clawing back some of the performance lost to immutability.

As an example, here is an implementation of a stack:
```ocaml
stack := {
  Data[#Empty{}]
  Push[val|
    curr_data := self Data.
    {
      self
      Data[#NonEmpty {
        Val[val]
        Next[curr_data]
      }]
    }
  ]
  Peek[
    self Data.
      Empty?[ABORT.]
      NonEmpty?[data| data Val.].
  ]
}
stack Push 3. Push 2. Peek. Print.
```
### Operators
```ocaml
true := {
  BoolVal[#True{}]
  &&[val|
    self BoolVal.
      True? [val]
      False? [self].
  ]
}
false := {true BoolVal[#False{}]}
true && false.
```
### While Loops
```ocaml
while := {val body|
  val!.
    True? [
      ignore := body!.
      self! val: val body: body.
    ]
    False? [{}].
}
; Will loop infinitely
while! {1 < 2.}
  body: {
    "hello world\n" Print.
  }.
```
### Functional List Manipulation
For brevity, the implementation of the list objects is omitted.
```ocaml
double_list := {list | list Map {val | val * 2.}. }
sum_list := {list | 
              list FoldLeft fn: {val acc | val + acc.} initial: 0. 
            }
{}
```
### Classes
A class is just a function that returns an object. For example, this:
```python
class circle:
    def __init__(self, radius):
        self.radius = radius
    def print(self):
        print(self.radius)
my_circle = circle(3)
my_circle.print()
```
can be implemented in qdbp like so:
```ocaml
make_circle := {radius |
  {
    Radius[radius]
    Print[self Radius. Print.]
  }
}
my_circle := make_circle! radius: 3.
my_circle Print.
```
### Single inheritance/Dynamic dispatch
Some cases of single inheritance can be mimicked with prototype extension.
```ocaml
basic_circle := {
  Radius[3]
  Print[self Radius. Print.]
}
colored_circle := {
  basic_circle
  Color["red"]
  Print[
    ignore := self Color. Print.
    self Radius. Print.
  ]
}
ignore := basic_circle Print.
colored_circle Print.
```
### Partial Objects
Consider the following object
```ocaml
comparison := {
  >=[val|
    self < val.
      False? [#True{}]
      True? [#False{}].
  ]
  <=[val|
    self > val.
      False? [#True{}]
      True? [#False{}].
  ]
  =[val|
    self >= val.
      True? [self <= val.]
      False? [#False{}].
  ]
  !=[val|
    self = val.
      True? [#False{}]
      False? [#True{}].
  ]
}
```
Now, any object that extends `comparison` and implements `<` and `>` gets the other comparison operators for free.
### More Method Logic Reuse
What if we wanted to add these comparison operators to an object that already existed rather than adding methods to the comparison object that already exists? Here is how we could do that
```ocaml
equals := {val other|
  other >= val.
    True? [other <= val.]
    False? [#False{}].
}
{
  some_object_that_already_has_geq_and_leq
  =[val| equals! val: self other: val.]
}
```
### For loop with iterator
```ocaml
for := {iter body|
  iter Data.
    None? [{}]
    Some?[data|
      ignore := body! (data Val).
      self! iter: data Next. body: body.
    ].
}
from := {val to|
  {
    Start[val]
    End[to]
    Val[self Start.]
    Next[
      start := (self Start) + 1.
      {
        self
        Start[start]
      }
    ]
    Data[
      (self Start) <= (self End).
        True? [ #Some self ]
        False? [#None{}].
    ]
  }
}
for!
  iter: (from! 1 to: 10)
  body: {val | val Print.}.
```
### Phantom Fields
Some values may have the same types but semantically mean different things. For example, an int could be a number of cookies or it could be a day of the month. When we perform operations, we can make sure we don't use the wrong values at compile time by using phantom fields.
```ocaml
day_12_of_month := {
  12
  DayOfMonthUnit[{}]
}
five_cookies := {
  5
  CookieUnit[{}]
}
fn_involving_days := {val |
  typecheck := {val DayOfMonthUnit.} ; compiletime check for unit
  ; Do something with val
  {}
}
ignore := fn_involving_days! day_12_of_month. ; ok
fn_involving_days! five_cookies. ; compilation error
```
However, qdbp's type system is limited. We cannot make, for example, methods that multiply units automatically like in c++. We can only handcheck for the existence of specific field names.

### Error handling
#### Returning Errors
```ocaml
safe_divide := {a b|
  b = 0.
    True? [#Error{}]
    False? [#Ok a / b.].
}
result := safe_divide! a: 1 b: 0.
result
  Error? ["error" Print.]
  Ok?[val| val Print.].
```
#### Propagating Errors
```ocaml
error := {
  Transform[fn|
    self Val.
      Error? [err| self]
      Ok? [val| {self Val[#Ok fn! val.]}].
  ]
}
safe_divide := {a b|
  b = 0.
    True? [{error Val[#Error{}]}]
    False? [{error Val[#Ok a / b.]}].
}
safe_divide_6 := {a b c d e f|
  (safe_divide! a: a b: b) 
    Transform fn: {val | val / c.}.
    Transform fn: {val | val / d.}.
    Transform fn: {val | val / e.}.
    Transform fn: {val | val / f.}.
}
safe_divide_6! a: 3996 b: 3 c: 1 d: 2 e: 2 f: 1. Val.
  Error? ["Error" Print.]
  Ok? [val | val Print.].
```
#### Abort
```ocaml
panic := {val |
  ignore := val Print.
  ABORT.
}
safe_divide := {a b|
  b = 0.
    True? [panic! "divide by 0\n".]
    False? [a / b.].
}
ignore := safe_divide! a: 1 b: 0.
"Shouldn't get here!!!" Print.
```
### Infinite Lists
```ocaml
pows_of_2_list := {
  Val[1]
  Next[
    cur_val := self Val.
    { self Val[cur_val * 2.] }
  ]
}
ignore := pows_of_2_list Val. Print.
pows_of_2_list := pows_of_2_list Next.
ignore := pows_of_2_list Val. Print.
pows_of_2_list := pows_of_2_list Next.
ignore := pows_of_2_list Val. Print.
pows_of_2_list := pows_of_2_list Next.
{}
```
### Modules
In `math.qdbp`, for example, we could have the code
```ocaml
{
  Factorial[val|
    val = 0.
      True? [1]
      False? [val * (self Factorial (val - 1)).].
  ]
  Abs[val|
    val < 0.
      True? [-1 * val.]
      False? [val].
  ]
}
```
Then in another file, we can have
```ocaml
math := @math
ignore := math Factorial 5. Print.
math Abs -3 Print.
```
### Defer
```ocaml
defer := {val after|
  result := after!.
  ignore := val!.
  result
}

defer! {"finished " Print.}
  after: {
    ignore := "doing fancy math\n" Print.
    1 + 1. + 3. * 15.
  }.
```
This can be used, for example, to handle file closing or for automatic benchmarking
### DSL Creation
We can create our own DSLs in qdbp, even though we don't have macros. Here is a sample DSL syntax for a build system we could implement
```ocaml
my_project
  AddLibDirectory "./lib".
  AddTestDirectory "./test".
  AddExecutable "bin/main.qdbp".
  SetOptimizationLevels
    performance: 3
    size: 2.
```
# Limitations
*The Little Language That Can't*

qdbp is in its infancy. The current implementation of qdbp
- Compiles slowly and produces inefficient code
- Gives terrible error messages
- Has barely any documentation

However, these limitations are all fixable and removing them will come with time. To that end, here is a shortlist of qdbp's agenda for the future. It is currently going through a rewrite that, when finished, will have the following changes:

- Performance:
  - [ ] Change the compilation target from OCaml to C and allow user to include external C files
  - [ ] Implement [Perceus Reference Counting](https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v1.pdf)
- Error Messages:
  - [ ] Add error states to the parser for better error messages
  - [ ] Make the type inference functional-style
  - [ ] Annotate each ast node with a type, potentially being an error type
  - [ ] Keep track of history during type inference to better pinpoint the origin of type errors
  - [ ] Use a recursive descent parser
- Documentation
  - Create a website with:
    - [ ] A language specification
  - [ ] Code Samples Directory
  - [ ] Clean up the compiler implementation

Longer term goals include the addition of

- [ ] Support for concurrency(Adding this while keeping within the philosophy of the language will be tricky)
- [ ] An LSP
- [ ] A REPL
- [ ] A debugger
- [ ] A package manager
# Grammar
qdbp is completely context free. Here is a grammar in the syntax of a [menhir](https://gitlab.inria.fr/fpottier/menhir) specification.
```ocaml
(* LOWEST PRECEDNCE *)
%nonassoc decl_in_prec
%right UPPER_ID
(* HIGHEST PRECEDNCE *)
program:
  | expr; EOF
expr:
  | LPAREN; expr; RPAREN;
  | record
  | variant
  | record_message
  | variant_message
  | declaration
  | variable
  | ocaml_call
  | import_expr
  | int_literal
  | empty_list
  | abort
  | string
meth:
  | LBRACE; arg_list; expr; RBRACE
  | LBRACE; expr; RBRACE
arg_list:
  | nonempty_list(LOWER_ID); PIPE
record:
  | LBRACKET; record_body; RBRACKET
  | LBRACKET; RBRACKET
  | closure
record_field:
  | UPPER_ID; meth; 
record_body:
  | expr; record_field+; 
  | record_field+; 
record_message:
  | expr; UPPER_ID; expr?; record_message_arg*; PERIOD;
  | LPAREN; expr; UPPER_ID; expr?; record_message_arg*; RPAREN;
record_message_arg:
  | ARG; expr; 
variant:
  | TAG; UPPER_ID; expr;

variant_meth:
  | LBRACE; LOWER_ID; PIPE; expr; RBRACE
  | LBRACE; expr; RBRACE
variant_message:
  | expr; tag_message+; PERIOD;
  | LPAREN; expr; tag_message+; RPAREN
tag_message:
  | UPPER_ID; QUESTION; variant_meth;
decl_in:
  | expr %prec decl_in_prec
declaration:
  | LOWER_ID; DECLARATION; expr; decl_in
variable:
  | LOWER_ID
closure:
  | LBRACKET; arg_list; expr; RBRACKET
  | LBRACKET; expr; RBRACKET
import_expr:
  | IMPORT
int_literal:
  | INT
empty_list:
  | LBRACE; RBRACE
abort:
  | ABORT; PERIOD
string:
  | STRING
ocaml_call:
  | MONEY; LOWER_ID LPAREN; expr*; RPAREN
```