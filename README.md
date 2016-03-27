Express
=======
Currently this is a simple and experimental core of a scripting language.

The language design is mostly inspired by Python, JavaScript and some inspiration from Object Pascal.


So far we have
--------------
- [x] _Operations:_
    - [x] `:=`, `+=`, `-=`, `*=`, `/=`, **~~`&= |= ^=`~~**
    - [x] `=`, `!=`, `<`, `>`, `<=`, `>=`
    - [x] `+`, `-`, `*`, ~~`**`~~, `/`, `|`, `&`, `^`, **~~`shr shl`~~**
    - [x] `and`, `or` `xor`
- [x] _Constructs:_ `if`..`else`, `for`, `while`, `repeat`, `print`
- [x] _Branch statements:_ `continue`, `break`, `return`
- [x] _Data types:_ `int`, `float`, `bool`, `list`, `string`, `char`
      ... and the "lack" of a datatype: `None`
- [x] _Script functions (the basics)_
- [x] _Methods supports local declarations, tho currently no closures (will error)_
- [x] _Special expressions: `x if condition else y`, more could be added._
- [x] _Garbage collector - which might, or might not work properly..._

Could be more stuff that I've failed to mention. But keep in mind that many feature added is probably iffy, and **a lot** of error handling is lacking.. 

