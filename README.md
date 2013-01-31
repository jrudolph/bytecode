# Mnemonics - A runtime bytecode generation DSL for Scala

For more info see [Mnemonics: Type-safe Bytecode Generation at Run Time](https://github.com/jrudolph/bytecode/raw/docs/mnemonics.pdf).

There are more papers in the [docs branch](https://github.com/jrudolph/bytecode/tree/docs).

## Example

Here's an example which generates the `+ 1` function:

```scala
scala> import java.lang.Integer
import java.lang.Integer

scala> val f = ASM[Integer, Integer](param =>
     |             _ ~
     |             param.load ~
     |             method((x: Integer) => x.intValue) ~
     |             bipush(1) ~
     |             iadd ~
     |             method((x: Int) => Integer.valueOf(x)))
f: java.lang.Integer => java.lang.Integer = <function1>

scala> f(12)
res0: java.lang.Integer = 13
```
