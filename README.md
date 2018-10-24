# Meh

Control flow utilities.

A simplified version of work presented by ``GeneralUtilities`*Failure*`` family of functions.

Points are:

 - To have control as opposed to use an undocumented functionality
 - Syntax familiar for Failure/Messages
 - Concept of tagged failures and raw failures does not exist. 
  You just throw whatever you want and if it matches Message/Failure syntax then Message/Failure will be assembled.

## Installation
 
### Manual
 
   Go to 'releases' tab and download appropriate .paclet file.
    
   Run `PacletInstall @ path/to/the.paclet` file
   
### Via ``MPM` ``
   
If you don't have ``MPM` `` yet, run:
   
    Import["https://raw.githubusercontent.com/kubapod/mpm/master/install.m"]
   
and then:
   
    Needs @ "MPM`"    
    MPM`MPMInstall["kubapod", "meh"]
    
## Big picture

It is mostly about syntactic sugar for `Catch/Throw` to cope well with `Messages/Failures`.

Basic rules:

- core symbols start with `M`: `MThrow`, `MFailureQ` etc. Sub-utilities do not e.g.: `StructValidate`.
- `MGenerate`, `MGenerateAll`, `MThrow`, `MThrowAll` have the same syntax with respect to Message/Failure related cases.  
  - `-All` means that a `Message` will be issued too. That also implies only a valid message input can be used while simple `MThrow` can accept whatever input you give it.
  - `Generate` just creates e.g. a `Failure` while `MThrow` throws is to a nearest enclosing `MCatch`.

`MThrow` can take `Message` syntax with optional first element which will be a Failure's tag. 

```Mathematica
MCatch[     MThrow["500", General::invty, Method]    ]
```
> `Failure["500", <|"MessageTemplate" :> General::invty, "MessageParameters" -> {Method}|> ]`
    
Replace `MThrow` with `MThrowAll` and a message will be issued.
 
Below is a more detailed symbols guide:

## Core symbols

### MFailureQ

```Mathematica
MFailureQ /@ {
    $Failed, $Aborted, $Canceled, Failure["any","Message"->"Generic message"]
}
```
> `{True,True,True,True}`

### MGenerate, MGenerateAll, MThrow, MThrowAll, MCatch

Basic usage:

```Mathematica
MGenerateAll[General::argt,foo,2,3,4]
```

> General::argt: foo called with 2 arguments; 3 or 4 arguments are expected.

> `Failure["argt", <|   "MessageTemplate" :> General::argt, "MessageParameters" -> {foo, 2, 3, 4} |> ]`

Use custom tags for a more fine grained control flow:

```Mathematica
MCatch @ MThrow["custom tag",General::argt,foo,2,3,4]
```
> `Failure["custom tag", <|"MessageTemplate" :> General::argt, "MessageParameters" -> {foo, 2, 3, 4}|>]`

`Association/Failure` oriented syntax:

```Mathematica
MCatch @ MThrow[foo::argx, <|"arg"->bar|>]
```
> `Failure["argx", <|"MessageTemplate" :> foo::argx, "MessageParameters" -> <|"arg" -> bar|>|>]`

Adding **payload** can be invaluable for complex flow:

```Mathematica
MCatch @ MThrow[
    "custom tag"
  , General::argt
  , foo,2,3,4
  , <|"Payload"->"additional information"|>
]
```

> `Failure[
       "custom tag"
     , <|"MessageTemplate" :> General::argt
       , "MessageParameters" -> {foo, 2, 3, 4}
       , "Payload" -> "additional information"
       |>
     ]
   `
   
### MHandleResult   

More idiomatic approach to `If[Test @ expr, exception @ expr, expr]`: 

```Mathematica
MCatch[
    $Aborted // MHandleResult[]; 1
]
```
> `Failure["err", <|"Message" -> "$Aborted"|>]`

```Mathematica
MCatch[
    NotAString[] // MHandleResult[
      Except[_String] , Function[res, MThrow[foo::string, Head[res], _String]]
    ]
  ]
```
> `Failure["string", <|"MessageTemplate" :> foo::string, "MessageParameters" -> {NotAString, _String}|> ]`

### M*OnFailure

More idiomatic approach to `If[ FailureQ[expr], handler @ expr, expr]`:

```Mathematica
$Failed // MOnFailure[foo]
```
> `foo[$Failed]`

```Mathematica
MCatch[ $Failed // MOnFailure[MThrow] ]
```
> `Failure["err", <|"Message" -> "$Failed"|>]`

There is a shorter version for throwing though:

```Mathematica
MCatch[ $Failed // MThrowOnFailure ]
```
> `Failure["err", <|"Message" -> "$Failed"|>]`

## Function construction

I find unevaluated expressions hard to manage with respect to flow control so I would like always to get a Failure:

```Mathematica
  foo[x_]:=x^2; 
  foo // MFailByDefault; 
  
  foo[1,2]
```
> foo::argpatt: There are no rules associated with signature foo[Integer, Integer].

> `Failure["argpatt", <|"MessageTemplate" :> foo::argpatt, "MessageParameters" -> {"foo[Integer, Integer]"}|>]`

## Validation

The idea behind those utilities originates from https://mathematica.stackexchange.com/q/116571/5478

```Mathematica
StructUnmatchedPositions[
  <|"a" -> <|"b" -> 2, "c"->3, "d" -> 4|>|>
, KeyValuePattern[{"a" -> KeyValuePattern[{"b" -> _Integer, "c" -> _String, "d" -> _List}]}]  
]
```
> `{ {Key["a"], Key["c"]}   ,   {Key["a"], Key["d"]} }`

```Mathematica
StructValidate[
  <|"a" -> <|"b" -> 2, "c"->3, "d" -> 4|>|>
, KeyValuePattern[{"a" -> KeyValuePattern[{"b" -> _Integer, "c" -> _Integer, "d" -> _Integer}]}]  
]
```
> `True`

```Mathematica
ClearAll[foo];
$fooPatt = KeyValuePattern[{"a" -> _Integer}];
foo // FailOnInvalidStruct[ $fooPatt ]
foo[ in: $fooPatt]:= in["a"]


foo @ <|"a" -> 1|>
```
> `1`

```Mathematica
foo @ <|"b" -> 1|>
```
> foo::invStruct: foo: Invalid values at positions:
> {Key[a]}
> Input needs to match:
>	<|a -> _Integer|>

> `Failure["400", <|
    "MessageTemplate" :> MessageName[foo, "invStruct"], 
     "MessageParameters" -> {foo, "\t{Key[a]}", "\t<|a -> _Integer|>"}|>]`

## Utilities

### ToKeyValue

```Mathematica
Module[{x = 1, y = 2, z = "string"}, ToKeyValue @ {x,y,z}]
```

> `{"x" -> 1, "y" -> 2, "z" -> "string"}`