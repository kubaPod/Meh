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

- core symbols start with `M`: `MThrow`, `MFailureQ` etc. Sub-utilities do not e.g.: `MValidate`.
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

