# Meh

Control flow utilities.

A simplified version of work presented by ``GeneralUtilities`*Failure*`` family of functions.

Points are:

 - have control as opposed to use an undocumented functionality
 - syntax familiar for Failure/Messages
 - Concept of tagged failures and raw failures does not exist. 
  You just throw whatever you want and if it matches Message/Failure syntax it will
  be nicely converted.

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
    
## Usage

It is mostly about syntactic sugar for `Catch/Throw` to cope well with `Messages/Failures`.

`MThrow` can take `Message` syntax with optional first element which will be a Failure's tag. 

Using `MThrowAll` will issue a message too.

```Mathematica
MCatch[     MThrow["500", General::invty, Method]    ]
    
  (* Failure["500", <|"MessageTemplate" :> General::invty, "MessageParameters" -> {Method}|> ] *)
      
```    
Replace MThrow with MThrowAll and a message will be issued.
 
 
But ``Meh` `` has also other utilities. `MHandleResult` is an extension to If/Switch.
It has nice operator form   
   
```Mathematica    
MCatch[
  "string output from 3rd party function" // List // MHandleResult[
    Except[_String], Function @ MThrow[General::string, 0, #]
  ]
]
    
  (* Failure[General, <|"MessageTemplate" :> General::string
     , "MessageParameters" -> {0, {"string output from 3rd party function"}}|> ]
  *)
  
  
 
```
  
and handles `_?FailureQ` by default, throwing it:

```Mathematica     
MCatch[
 $Failed // MHandleResult[]
]     

    (* Failure["General", <|"Message" -> "$Failed"|>] *)
```    

`MFailureQ` will be more sophisticated but currently it fixes the problem that built-in `FailureQ` does not recognize `$Canceled` as a failure.

```Mathematica
MFailureQ /@ {"test", $Failed, $Aborted, $Canceled, Failure["any", "Message" -> "Generic message"]}

  (* {False, True, True, True, True} *)
```
    
    