# Meh

Control flow utilities.

It happens that for more complicated or interactive applications solutions offered by native `Messages` and related functions are not enough. 
There was an attempt to address this problem by developers of ``GeneralUtilities` `` but it is still undocumented and not polished. 

This package is meant to provide solutions for easier exceptions handling and keep it idiomatic/ natural to regular WL experience.

It also comes with few minor utilities that I found to be used by me quite frequently.

It is still under development, there is a brief [documentation in WIKI](https://github.com/kubaPod/Meh/wiki/Symbols-Guide)

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
    
## Usage teaser

```Mathematica
MCatch[
  MThrowAll["500", General::appname, "1asd"]
]
```

> `General::appname`: The name 1asd is not valid for the application. A valid name starts with a letter and is followed by letters and digits.

> ```
> Failure["500", <| "MessageTemplate" :> General::appname, "MessageParameters" -> {"1asd"}]]
>  ```

```Mathematica
foo // ClearAll
$fooPatt = KeyValuePattern[{"a" -> _Integer, "c" -> _String}];

foo // MValidateByDefault[ $fooPatt ]

foo[ in: $fooPatt]:= in["a"]

foo @ <|"a" -> 1, "c" -> "c"|>

foo @ <|"b" -> 1, "c" -> {1}|>
```

> `1`

> `foo::InvalidArg`: Argument No. 1 has invalid structure at:<br>
>	{Key[c]}	List <br>
>	{Key[a]}	Missing[] <br>
> It needs to match: <br>
>	<|a -> _Integer, c -> _String|>

> ```Failure[...]```
