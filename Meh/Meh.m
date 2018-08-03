(* ::Package:: *)

(* ::Chapter::Closed:: *)
(* Metadata*)


(* Mathematica Package *)

(* :Title: Meh *)
(* :Context: Meh` *)
(* :Author: Kuba (kuba.pod@gmail.com) *)
(* :Date: Wed 4 Jul 2018 12:48:24 *)

(* :Keywords: *)
(* :Discussion: *)
(* The idea:
     The goals are to have a set of mini utilities that can be used to gradually enachnce existing/new code. 
     It should be more or less consistent and work well with Messages system.
     (easy to learn, idiomatic, consistent) > (optimized, general system)
 *)



(* ::Chapter:: *)
(* Begin package*)


BeginPackage["Meh`"];

Unprotect["`*", "`*`*"]
ClearAll["`*", "`*`*"]

  Meh;
  
  MFailureQ;  MGenerateFailure;  MGenerateAll;
  
  MCatch;  MThrow;  MThrowAll;
  
  MHandleResult;    MOnFailure;  MThrowOnFailure;  MRetryOnFailure;
  
  MFailByDefault;
  
  FailOnInvalidStruct;  StructMatch;  MatchedElement;  StructValidate;  
  StructUnmatchedPositions;
  
  FailToHTTPResponse
  
  

Begin["`Private`"];



(* ::Chapter:: *)
(* Implementation code*)


(* ::Section::Closed:: *)
(*misc*)


  Meh::usage = "Meh is a symbol that stores common messages used by Meh`";
  
  Meh::match = "Unexpected error, expression with head `` does not match ``.";
  
  Meh::argpatt = "There are no rules associated with signature ``."; 
  


inputToSignature // Attributes = {HoldAllComplete};

inputToSignature::usage = "Is a helper function for _::argpatt to transform unknown user input to a sequence of heads";

inputToSignature[head_[spec___]]:= ToString[#, OutputForm]& @ StringForm[
  "``[``]"
, head
, Row[ Thread @ HoldForm[{spec}][[{1}, ;;, 0]], ", "]
];


(*TODO: 

  ThrowOnMessage
*)


(* ::Section::Closed:: *)
(*Function construction*)


MFailByDefault::usage = "foo // MFailByDefault makes foo to issue a message and return a Failure when unknown input is provided.";

MFailByDefault[symbol_Symbol]:= (
  symbol::argpatt = Meh::argpatt
; symbol[x___]:= MGenerateAll[symbol::argpatt, inputToSignature[symbol[x]]]
);
  


(* ::Section:: *)
(*Core*)


(* ::Subsection::Closed:: *)
(*MFailureQ*)


  MFailureQ[ _Failure|$Canceled|$Aborted|$Failed ]=True; 
  (*Oone could think of an unevaluated pattern being here to but a) it adds complexity b) should not be enforced in case of symbolic wrappers/constructors. 
     So this will be handled by mHandle *)
  MFailureQ[_]=False



(* ::Subsection::Closed:: *)
(*MGenerateFailure; MGenerateAll*)


(* ::Subsubsection::Closed:: *)
(*notes*)


(*
  - handle temp messages
  - handle messageName
  - handle additional payload
  - handle custom tags
  - handle messages
  
  Full signature: MThrow[tag_String, msg_MessageName, args: ___ | _Association, payload___Rule ]:=
*)  

 (* quick temp messages, discouraged ;) *)
 (* MThrow[tempMsg:_String, args___]         := MThrow @ MGenerateFailure["dev", tempMsg, {args} ];
  MThrow[tempMsg:_String, args:_Association]:= MThrow @ MGenerateFailure["dev", tempMsg, args ];*)




(* ::Subsubsection::Closed:: *)
(*misc*)


  MGenerateFailure // Attributes = { HoldAll };  
  
  MGenerateAll // Attributes = { HoldAll };
  
  MGenerateFailure::usage = "MGenerateFailure[spec___] generates a Failure if spec matches special syntax. " <> 
    "Else, single argument, returns it. Else, for multiple arguments generates a failure + message.";
    
  MGenerateFailure::argpatt = MGenerateAll::argpatt = Meh::argpatt;


(* ::Subsubsection::Closed:: *)
(*message/failure like syntactic sugar*)


MGenerateFailure[ 
    msg  : MessageName[head : _Symbol, name : _String], 
    args : ___ 
  ]:= MGenerateFailure[name, msg, args];


MGenerateFailure[ 
    tag     : _String | _Symbol, 
    msg     : HoldPattern[MessageName[head:_Symbol, name:_String]], 
    args    : _Association     
  ] :=  MGenerateFailure[tag, msg, args, <||>]


(* ::Subsubsection::Closed:: *)
(*core*)


  MGenerateFailure[
    tag     : _String | _Symbol
  , templ   :(_String | _MessageName)
  , args    : _List | _Association
  , payload : _Association : <||>
  ]:=Failure[
    tag
  , <|"MessageTemplate" :> templ, "MessageParameters" -> args, payload|>
  ];  


(* this needs to be after (args    : _List | _Association) one because of specificity *)
MGenerateFailure[ 
    tag     : _String | _Symbol , 
    msg     : HoldPattern[ MessageName[head:_Symbol, name:_String]], 
    args    : ___,
    payload : _Association : <||>
  ]:= MGenerateFailure[tag, msg, {args}, payload ]


MGenerateAll[
    tag     : _String | _Symbol | PatternSequence[]
  , msg     : _MessageName
  , args    : __
  , payload : _Association : <||>
  ]:=(Message[msg, args]; MGenerateFailure[tag, msg, args, payload]);


MGenerateFailure[expr : $Failed | $Canceled | $Aborted]:=  Failure["err", <|"Message" -> ToString[expr]|>]


(* ::Subsubsection::Closed:: *)
(*defaults*)


MGenerateFailure[ whateverElse_ ]:= whateverElse;
MGenerateAll[ whateverElse_ ]:= whateverElse;


input : MGenerateFailure[whateverElse__]:= (
  Message[MGenerateFailure::argpatt, inputToSignature[input] ]
; Failure["argpatt"
  , <|"Message" -> ToString @ StringForm[MGenerateFailure::argpatt, inputToSignature[input] ]|>
  ]  
);

input : MGenerateAll[whateverElse__]:= (
  Message[MGenerateAll::argpatt, inputToSignature[input] ]
; Failure["argpatt"
  , <|"Message" -> ToString @ StringForm[MGenerateAll::argpatt, inputToSignature[input] ]|>
  ]  
)


(* ::Subsection::Closed:: *)
(*MThrow / MCatch*)


  $MehTag = "MEH";    


  MCatch = Function[expr, Catch[expr, $MehTag], HoldAllComplete];


  MThrow // Attributes = {HoldAll};  


  MThrow[ f : _ ]:=Throw[f, $MehTag ];  


  MThrow[ f : $Failed | $Canceled | $Aborted ] := Throw[
    MGenerateFailure[f]
  , $MehTag
  ]


  MThrow[ f : ___ ] := Throw[
    MGenerateFailure[f]
  , $MehTag
  ];


(* ::Subsection::Closed:: *)
(*MThrowAll*)


  MThrowAll // Attributes = {HoldAll};
  
  MThrowAll[spec___]:= MThrow @ MGenerateAll[spec]; 


(* ::Section:: *)
(*Flow control*)


(* ::Subsection::Closed:: *)
(*MHandleResult*)


MHandleResult::usage = "MHandleResult[(patt -> handler)..] creates an operator:" <>
  "Function[ input, Switch[input, patt, handler, .., _?MFailureQ, MThrow, _, Identity] @ input].";
  
MHandleResult[rules___]:=Function[
  expr
, Switch[expr, rules, _?MFailureQ, MThrow, _, Identity] @ expr
]

(* It should throw or return the input but it is up to the user what rules are put there *)
(* It makes ThrowOnFailure redundant as it contains that rule by default. *)


(* ::Subsection::Closed:: *)
(*M*OnFailure*)


(* everything here is a syntactic sugar and can be done with MHandleResult only*)
(* But maybe someone just needs to handle failures and be verbose, additionally
   keeping GeneralUtilities` like names, then it is for them.
 *)


MOnFailure::usage = "expr // MOnFailure[foo] does foo[expr] /; MFailureQ[expr]";

MOnFailure[foo_][res_?MFailureQ]:= foo @ res; 
MOnFailure[foo_][res_]:= res;


MThrowOnFailure::usage = "expr // MThrowOnFailure MThrow-s expr if MFailureQ[expr]";

MThrowOnFailure[res_?MFailureQ] := MThrow @ res;
MThrowOnFailure[res_]:= res


(* ::Subsection::Closed:: *)
(*MRetryOnFailure*)


MRetryOnFailure // Attributes = {HoldFirst};

MRetryOnFailure::usage = "MRetryOnFailure[expr, n], if FailureQ[expr] then tries again, n times. "<>
  "So in total at most n+1 evaluations of expr.";


MRetryOnFailure[]:=MRetryOnFailure[1];


MRetryOnFailure[n_Integer]:= Function[
  expr
, MRetryOnFailure[expr, n]
, HoldAll
];


MRetryOnFailure[expr_, n: _Integer : 1]:= Module[{i = 0, result},
  While[
    ++i
  ; result = expr
  ; FailureQ @ result && i <= n
  ]
; result
];


MRetryOnFailure // MFailByDefault;


(* ::Section::Closed:: *)
(*Struct Validation*)


(* ::Subsection::Closed:: *)
(*FailOnInvalidStruct*)


FailOnInvalidStruct[structPattern_, argPost_Integer : 1]:= Function[function, FailOnInvalidStruct[function, structPattern, argPost]]

FailOnInvalidStruct[function_Symbol, structPattern_, argPos : _Integer : 1]:=(
  function::invStruct = Meh::invStruct;
  function[input___]:=MsgAndTaggedFAIL[
    "400"
  , function::invStruct
  , function
  , StructUnmatchedPositions[{input}[[argPos]], structPattern, 3] // StringRiffle[#, {"\t", "\n\t",""}]&
  , "\t" <> ToString[structPattern/.KeyValuePattern->Association]
  ]
);


(* ::Subsection:: *)
(*StructMatch*)


StructMatch // ClearAll;
MatchedElement // ClearAll;
MatchedElement // Protect;

(*TODO: handle empty lists for Repeated*)
$multiPattern = Verbatim/@(Repeated|RepeatedNull);


StructMatch[
  expr : KeyValuePattern[{}]
, kvp_KeyValuePattern
]:=  Module[
  {kvpAsso = Association @@ kvp}
, Merge[ Apply[StructMatch] ] @ { KeyTake[Keys[kvpAsso]] @ expr, kvpAsso}
];


StructMatch[
  {}
, {Verbatim[Repeated][_KeyValuePattern,___]}
] = MatchedElement[False]


StructMatch[expr:{__},{$multiPattern[kvp_KeyValuePattern,___]}]:=  StructMatch[#,kvp]& /@ expr;


StructMatch[ expr_, kvp:Except[_KeyValuePattern] ]:=  MatchedElement[ MatchQ[expr,kvp] ];


StructMatch[arg___]:=MatchedElement[False];


(* ::Subsection:: *)
(*StructValidate*)


StructValidate // ClearAll; (*not sure it makes sense, why not MatchQ[expr, patt]?*)
StructValidate[patt_]:=Function[expr, StructValidate[expr, patt]];
StructValidate[expr_, patt_]:=FreeQ[StructMatch[expr,patt], MatchedElement[False]];


(* ::Subsection:: *)
(*StructUnmatchedPositions*)


StructUnmatchedPositions // ClearAll;
StructUnmatchedPositions[expr_, patt_]:= Replace[Position[StructMatch[expr,patt], MatchedElement[False]], {} -> 0, {1}];
StructUnmatchedPositions[expr_, patt_, n_Integer?Positive]:= Take[StructUnmatchedPositions[expr, patt], UpTo[n]];


(* ::Section:: *)
(*migrated*)


(* ::Subsection::Closed:: *)
(*FailToHTTPResponse*)


FailToHTTPResponse// ClearAll;



(*TODO: failure generate? *)
FailToHTTPResponse[failure:($Failed|$Aborted|$Canceled)]:= FailToHTTPResponse @ Failure["500","MessageTemplate"->ToString[failure]]



FailToHTTPResponse[
    f : Failure[
      tag_String?(StringMatchQ[DigitCharacter..])
    , asso_
    ]
  ]:=HTTPResponse[
      ExportString[ <| "Message" -> FailureString[f] |>, "RawJSON", "Compact"->True ]
    , <|"StatusCode" -> tag, "ContentType"->"application/json"|>
    , CharacterEncoding -> None (*because RawJSON already did it*)
  ]


FailToHTTPResponse[
    f : Failure[
      tag:(_String|_Symbol)
    , asso_
    ]
  ]:=HTTPResponse[
      ExportString[ <| "Tag" -> ToString[tag], "Message" -> FailureString[f] , "MessageList" -> ToString @ $MessageList|>, "RawJSON", "Compact"->True ]
    , <|"StatusCode" -> "500", "ContentType"->"application/json"|>
    , CharacterEncoding -> None (*because RawJSON already did it*)
  ]


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
