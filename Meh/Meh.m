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
  
  MFailureQ;
  MGenerateFailure;
  MGenerateAll;
  
  MCatch;
  MThrow;
  MThrowAll;
  
  MHandleResult;  
  MOnFailure;
  MThrowOnFailure;
  
  MFailByDefault;
  

Begin["`Private`"];



(* ::Chapter:: *)
(* Implementation code*)


(* ::Section:: *)
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


(* ::Section:: *)
(*Core*)


(* ::Subsection::Closed:: *)
(*MFailureQ*)


  MFailureQ[ _Failure|$Canceled|$Aborted|$Failed ]=True; 
  (*Oone could think of an unevaluated pattern being here to but a) it adds complexity b) should not be enforced in case of symbolic wrappers/constructors. 
     So this will be handled by mHandle *)
  MFailureQ[_]=False



(* ::Subsection:: *)
(*MGenerateFailure; MGenerateAll*)


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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


(* ::Subsubsection:: *)
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


(* ::Subsection:: *)
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


(* ::Subsection:: *)
(*MThrowAll*)


  MThrowAll // Attributes = {HoldAll};
  
  MThrowAll[spec___]:= MThrow @ MGenerateAll[spec]; 


(* ::Section:: *)
(*Flow control*)


(* ::Subsection:: *)
(*MHandleResult*)


MHandleResult::usage = "MHandleResult[(patt -> handler)..] creates an operator:" <>
  "Function[ input, Switch[input, patt, handler, .., _?MFailureQ, MThrow, _, Identity] @ input].";
  
MHandleResult[rules___]:=Function[
  expr
, Switch[expr, rules, _?MFailureQ, MThrow, _, Identity] @ expr
]

(* It should throw or return the input but it is up to the user what rules are put there *)
(* It makes ThrowOnFailure redundant as it contains that rule by default. *)


(* ::Subsection:: *)
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


(* ::Section:: *)
(*Function construction*)


MFailByDefault::usage = "foo // MFailByDefault makes foo to issue a message and return a Failure when unknown input is provided.";

MFailByDefault[symbol_Symbol]:= (
  symbol::argpatt = Meh::argpatt
; symbol[x___]:= MGenerateAll[symbol::argpatt, inputToSignature[symbol[x]]]
);
  


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
