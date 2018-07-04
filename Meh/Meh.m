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



(* ::Chapter::Closed:: *)
(* Begin package*)


BeginPackage["Meh`"];

Unprotect["`*", "`*`*"]
ClearAll["`*", "`*`*"]

  Meh;
  
  MFailureQ;
  
  MCatch;
  MThrow;
  MThrowAll;
  
  MHandleResult;

Begin["`Private`"];



(* ::Chapter:: *)
(* Implementation code*)


(* ::Section::Closed:: *)
(*misc*)


  Meh::usage = "Meh is a symbol that stores common messages used by Meh`";
  
  Meh::match = "Unexpected error, expression with head `` does not match ``.";
  
  $MehTag = "MEH";  
  


(* ::Section::Closed:: *)
(*MFailureQ*)



  MFailureQ[ _Failure|$Canceled|$Aborted|$Failed ]=True; 
  (*Oone could think of an unevaluated pattern being here to but a) it adds complexity b) should not be enforced in case of symbolic wrappers/constructors. 
     So this will be handled by mHandle *)
  MFailureQ[_]=False



(* ::Section::Closed:: *)
(*MFailureGenerate*)


  MFailureGenerate // Attributes = { HoldAll };
  (*
  MFailureGenerate // Options = {
    "Tag" \[Rule] Automatic
  , "Payload" \[Rule] <||>  
  };
  *)
  
  MFailureGenerate[
    tag     : _String | _Symbol
  , templ   :(_String | _MessageName)
  , args    : _List | _Association
  , payload : _Association : <||>
  ]:=Failure[
    tag
  , <|"MessageTemplate" :> templ, "MessageParameters" -> args, payload|>
  ]
  


(* ::Section::Closed:: *)
(*MThrow / MCatch*)


  MCatch = Function[expr, Catch[expr, $MehTag], HoldAllComplete];


(*
  - handle temp messages
  - handle messageName
  - handle additional payload
  - handle custom tags
  - handle messages
  
  Full signature: MThrow[tag_String, msg_MessageName, args: ___ | _Association, payload___Rule ]:=
*)  
  MThrow // Attributes = {HoldAll};
  
  (*MThrow // Options = MFailureGenerate // Options;*)

  MThrow[ f : _ ] := Throw[f, $MehTag];

  MThrow[ expr : $Failed | $Canceled | $Aborted ]:= MThrow @ Failure["General", <|"Message" -> ToString[expr]|>]

    (* quick temp messages, discouraged ;) *)
 (* MThrow[tempMsg:_String, args___]         := MThrow @ MFailureGenerate["dev", tempMsg, {args} ];
  MThrow[tempMsg:_String, args:_Association]:= MThrow @ MFailureGenerate["dev", tempMsg, args ];*)

  MThrow[ 
    msg  : MessageName[head : _Symbol, name : _String], 
    args : ___ 
  ]:= MThrow[head, msg, args];
  
  MThrow[ 
    tag     : _String | _Symbol, 
    msg     : HoldPattern[MessageName[head:_Symbol, name:_String]], 
    args    : _Association, 
    payload : _Association : <||>
  ] := MThrow @ MFailureGenerate[tag, msg, args, payload]
  (* order matters, do not shuffle *)
  MThrow[ 
    tag     : _String | _Symbol , 
    msg     : HoldPattern[ MessageName[head:_Symbol, name:_String]], 
    args    : ___,
    payload : _Association : <||>
  ]:= MThrow @ MFailureGenerate[tag, msg, {args}, payload ];
  
  
  
  



(* ::Section::Closed:: *)
(*MThrowAll*)


  MThrowAll // Attributes = {HoldAll};
  
  MThrowAll[
    tag     : _String | _Symbol | PatternSequence[]
  , msg     : _MessageName
  , args    : __
  , payload : _Association : <||>
  ]:=(Message[msg, args]; MThrow[tag, msg, args, payload]) 
  (*GIGO: Message can't handle parameters from association so think!*)
   


(* ::Section::Closed:: *)
(*MHandleResult*)


MHandleResult::usage = "MHandleResult[(patt -> handler)..] creates an operator:" <>
  "Function[ input, Switch[input, patt, handler, .., _?MFailureQ, MThrow, _, Identity] @ input].";
  
MHandleResult[rules___]:=Function[
  expr
, Switch[expr, rules, _?MFailureQ, MThrow, _, Identity] @ expr
]

(* It should throw or return the input but it is up to the user what rules are put there *)
(* It makes ThrowOnFailure redundant as it contains that rule by default. *)


(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
