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

Unprotect["`*", "`*`*"];
ClearAll["`*", "`*`*"];

Needs @ "GeneralUtilities`";

  
  ToKeyValue;  OptionLookup;  NotebookAliveQ;

  Meh;
  
  MFailureQ;  MGenerateFailure;  MGenerateAll;
  
  MCatch;  MThrow;  MThrowAll;
  
  MHandleResult;    MOnFailure;  MThrowOnFailure;  MRetryOnFailure;
  
  MFailByDefault;
  
  MFailureToHTTPResponse;
  
  MCheckValue;
  
  MExpect;
  
  APIMessage;  AmbientCheck;  PrintLoggerBlock;  CloudTopLevelFunction;  CloudDecorator;
  
  MValidateByDefault;  MValidateScan;  MValidationResult;  MValidate;  MInvalidContents;
  
  LogDialogBlock;  LogDialog;  LogWrite; LogDialogProgressIndicator;
  
  

Begin["`Private`"];



(* ::Chapter:: *)
(* Implementation code*)


(* ::Section:: *)
(*Association Utilities*)


ToKeyValue::usage = "ToKeyValue[symbol] is a small utility that generates \"symbol\" -> symbol which shortens association assembling.";

ToKeyValue // Attributes = {HoldAll, Listable};

ToKeyValue // MFailByDefault;

ToKeyValue[sym_Symbol]:= StringTrim[SymbolName @ Unevaluated @ sym, "$".. ~~ DigitCharacter..] -> sym;




(* ::Section::Closed:: *)
(*FrontEnd Utilities*)


NotebookAliveQ[nb_NotebookObject]:=  NotebookInformation[nb] =!= $Failed;
NotebookAliveQ[___]:=False;


(*TODO: absolute options
OptionLookup[name_,function_,explicit_List]:=Replace[
OptionValue[function,FilterRules[explicit,Options[function]],name],
Echo@First@Cases[AbsoluteOptions[function],(name->rules_List)\[RuleDelayed]rules]
]

ClearAll[foo];
foo//Options={a\[Rule]Automatic,b->b0};
foo/:foo//AbsoluteOptions={a\[Rule]{Automatic\[Rule]auto}};

foo[x_,opts___Rule]:={x,OptionLookup[a,foo,{opts}]}

foo[7] (*works*)
foo[7,a\[Rule]1] (*works*)
foo[7,AnotherOption\[Rule]1] (*no message, yes!*)
*)


(* ::Section:: *)
(*Meh misc*)


  Meh::usage = "Meh is a symbol that stores common messages used by Meh`";
  
  Meh::match = "Unexpected error, expression with head `` does not match ``.";
  
  Meh::argpatt = "There are no rules associated with signature ``."; 
  
  
  Meh::invInput = "Invalid input for ``";

  Meh::InvalidArg = StringRiffle[
    {"Argument No. `` has invalid structure at:", "``", "It needs to match:","``"},
    "\n"
  ];
  
  Meh::NoArg = "Function called without arguments.";

  Meh::invStructHttp = StringRiffle[
    {"Invalid HTTPRequest. Body values at positions:", "``", "HTTPRequest.Body needs to match:", "``"},
    "\n\n"
  ];


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
  


OptionLookup::usage = "OptionLookup[option, function, {opts__}] works like OptionValue[option] but does not require to use OptionsPattern[{...}] for the function.";
OptionLookup[name_,function_,explicit_List]:=OptionValue[function,FilterRules[explicit,Options[function]],name];


(* ::Section:: *)
(*Core*)


(* ::Subsection::Closed:: *)
(*MFailureQ*)


  MFailureQ[ _Failure | $Canceled | $Aborted | $Failed ]=True;
  (*Oone could think of an unevaluated pattern being here to but a) it adds complexity b) should not be enforced in case of symbolic wrappers/constructors. 
     So this will be handled by mHandle *)
  MFailureQ[_]=False;


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


(* ::Subsubsection:: *)
(*MGenerateFailure: message/failure like syntactic sugar*)


MGenerateFailure[ 
    msg  : MessageName[head : _Symbol, name : _String], 
    args : ___ 
  ]:= MGenerateFailure[name, msg, args];


MGenerateFailure[ 
    tag     : _String | _Symbol, 
    msg     : HoldPattern[MessageName[head:_Symbol, name:_String]], 
    args    : _Association     
  ] :=  MGenerateFailure[tag, msg, args, <||>];


(* ::Subsubsection:: *)
(*MGenerateFailure; MGenerateAll*)


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
  ]:= MGenerateFailure[tag, msg, {args}, payload ];


MGenerateAll[
    tag     : _String | _Symbol | PatternSequence[]
  , msg     : _MessageName
  , args    : __
  , payload : _Association : <||>
  ]:=(
    Message[msg, args]
  ; MGenerateFailure[tag, msg, args, payload]
  );


(*MGenerateFailure[expr : $Failed | $Canceled | $Aborted]:=  Failure["err", <|"Message" -> ToString[expr]|>]*)
(*TODO: I do not like "err"*)
(*TODO: GeneralErrorQ needed, to detect whether a _?FailureQ is also one of $Failed | $Canceled | $Aborted or generated from them*)


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
);


(* ::Subsection::Closed:: *)
(*MThrow / MCatch*)


  $MehTag = "MEH";    


  MCatch = Function[expr, Catch[expr, $MehTag], HoldAllComplete];


  MThrow // Attributes = {HoldAll};  


  MThrow[ f : Except[_MessageName] ]:=Throw[f, $MehTag ];  

(*

  MThrow[ f : $Failed | $Canceled | $Aborted ] := Throw[
    MGenerateFailure[f]
  , $MehTag
  ]

*)

  MThrow[ f : ___ ] := Throw[
    MGenerateFailure[f]
  , $MehTag
  ];


(* ::Subsection::Closed:: *)
(*MThrowAll*)


  MThrowAll // Attributes = {HoldAll};
  
  MThrowAll[spec___]:= MThrow @ MGenerateAll[spec]; 


(* ::Section::Closed:: *)
(*Flow control*)


(* ::Subsection::Closed:: *)
(*MHandleResult*)


MHandleResult::usage = "MHandleResult[(patt -> handler)..] creates an operator:" <>
  "Function[ input, Switch[input, patt, handler, .., _?MFailureQ, MThrow, _, Identity] @ input].";
  
  
  
MHandleResult[rules___]:=Function[
  expr
, Switch[expr
  , rules
  , _?MFailureQ, MThrow
  , _          , Identity
  ] @ expr
];

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
MThrowOnFailure[res_]:= res;


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


(* ::Subsection::Closed:: *)
(*MFailureToHTTPResponse*)


MFailureToHTTPResponse// ClearAll;



(*TODO: failure generate? *)

MFailureToHTTPResponse[ failure:($Failed|$Aborted|$Canceled) ]:= 
  MFailureToHTTPResponse @ Failure["500","MessageTemplate" -> ToString[failure]];



MFailureToHTTPResponse[
    f : Failure[
      tag_
    , asso_
    ]
  ]:=With[
  { payload = If[# === <||>, <||>, "Payload" -> Compress @ #]& @ KeyDrop[{"MessageTemplate","MessageParameters"}] @ asso 
  }
, HTTPResponse[
    ExportString[
      <|  
        "Message" -> FailureString @ f
      , "Payload" -> failureToPayload @ f 
      |>
    , "RawJSON", "Compact"->True 
    ]
  , <|
      "StatusCode" -> failureToStatusCode @ f
    , "ContentType"->"application/json" 
    |>
  , CharacterEncoding -> None (*because RawJSON already did it*)
  ]
];


failureToStatusCode[ Failure[ tag_String?(StringMatchQ[DigitCharacter..]) , asso_ ] ]:= tag; 
failureToStatusCode[ _?MFailureQ ]:= "500";
failureToStatusCode // MFailByDefault;


failureToPayload[ Failure[ tag_, asso_ ] ] := <|
  KeyDrop[{"MessageTemplate","MessageParameters"}] @ asso
, "MessageList" -> ToString @ $MessageList  
|>;


(* ::Section::Closed:: *)
(*LogDialog (experimental)*)


$CurrentLogTag := "global";
$CurrentLogDialog := LogDialog[$CurrentLogTag];


(* ::Subsection::Closed:: *)
(*CreateLogDialog*)


CreateLogDialog::usage = "CreateLogDialog[tag_., opts___] creates a new log dialog associated with the tag.";

CreateLogDialog // Options = {
  "Header" -> ""
, "ProgressIndicator" -> Automatic  
};

CreateLogDialog[opts___Rule]:= CreateLogDialog[ $CurrentLogTag, opts ];

CreateLogDialog[tag_String, opts___Rule]:= Module[
  { dockedCells
  , progressIndicator = OptionLookup["ProgressIndicator", CreateLogDialog, {opts}] // Replace[ Automatic -> ProgressIndicator[Appearance->"Percolate"] ]
  , header = OptionLookup["Header", CreateLogDialog, {opts}]
  }

, dockedCells = {Cell[ BoxData @ ToBoxes @ Grid[
    {{ 
       Style[header, "Subsection"]
     , PaneSelector[
           { True -> progressIndicator
           , False -> Spacer[{0,0}]
           }
         , Dynamic @ CurrentValue[EvaluationNotebook[], {TaggingRules, "processing"}]
         , ImageSize    -> Full
         , ImageMargins -> 0
         , FrameMargins -> 0
         , Alignment    -> {Right, Center}
         
         ]
     }}, Alignment->{Left,Center}]
   , Background-> GrayLevel@.9
   , CellFrameMargins->15
   ]  
 }
 

; LogDialog[tag] = CreateDocument[
    {}
  , StyleDefinitions     -> "Dialog.nb"
  , Sequence @@ FilterRules[{opts}, {Options[CreateDocument], BaseStyle -> {}}]  
  , DockedCells          -> dockedCells
  , TaggingRules         -> {"LogDialog" -> True, "processing" -> True, "logTag" -> tag}
  , CellLabelPositioning -> Automatic
  , ShowCellLabel        -> True
  , WindowElements       -> {"VerticalScrollBar"}
  
  , WindowSize           -> 600{1, 1/GoldenRatio}
  , WindowTitle          -> "Progress dialog"  
  ]
];


(* ::Subsection::Closed:: *)
(*LogDialog*)


LogDialog::usage = "LogDialog[tag_.] retruns a specific or current log dialog's NotebookObject. Or $Failed if none is found.";

LogDialog::doesNotExist = "LogDialog associated with `1` does not exist.";
LogDialog::invalidTag = "`1` is not a valid tag. Tag needs to be a string.";
LogDialog::duplicatedDialogs = "Multiple dialogs associated with a tag `1` are found. The most recent one is returned. "<>
  "Use Select[Notebooks[], CurrentValue[#, {TaggingRules, \"tag\"}] === `1` &] to list them all.";

LogDialog[]:= $CurrentLogDialog;

LogDialog[tag_String]:= Module[{candidates}
, candidates = Select[Notebooks[], CurrentValue[#, {TaggingRules, "tag"}] === tag &]
; Switch[candidates
  , {}
  , MGenerateFailure[LogDialog::doesNotExist, tag]
  
  , {__NotebookObject}
  , If[Length @ candidates > 1, Message[LogDialog::duplicatedDialogs, tag]]
  ; LogDialog[tag] = First @ candidates
  
  ]
];

LogDialog[x_,___]:=(Message[LogDialog::invalidTag, x]; $Failed);



(* ::Subsection:: *)
(*LogDialogBlock*)


LogDialogBlock::usage = "LogDialogBlock[logTag_., opts][expr] makes sure that a LogDialog[logTag] exists "<>
  "and creates an environment for expr evaluation to manage logging. "<>
  "LogWrite[msg] should write to that dialog as well as LogDialog[] should return an associated NotebookObject";

LogDialogBlock // Options = {
  "StopProgressIndicator" -> True
, "AutoClose" -> False  
};

LogDialogBlock[ patt___Rule ]:=LogDialogBlock[ $CurrentLogTag, patt];

LogDialogBlock[tag_String, opts___Rule]:= With[
  { 
    stopIndicator = TrueQ @ OptionLookup["StopProgressIndicator", LogDialogBlock, {opts}  ]
  , autoClose     = OptionLookup["AutoClose", LogDialogBlock, {opts}  ]
  }
, Function[
    expression
  , Block[ {$CurrentLogDialog, $CurrentLogTag = tag}
    , If[ 
        Not @ NotebookAliveQ @ LogDialog[tag]
      , CreateLogDialog[tag, opts]
      ]
    ; $CurrentLogDialog = LogDialog[tag]
    ; With[{res = expression, nb = $CurrentLogDialog}
      , If[stopIndicator, LogDialogProgressIndicator @ False ]
      ; If[Positive @ autoClose, RunScheduledTask[NotebookClose @ nb, {autoClose}]]
      ; res
      ]
    ]
  , HoldAll  
  ]
];


(* ::Subsection::Closed:: *)
(*LogWrite*)


LogWrite::usage = "LogWrite[tag_., msg] writes a message to a log dialog associated with the tag.";

LogWrite[msg_]:=LogWrite[$CurrentLogDialog, msg];

LogWrite[tag_, msg: Except[_Cell]]:= LogWrite[tag, Cell[BoxData @ ToBoxes @ msg, "Output", CellLabel->DateString[{"Time",".","Millisecond"}]]];

LogWrite[nb_NotebookObject, msg_Cell]:=NotebookWrite[  nb, msg, After];



(* ::Subsection:: *)
(*Dialog utilities*)


LogDialogProgressIndicator[v : True | False]:=LogDialogProgressIndicator[ $CurrentLogDialog, v];
LogDialogProgressIndicator[nb_NotebookObject, val_]:= CurrentValue[nb, {TaggingRules, "processing"}] = val;


(* ::Section:: *)
(*Validation (beta)*)


(*TODO: distinguis invalid from missing etc*)
(*TODO: strict patterns: currently additional keys are ignored but maybe they should be, optionally, considered invalid*)
(*TODO: shouldn't MValidateByDefault be MFailOnInvalidStruct? *)


(* ::Subsection:: *)
(*MValidateByDefault*)


(*TODO: this should rather be By-default-validate-input-with-respect-to-given-struct*)


MValidateByDefault[structPattern_, argPost_Integer : 1]:= Function[function, MValidateByDefault[function, structPattern, argPost]];

MValidateByDefault[function_Symbol, structPattern_, argPos : _Integer : 1]:=(
  function::NoArg = Meh::NoArg;
  function::InvalidArg = Meh::InvalidArg;  
  
  (*we don't want additional function[]:= downvalue in case there already is one*)
  function[input___] /; Length[{input}] < 1 :=MGenerateAll[ "400", function::NoArg, function  ];
  
  function[input___]:= MGenerateAll[
      "400"
    , function::InvalidArg
    , argPos
    , {#, ToString @ #2[[2]]}& @@@ MInvalidContents[{input}[[argPos]], structPattern, 3] // StringRiffle[#, "\n", {"\t", "\t", ""}]&
    , "\t" <> ToString[structPattern /. KeyValuePattern->Association]
    ]
);


(* ::Subsection:: *)
(*MValidateScan*)


MValidateScan // ClearAll;
MValidationResult // ClearAll;
MValidationResult // Protect;

(*TODO: handle empty lists for Repeated*)
(*TODO: held expressions? *)
(*TODO: missing *)
(*TODO: strict KVP *)
MValidationResult::usage = "MValidationResult[True] or Matched[False, payload_]. MValidationResult is a symbolic wrapper used by MValidateScan to mark nested results of matching.";


MValidateScan::usage = "MValidateScan[expr, patter] returns expr with its elements replaced by MValidationResult[True] or MValidationResult[False, _]." <>
  " Currently it only scans KeyValuePatterns and other expressions are just MatchQ-ed.";


(* Matching associations *)
MValidateScan[
  expr    : KeyValuePattern[{}]
, pattern : _KeyValuePattern
]:=  Module[
  { patternAssociation = Association @@ pattern }
, Merge[ 
    { 
      KeyTake[Keys[patternAssociation]] @ expr (* we don't care about additional keys now*)
    , patternAssociation
    }
  , Apply[MValidateScan]
  ]  
];


(*This needs to be done better now, currently it is supposed to used during merging*)
MValidateScan[ _ ]:=MValidationResult[False, Missing[]]


(* I don't exactly remember why it is here, probably because MatchQ[{},KeyValuePattern[{}]] is True *)
MValidateScan[
  {}
, { Verbatim[Repeated][_KeyValuePattern, ___] }
] = MValidationResult[False];


(* Matching  uniform datasets *)

$multiPattern = Verbatim /@ ( Repeated|RepeatedNull );

MValidateScan[
  expr : {__}
, { $multiPattern[kvp_KeyValuePattern,___]}]:=  MValidateScan[#,kvp]& /@ expr;


(*Generic case*)
MValidateScan[
  expr_
, kvp_ (*: Except[_KeyValuePattern] *)
]:=  If[ 
  MatchQ[expr, kvp]
, MValidationResult[True]
, MValidationResult[False, Head @ expr]
];



MValidateScan // MFailByDefault


(* ::Subsection::Closed:: *)
(*MValidate*)


MValidate::usage = "MValidate[expr, patt] or MValidate[patt] @ expr returns True/False if MValidateScan[expr, patt] was successful/not successful.";


MValidate // ClearAll; (*not sure it makes sense, why not MatchQ[expr, patt]?*)

MValidate[patt_]:=Function[expr, MValidate[expr, patt]];

MValidate[expr_, patt_]:=FreeQ[MValidateScan[expr,patt], MValidationResult[False, ___]];


(* ::Subsection::Closed:: *)
(*MInvalidContents*)


MInvalidContents // ClearAll; (*TODO: position and element*)
MInvalidContents::usage = "MInvalidContents[expr, patt] returns {position -> MValidationResult[False,_] ..} from MValidateScan[expr, patt]."
MInvalidContents[expr_, patt_]:=  MValidateScan[expr,patt] // Function[mExpr
  , Thread[# -> Extract[mExpr, #]]& @ Position[mExpr, MValidationResult[False, ___]]
];

MInvalidContents[expr_, patt_, n_Integer?Positive]:= Take[MInvalidContents[expr, patt], UpTo[n]];


(* ::Section::Closed:: *)
(*migrated (experimental)*)


(* ::Subsection::Closed:: *)
(*MCheckValue*)


MCheckValue::usage = "Not fully intergrated. MCheckValue[expr, test, action] does TrueQ[test[expr]] ? expr : action.";


MCheckValue // Attributes = {HoldAll};


MCheckValue[expr_, test_, action_]:=If[ 
  Not @ TrueQ @ test @ expr
, action
, expr
];


MCheckValue[
  test : _ : Identity
, action : _ : Throw @ $Failed

]:=Function[expr, MCheckValue[expr,test,action]];


(* ::Section::Closed:: *)
(*api utils migrated (experimental)*)


(* ::Subsection::Closed:: *)
(*Discussion*)


(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: APIUtilities *)
(* :Context: APIUtilities` *)
(* :Author: Kuba *)
(* :Date: 2017-11-16 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Kuba *)
(* :Keywords: *)
(* :Discussion:
    The idea is to have a mini exceptions handling framework,
    together with logging
    and what we want to see at the end is a HTTPResponse not a Failure
    
    So the way to go is to use API/Form procedure template like so:
    
    
    APIFunction[
      { stuff }
    , PrintLoggerBlock @                 (*1*)
      MOnFailure[MFailureToHTTPResponse] @ (*2*)
      MCatch @                     (*3*)
      AmbientCheck @                     (*4*)
      Module[{}                          (*5*)
      , Print["calculating something"];1+1/0;Print["DEBUG", "done"];123
      ] &
    ]
    
    1. PrintLoggerBlock instantiates a log file in {object/path}<>LOG/{dateString}.txt
       and log all Print or messages there. See def for more info.
      
    2. At the end we are expecting an HTTPResponse but if Failure occured we convert it to HTTResponse
    
    3. catches any ThrowFailure or MThrow but we use tagged because the tag will contain
       status code to use by MFailureToHTTPResponse
       
    4. Like Check but it throws 'unknown error' with 500 tag on any message. See def for more syntax.
    
    
    Furthermore, replacing 1+2 with PrintBlock (tbd) leaves us with a method perfectly suited for desktop flow.
*)


(* ::Subsection::Closed:: *)
(*messages*)


  APIMessage::usage = "APIMessage is a symbol to store common messages for api methods";

  APIMessage::input = "Failed to parse input as ``";
  APIMessage::jsonInput = "Failed to parse input as a JSON object";
  APIMessage::err = "Unexpected error";
  APIMessage::src = "Source file is missing: ``";
  


(* ::Subsection::Closed:: *)
(*CloudTopLevelFunction*)


  CloudTopLevelFunction::usage = "CloudTopLevelFunction is a wrapper for a top level cloud side functions. You may want to add AmbientCheck before.";
  
  CloudTopLevelFunction // Attributes = {HoldAll};
  
  CloudTopLevelFunction[opts__Rule]:=Function[proc, CloudTopLevelFunction[proc, opts], HoldAll];
  
  CloudTopLevelFunction[proc:Except[_Rule], opts___?OptionQ]:= PrintLoggerBlock[
    MOnFailure[MFailureToHTTPResponse] @ MCatch @ proc
  , opts
  ];



(* ::Subsection::Closed:: *)
(*CloudDecorator*)


  CloudDecorator::usage = "Function returns Failure for unknown input." <>
      " In $CloudEvaluation it will be converted to HTTPResponse.";

  CloudDecorator[symbol_]:= (

    symbol[___]:=  If[
      $CloudEvaluation
    , MFailureToHTTPResponse
    , Identity
   ] @ MCatch @ MThrow["400", APIMessage::input, symbol]

  );




(* ::Subsection::Closed:: *)
(*AmbientCheck*)


  AmbientCheck // Attributes = {HoldAll};
  AmbientCheck[expr_]:=AmbientCheck[expr, APIMessage::err, Null];
  AmbientCheck[expr_, msg_, param_]:=AmbientCheck[expr, msg, param, "500"];
  AmbientCheck[expr_, msg_, args_, status_]:= Check[
    expr
  , MThrow[status, msg, args]
  ];


(* ::Subsection::Closed:: *)
(*PrintLoggerBlock*)


  PrintLoggerBlock // ClearAll;
  PrintLoggerBlock::usage = "Wram API or Form second argument's function body with it to instantiate log file"<>
    " and redirect Print and messages there.";

  PrintLoggerBlock // Attributes = {HoldAllComplete};
  PrintLoggerBlock // Options = {
    "LogFile" -> Automatic,
    "LogHeader" -> Automatic,
    "ResponseLogFunction" -> (Print@ToString@ToString[Short@ToString@InputForm[#], StandardForm]&)
  };

  PrintLoggerBlock[expr___] /; Not[$CloudEvaluation] := expr;

  PrintLoggerBlock[option__Rule]:= Function[expr, PrintLoggerBlock[expr, option], HoldAllComplete];

  PrintLoggerBlock[
    expr:Except[_Rule]
  , OptionsPattern[]
  
  ] /; $CloudEvaluation :=
  Module[
  {logFile, logStream, log, path, res, timeMark, finalLogFunction }
  
  , timeMark = AbsoluteTime[]
  ; logFile = OptionValue["LogFile"] /. Automatic :> cloudObjectLogFile[$EvaluationCloudObject]
  ; finalLogFunction = OptionValue["ResponseLogFunction"]
    
  ; If[Not @ FileExistsQ @ #, CloudPut["", #]]& @ logFile
  
  ; logStream = OpenAppend[logFile,FormatType->(OutputForm),PageWidth->Infinity]
  
  ; log[type:"INFO"|"DEBUG"|"MESSAGE":"INFO", msg__]:=Write[
      logStream
    , DateString[{"Time",".","Millisecond"," "}]
    , $SessionID, " "
    , StringPadRight[type,10]
    , StringRiffle[ToString[#,InputForm]&/@{msg}, " "]
    ]
  
  ; Switch[ OptionValue["LogHeader"]
    , False, {}
    , Automatic, AddAutoLogHeader[log, logStream]
    , _, log[OptionValue["LogHeader"]]
    ]
  
  
  
  ; Block[{Print = log, Echo, messageString }
    , Print = log
    ; Echo[input_, label_: "", pipe_: Identity] := CompoundExpression[
        Print["INFO", label /. "" :> (##&[]), pipe@input]
      , input
      ]
    ; messageString[Hold[Message[msg:MessageName[head_,name_],args___],_]]:=ToString[StringForm[msg/.Messages[head],args]]  
    
    ; Internal`HandlerBlock[ (*we could add to $Messages or $Output but then we can't control headers*)
      { "Message"
      , If[Last[#]
        , Print["MESSAGE",  messageString[#]]
        ]&
      }
      , res = expr
      ; finalLogFunction @ res
      ; Print @ StringTemplate["Kernel evaluation time `` [s]"][AbsoluteTime[]-timeMark]
      ; Close @ logStream
      ; res
    ]
  ]
];

    AddAutoLogHeader[ log_, logStream_]:=CompoundExpression[
      log[ StringTemplate["Date:        ``"]@DateString["ISODate"]]
    , log[ StringTemplate["CloudObject: ``"]@$EvaluationCloudObject]
    , log[ StringTemplate["Requester:   ``"]@$RequesterWolframID]
    , Write[logStream,  ""]
    
    ];
  
    cloudObjectLogFile[cloudObj_]:= Module[{path}
    , Needs["CloudObject`"]
    ; path = CloudObjectInformation[$EvaluationCloudObject,"Path"]
    ; path = If[StringQ @ #, FileNameDrop[#,1], "logs/general.txt"]& @ path
  
    ; StringTemplate["``_logs/``/``.txt"][
        path
      , DateString["ISODate"]
      , DateString[{"Time", ":", "Millisecond"}] // StringReplace[":"->"-"]
      ]
    ];





(* ::Chapter:: *)
(* End package*)


End[];

EndPackage[];
