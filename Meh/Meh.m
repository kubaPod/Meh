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

Needs @ "GeneralUtilities`";

  Meh;
  
  MFailureQ;  MGenerateFailure;  MGenerateAll;
  
  MCatch;  MThrow;  MThrowAll;
  
  MHandleResult;    MOnFailure;  MThrowOnFailure;  MRetryOnFailure;
  
  MFailByDefault;
  
  FailOnInvalidStruct;  StructMatch;  MatchedElement;  StructValidate;  
  StructUnmatchedPositions;
  
  MFailureToHTTPResponse
  
  MCheckValue;
  
  MExpect;
  
  
  
   APIMessage;
  AmbientCheck;
  PrintLoggerBlock;

  CloudTopLevelFunction;
  CloudDecorator;

Begin["`Private`"];



(* ::Chapter:: *)
(* Implementation code*)


(* ::Section::Closed:: *)
(*misc*)


  Meh::usage = "Meh is a symbol that stores common messages used by Meh`";
  
  Meh::match = "Unexpected error, expression with head `` does not match ``.";
  
  Meh::argpatt = "There are no rules associated with signature ``."; 
  
  
  Meh::invInput = "Invalid input for ``";

  Meh::invStruct = StringRiffle[
    {"``: Invalid values at positions:", "``", "Input needs to match:","``"},
    "\n\n"
  ];

  Meh::invStructHttp = StringRiffle[
    {"Invalid HTTPRequest. Body values at positions:", "``", "HTTPRequest.Body needs to match:", "``"},
    "\n\n"
  ]


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
  


(* ::Section::Closed:: *)
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
(*MGenerateFailure: message/failure like syntactic sugar*)


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
  ]:= MGenerateFailure[tag, msg, {args}, payload ]


MGenerateAll[
    tag     : _String | _Symbol | PatternSequence[]
  , msg     : _MessageName
  , args    : __
  , payload : _Association : <||>
  ]:=(
    Message[msg, args]
  ; MGenerateFailure[tag, msg, args, payload]
  );


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


(* ::Subsection::Closed:: *)
(*MFailureToHTTPResponse*)


MFailureToHTTPResponse// ClearAll;



(*TODO: failure generate? *)

MFailureToHTTPResponse[ failure:($Failed|$Aborted|$Canceled) ]:= 
  MFailureToHTTPResponse @ Failure["500","MessageTemplate" -> ToString[failure]]



MFailureToHTTPResponse[
    f : Failure[
      tag_String?(StringMatchQ[DigitCharacter..])
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
]


failureToStatusCode[ Failure[ tag_String?(StringMatchQ[DigitCharacter..]) , asso_ ] ]:= tag; 
failureToStatusCode[ _?MFailureQ ]:= "500";
failureToStatusCode // MFailByDefault


failureToPayload[ Failure[ tag_, asso_ ] ] := <|
  KeyDrop[{"MessageTemplate","MessageParameters"}] @ asso
, "MessageList" -> ToString @ $MessageList  
|>;


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


(* ::Section::Closed:: *)
(*migrated*)


(* ::Subsection:: *)
(*MCheckValue*)


MCheckValue::usage = "MCheckValue[expr, test, action] - TrueQ[test[expr]] ? expr : action";


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
(*api utils migrated*)


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

  PrintLoggerBlock[expr___] /; Not[$CloudEvaluation] := expr

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
