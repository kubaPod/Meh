(* ::Package:: *)

Needs @ "Meh`";


outputFormString = ToString[#, Symbol["OutputForm"]]& ;
foo::argx = "foo `arg`";
foo::argy = "foo `1`";
foo::string = "`` is not a String";


(* ::Subsection:: *)
(*MFailureQ*)


VerificationTest[(* 1 *)
  MFailureQ /@ {
    $Failed, $Aborted, $Canceled, Failure["any","Message"->"Generic message"]
  }
, {True,True,True,True}  
]


(* ::Subsection::Closed:: *)
(*MCatch / MThrow*)


VerificationTest[
  MCatch[MThrow/@{$Failed,$Canceled,$Aborted}],
  Failure["err", <|"Message" -> "$Failed"|>],
  TestID -> "cdeccebc-bfed-446e-a7ce-58e8a0ae48cc"
]


VerificationTest[
  MCatch@*MThrow/@{
    $Failed, $Canceled, $Aborted
  , Failure[], Failure["General",<|"MessageTemplate"->"``","MessageParameters"->{1}|>]
  , whatever
  }
, { Failure["err", <|"Message" -> "$Failed"|>]
  , Failure["err", <|"Message" -> "$Canceled"|>]
  , Failure["err", <|"Message" -> "$Aborted"|>]
  , Failure[]
  , Failure["General", <|"MessageTemplate" -> "``", "MessageParameters" -> {1}|>]
  , whatever
  }
, TestID -> "e2481006-2edb-4efe-964d-24c0ab93a453"
]


VerificationTest[
  MCatch @ MThrow @ Failure["tag",<|"Message"->"static"|>],
  Failure["tag", <|"Message" -> "static"|>],
  TestID -> "83b0a819-0923-4bf3-b15a-f0ea5c20943d"
]


(* ::Subsection::Closed:: *)
(*MGenerateFailure*)


(* a lot is tested via MThrow* tests so I do not duplicate it here *)


VerificationTest[
  MGenerateFailure["invalid", "string"]
, Failure["argpatt", <|"Message" -> "There are no rules associated with signature MGenerateFailure[String, String]."|>]
, {MGenerateFailure::argpatt}
, TestID -> "28fd432a-0c96-4e40-96e3-102c265be76a"
]


VerificationTest[
  MGenerateFailure["should act as an Identity for a single argument"]
, "should act as an Identity for a single argument"
, TestID -> "7227a794-ecd5-49aa-8bc0-58c2665f4060"
]


VerificationTest[
  MGenerateFailure[ $Aborted]
, Failure["err", <|"Message" -> "$Aborted"|>]
, TestID -> "eb78016c-3cab-4629-b789-d38e94a4679d"
]


VerificationTest[
  MCatch @ MThrowAll["invalid", "string"]
, Failure["argpatt", <|"Message" -> "There are no rules associated with signature MGenerateAll[String, String]."|>]
, {MGenerateAll::argpatt}
, TestID -> "c920935d-ce0b-4b44-b0e7-949d4883e3b4"
]


VerificationTest[
  MGenerateAll[General::argt,foo,2,3,4]
, Failure["argt", <|"MessageTemplate" :> General::argt, "MessageParameters" -> {foo, 2, 3, 4}|>]
, {General::argt}
, TestID -> "d1155b30-e4ea-481e-a660-acce042b5ff2"
]


(* ::Subsection:: *)
(*message::name throw/throwAll*)


(* ::Subsubsection:: *)
(*Message like syntax *)


VerificationTest[
  MCatch @ MThrow[General::argt,foo,2,3,4],
  Failure["argt", <|"MessageTemplate" :> General::argt, "MessageParameters" -> {foo, 2, 3, 4}|>],
  TestID -> "2f9c8c4a-3296-488e-9c88-a57174cb1839"
]


VerificationTest[
  MCatch @ MThrowAll[General::argt,foo,2,3,4],
  Failure["argt", <|"MessageTemplate" :> General::argt, "MessageParameters" -> {foo, 2, 3, 4}|>],
  General::argt,
  TestID -> "c51e6c29-02b4-45d4-999e-9ef8364a3afc"
]


(* ::Subsubsection:: *)
(*Same but with custom tag*)


VerificationTest[
  MCatch @ MThrow["custom tag",General::argt,foo,2,3,4],
  Failure["custom tag", <|"MessageTemplate" :> General::argt, "MessageParameters" -> {foo, 2, 3, 4}|>],
  TestID -> "ed8d64e4-75b0-4c68-bda0-fc635786a82f"
]


VerificationTest[
  MCatch @ MThrowAll["custom tag",General::argt,foo,2,3,4],
  Failure["custom tag", <|"MessageTemplate" :> General::argt, "MessageParameters" -> {foo, 2, 3, 4}|>],
  General::argt,
  TestID -> "568d41cf-8ee3-4805-a223-fc8346d00ba4"
]


(* ::Subsubsection:: *)
(*Failure like syntax*)


VerificationTest[
  MCatch @ MThrow[foo::argx, <|"arg"->bar|>],
  Failure["argx", <|"MessageTemplate" :> foo::argx, "MessageParameters" -> <|"arg" -> bar|>|>],
  TestID -> "1b2f7ddc-b2f7-42bb-82a4-de304864bedd"
]


(* ::Subsubsection:: *)
(*Same but with custom tag*)


VerificationTest[
  MCatch @ MThrow[bar,foo::argx,<|"arg"->bar|>],
  Failure[bar, <|"MessageTemplate" :> foo::argx, "MessageParameters" -> <|"arg" -> bar|>|>],
  TestID -> "34f9f256-3b1f-41ad-b584-76864bb1b39b"
]


(* ::Subsection:: *)
(*payload*)


(* ::Text:: *)
(*Failure like syntax with additional payload*)


VerificationTest[
  MCatch @ MThrow[
    bar
  , foo::argx
  , <|"arg"->bar|>
  , <|"Payload"->"additional information"|>
  ]
, Failure[bar
  , <|"MessageTemplate" :> foo::argx
    , "MessageParameters" -> <|"arg" -> bar|>
    , "Payload" -> "additional information"
    |>
  ]
, TestID -> "d4b3e988-550f-4fe5-9212-c7ffc737c2b3"
]


VerificationTest[
  MCatch @ MThrow[
    "custom tag"
  , General::argt
  , foo,2,3,4
  , <|"Payload"->"additional information"|>
  ]
, Failure[
    "custom tag"
  , <|"MessageTemplate" :> General::argt
    , "MessageParameters" -> {foo, 2, 3, 4}
    , "Payload" -> "additional information"
    |>
  ]
, TestID -> "1a6f5d87-c8ec-4dda-b0b3-efcd7e9cd443"
]


(* ::Subsection:: *)
(*MHandleResult*)


VerificationTest[
  MCatch[
    $Aborted // MHandleResult[]; 1
  ]
, Failure["err", <|"Message" -> "$Aborted"|>]
, TestID -> "bf83f8f6-2d77-47b2-96b0-7dc4750e778d"
]


VerificationTest[
  MCatch[
    {"string"} // MHandleResult[
      Except[_String] , Function[res, MThrow[foo::string, Head[res], _String]]
    ]
  ]
, Failure["string"
  , <|"MessageTemplate" :> foo::string, "MessageParameters" -> {List, _String}|>
  ]
, TestID -> "aa27e31c-8219-4ecb-861c-0b36c679cf74"
]


VerificationTest[
  MCatch[
    {"string"} // MHandleResult[
      Except[_String] , Function[res, MThrowAll[foo::string, Head[res], _String]]]
  ]
, Failure["string", <|"MessageTemplate" :> foo::string, "MessageParameters" -> {List, _String}|>]
, {foo::string}
, TestID -> "1754dc50-0240-492f-8a31-10ca32b964cd"
]


VerificationTest[
  MCatch[
"string" // MHandleResult[
  Except[_String] , Function[res, MThrowAll[foo::string, Head[res], _String]]]
]
, "string"
, TestID -> "1754dc50-0240-492f-8a31-10ca32b964cd"
]


VerificationTest[
  MCatch[
"string" // MHandleResult[
  Except[_String] , Function[res, MThrowAll["500", foo::string, Head[res], _String]]]
]
, "string"
, TestID -> "1754dc50-0240-492f-8a31-10ca32b964cd"
]


VerificationTest[
  MCatch[
List@"string" // MHandleResult[
  Except[_String] , Function[res, MThrowAll["500", foo::string, Head[res], _String]]]
]
, Failure["500", <|"MessageTemplate" :> foo::string, "MessageParameters" -> {List, _String}|>]
, {foo::string}
, TestID -> "1754dc50-0240-492f-8a31-10ca32b964cd"
]


(* ::Subsection:: *)
(*M*OnFailure*)


VerificationTest[
  $Failed // MOnFailure[List]
, {$Failed}
, TestID -> "846538e8-04f2-4f8a-baf5-afa5b6656990"
]


VerificationTest[
  {} // MOnFailure[List]
, {}
, TestID -> "846538e8-04f2-4f8a-baf5-afa5b6656991"
]


VerificationTest[
  MCatch[ $Failed // MOnFailure[MThrow] ]
, Failure["err", <|"Message" -> "$Failed"|>]
, TestID -> "846538e8-04f2-4f8a-baf5-afa5b6656992"
]


VerificationTest[
  MCatch[ $Failed // MThrowOnFailure ]
, Failure["err", <|"Message" -> "$Failed"|>]
, TestID -> "846538e8-04f2-4f8a-baf5-afa5b6656993"
]


(* ::Section::Closed:: *)
(*Function construction*)


(* ::Subsection:: *)
(*FailByDefault*)


VerificationTest[
  foo[x_]:=x^2; foo // MFailByDefault ; foo[1,2]
, Failure["argpatt", <|"MessageTemplate" :> foo::argpatt, "MessageParameters" -> {"foo[Integer, Integer]"}|>]
, {foo::argpatt}
, TestID -> "853f20f1-3739-4f66-8449-083b6593ad55"
];


(* ::Subsection:: *)
(*RetryOnFailure*)


VerificationTest[
  Block[{i=0}
, {MRetryOnFailure[i++; $Failed, 2  ]  , i }
]
, {$Failed, 3}
, TestID -> "5c8421b7-0a3e-41d6-9c3e-d61554673ed1"
]


VerificationTest[
  Block[{i=0}
, {MRetryOnFailure[i++; $Failed ]  , i }
]
, {$Failed, 2}
, TestID -> "f51382ec-06d3-4e22-9c5f-227aa8ed6a8e"
]


Block[{i=0}
, {MRetryOnFailure[i++; $Failed, 2  ]  , i }
]


(* ::Section:: *)
(*Control flow*)


(* ::Subsection::Closed:: *)
(*MFailureToHTTPResponse*)


previewHTTPResponse = Apply[{ ImportString[ByteArrayToString@#, "RawJSON"], ##2}&];


VerificationTest[
  previewHTTPResponse @ MFailureToHTTPResponse @ $Failed
, {<|"Message"->"$Failed","Payload"-><|"MessageList"->"{}"|>|>,<|"StatusCode"->"500","ContentType"->"application/json"|>,CharacterEncoding->None}
, TestID -> "generic err ToHTTPResponse"
]


VerificationTest[
  previewHTTPResponse @ MFailureToHTTPResponse @ Failure["404", <|"MessageTemplate" -> "test 404"|>]
, {<|"Message"->"test 404","Payload"-><|"MessageList"->"{}"|>|>,<|"StatusCode"->"404","ContentType"->"application/json"|>,CharacterEncoding->None}
, TestID -> "failure with status code ToHTTPResponse"
]


VerificationTest[
  previewHTTPResponse @ MFailureToHTTPResponse @ (1/0; Failure["404", <|"MessageTemplate" -> "test 404"|>])
, {<|"Message" -> "test 404", "Payload" -> <|"MessageList" -> "{Power::infy}"|>|>, <|"StatusCode" -> "404", "ContentType" -> "application/json"|>, CharacterEncoding -> None}
, {Power::infy}
, TestID -> "message list capturing for ToHTTPResponse"
]


VerificationTest[
  previewHTTPResponse @ MFailureToHTTPResponse @ (1/0; Failure["404", <|"MessageTemplate" -> "test 404", "PageAddress" -> "127.0.0.1"|>])
, {<|"Message" -> "test 404", "Payload" -> <|"PageAddress" -> "127.0.0.1", "MessageList" -> "{Power::infy}"|>|>, <|"StatusCode" -> "404", "ContentType" -> "application/json"|>, CharacterEncoding -> None}
, {Power::infy}
, TestID -> "Payload in ToHTTPResponse"
]


VerificationTest[
  previewHTTPResponse @ MFailureToHTTPResponse @ Failure[Symbol, <|"MessageTemplate" -> "test 404"|>]
, {<|"Message" -> "test 404", "Payload" -> <|"MessageList" -> "{}"|>|>, <|"StatusCode" -> "500", "ContentType" -> "application/json"|>, CharacterEncoding -> None}
, TestID -> "symbol in tag"
]


VerificationTest[
  previewHTTPResponse @ MFailureToHTTPResponse @ Failure["any string", <|"MessageTemplate" -> "test 404"|>]
, {<|"Message" -> "test 404", "Payload" -> <|"MessageList" -> "{}"|>|>, <|"StatusCode" -> "500", "ContentType" -> "application/json"|>, CharacterEncoding -> None}
, TestID -> "any string in tag"
]


(* ::Section:: *)
(*StructValidation*)


(* ::Subsection::Closed:: *)
(*StructValidate*)


VerificationTest[
  StructValidate[
  <|"a" -> <|"b" -> 2, "c"->3, "d" -> 4|>|>
, KeyValuePattern[{"a" -> KeyValuePattern[{"b" -> _Integer, "c" -> _Integer, "d" -> _Integer}]}]  
]
, True
, TestID -> "d6201a9f-2904-449f-b3ed-e504292df7c7"
]


VerificationTest[
  StructValidate[
  <|"a" -> <|"b" -> 2, "c"->3, "d" -> 4|>|>
, KeyValuePattern[{"a" -> KeyValuePattern[{"b" -> _Integer, "c" -> _String, "d" -> _List}]}]  
]
, False
, TestID -> "a1add116-bcfa-4405-b0e5-b00e08793892"
]


(* ::Subsection::Closed:: *)
(*StructUnmatchedPositions*)


VerificationTest[
  StructUnmatchedPositions[
  <|"a" -> <|"b" -> 2, "c"->3, "d" -> 4|>|>
, KeyValuePattern[{"a" -> KeyValuePattern[{"b" -> _Integer, "c" -> _String, "d" -> _List}]}]  
]
, {{Key["a"], Key["c"]}, {Key["a"], Key["d"]}}
, TestID -> "3669a3c5-2875-4984-bd6d-35e620cec327"
]


(* ::Subsection:: *)
(*FailOnInvalidStruct*)


ClearAll[foo];
$fooPatt = KeyValuePattern[{"a" -> _Integer}];
foo // FailOnInvalidStruct[ $fooPatt ]
foo[ in: $fooPatt]:= in["a"]


VerificationTest[
  foo @ <|"a" -> 1|>
, 1
, TestID -> "9e1c0021-71e8-4820-8ca9-107726dc35dc"
]


VerificationTest[
  foo @ <|"b" -> 1|>
, Failure["400", <|"MessageTemplate" :> foo::invStruct, "MessageParameters" -> {foo, "\t{Key[a]}", "\t<|a -> _Integer|>"}|>]
, {foo::invStruct}
, TestID -> "f6b5fd29-4899-4f06-8021-2f55c3ec4d02"
]


(* ::Section:: *)
(*Utilities*)


(* ::Subsection::Closed:: *)
(*ToKeyValue*)


VerificationTest[
  Block[{x = 1, y = 2}, ToKeyValue @ {x, y}]
, {"x" -> 1, "y" -> 2}
, TestID -> "6b3e93c2-201e-43ab-aaec-a9a5dd7c6276"
]


VerificationTest[
  Module[{x = 1, y = 2, z = "string"}, ToKeyValue @ {x,y,z}]
, {"x" -> 1, "y" -> 2, "z" -> "string"}
, TestID -> "95412c7c-42f7-46f8-a5a5-7fcc3819dd28"
]
