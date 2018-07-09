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


(* ::Subsection:: *)
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


(* ::Subsection:: *)
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


(* ::Subsubsection::Closed:: *)
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


(* ::Subsection:: *)
(*FailByDefault*)


VerificationTest[
  foo[x_]:=x^2; foo // MFailByDefault ; foo[1,2]
, Failure["argpatt", <|"MessageTemplate" :> foo::argpatt, "MessageParameters" -> {"foo[Integer, Integer]"}|>]
, {foo::argpatt}
, TestID -> "853f20f1-3739-4f66-8449-083b6593ad55"
];
