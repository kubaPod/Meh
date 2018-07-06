(* ::Package:: *)

Needs @ "Meh`";


outputFormString = ToString[#, Symbol["OutputForm"]]& ;
foo::argx = "foo `arg`";
foo::argy = "foo `1`";
foo::string = "`` is not a String";


(* ::Subsection:: *)
(*Generic*)


VerificationTest[(* 1 *)
  MFailureQ /@ {
    $Failed, $Aborted, $Canceled, Failure["any","Message"->"Generic message"]
  }
, {True,True,True,True}  
]


VerificationTest[
  MCatch[MThrow/@{$Failed,$Canceled,$Aborted}],
  Failure["General", <|"Message" -> "$Failed"|>],
  TestID -> "cdeccebc-bfed-446e-a7ce-58e8a0ae48cc"
]


VerificationTest[
  MCatch@*MThrow/@{
    $Failed, $Canceled, $Aborted
  , Failure[], Failure["General",<|"MessageTemplate"->"``","MessageParameters"->{1}|>]
  , whatever
  }
, { Failure["General", <|"Message" -> "$Failed"|>]
  , Failure["General", <|"Message" -> "$Canceled"|>]
  , Failure["General", <|"Message" -> "$Aborted"|>]
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
(*message::name throw/throwAll*)


(* ::Subsubsection::Closed:: *)
(*Message like syntax *)


VerificationTest[
  MCatch @ MThrow[General::argt,foo,2,3,4],
  Failure[General, <|"MessageTemplate" :> General::argt, "MessageParameters" -> {foo, 2, 3, 4}|>],
  TestID -> "2f9c8c4a-3296-488e-9c88-a57174cb1839"
]


VerificationTest[
  MCatch @ MThrowAll[General::argt,foo,2,3,4],
  Failure[General, <|"MessageTemplate" :> General::argt, "MessageParameters" -> {foo, 2, 3, 4}|>],
  General::argt,
  TestID -> "c51e6c29-02b4-45d4-999e-9ef8364a3afc"
]


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
(*Failure like syntax*)


VerificationTest[
  MCatch @ MThrow[foo::argx,<|"arg"->bar|>],
  Failure[foo, <|"MessageTemplate" :> foo::argx, "MessageParameters" -> <|"arg" -> bar|>|>],
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
, Failure["General", <|"Message" -> "$Aborted"|>]
, TestID -> "bf83f8f6-2d77-47b2-96b0-7dc4750e778d"
]


VerificationTest[
  MCatch[
    {"string"} // MHandleResult[
      Except[_String] , Function[res, MThrow[foo::string, Head[res], _String]]
    ]
  ]
, Failure[foo
  , <|"MessageTemplate" :> foo::string, "MessageParameters" -> {List, _String}|>
  ]
, TestID -> "aa27e31c-8219-4ecb-861c-0b36c679cf74"
]


VerificationTest[
  MCatch[
    {"string"} // MHandleResult[
      Except[_String] , Function[res, MThrowAll[foo::string, Head[res], _String]]]
  ]
, Failure[foo, <|"MessageTemplate" :> foo::string, "MessageParameters" -> {List, _String}|>]
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
