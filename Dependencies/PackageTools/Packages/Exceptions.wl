(* ::Package:: *)

(* ::Section:: *)
(*Exceptions*)


Exception::usage="Default head for throwing errors";


$ExceptionTag::usage="A default error tag";
ExceptionBlock::usage="ExceptionBlock[tag, expr] creates a block to catch errors";


ThrowException::usage="ThrowException[tag, msg] throws an exception without emitting\
 a message";
CatchException::usage="CatchException[tag, expr] catches an exception";
$CatchExceptionCallback::usage="the default exception handler";
$ExceptionErrorMessage::usage="The default error message template";


ThrowMessage::usage="ThrowMessage[tag, msg] emits a message without throwing an error";
CatchMessage::usage="CatchMessage[expr] catches a message via Check";
CatchMessageCallback::usage="the default exception handler for messages";


CreateFailureException::usage="CreateFailureException[tag, msg] creates an exception\
 that's formatted as a FailureObject";
RaiseException::usage="RaiseException[tag] throws and error and emits a message";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*PackageThrowException*)


ThrowException[value:Except[_Failure], tag:_String:"Failure"]:=
  Throw[value, Exception[tag]];
ThrowException[f_Failure]:=
  Throw[f, Exception[f[[1]]]];


(* ::Subsubsection::Closed:: *)
(*PackageCatchException*)


$CatchExceptionCallback=(#&);


CatchExceptionHandler[tag_]:=
  $CatchExceptionCallback;


CatchException[
  expr:Except[_String], 
  tag:_String|Verbatim[Alternatives][__String]:"Failure", 
  callback_:Automatic
  ]:=
  Catch[
    expr,
    If[StringQ@tag, Exception[tag], Exception/@tag], 
    Replace[callback, Automatic:>CatchExceptionHandler[tag]]
    ];
CatchException[tag_String, callback_:Automatic]:=
  Function[Null,
    CatchException[#, tag, callback],
    HoldFirst
    ];
CatchException~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*ThrowMessage*)


$ExceptionErrorMessage=
  "Encountered exception '``'";


Options[ThrowMessage]=
  {
    "MessageParameters":>{}
    };
ThrowMessage[
  msg_MessageName, 
  body:_?StringQ,
  ops:OptionsPattern[]
  ]:=
  (
    Set[msg, body];
    Message[msg, Sequence@@OptionValue["MessageParameters"]]
    );
ThrowMessage[
  tag_?StringQ,
  body_?StringQ,
  ops:OptionsPattern[]
  ]:=
  ThrowMessage[
    MessageName[Exception, tag],
    body,
    ops
    ];
ThrowMessage[
  tag_?StringQ
  ]:=
  ThrowMessage[
    MessageName[Exception, tag],
    $ErrorMessage,
    "MessageParameters"->{tag}
    ];
ThrowMessage~SetAttributes~HoldAll


(* ::Subsubsection::Closed:: *)
(*CatchMessage*)


$CheckErrorMessage=
  "Check caught exceptions '``'";


$CatchMessageCallback=
  Function[
    RaiseExcption[
      "Check",
      $CheckErrorMessage,
      "MessageParameters"->Thread[HoldForm[$MessageList]]
      ]
    ];


CatchMessage[
  head_,
  expr_,
  failexpr_:Automatic,
  msg:{___String}:{}
  ]:=
  With[{t=If[Head[Unevaluated[head]]=!=Symbol, head, Unevaluated[head]]},
    Replace[
      Thread[Map[Hold[MessageName[head, #]]&, msg], Hold],
      {
        {}:>
          Check[expr, 
            Replace[failexpr, {Automatic:>$CatchMessageCallback[]}]
            ],
        Hold[msgs_]:>
          Check[expr, 
            Replace[failexpr, {Automatic:>$CatchMessageCallback[]}], 
            msgs
            ],
        _:>
          Replace[failexpr, {Automatic:>$CatchMessageCallback[]}]
        }
      ]
    ];
CatchMessage~SetAttributes~HoldAll;


(* ::Subsubsection::Closed:: *)
(*CreateFailureException*)


Options[CreateFailureException]=
  Options[ThrowMessage];
CreateFailureException[
  msg_MessageName,
  body_?StringQ,
  ops:OptionsPattern[]
  ]:=
  Replace[
    OptionValue[Automatic, Automatic, "MessageParameters", Hold],
    Hold[params_]:>
      Failure[
        Hold[msg][[1, 2]],
        <|
          "MessageTemplate":>
            msg,
          "MessageParameters":>
            params
          |>
        ]
    ];
CreateFailureException[
  head:_Symbol:Exception,
  tag:_?StringQ:"Exception",
  body_?StringQ,
  ops:OptionsPattern[]
  ]:=
  (
    Set[MessageName[head, tag], body];
    CreateFailureException[
      MessageName[head, tag],
      body,
      ops
      ]
    );
CreateFailureException~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*RaiseException*)


RaiseException//Clear;
Options[RaiseException]=
  Options[CreateFailureException]
RaiseException[
  msg_MessageName,
  body_?StringQ,
  ops:OptionsPattern[]
  ]:=
  (
    ThrowMessage[msg, body, 
      Evaluate@FilterRules[{ops}, Options[ThrowMessage]]
      ];
    ThrowException[
      CreateFailureException[msg, body, ops]
      ]
    );
RaiseException[
  head:_Symbol:Exception,
  tag_?StringQ,
  body_?StringQ,
  ops:OptionsPattern[]
  ]:=
  RaiseException[
    MessageName[head, tag],
    body,
    ops
    ];
RaiseException[
  tag:_?(StringQ[#]&&StringMatchQ[#, WordCharacter..]&),
  ops:OptionsPattern[]
  ]:=
  RaiseException[
    tag,
    $ExceptionErrorMessage,
    {
      ops,
      "MessageParameters"->{tag}
      }
    ];
RaiseException[
  Optional[Automatic, Automatic],
  body:_?StringQ,
  ops:OptionsPattern[]
  ]:=
  RaiseException[
    Evaluate@$ExceptionTag,
    body,
    ops
    ];
RaiseException[
  tag:Automatic|_String,
  body_String,
  args:__?(Not@*OptionQ)
  ]:=
  RaiseException[tag, body, "MessageParameters"->{args}];
RaiseException~SetAttributes~HoldFirst


(* ::Subsubsection::Closed:: *)
(*ExceptionBlock*)


If[!ValueQ[$ExceptionStack], $ExceptionStack={}];
Protect[$ExceptionStack];


$ExceptionTag="Failure";


ExceptionBlock//Clear
ExceptionBlock[
  expr_,
  tags:{__String}
  ]:=
  Block[
    {
      $ExceptionTag=tags[[1]],
      $ExceptionStack=Join[$ExceptionStack, tags],
      result,
      tagAlts=Alternatives@@tags
      },
    result=CatchException[expr, tagAlts, #&];
    If[MatchQ[result, Failure[_String?(StringEndsQ[tagAlts]), _]]&&
        MemberQ[Most@$ExceptionStack, tagAlts],
      ThrowException[result]
      ];
    result
    ];
ExceptionBlock[
  expr_,
  tag:_String
  ]:=ExceptionBlock[expr, {tag}];
ExceptionBlock[tag:_String|{__String}]:=
  Function[Null, ExceptionBlock[#, tag], HoldFirst];
ExceptionBlock[Optional[Automatic, Automatic]]:=
  Function[Null, ExceptionBlock[#, $ExceptionTag], HoldFirst];
ExceptionBlock~SetAttributes~HoldFirst;


(* ::Subsubsection::Closed:: *)
(*End*)


End[]
