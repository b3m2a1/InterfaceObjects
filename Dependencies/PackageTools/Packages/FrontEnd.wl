(* ::Package:: *)

(* ::Subsection:: *)
(*FrontEnd*)


BeginDisabledSymbolColoring::usage="Disables symbol coloring if it's not already off";
EndDisabledSymbolColoring::usage="Enables symbol coloring if it's not already on";
WithDisabledSymbolColoring::usage="Disables symbol coloring in a block";
WithEnabledSymbolColoring::usage="Enables symbol coloring in a block";


RemoveFrontEndSymbolColoring::usage="Removes symbol coloring from a set of symbols";
AddFrontEndSymbolColoring::usage="Adds symbol coloring to a set of symbols";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*BeginDisabledSymbolColoring*)


BeginDisabledSymbolColoring[]/;$Notebooks:=
  Module[{ctr=$FEHidingCounter},
    If[!ValueQ@ctr, ctr=0];
    If[ctr===0, Internal`SymbolList[False]];
    $FEHidingCounter = ctr+1;
    ]


(* ::Subsubsection::Closed:: *)
(*EndDisabledSymbolColoring*)


EndFEHiding[]/;$Notebooks:=
  Module[{ctr=$FEHidingCounter},
    If[!ValueQ@ctr, ctr=0];
    If[ctr===1, Internal`SymbolList[True]];
    $FEHidingCounter = Max@{ctr-1, 0};
    ]


(* ::Subsubsection::Closed:: *)
(*WithDisabledSymbolColoring*)


WithDisabledSymbolColoring[expr_]/;$Notebooks:=
  Internal`WithLocalSettings[
    BeginDisabledSymbolColoring[],
    expr,
    EndDisabledSymbolColoring[]
    ];
WithDisabledSymbolColoring~SetAttributes~HoldAll;


(* ::Subsubsection::Closed:: *)
(*WithEnabledSymbolColoring*)


WithEnabledSymbolColoring[expr_]/;$Notebooks:=
  If[TrueQ[$FEHidingCounter>0],
    Block[{$FEHidingCounter=1},
      Internal`WithLocalSettings[
        EndDisabledSymbolColoring[],
        expr,
        BeginDisabledSymbolColoring[]
        ]
      ],
    expr
    ];
WithEnabledSymbolColoring~SetAttributes~HoldAll;


(* ::Subsubsection::Closed:: *)
(*RemoveFrontEndSymbolColoring*)


RemoveFrontEndSymbolColoring[
  syms__Symbol,
  cpath:{__String}|Automatic:Automatic,
  mode:"Update"|"Set":"Update"
  ]/;$Notebooks:=
  With[
    {
      stuff=
        Map[
          Function[
            Null,
            {Context@#, SymbolName@Unevaluated@#},
            HoldAllComplete
            ],
          HoldComplete[syms]
          ]//Apply[List]
        },
    KeyValueMap[
      FrontEndExecute@
      If[mode==="Update",
        FrontEnd`UpdateKernelSymbolContexts,
        FrontEnd`SetKernelSymbolContexts
        ][
        #,
        Replace[cpath,
          Automatic->$ContextPath
          ],
        {{#,{},#2,{},{}}}
        ]&,
      GroupBy[stuff,First->Last]
      ];
    ];
RemoveFrontEndSymbolColoring[
  names_String,
  mode:"Update"|"Set":"Update"
  ]:=
  Replace[
    Thread[ToExpression[Names@names,StandardForm,Hold],Hold],
    Hold[{s__}]:>RemoveFrontEndSymbolColoring[s,mode]
    ];
RemoveFrontEndSymbolColoring~SetAttributes~HoldAllComplete;


(* ::Subsubsection::Closed:: *)
(*AddFrontEndSymbolColoring*)


AddFrontEndSymbolColoring[
  syms__Symbol,
  cpath:{__String}|Automatic:Automatic,
  mode:"Update"|"Set":"Update"
  ]/;$Notebooks:=
  With[{stuff=
    Map[
      Function[Null,
        {Context@#,SymbolName@Unevaluated@#},
        HoldAllComplete],
      HoldComplete[syms]
      ]//Apply[List]
    },
    KeyValueMap[
      FrontEndExecute@
      If[mode==="Update",
        FrontEnd`UpdateKernelSymbolContexts,
        FrontEnd`SetKernelSymbolContexts
        ][
        #,
        Replace[cpath,Automatic->$ContextPath],
        {{#,{},{},#2,{}}}
        ]&,
      GroupBy[stuff,First->Last]
      ];
    ];
AddFrontEndSymbolColoring[
  names_String,
  mode:"Update"|"Set":"Update"
  ]:=
  Replace[
    Thread[ToExpression[Names@names,StandardForm,Hold],Hold],
    Hold[{s__}]:>AddFrontEndSymbolColoring[s, mode]
    ];
AddFrontEndSymbolColoring~SetAttributes~HoldAllComplete;


(* ::Subsubsection::Closed:: *)
(*End*)


End[]
