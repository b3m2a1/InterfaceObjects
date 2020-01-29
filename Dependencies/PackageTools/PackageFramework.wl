(* ::Package:: *)

(* ::Title:: *)
(*PackageFramework*)


(* ::Text:: *)
(*A framework for extending the built-in BeginPackage/EndPackage-style package mechanism.*)
(*Provides developer utilities so that packages may be developed with less effort and more features.*)


(* ::Text:: *)
(*This single package file allows for bootstrapping of the system, providing the capabilities necessary to do the core loading, but leaving out many utilities like auto-completion, exception handling, etc.*)


BeginPackage["PackageFramework`"]


LoadPackage::usage=
	"LoadPackage[pkg] loads a PackageFrameworkPackage";


(* ::Subsubsection:: *)
(*Package*)


(* ::Text:: *)
(*Stuff specific to the operation of the package*)


BeginPackage["`Package`"];


PackageFrameworkPackage::usage=
  "PackageFrameworkPackage[ops] represents a package and provides methods on this package";
$PackageFrameworkPackages::usage=
  "$PackageFrameworkPackages is a manager symbol so that can avoid a reload if unnecessary";
$CurrentPackage::usage="The current package as configured by the loader";
$PackageFrameworkVersion::usage="The version of the package framework in play \
(mostly used to check if standalone)";


(* ::Text:: *)
(*There are some aspects of the design of this framework that require a level of mutability that the language wouldn't naturally provide.*)


(* ::Text:: *)
(*To circumvent this issue we can bind a set of property setting and getting mechanisms on the unique PackageFrameworkPackage object that a given context will have bound to it (in $PackageFrameworkPackages). By doing this we're able to get a nice balance of mutability and immutability*)


ResolvePackageFrameworkPackage::usage="Resolves to the unique package object if needed";
PackageCreateCurrentPackageOptionInterface::usage=
  "Implementation function that lets a symbol become a hook into a given attribute of a package";
BindPackageMethod::usage=
  "Adds a method on the general PackageFrameworkPackage interface that makes it possible to call\
 into packages";
PackageFrameworkConfig::usage="Discovers the config settings for a given package / directory";
PackageFrameworkPackageQ::usage="Tests whether the object is an initialized package for not";
PackageFrameworkPackageOption::usage="Gets an option value for a package";
PackageFrameworkPackageOptionValue::usage="Gets an option value for a package";
PackageFrameworkPackageMutate::usage="The mutation handler for PackageFrameworkPackage objects";


EndPackage[]


(* ::Subsubsection:: *)
(*Data*)


BeginPackage["`Data`"]


PackageName::usage="PackageName[pkg] pulls the name out of pkg";
PackageHead::usage="PackageHead[pkg] returns the default Head for pkg";
PackageContexts::usage="PackageContexts[pkg] returns the set of Contexts used by the package";
PackageSymbols::usage="PackageSymbols[pkg] returns the symbols provided by the package";
PackageFileContexts::usage="PackageFileContexts[pkg] The contexts for the files in pkg";
PackageDeclaredPackages::usage="PackageDeclaredPackages[pkg] the set of packages found and \
declared via the autoloader for pkg";
PackageLoadedPackages::usage="PackageLoadedPackages[pkg] the set of already loaded packages";
PackagePreloadedPackages::usage="PackagePreloadedPackages[pkg] gives the packages to be preloaded";


PackageRoot::usage="PackageRoot[pkg] returns the root directory for the package
PackageRoot[pkg, ctype] returns the root directory for the content type ctype";
PackageFileNames::usage="PackageFileNames[pkg, pat] returns the files in the package matching pat";
PackageFilePath::usage="PackageFilePath[pkg, path] returns the file path using PackageRoot as the root";
PackageFileFromName::usage="PackageFileFromName[pkg, name] returns the package file pointed to by name";


EndPackage[]


(* ::Subsubsection:: *)
(*Loading*)


BeginPackage["`Loading`"]


PackageExecute::usage="PackageExecute[pkg, expr] executes expr in the right context path";


PackagePullDeclarations::usage="PackagePullDeclarations[pkg] finds all the declared symbols \
in a package";
PackageAutoloadPackage::usage="PackageAutoloadPackage[pkg] loads a package via PackageExecute when the \
symbol it is bound to is used";
PackageLoadDeclare::usage="PackageLoadDeclare[pkg, file] handles the declaration process for single \
package file";


PackageFrameworkPackageLoad::usage="PackageFrameworkPackageLoad[pkg] handles the entire declaration \
process for a package";
PackageFrameworkPackageGet::usage=
  "Loads a specific subpackage file using Get with the appropriate directory structure";
PackageFrameworkPackageNeeds::usage=
  "Loads a specific subpackage file if it hasn't already been loaded";
PackageEnsureLoad::usage="PackageEnsureLoad[pkg] loads the package if it has not already been loaded";
PackageCompleteLoadProcess::usage="PackageCompleteLoadProcess[pkg] finalizes the load process for \
the package";


PackageConfigurePackageHelpers::usage="PackageConfigurePackageHelpers[pkg] sets up symbols that \
the package can use inside itself";


EndPackage[];


(* ::Subsubsection::Closed:: *)
(*Dependencies*)


(* ::Text:: *)
(*We'll include a sub-sample of the dependency management functions we want to provide overall. This will be the bare-bones interface that we can use to bootstrap the full version of the paclet from this single loader file.*)


BeginPackage["`Dependencies`"]


PackageLoadingMode::usage="PackageLoadingMode[pkg] returns whether the package is loaded as a \
\"Primary\" package or as a \"Dependency\"";
PackageDependencies::usage="PackageDependencies[pkg] returns the list of dependencies of the package";
PackageDependencyBase::usage="PackageDependencyBase[pkg] returns the server from which dependencies \
will be loaded";
PackageCheckPacletDependency::usage="PackageCheckPacletDependency[pkg, dep] checks that a paclet is \
installed";
PackageInstallPacletDependency::usage="PackageInstallPacletDependency[pkg, dep] installs a dependency \
paclet";
PackageUpdatePacletDependency::usage="PackageUpdatePacletDependency[pkg, dep] updates a dependency \
paclet";
PackageLoadPacletDependency::usage="PackageLoadPacletDependency[pkg, dep] loads a dependency paclet";
PackageEnsureLoadDependency::usage="PackageLoadPacletDependency[pkg, dep] loads a dependency paclet \
if is has not already been loaded";
PackageEnsureLoadDependencies::usage="PackageLoadPacletDependency[pkg] loads all of the dependencies \
of a package if they have not already been loaded";
PackageDependencyContexts::usage="PackageDependencyContexts[pkg] returns a context managed to be a \
dependency";
PackageExposeDependencies::usage="PackageExposeDependencies[pkg] adds the dependencies to $ContextPath";


EndPackage[]


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


$PackageFrameworkVersion::doc="
Dunno if I'll ever use this, but this should either be standalone-version or paclet-version \
where version is semantically versioned.

The idea will be to allow for more dynamic loading of the PackageFramework (potentially), maybe using \
some helper function
";
$PackageFrameworkVersion="standalone-0.0.0";


(* ::Subsection:: *)
(*Package Objects / Mutation*)


(* ::Subsubsection::Closed:: *)
(*$PackageFrameworkPackages*)


If[!ValueQ[$PackageFrameworkPackages],
  $PackageFrameworkPackages = <||>
  ];


(* ::Subsubsection::Closed:: *)
(*ResolvePackageFrameworkPackage*)


ResolvePackageFrameworkPackage[loc_]:=
  Lookup[$PackageFrameworkPackages, loc, None]


(* ::Subsubsection:: *)
(*PackageFrameworkConfig*)


$PacletInfoFileNames={
  "PacletInfo.wl", "PacletInfo.m"
  };


$LegacyInfoFileNames={
  "LoadInfo.m", "LoadInfo.wl",
  "BundleInfo.m", "BundleInfo.wl"
  };


PackageFrameworkConfig::doc="
	Makes it possible to support the legacy design from when this was all part of BTools
and I expected a Config folder at the root of every package

The new design attempts to unify my two major use cases: 
	1) making new packages with my standard layout and which will auto-context for you
	2) hooking into existing packages for extension and introspection
";
PackageFrameworkConfig[ops_, paclet_, rootDirectory_]:=
  Module[
    {
      pacletData = paclet, 
      legacySettings = {},
      legacyConfigFiles,
      extensionData
      },
     Switch[pacletData,
       None,
         (* proceed to try to find the missing PacletInfo.m or PacletInfo.wl file *)
         pacletData = 
           Catch[
             Do[
               If[FileExistsQ[FileNameJoin@{rootDirectory, p}],
                 Throw[
                   Association@@PacletManager`CreatePaclet@
                     FileNameJoin@{rootDirectory, p}
                   ]
                 ],
               {p, $PacletInfoFileNames}
               ];
             <||>
             ],
       _PacletManager`Paclet|_System`PacletObject,
         pacletData = Association@@pacletData
       ];
    extensionData=
      Association@
        Replace[
          Lookup[pacletData, "Extensions", {}],
          {key_String, o___}:>
            (key->Association@Flatten@{o}),
          1
          ];
    legacyConfigFiles=
      FileNames[
        Alternatives@@$LegacyInfoFileNames, 
        {
          FileNameJoin@{rootDirectory, "Config"},
          FileNameJoin@{rootDirectory, "Private"},
          FileNameJoin@{rootDirectory, "Private", "Config"}
          }
        ];
    legacySettings=
      Flatten@Map[
        Replace[
          Quiet[
            Import@#,
            {
              Import::nffil,
              Import::chtype
              }
            ],
          Except[KeyValuePattern[{}]]:>
            {}
          ]&,
       legacyConfigFiles
       ];
    Join[
       <|
         "Name"->Lookup[pacletData, "Name"],
         "Location"->Lookup[pacletData, "Location"],
         "Context"->Replace[
           Lookup[Lookup[pacletData, "Kernel", <||>], "Context"],
           {
             m_Missing:>{FileBaseName@rootDirectory<>"`"},
             e_:>Flatten@{e}
             }
           ]
         |>,
       Lookup[extensionData, "PackageFramework", <||>]
       ]//Merge[
       Flatten@{
         ops,
         #,
         legacySettings,
         Options[PackageFrameworkPackage]
         },
       Replace[{
         r:{(Except[{}, _?OptionQ]|_Association)..}:>Merge[r, First],
         {e_, ___}:>e
         }]
       ]&
    ]


(* ::Subsubsection:: *)
(*PackageFrameworkPackage*)


PackageFrameworkPackage::doc="
Can be initialized directly from an association, from a paclet, or from a directory

Supports the following keys:
  Name: Package name
  Location: Package location
  Context: Root context(s) to use and from which to derive subcontexts
  PackageRoot: where to find packages (defaults to top-level Package directory)
  ResourceRoot: where to find resources (defaults to top-level Resource directory)
  ExtraContexts: a set of extra contexts to expose once loading is done
  ContextMap: a mapping from standard context paths to custom ones
  Mode: the loading mechanism (Dependency or Primary) for the package
  DecoloredPackages: the list of packages that should be hidden from the FE
  PackageScope: the list of packages that should be package scoped and not exposable
  PreloadPackages: the list of packages that should be loaded when the package is loaded
  AutoloadIngnored: the list of symbol names to be ignored by the autoloader
  Dependencies: the list of dependencies (with suboptions) for the package
  PackageContexts: the list of contexts that should be available when loading
  RemovePaths: the set of paths to be removed from the bundled package
  RemovePatterns: the set of path patterns to be remove from the bundled package
";
Options[PackageFrameworkPackage]=
  {
    "Name"->Automatic,
    "Location"->Automatic,
    "Context"->Automatic,
    "PackageRoot"->"Packages",
    "ResourceRoot"->"Resources",
    "DependenciesRoot"->"Dependencies",
    "ExtraContexts"->{},
    "ContextMap"->{},
    "LoadingMode"->Automatic,
    "AutoloadIgnored"->{},
    "DecoloredPackages"->{},
    "PackageScope"->{},
    "PreloadPackages"->{},
    "Dependencies"->{},
    "RemovePaths" -> {"Private", "project", "GitHub", ".git"}, 
    "RemovePatterns" -> {
       "Packages/*.nb", 
       "Packages/*/*.nb", 
       "Packages/*/*/*.nb", 
       ".DS_Store"
       }
    };
PackageFrameworkPackage[
  pac:_PacletManager`Paclet|_System`PacletObject,
  ops:OptionsPattern[]
  ]:=
  With[{pkg=ConstructPackageFrameworkPackage[pac, ops]},
    pkg/;pkg=!=$Failed
    ];
PackageFrameworkPackage[
  loc:_String?DirectoryQ,
  ops:OptionsPattern[]
  ]:=
  With[{pkg=ConstructPackageFrameworkPackage[loc, ops]},
    pkg/;pkg=!=$Failed
    ];
PackageFrameworkPackage[
  ops:OptionsPattern[]
  ]:=
  With[{pkg=ConstructPackageFrameworkPackage[ops]},
    pkg/;pkg=!=$Failed
    ];
PackageFrameworkPackage[a_Association]?(
  Function[Null, Not@System`Private`ValidQ[Unevaluated[#]], HoldFirst]
  ):=
  With[{pkg=ConstructPackageFrameworkPackage@a},
    pkg/;pkg=!=$Failed
    ]


(* ::Subsubsection::Closed:: *)
(*Formatting*)


MakeBoxes[p:PackageFrameworkPackage[a_]?PackageFrameworkPackageQ, StandardForm]:=
  BoxForm`ArrangeSummaryBox[
    PackageFrameworkPackage,
    p,
    None,
    {
      BoxForm`MakeSummaryItem[{"Name: ", a["Name"]}, StandardForm]
      },
    { 
      BoxForm`MakeSummaryItem[{"Context: ", a["Context"]}, StandardForm],
      BoxForm`MakeSummaryItem[{"Location: ", a["Location"]}, StandardForm]
      },
    StandardForm
    ]


(* ::Subsubsection::Closed:: *)
(*ConstructPackageFrameworkPackage*)


ConstructPackageFrameworkPackage[
  pkg:_PacletManager`Paclet|_System`PacletObject,
  ops:OptionsPattern[]
  ]:=
  With[{loc=pkg["Location"]},
    If[StringQ@loc&&DirectoryQ@loc,
      Replace[
        ResolvePackageFrameworkPackage[],
        None:>
          ConstructPackageFrameworkPackage[
            PackageFrameworkConfig[{ops}, pkg, loc],
            False
            ]
        ],
      $Failed
      ]
    ];
ConstructPackageFrameworkPackage[
  loc:_String?DirectoryQ,
  ops:OptionsPattern[]
  ]:=
  Replace[
    ResolvePackageFrameworkPackage[loc],
    None:>
      ConstructPackageFrameworkPackage[
        PackageFrameworkConfig[{ops}, None, loc],
        False
        ]
    ];
ConstructPackageFrameworkPackage[
  ops:OptionsPattern[]
  ]:=
  With[{loc=OptionValue["Location"]},
    If[StringQ[loc]&&DirectoryQ[loc],
      Replace[
        ResolvePackageFrameworkPackage[loc],
        None:>
          ConstructPackageFrameworkPackage[
            PackageFrameworkConfig[{ops}, None, loc],
            False
            ]
        ],
      $Failed
      ]
    ];
ConstructPackageFrameworkPackage[a_Association, validate_:True]:=
  If[validate,
    With[{loc=a["Location"]},
      If[StringQ[loc]&&DirectoryQ[loc],
        Replace[
          ResolvePackageFrameworkPackage[loc],
          None:>
            With[{config=PackageFrameworkConfig[Normal[a], None, loc]},
              ConstructPackageFrameworkPackage[a, False]
              ]
          ],
        $Failed
        ]
      ],
   System`Private`SetValid@Unevaluated@PackageFrameworkPackage[a]
   ]


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageQ*)


PackageFrameworkPackageQ[p:PackageFrameworkPackage[_Association?AssociationQ]]:=
  System`Private`ValidQ[p]
PackageFrameworkPackageQ[_]:=False


(* ::Subsubsection::Closed:: *)
(*PropertyValue / SetProperty / RemoveProperty / PropertyList*)


If[!ValueQ@$PackagePropertyStore,
  $PackagePropertyStore = Language`NewExpressionStore["<PackageProperties>"];
  ];


(* ::Subsubsubsubsection::Closed:: *)
(*PPSPropContainsQ*)


PPSPropContainsQ[x_]:=
	$PackagePropertyStore@"containsQ"[x];
PPSPropContainsQ[x_, p_]:=
	$PackagePropertyStore@"containsQ"[x, p];


(* ::Subsubsubsubsection::Closed:: *)
(*PPSPropGet*)


PPSPropGet[x_, p_String]:=
	$PackagePropertyStore@"get"[x, p];
PPSPropGet[x_, {p_String}]:=
  PPSPropGet[x, p];


(* ::Subsubsubsubsection::Closed:: *)
(*PPSPropSet*)


PPSPropSet[x_, p_, v_]:=
	$PackagePropertyStore@"put"[x, p ,v];


(* ::Subsubsubsubsection::Closed:: *)
(*PPSPropRemove*)


PPSPropRemove[x_]:=
	$PackagePropertyStore@"remove"[x];
PPSPropRemove[x_, p_]:=
	$PackagePropertyStore@"remove"[x, p];


(* ::Subsubsubsubsection::Closed:: *)
(*PPSPropKeys*)


PPSPropKeys[]:=
	$PackagePropertyStore@"getKeys"[];
PPSPropKeys[x_]:=
	$PackagePropertyStore@"getKeys"[x];


(* ::Subsubsubsubsection::Closed:: *)
(*PPSPropList*)


PPSPropList[]:=
	$PackagePropertyStore@"listTable"[];


(* ::Subsubsubsubsection::Closed:: *)
(*PPSCopyProperties*)


PPSCopyProperties[a_, b_]:=
  Do[PPSPropSet[b, PPSPropGet[a, k]], {k, PPSPropKeys[a]}]


(* ::Subsubsubsubsection::Closed:: *)
(*SetProperty*)


$$hold~SetAttributes~HoldAllComplete;
PackageFrameworkPackageSetProperty[pkg_, p_->v_]:=
  PPSPropSet[pkg, p, v];
PackageFrameworkPackageSetProperty[pkg_, p_:>v_]:=
  PPSPropSet[pkg, p->$$hold[v]];
PackageFrameworkPackageSetProperty[pkg_, {p_String}->v_]:=
  PPSPropSet[pkg, p, v];
PackageFrameworkPackageSetProperty[pkg_, {p_String, r__}->v_]:=
  Module[{a=PPSPropGet[pkg, p]},
    a[[r]]=v;
    PackageFrameworkPackageSetProperty[pkg, p->a];
    ];


PackageFrameworkPackage/:
  SetProperty[pkg_PackageFrameworkPackage, p_->v_]:=
	  PPSPropSet[pkg, p, v];
PackageFrameworkPackage/:
  SetProperty[pkg_PackageFrameworkPackage, p_:>v_]:=
	  PPSPropSet[pkg, p, v]


(* ::Subsubsubsubsection::Closed:: *)
(*PropertyValue*)


PackageFrameworkPackagePropertyValue[pkg_, p_]:=
  Replace[
  	  PPSPropGet[pkg, p],
  		{
  			Null:>If[!PPSPropContainsQ[pkg, p], Missing["PropertyAbsent", p], Null],
  			$$hold[v_]:>v
  			}
  		];
PackageFrameworkPackagePropertyValue[pkg_, {p_String}]:=
  PackageFrameworkPackagePropertyValue[pkg, p];
PackageFrameworkPackagePropertyValue[pkg_, {p_String, r__}]:=
  With[{a=PackageFrameworkPackagePropertyValue[pkg, p]},
    If[a=!=Missing["PropertyAbsent", p],
      a[[r]],
      a
      ]
    ]


PackageFrameworkPackage/:
  PropertyValue[pkg_PackageFrameworkPackage, p_]:=
  	PackageFrameworkPackagePropertyValue[pkg, p]


(* ::Subsubsubsubsection::Closed:: *)
(*RemoveProperty*)


PackageFrameworkPackageRemoveProperty[pkg_PackageFrameworkPackage, p_]:=
  Replace[
    PPSPropRemove[pkg, p],
    {
      $$hold[v_]:>v
      }
    ];
PackageFrameworkPackageRemoveProperty[pkg_PackageFrameworkPackage, {p_String, ___}]:=
  PackageFrameworkPackageRemoveProperty[pkg, p]


PackageFrameworkPackage/:
  RemoveProperty[pkg_PackageFrameworkPackage, p_]:=
    PackageFrameworkPackageRemoveProperty[pkg, p];


(* ::Subsubsubsubsection::Closed:: *)
(*PropertyList*)


PackageFrameworkPackagePropertyList[pkg_]:=
  Replace[PPSPropKeys[pkg], Null->Missing["PackageAbsent", pkg]]


PackageFrameworkPackage/:
  PropertyList[pkg_PackageFrameworkPackage]:=
    PackageFrameworkPackagePropertyList[pkg];


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageOptionValue*)


PackageFrameworkPackageOptionValue[
  pkg:PackageFrameworkPackage[a_Association]?PackageFrameworkPackageQ, 
  keys:__String
  ]:=
  Fold[Lookup[#, #2, Return[Missing["KeyAbsent", #2], Fold]]&, a, {keys}];


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageSetOptions*)


PackageFrameworkPackageSetOptions[
  pkg:PackageFrameworkPackage[a_Association]?PackageFrameworkPackageQ, 
  ops:(
    (Rule|RuleDelayed)[_String|{__String}, _]|
    {
      (Rule|RuleDelayed)[_String|{__String}, _]..
      }
    )
  ]:=
  With[{aa=ReplacePart[a, ops]},
    With[{new=System`Private`SetValid@Unevaluated@PackageFrameworkPackage[aa]},
      PPSCopyProperties[pkg, new];
      new
      ]
    ]


p_PackageFrameworkPackage?PackageFrameworkPackageQ[keys:__String]:=
  PackageFrameworkPackageOptionValue[p, keys];
p_PackageFrameworkPackage?PackageFrameworkPackageQ[keys:__String]:=
  PackageFrameworkPackageOptionValue[p, keys];
PackageFrameworkPackage/:
  (Set[pkg_PackageFrameworkPackage?PackageFrameworkPackageQ[keys:__String], value_]):=
    PackageFrameworkPackageSetOptions[pkg, {keys}->value];


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageMutate*)


PackageFrameworkPackageMutate//ClearAll
PackageFrameworkPackageMutate~SetAttributes~HoldAllComplete
PackageFrameworkPackageMutate[
  s_Symbol?PackageFrameworkPackageQ[keys:__String]=value_
  ]:=
  (s=PackageFrameworkPackageSetOptions[s, {keys}->value]);
PackageFrameworkPackageMutate[___]:=Language`MutationFallthrough


(* ::Subsubsection::Closed:: *)
(*CreatePackageSymbolInterface*)


(* ::Text:: *)
(*The name of the method is stupidly long, of course, but the basic idea is to make it possible to work really easily with the $CurrentPackage since that's the only one we'll ever really care about at any given moment in time*)
(**)
(*We'll provide an accessor method that can be bound dynamically to the value of the symbol so that it can be applied to $CurrentPackage and then we'll also provide a normal setter-interface that will have to work through SetProperty and friends...I think. Or I guess that could be a default too.*)


SetAttributes[CreatePackageSymbolInterface, HoldFirst];
CreatePackageSymbolInterface::doc="
Binds the PropertyValue/SetProperty interface to the specified symbol and uses the specified string \
as the key.
";
CreatePackageSymbolInterface[
  symbol_, 
  setting_String,
  accessor_:None,
  setter_:None
  ]:=(
  Unprotect[Unevaluated@symbol];
  ClearAll[Unevaluated@symbol];
  With[
    {
      a = If[accessor === None, PropertyValue, accessor],
      s = If[setter === None, SetProperty, setter]
      },
    symbol := a[$CurrentPackage, setting];
    symbol /: Set[symbol, value_]:=
      s[$CurrentPackage, setting->value];
    symbol /: Set[symbol[k__], value_]:=
      s[$CurrentPackage, {setting, k}->value];
    symbol /: AppendTo[symbol, value_]:=
      s[$CurrentPackage, setting->Append[a[$CurrentPackage], value]];
    symbol /: AssociateTo[symbol, key_->value_]:=
      s[$CurrentPackage, {setting, key}->value];
    ];
  Protect[symbol];
  );


(* ::Subsubsection::Closed:: *)
(*BindPackageMethod*)


(* ::Text:: *)
(*Just makes it so that the PackageFrameworkPackage can make use of down-value type methods. We might want to add to it later, but for the moment it makes it very clean and clear to work with*)


BindPackageMethod[name_, method_]:=
  (
    (pkg:_PackageFrameworkPackage?PackageFrameworkPackageQ)@name[args___]:=method[pkg, args]
    )


(* ::Subsection:: *)
(*Load Package*)


(* ::Subsubsection:: *)
(*LoadPackage*)


(* ::Text:: *)
(*This is the heart of the system. Everything in this part of the package is basically in service to this method.*)


(* ::Text:: *)
(*Things like symbol coloring and autocompletions can come in the full version of the package. The redux version will leave them out.*)


ClearAll[LoadPackage]
LoadPackage::doc="

";
LoadPackage::npkg="`` is not a valid package";
Options[LoadPackage]:=
  {
    "Reload"->False,
    "PreloadHandler"->None,
    "PostloadHandler"->None,
    "Hook"->None
    };
LoadPackage[
  pkg_PackageFrameworkPackage, 
  parent:_PackageFrameworkPackage|Automatic|None:None,
  ops:OptionsPattern[]
  ]:=
  With[
    {
      $Name=PackageName[pkg],
      pre=OptionValue["PreloadHandler"],
      post=OptionValue["PostloadHandler"],
      hook=OptionValue["Hook"],
      reload=TrueQ@OptionValue["Reload"]
      },
    Block[
      {
        $PackageLoadStack = 
          If[!ListQ@$PackageLoadStack, {}, {pkg, $PackageLoadStack}],
        $ParentPackage=
          Replace[
            parent,
            Automatic:>
              If[PackageLoadingMode[pkg]==="Dependency", 
                $CurrentPackage, 
                None
                ]
            ],
        $CurrentPackage=pkg,
        $ParentContext,
        $PackageContext,
        $PackageFrameworkSubcontext,
        $DependencyLoad=PackageLoadingMode[pkg]==="Dependency"
        },
      (* this has to be set first *)
      $ParentContext = 
        Replace[{l_, ___}:>l]@
          Replace[$ParentPackage, 
            {
              None->"",
              e_:>PackageContext[e, $ParentContext]
              }
            ];
      $PackageContext =
        PackageContext[pkg, $ParentContext];
      $PackageFrameworkSubcontext =
        $PackageContext <> "PackageScope`";
      If[reload || !KeyExistsQ[$PackageFrameworkPackages, $PackageContext],
      
        $PackageFrameworkPackages[$PackageContext]=$CurrentPackage;
        SetProperty[$CurrentPackage, 
          "TopLevelLoad"->
            If[$DependencyLoad,
              False,
              MemberQ[$ContextPath, "Global`"]
              ]
          ];
        SetProperty[pkg, (* should I use the current version of PackageContexts here...? *)
          "Contexts"->{$PackageContext, $PackageFrameworkSubcontext}
          ];
        SetProperty[pkg,
          "ParentContext"->$ParentContext
          ];
          
        Replace[hook, None->(#2&)][
          pkg,
          Internal`WithLocalSettings[
            BeginPackage[$PackageContext];
            PrependTo[$ContextPath, $PackageFrameworkSubcontext];
            pre[pkg],
            
            PackageConfigurePackageHelpers[pkg];
            PackageFrameworkPackageLoad[pkg];
            PackageCompleteLoadProcess[pkg];
            (*pkg*),
            
            post[pkg];
            EndPackage[];
            ]
          ],
        pkg
        ]
      ]
    ];
LoadPackage[d_String?DirectoryQ, ops:OptionsPattern[]]:=
  With[{p=PackageFrameworkPackage[d]},
    LoadPackage[
      p, 
      ops
      ]/;PackageFrameworkPackageQ[p]
    ];
LoadPackage[
  ops:OptionsPattern[]
  ]:=
  With[{d=DirectoryName@$InputFileName},
    With[{p=PackageFrameworkPackage[d]},
      LoadPackage[
        p, 
        ops
        ]/;PackageFrameworkPackageQ[p]
      ]/;StringLength[d]>0
    ];
LoadPackage[a_, ___]/;(Message[LoadPackage::npkg, a]):=Null;


(* ::Subsection:: *)
(*Context / File Management*)


(* ::Subsubsection::Closed:: *)
(*PackageName*)


BindPackageMethod["Name", PackageName]


PackageName//Clear
PackageName[pkg_PackageFrameworkPackage]:=
  pkg["Name"];


(* ::Subsubsection::Closed:: *)
(*PackageHead*)


BindPackageMethod["Head", PackageHead]


PackageHead[pkg_]:=
  With[{c=PackageContext[pkg], n=PackageName[pkg]},
    Symbol[c<>StringDelete[n, Except["$"|WordCharacter]]]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageRootContext*)


PackageRootContext//Clear
PackageRootContext[pkg_PackageFrameworkPackage]:=
  Replace[PackageFrameworkPackageOptionValue[pkg, "Context"], {p_, ___}:>p]


(* ::Subsubsection::Closed:: *)
(*PackageContext*)


BindPackageMethod["Context", PackageContext]


PackageContext//Clear
PackageContext[pkg_PackageFrameworkPackage, root_String]:=
  root<>PackageRootContext[pkg];
PackageContext[pkg_PackageFrameworkPackage, Optional[Automatic, Automatic]]:=
  PackageContext[pkg, PropertyValue[pkg, "ParentContext"]]


(* ::Subsubsection::Closed:: *)
(*PackageContexts*)


BindPackageMethod["Contexts", PackageContexts]


(*CreatePackageSymbolInterface[$PackageContexts, "Contexts", PackageContexts[#]&];*)


PackageContexts[pkg_]:=
  Replace[
    PropertyValue[pkg, "Contexts"],
    _Missing:>(SetProperty[pkg, "Contexts"->{}])
    ]


(* ::Subsubsection::Closed:: *)
(*PackageExtendContextPath*)


BindPackageMethod["AddContexts", PackageExtendContextPath]


PackageExtendContextPath[pkg_, cp:{__String}]:=
  SetProperty[
    pkg,
    "Contexts"->
      DeleteCases[
        DeleteDuplicates@
          Join[PackageContexts[pkg], cp],
        "System`"|"Global`"
        ]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageExposeContexts*)


PackageExposeContexts[pkg_]:=
  If[ListQ@pkg["ExtraContexts"],
    PackageExtendContextPath[
      pkg,
      pkg["ExtraContexts"]
      ]
    ];


(* ::Subsubsection::Closed:: *)
(*PackageContextMapping*)


PackageContextMapping[pkg_PackageFrameworkPackage]:=
  pkg["ContextMap"]


(* ::Subsubsection::Closed:: *)
(*PackageInitialContextPath*)


PackageInitialContextPath::doc="
PackageInitialContextPath[pkg]

Converts the package layout into a set of contexts that will be global to the package
Checks first against the contexts that the package asks to expose

We assumed originally that $Context would be the lookup root for the 
main package itself, but this assumption can be relaxed at this point
As long as PackageContext[pkg] provides the fully-qualified name of the package 
we should be able to very normally manage the dependency packages

The second core assumption was that the directory structure would directly
map onto the $PackageContextPath, but this might not be how we want to do it

The only workable alternative to this is to force each directory to map onto a
context but provide a mapping that would allow us to revise the used context
at resolution time

e.g. We'd have Pkg/Sub/Subsub \[Rule] PkgContext`Sub`Subsub` but then we 
have a post-processing map that takes PkgContext`Sub`Subsub` \[Rule] PkgContext`NewPath`
";
PackageInitialContextPath[pkg_, includeOrig:True|False:True]:=
  Module[
    {
      packages,
      packageRoot=PackageRoot[pkg],
      packagePackageRoot = PackageRoot[pkg, "Packages"], 
      (* I regret this name already... *)
      name = PackageName[pkg],
      cont = PackageRootContext[pkg],
      opath = If[includeOrig, $ContextPath, {"System`"}],
      contextRemapping = PackageContextMapping[pkg],
      (* we'll add some kind of option to allow this to be remapped *)
      rootDepth,
      packageDirectories,
      packageContextPath
      },
    rootDepth = FileNameDepth@packagePackageRoot;
    packageDirectories = FileNames["*", packagePackageRoot, Infinity];
    packageDirectories = Select[packageDirectories, DirectoryQ];
    packageDirectories = 
      StringReplace[
        FileNameDrop[#, rootDepth],
        $PathnameSeparator->"`"
        ]&/@packageDirectories;
    packageDirectories = 
      Select[
        packageDirectories,
        StringMatchQ[("$"|WordCharacter)..]
        ];
    packageContextPath = 
      Join[
        opath,
        cont<>#&/@packageDirectories
        ];
    DeleteDuplicates[
      packageContextPath /. contextRemapping
      ]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageFileContexts*)


(*CreatePackageSymbolInterface[$PackageFileContexts, "FileContexts", PackageFileContexts[#]&];*)


PackageFileContexts[pkg_]:=
  Replace[
    PropertyValue[pkg, "FileContexts"],
    _Missing:>(
      SetProperty[pkg, "FileContexts"-><||>];
      <||>
      )
    ]


(* ::Subsubsection::Closed:: *)
(*PackageLoadedPackages*)


(*CreatePackageSymbolInterface[$LoadedPackages, "LoadedPackages", PackageLoadedPackages[#]&];*)


PackageLoadedPackages[pkg_]:=
  Replace[
    PropertyValue[pkg, "LoadedPackages"],
    _Missing:>(
      SetProperty[pkg, "LoadedPackages"->{}];
      {}
      )
    ]


(* ::Subsubsection::Closed:: *)
(*PackageAddLoadedPackages*)


PackageAddLoadedPackages[pkg_, pkgFiles_]:=
  SetProperty[pkg, 
    "LoadedPackages"->DeleteDuplicates@Flatten[{PropertyValue[pkg, "LoadedPackages"], pkgFiles}]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageFileContext*)


(* ::Text:: *)
(*Gotta make this more flexible.*)
(*The basic idea will be to allow the Package to declare its chosen "root" context, then we build off of that.*)
(*Subdirectory names _might_ be allowed to map themselves to a different context in the future?*)


PackageFileContextPath[pkg_PackageFrameworkPackage, f_String?DirectoryQ]:=
  FileNameSplit[
    FileNameDrop[
      f,
      FileNameDepth[PackageRoot[pkg, "Packages"]]
      ]
    ];
PackageFileContextPath[pkg_PackageFrameworkPackage, f_String?FileExistsQ]:=
  PackageFileContextPath[pkg, DirectoryName@f];


PackageFileContext[pkg_PackageFrameworkPackage, f_String?DirectoryQ]:=
  Module[
    {
      s=PackageFileContextPath[pkg, f], 
      root=StringTrim[PackageContext[pkg], "`"],
      contextRemapping = PackageContextMapping[pkg],
      cont
      },
    cont = StringRiffle[Append[""]@Prepend[s, root], "`"];
    Replace[cont, contextRemapping]
    ];
PackageFileContext[pkg_PackageFrameworkPackage, f_String?FileExistsQ]:=
  PackageFileContext[pkg, DirectoryName[f]];


(* ::Subsubsection::Closed:: *)
(*PackageSymbols*)


(*CreatePackageSymbolInterface[$PackageSymbols, "Symbols", PackageSymbols[#]&];*)


BindPackageMethod["Symbols", PackageSymbols]


PackageSymbols[pkg_]:=
  Replace[
    PropertyValue[pkg, "Symbols"],
    _Missing:>(
      SetProperty[pkg, "Symbols"->{}];
      {}
      )
    ]


(* ::Subsubsection::Closed:: *)
(*PackageRoot*)


BindPackageMethod["Directory", PackageRoot]


PackageRoot[PackageFrameworkPackage[a_]?PackageFrameworkPackageQ, Optional[None, None]]:=
  a["Location"];
PackageRoot[PackageFrameworkPackage[a_]?PackageFrameworkPackageQ, k_String]:=
  FileNameJoin@{
    a["Location"],
    Lookup[a, k<>"Root", k]
    }


(* ::Subsubsection::Closed:: *)
(*PackageFileNames*)


BindPackageMethod["FileNames", PackageFileNames]


PackageFileNames[
  pkg_PackageFrameworkPackage, 
  pattern_?StringPattern`StringPatternQ, 
  root:_String|None, 
  e___
  ]:=
  FileNames[pattern, PackageRoot[pkg, root]]


(* ::Subsubsection::Closed:: *)
(*PackageFilePath*)


BindPackageMethod["Path", PackageFilePath]


PackageFilePath[
  pkg_PackageFrameworkPackage,
  p_String,
  r___String
  ]:=
  FileNameJoin@{
    Replace[PackageRoot[pkg, p], _Missing->p],
    r
    };


(* ::Subsubsection::Closed:: *)
(*PackageFileFromName*)


PackageFileFromName//Clear
PackageFileFromName[pkg_PackageFrameworkPackage, f_]:=
  Module[
    {
      fBase=f
      },
    If[!FileExistsQ[fBase],
      fBase = PackageFilePath[pkg, "Packages", f<>".m"]
      ];
    If[!FileExistsQ[fBase],
      fBase = PackageFilePath[pkg, "Packages", f<>".wl"]
      ];
    fBase
    ];


(* ::Subsection:: *)
(*Loading Helpers*)


(* ::Subsubsection::Closed:: *)
(*PackageExecute*)


PackageExecute//Clear
PackageExecute[
  pkg_PackageFrameworkPackage, 
  expr_, 
  context:_String|Automatic:Automatic
  ]:=
  With[{ctxs=PackageContexts[pkg]},
    Block[{$CurrentPackage=pkg},
      Internal`WithLocalSettings[
        Begin[Replace[context, Automatic->ctxs[[1]]]];
        System`Private`NewContextPath@
          Prepend[ctxs, "System`"];,
        expr,
        System`Private`RestoreContextPath[];
        End[];
        ]
      ]
    ];
PackageExecute~SetAttributes~HoldRest


(* ::Subsubsection:: *)
(*PackagePullDeclarations*)


(* ::Text:: *)
(*This is another big place where we need to introduce greater flexibility*)
(*The current design looks for a few things:*)
(**)
(*Begin[...] / BeginPackage[...]*)
(**)
(*We need to add some flexbility to this design, though.*)
(**)
(*Any statement up until a Begin["`Private*`"] should be allowed to operate as normal (but the number of necessary End[] and EndPackage[] statements should be incremented). The reason for this is that we'd like to be able to handle any type of auto-completion as longs as it's generally within the traditional Mathematica package design framework.*)


(* ::Text:: *)
(*This basically means we need to handle a few distinct subcases of package layouts. We can have the classic:*)
(**)
(*  ExposedSymbolA::usage="....";*)
(*  ExposedSymbolB::usage="....";*)
(*  ExposedSymbolC::usage="....";*)
(**)
(*  Begin["`Private`"]*)
(*  (* End Here *)*)
(**)
(*But I'd also like to be able to appropriately handle the case of *)
(**)
(*  BeginPackage["MainPackageName`"]*)
(*  *)
(*  ExposedSymbolA::usage="....";*)
(*  ExposedSymbolB::usage="....";*)
(*  ExposedSymbolC::usage="....";*)
(**)
(*  Begin["`Private`"]*)
(*  (* End Here *)*)
(*  *)
(*On the other hand, we might also want to support something like:*)
(**)
(*  BeginPackage["`SubPackageName`"]*)
(*  *)
(*  ExposedSymbolA::usage="....";*)
(*  ExposedSymbolB::usage="....";*)
(*  ExposedSymbolC::usage="....";*)
(**)
(*  Begin["`Private`"]*)
(*  (* End Here *)*)
(**)
(*but in that case there's some question as to how we want to implement this. Should we be placing autocompletions onto these sub-symbols too?*)


(* ::Text:: *)
(*Another question that arises: Should we be overloading stuff like Begin and BeginPackage in this initialization phase? That would make it possible to not have to catch every possible pattern inside PackagePullDeclarationsAction, but at the same time runs the risk of massive breakage if things go wrong.*)
(**)
(*Something to think about in the future...*)


PackagePullDeclarationsAction//Clear
PackagePullDeclarationsAction[
  a:Hold[
    (h:Begin|BeginPackage)[e_, r___]|
    CompoundExpression[
      (h:Begin|BeginPackage)[e_, r___], 
      ___
      ]
    ]
  ]:=
  If[StringQ[e]&&StringStartsQ[e, "`Private"],
    Throw[End, $EndPackageDeclaration[$CurrentPackageFile]],
    h[e, r]; (* allow the Begin or BeginPackage to activate *)
    Switch[h, 
      Begin, $EndCallStack = {End, $EndCallStack},
      BeginPackage, $EndCallStack = {EndPackage, $EndCallStack}
      ];
    AssociateTo[$PackageContexts, $Context->None]
    ];
PackagePullDeclarationsAction[
  a:Hold[
    (h:End|EndPackage)[]|
    CompoundExpression[
      (h:End|EndPackage)[___], 
      ___
      ]
    ]
  ]:=
  (
    h[];
    If[Length@$EndCallStack == 2,
      $EndCallStack = $EndCallStack[[2]]
      ];
    );
PackagePullDeclarationsAction[
  p:
    Hold[
      _PackageFEHiddenBlock|_PackageScopeBlock|
      CompoundExpression[
        _PackageFEHiddenBlock|_PackageScopeBlock,
        ___
        ]
      ]
  ]/;TrueQ[$AllowPackageRescoping]:=
  (
    ReleaseHold[p];
    Sow[p];
    );
PackagePullDeclarationsAction[e:Except[Hold[Expression]]]:=
  Sow@e;


PackagePullDeclarations[pkg_, pkgFile_]:=
  Module[{decStatements, stream, packageSymbols},
    Block[
      {
        $EndCallStack = {}, 
          (* 
            We'll sow in End and EndPackage statement as necessary so that we can just
            map over the flattened version of the stack after we're done with finding all
            of our package declarations
            *)
        $PackageLoadedContexts = 
          <| PackageFileContext[pkg, pkgFile] -> None |>,
        $CurrentPackageFile = pkgFile
        },
      Internal`WithLocalSettings[
        stream = OpenRead[pkgFile],
        decStatements = 
          Reap[
            Catch[
              Do[ 
                (* read the package block-by-block and cache the relevant symbols *)
                If[
                  Length[
                    ReadList[
                      stream,
                      PackagePullDeclarationsAction@Hold[Expression],
                      1
                      ]
                    ]===0,
                    Throw[
                      EndOfFile,
                      $EndPackageDeclaration[$CurrentPackageFile]
                      ]
                  ],
                Infinity
                ],
              $EndPackageDeclaration[$CurrentPackageFile]
             ]
          ][[2]],
        Close[stream];
        Map[#[]&, Flatten@$EndCallStack];
        ];
    packageSymbols=
      Cases[
        decStatements,
        s_Symbol?(
          Function[
            Null,
            Quiet[KeyExistsQ[$PackageLoadedContexts, Context[#]]],
            HoldAllComplete
            ]
            ):>
            HoldPattern[s],
        Infinity
        ];
    SetProperty[pkg, 
      "Symbols"->
        DeleteDuplicates@Join[PackageSymbols[pkg], packageSymbols]
      ];
    SetProperty[pkg, 
      "Contexts"->
        DeleteDuplicates@Join[PackageContexts[pkg], Keys@$PackageLoadedContexts]
      ];
    pkgFile->{Keys@$PackageLoadedContexts, packageSymbols}
    ] 
  ]


(* ::Subsubsection:: *)
(*PackageAutoloadPackage*)


(* ::Text:: *)
(*Handles the actual loading of a file. This is set up by the declarer to autoload all the package symbols.*)


PackageAutoloadPackage//Clear
PackageAutoloadPackage[pkg_PackageFrameworkPackage, heldSym_, context_, pkgFile_->syms_]:=
  Block[
    {
      $loadingChain=
        If[!AssociationQ@$inLoad, 
          $loadingChain = <| pkg -> {} |>,
          If[!KeyExistsQ[$loadingChain, pkg],
            Append[$loadingChain, pkg->{}],
            $loadingChain
            ]
          ],
      $inLoad=
        If[!AssociationQ@$inLoad, 
          <|pkg->False|>, 
          If[!KeyExistsQ[$inLoad, pkg],
            Append[$inLoad, pkg->False],
            TrueQ[$inLoad]
            ]
          ],
      $$inLoad=TrueQ[$inLoad[pkg]],
      ignored=With[{ss=pkg["AutoloadIgnored"]}, nonProtectedSym[ss]]
      },
    If[!MemberQ[$loadingChain[pkg], pkgFile],
      AppendTo[$loadingChain[pkg], pkgFile];
      $inLoad[pkg]=True;
      Internal`WithLocalSettings[
        (* holds the CPath so it can be restored later *)
        System`Private`NewContextPath@$ContextPath,
        
        (* Clear, but only for OwnValues *)
        Replace[syms,
          (s_Symbol?ignored)|Verbatim[HoldPattern][s_Symbol?ignored]:>
            (OwnValues[s]={}),
          1
          ];
        PackageEnsureLoad[pkg];
        PackageFrameworkPackageGet[pkg, context[[1]], pkgFile];
        PackageAddLoadedPackage[pkg, pkgFile],
          
        System`Private`RestoreContextPath[]
        ];
      ReleaseHold[heldSym]
      ]
    ];


(* ::Subsubsection:: *)
(*PackageDeclarePackage*)


(*CreatePackageSymbolInterface[$DeclaredPackages, "DeclaredPackages", PackageDeclaredPackages[#]&];*)


PackageDeclaredPackages[pkg_]:=
  Replace[
    PropertyValue[pkg, "DeclaredPackages"],
    _Missing:>(
      SetProperty[pkg, "DeclaredPackages"-><||>]; 
      <||>
      )
    ]


nonProtectedSym[protectedSyms_]:=
  Function[Null, nonProtectedSym[#, protectedSyms], HoldAllComplete];
nonProtectedSym[sym_, protectedSyms_]:=
  !MemberQ[protectedSyms, SymbolName@Unevaluated[sym]]
nonProtectedSym~SetAttributes~HoldAllComplete


PackageDeclarePackage[
  pkg_, 
  pkgFile_->{contexts_, syms_}
  ]:=
  With[{test=With[{e=pkg["AutoloadIgnored"]}, nonProtectedSym[e]]},
    SetProperty[pkg, {"DeclaredPackages", pkgFile}->syms];
    SetProperty[pkg, {"FileContexts", pkgFile}->contexts];
    Replace[
      syms,
      {
        (s:_Symbol?test)|
          (Verbatim[HoldPattern][s_Symbol?test]):>
          SetDelayed[
            s,
            PackageAutoloadPackage[pkg, HoldPattern[s], contexts, pkgFile->syms]
            ],
        e_->Nothing
        },
      1
      ]
    ];


(* ::Subsubsection:: *)
(*PackageLoadDeclare*)


PackageLoadDeclare//Clear
PackageLoadDeclare[pkg_PackageFrameworkPackage, pkgFile_String]:=
  If[!MemberQ[PackageLoadedPackages[pkg], pkgFile],
    If[!KeyMemberQ[PackageDeclaredPackages[pkg], pkgFile],
      If[AssociationQ[$PackageDeclarationList]&&KeyExistsQ[$PackageDeclarationList, pkg],
        $PackageDeclarationList[pkg]=
          Append[$PackageDeclarationList[pkg], PackagePullDeclarations[pkg, pkgFile]],
        PackageDeclarePackage[
          pkg,
          PackagePullDeclarations[pkg, pkgFile]
          ]
        ]
      ],
    PackageFrameworkPackageGet[pkg, pkgFile]
    ];


(* ::Subsubsection:: *)
(*PackageFrameworkPackageLoad*)


BindPackageMethod["Load", PackageFrameworkPackageLoad];


PackageFrameworkPackageLoad//Clear
PackageFrameworkPackageLoad::doc="
Loops over all of the available files in the package and \
creates subcontexts for directories and configures auto-loading for the package files
";
PackageFrameworkPackageLoad[pkg_PackageFrameworkPackage, dir_, listing_]:=
  Module[
    {
      fileNames=
        Select[
          FileNames["*", dir],
          DirectoryQ@#||MatchQ[FileExtension[#], "m"|"wl"]&
          ],
       preload,
       packages,
       postload
      },
    (* preloaded packages *)
    preload = 
      Select[fileNames, 
        StringMatchQ[
          ToLowerCase@FileNameTake[#],
          "__pre__."~~("m"|"wl")
          ]&
        ];
    If[Length@preload>0, Get[preload[[-1]]]];
    (* normally loaded packages *)
    packages = Select[fileNames, StringFreeQ["__"]@*FileBaseName];
    SetProperty[pkg, {"PackageListing", listing}->packages];
    PackageFrameworkPackageLoad[pkg, packages];
    (* packages loaded after all the others at that level *)
    postload = 
      Select[fileNames, 
        StringMatchQ[
          ToLowerCase@FileNameTake[#],
          "__post__."~~("m"|"wl")
          ]&
        ];
    If[Length@postload>0, Get[postload[[-1]]]];
    ];


PackageFrameworkPackageLoad[pkg_PackageFrameworkPackage, dir_String?DirectoryQ]:=
  With[
    {baseContext = PackageFileContext[pkg, dir]},
    If[AllTrue[StringSplit[baseContext, "`"], StringMatchQ[(WordCharacter|"$")..]],
      Internal`WithLocalSettings[
        Begin[baseContext],
        PackageAddContexts[pkg, baseContext];
        PackageFrameworkPackageLoad[
          pkg,
          dir, 
          FileNameDrop[dir, FileNameDepth[PackageRoot[pkg, "Packages"]]]
          ],
        End[];
        ]
      ]
    ];
PackageFrameworkPackageLoad[pkg_PackageFrameworkPackage, file_String?FileExistsQ]:=
  PackageLoadDeclare[pkg, file];
PackageFrameworkPackageLoad[pkg_PackageFrameworkPackage]:=
  Block[
    {
      $PackageDeclarationList=
        If[!AssociationQ@$PackageDeclarationList, 
          <|pkg->{}|>, 
          Append[$PackageDeclarationList,pkg->{}]
          ]
      },
      PackageExecute[
        pkg,
        SetProperty[pkg, "PackageListing"-><||>];
        PackageFrameworkPackageLoad[
          pkg,
          PackageRoot[pkg, "Packages"],
          PackageName[pkg]
          ];
        PackageDeclarePackage[
          pkg,
          #
          ]&/@$PackageDeclarationList[pkg]
        ]
    ];
PackageFrameworkPackageLoad~SetAttributes~Listable;


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageGet*)


BindPackageMethod["Get", PackageFrameworkPackageGet]


PackageFrameworkPackageGet//Clear
PackageFrameworkPackageGet[pkg_PackageFrameworkPackage, f_]:=
  PackageFrameworkPackageGet[pkg, PackageFileContext[pkg, f], f];
PackageFrameworkPackageGet[pkg_PackageFrameworkPackage, c_, f_]:=
  PackageExecute[
      pkg,
      Get@PackageFileFromName[pkg, f],
      c
      ];


(* ::Subsubsection::Closed:: *)
(*PackageFrameworkPackageNeeds*)


BindPackageMethod["Needs", PackageFrameworkPackageNeeds]


PackageFrameworkPackageNeeds[pkg_, pkgFile_String?FileExistsQ]:=
  If[!MemberQ[PackageLoadedPackages[pkg], pkgFile],
    If[KeyMemberQ[PackageDeclaredPackages[pkg], pkgFile],
      PackageLoadDeclare[pkg, pkgFile],
      Do[PackageLoadDeclare[pkg, pkgFile], 2] (* why twice...? *)
      ];
    ];


PackageFrameworkPackageNeeds[pkg_, pkgName_String]:=
  Module[{pf=PackageFilePath[pkg, "Packages", pkgName<>".m"]},
    If[!FileExistsQ@pf, PackageFilePath[pkg, "Packages", pkgName<>".wl"]];
    If[FileExistsQ@pf,
      PackageFrameworkPackageNeeds[pkg, pf],\.00 \.00
      $Failed
      ]
   ];


(* ::Subsubsection::Closed:: *)
(*PackageEnsureLoad*)


PackageEnsureLoad[pkg_]:=
  If[!TrueQ[PropertyValue[pkg, "LoadingFlag"]],
    SetProperty[pkg, "LoadingFlag"->True];
    PackageEnsureLoadDependencies[pkg];
    PackageExposeContexts[pkg];
    ];


(* ::Subsubsection::Closed:: *)
(*PackagePreloadedPackages*)


PackagePreloadedPackages[pkg_]:=
  pkg["PreloadPackages"]


(* ::Subsubsection::Closed:: *)
(*PackageCompleteLoadProcess*)


PackageCompleteLoadProcess::doc="
Exists in this version of the PackageFramework simply to indicate that the full version will \
do more with this. At this point doesn't really do much.
";
PackageCompleteLoadProcess[pkg_]:=
  (
    PackageFrameworkPackageGet[pkg, #]&/@
        PackagePreloadedPackages[pkg];
    pkg
    )


(* ::Subsection:: *)
(*Dependencies*)


(* ::Subsubsection::Closed:: *)
(*PackageLoadingMode*)


PackageLoadingMode[pkg_]:=
  Replace[pkg["LoadingMode"],
    Automatic:>
      If[TrueQ@$PackageFrameworkInDependencyLoad, "Dependency", "Primary"]
    ]


(* ::Subsubsection::Closed:: *)
(*PackageDependencies*)


PackageDependencies[pkg_PackageFrameworkPackage]:=
  pkg["Dependencies"]


(* ::Subsubsection::Closed:: *)
(*PackageDependencyBase*)


PackageDependencyBase[pkg_PackageFrameworkPackage]:=
  Replace[
    pkg["DependencyServer"],
    _Missing->Automatic
    ]


(* ::Subsubsection::Closed:: *)
(*PackageCheckPacletDependency*)


PackageCheckPacletDependency[dep_]:=
  Length@PacletManager`PacletFind[StringDelete[dep, "`"]]>0


(* ::Subsubsection::Closed:: *)
(*PackageInstallPacletDependency*)


PackageFrameworkPackage::nodep="Couldn't load dependency `` of type ``";


$PacletRepositoryServer="http://raw.githubusercontent.com/paclets/PacletServer/master";


Options[PackageInstallPacletDependency]=
  Options[PacletManager`PacletInstall];
PackageInstallPacletDependency[
  deps:{
    __String?(
      StringMatchQ[
        (LetterCharacter|"_"|"`")~~(WordCharacter|"_"|"`")..
        ]
    )}, 
  ops:OptionsPattern[]
  ]:=
  Block[{site, pacs, pac},
    pacs=
      StringDelete[deps, "`"];
    site=
      Replace[OptionValue["Site"],
        {
          s_String?(
            URLParse[#, "Domain"]==="github.com"&
            ):>
          URLBuild@
            <|
              "Scheme"->"http",
              "Domain"->"raw.githubusercontent.com",
              "Path"->
                Function[If[Length[#]==2, Append[#, "master"], #]]@
                  DeleteCases[""]@URLParse[s, "Path"]
              |>,
          None->
            Automatic,
          Except[_String]->
            $PacletRepositoryServer
          }
        ];
    pac=First@pacs;
    Monitor[
      MapThread[
        Check[
          PacletManager`PacletInstall[
            pac=#,
            "Site"->site,
            ops
            ],
          Message[PackageFrameworkPackage::nodep, #2, "Paclet"];
          $Failed
          ]&,
        {
          pacs,
          deps
          }
        ],
      Internal`LoadingPanel[
        TemplateApply[
          "Loading paclet `` from site ``",
          {pac, site}
          ]
        ]
      ]
    ]


PackageInstallPacletDependency[
  dep:_String?(
    StringMatchQ[
      (LetterCharacter|"_"|"`")~~(WordCharacter|"_"|"`")..
      ]
    ), 
  ops:OptionsPattern[]
  ]:=First@PackageInstallPacletDependency[{dep}, ops]


(* ::Subsubsection::Closed:: *)
(*PackageUpdatePacletDependency*)


PackageFrameworkPackage::nodup="Couldn't update dependency `` of type ``";


Options[PackageUpdatePacletDependency]=
  {
    "Sites"->Automatic
    };
PackageUpdatePacletDependency[
  deps:{__String?(StringMatchQ[(LetterCharacter|"_")~~(WordCharacter|"_")..])}, 
  ops:OptionsPattern[]
  ]:=
  Block[
    {
      added=<||>,
      ps=PacletManager`PacletSites[],
      pac
      },
    Replace[
      Replace[OptionValue["Sites"], Automatic:>{$PacletRepositoryServer}],
      {
        s_String:>
          If[!MemberQ[ps, PacletManager`PacletSite[s, ___]],
            added[s]=True
            ],
        p:PacletManager`PacletSite[__]:>
          If[!MemberQ[ps, p],
            added[p]=True
            ]
        },
      1
      ];
    pac=StringDelete[deps[[1]], "`"];
    Internal`WithLocalSettings[
      KeyMap[PacletManager`PacletSiteAdd, added],
      Monitor[
        MapThread[
          Check[
            PacletManager`PacletCheckUpdate[pac=#],
            Message[PackageFrameworkPackage::nodup, #2, "Paclet"];
            $Failed
            ]&,
          {
            StringDelete[deps, "`"],
            deps
            }
          ],
        Internal`LoadingPanel[
          "Updating paclet ``"~TemplateApply~pac
          ]
        ],
      KeyMap[PacletManager`PacletSiteRemove, added]
      ]
    ];


PackageUpdatePacletDependency[
  dep:_String?(StringMatchQ[(LetterCharacter|"_")~~(WordCharacter|"_")..]), 
  ops:OptionsPattern[]
  ]:=
  First@PackageUpdatePacletDependency[{dep}, ops]


(* ::Subsubsection::Closed:: *)
(*PackageLoadPacletDependency*)


Options[PackageLoadPacletDependency]=
  Join[
    Options[PackageInstallPacletDependency],
    {
      "Update"->False
      }
    ];
PackageLoadPacletDependency[pkg_, dep_String?(StringEndsQ["`"]), ops:OptionsPattern[]]:=
  Internal`WithLocalSettings[
    System`Private`NewContextPath[{dep, "System`"}];,
    If[PackageCheckPacletDependency[pkg, dep],
      If[TrueQ@OptionValue["Update"],
        PackageUpdatePacletDependency[
          pkg,
          dep,
          "Sites"->Replace[OptionValue["Site"], s_String:>{s}]
          ]
        ],
      PackageInstallPacletDependency[
        pkg,
        dep, 
        FilterRules[{ops}, Options@PackageInstallPacletDependency]
        ]
      ];
    Needs[dep];
    PackageExtendContextPath[
      pkg,
      Select[$Packages, 
        StringStartsQ[#, dep]&&StringFreeQ[#, "`*Private`"]&
        ]
      ];,
    System`Private`RestoreContextPath[];
    ]


(* ::Subsubsection::Closed:: *)
(*PackageEnsureLoadDependency*)


dependencyPackageMainPackageRoot[pkg_]:=
  Module[{pfp},
    pfp = 
      SplitBy[
        FileNameSplit[PackageRoot[pkg]],
        #==="Dependencies"&
        ];
    Reap[
      Fold[
        If[#2==="Dependencies", 
          Sow,
          Identity
          ]@
          FileNameJoin@Flatten@{#, #2}&,
        pfp
        ]
      ][[2]]
    ]


PackageEnsureLoadDependency//Clear
Options[PackageEnsureLoadDependency]=
  Join[
    Options@PackageLoadPacletDependency,
    {
      "Bundled"->True
      }
    ];
PackageEnsureLoadDependency[pkg_PackageFrameworkPackage, dep_, ops:OptionsPattern[]]:=
  Module[
    {
      depsDir,
      pfp,
      foundFile,
      bund=TrueQ@Quiet@OptionValue[PackageEnsureLoadDependency, ops, "Bundled"]
      },
     depsDir=
       If[PackageLoadingMode[pkg]==="Dependency",
         DeleteDuplicates@Flatten@{
          ParentDirectory@PackageRoot[pkg],
          PackageRoot[pkg, "Dependencies"],
          dependencyPackageMainPackageRoot[pkg]
         },
        {PackageRoot[pkg, "Dependencies"]}
        ];
     If[bund,
       If[AnyTrue[depsDir, DirectoryQ],
         foundFile=
           Block[
             {
               $Path=depsDir,
               PacletManager`PacletManagerEnabled
               },
             FindFile[dep]
             ];
         ];
       If[!StringQ@foundFile, 
         foundFile=FindFile[dep]
         ];
       bund=StringQ@foundFile
       ];
     Block[{$PackageFrameworkInDependencyLoad=True},
       Quiet[(* this is a temporary hack until WRI fixes a $ContextPath bug *)
         If[!bund,
           PackageLoadPacletDependency[
             dep,
             Sequence@@
               FilterRules[
                 {
                   ops,
                   "Update"->True,
                   "Loading"->Get
                   },
                 Options@PackageLoadPacletDependency
                 ]
             ],
           Lookup[Flatten@{ops}, "Loading", Get]@foundFile
           (* I have my reasons to do this rather than Needs... but it could change... *)
           ],
        General::shdw
        ]
      ];
    ];


(* ::Subsubsection::Closed:: *)
(*PackageEnsureLoadDependencies*)


PackageEnsureLoadDependencies[pkg_PackageFrameworkPackage]:=
  If[!TrueQ@PropertyValue[pkg, "DependenciesLoadedFlag"],
    SetProperty[pkg, "DependenciesLoadedFlag"->True];
    Module[
      {
        deps=PackageDependencies[pkg],
        site=Replace[PackageDependencyBase[pkg], Automatic->$PacletRepositoryServer],
        ctx
        },
       PackageExecute[
         pkg,
         Internal`WithLocalSettings[
           ctx = $Context;
           Begin["`Dependencies`"],
           PackageEnsureLoadDependency[
             pkg,
             Sequence@@Flatten@{#, "Site"->site}
             ]&/@deps;
           PackageExposeDependencies[pkg],
           End[]; 
           (* just in case some abort happened in a dependency load *)
           $Context = ctx;
           ]
         ]
      ];
   ];


(* ::Subsubsection::Closed:: *)
(*PackageDependencyContexts*)


PackageDependencyContexts[pkg_]:=
  PackageDependencies[pkg][[All, 1]];


(* ::Subsubsection::Closed:: *)
(*PackageExposeDependencies*)


PackageExposeDependencies[pkg_, deps_, permanent:True|False:False]:=
  Module[
    {
      cdeps,
      loadQ,
      depCs
      },
    depCs=
      {
        If[PackageLoadingMode[pkg]==="Dependency",
          StringSplit[PackageContexts[pkg][[1]], "Dependencies`", 2][[1]]<>
            "Dependencies`",
          Nothing
          ],
        PackageContexts[pkg][[1]]<>"Dependencies`"
        };
    cdeps=
      With[{ctx=#},
        SelectFirst[
          depCs,
          Length@Join[Names[#<>ctx<>"*"], Names[#<>ctx<>"*`*"]]>0&,
          ""
          ]<>#
        ]&/@deps;
    (* only gonna be used within PackageExecute...? *)
    $ContextPath=
      DeleteDuplicates@
        Join[cdeps, $ContextPath];
    If[permanent,
      PackageExtendContextPath[pkg, cdeps]
      ];
    cdeps
    ];
PackageExposeDependencies[pkg_]:= 
  PackageExposeDependencies[
    pkg,
    PackageDependencyContexts[pkg],
    True
    ]


(* ::Subsection:: *)
(*Package Helpers*)


(* ::Subsubsection::Closed:: *)
(*PackageConfigurePackageHelpers*)


(* ::Text:: *)
(*Originally these were the constants that every package set in its load script*)
(*Now that we're moving to a single loader _package_ these will all be references to a $CurrentPackage object*)
(*that will be bound by PackageFramework`LoadPackage[...], which will be the main entry point into loading*)
(*packages these days*)


makePackageScopeSymbol[cont_, name_]:=
  ToExpression[cont<>"PackageScope`"<>name, StandardForm, 
    Function[
      Null,
      ClearAll[#];
      #,
      HoldAllComplete
      ]
    ]


PackageConfigurePackageHelpers[pkg_]:=
  With[{cont=PackageContext[pkg]},
    With[
      {
        $PackageObject=makePackageScopeSymbol[cont, "$PackageObject"],
        $PackageName=makePackageScopeSymbol[cont, "$PackageName"],
        $PackageHead=makePackageScopeSymbol[cont, "$PackageHead"],
        $PackageDirectory=makePackageScopeSymbol[cont, "$PackageDirectory"]
        (*$PackageContexts=Symbol[packageScope<>"$PackageContexts"],
        $PackageSymbols=Symbol[packageScope<>"$PackageSymbols"],
        $PackageListing=Symbol[packageScope<>"$PackageListing"],
        $PackageFileContexts=Symbol[packageScope<>"$PackageFileContexts"],
        $LoadedPackages=Symbol[packageScope<>"$LoadedPackages"],
        $DeclaredPackages=Symbol[packageScope<>"$DeclaredPackages"],
        $PackageLoadingMode=Symbol[packageScope<>"$PackageLoadingMode"],
        $PackageDependencies=Symbol[packageScope<>"$PackageDependencies"],
        $PackageDependencyBase=Symbol[packageScope<>"$PackageDependencyBase"]*)
        },
        (* usage messages *)
        $PackageObject::usage="The PackageFrameworkPackage for this package";
        $PackageHead::usage="The manager symbol for this package";
        $PackageName::usage="Applies PackageName to $CurrentPackage";
        $PackageDirectory::usage="Applies PackageRoot to $CurrentPackage";
        (*$PackageListing::usage="The listing of packages";
        $PackageFileContexts::usage="The contexts for files in $CurrentPackage";
        $PackageSymbols::usage="Applies PackageSymbols to $CurrentPackage";
        $LoadedPackages::usage="The set of loaded packages for $CurrentPackage";
        $DeclaredPackages::usage="The set of packages found and declared via the autoloader";
        $PackageLoadingMode::usage="A flag that determines whether the package is primary or a 
dependency";
        $PackageDependencies::usage="A set of dependencies for the package to load";
        $PackageDependencyBase::usage="The server from which dependencies should be loaded";
        $PackageContexts::usage="The set of Contexts used by $CurrentPackage";*)
        (* bindings *)
        $PackageObject=pkg;
        $PackageHead=PackageHead[pkg];
        $PackageName=PackageName[pkg];
        $PackageDirectory=PackageRoot[pkg];
        (*$PackageListing:=PackageListing[pkg];
        $PackageFileContexts:=PackageFileContexts[pkg];
        $PackageSymbols:=PackageSymbols[pkg];
        $LoadedPackages:=PackageLoadedPackages[pkg];
        $DeclaredPackages:=PackageSymbols[pkg];*)
      ]
    ]


(* ::Subsection:: *)
(*End Private*)


End[];


(* ::Section:: *)
(*End Package*)


(* ::Subsection:: *)
(*EndPackage*)


EndPackage[];
