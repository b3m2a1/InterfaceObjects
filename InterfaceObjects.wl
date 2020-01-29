(* ::Package:: *)

(* ::Section:: *)
(*Package Loader*)


(* ::Text:: *)
(*Default loader template from the PackageFramework  *)


Block[{$Path=Prepend[$Path, DirectoryName@$InputFileName]},
  Needs["PackageFramework`"]
  ];


PackageFramework`LoadPackage[]
