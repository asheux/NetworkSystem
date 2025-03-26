(* ::Package:: *)

Package["WolframInstitute`NetworkSystem`"]

ClearAll["WolframInstitute`NetworkSystem`*", "WolframInstitute`NetworkSystem`**`*"]

$PacletPath = ExpandFileName[FileNameJoin[{DirectoryName[$InputFileName], ".."}]]

If[ $FrontEnd =!= Null,
    FrontEndExecute[FE`systemQ[FE`s_] := StringMatchQ[Quiet[Check[Context[FE`s], ""]], "System`" | "WolframInstitute`NetworkSystem`"]];
    Scan[
        FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]] &,
        Import[FileNameJoin[{PacletObject["WolframInstitute/NetworkSystem"]["Location"], "AutoCompletionData", "specialArgFunctions.tr"}], "WL"]
    ]
]
