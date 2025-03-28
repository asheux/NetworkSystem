(* ::Package:: *)

Package["WolframInstitute`NetworkSystem`"]


PackageExport[NetworkSystemRule]
PackageExport[GetDepthOneRules]

NetworkSystemRule::argx = "NetworkSystemRule called with incorrect number or type of arguments:
    Arguments must be of the form: NetworkSystemRule[code_Integer, depth_Integer].";
NetworkSystemRule::rangeError = "Index out of range for depth=`1`. Max index is `2`. Depth can be specified using the option 'Depth' -> d.";

decodeOneLink[code_, d_] := Module[{f, baseDigits, basemap},
    f[i_] := IntegerDigits[i, 6, d + 1];
    baseDigits = If[! MemberQ[Range[0, 5], code], f[code], code];
    basemap = If[d == 1,
        {1 -> {1}, 2 -> {2}, 3 -> {{1}, {1}}, 4 -> {{1}, {2}}, 5 -> {{2}, {1}}, 6 -> {{2}, {2}}},
        {1 -> {1}, 2 -> {2}, 3 -> {1, 1}, 4 -> {1, 2}, 5 -> {2, 1}, 6 -> {2, 2}}
    ];
    (1 + Replace[baseDigits, {{1, 0, x_} :> x, {1 | 0, x_, y_} :> {x, y}}]) /. basemap
]

decodeOneCaseIndex[indexcase_, d_] := Module[{sz = 6^d, up, down},
    up = Quotient[indexcase, sz];
    down = Mod[indexcase, sz];
    {decodeOneLink[up, d], decodeOneLink[down, d]}
]

NetworkSystemRule[index_?IntegerQ, d_?IntegerQ] := Module[{localpairs, c, base = (6^d)^2},
    localpairs = Tuples[Table[Range[1, 2^k], {k, d}]];
    c = Length[localpairs];
    If[index >= base^c,
        Return[
            Failure[
                "RangeError", 
                <|
                    "MessageTemplate" -> NetworkSystemRule::rangeError,
                    "MessageParameters" -> {d, base^c - 1}
                |>
            ]
        ]
    ];
    {d, MapThread[If[IntegerQ[#], {#}, #] -> #2 &,
        {localpairs, decodeOneCaseIndex[#, d] & /@ IntegerDigits[index, base, c]}]
    }
]
NetworkSystemRule[___] := Failure["BadArg", <|"MessageTemplate" -> NetworkSystemRule::argx|>];

GetDepthOneRules[code_Integer] := {1, Table[{i} -> Table[
     {{1}, {2}, {{1}, {1}}, {{1}, {2}}, {{2}, {1}}, {{2}, {2}}}[[1 +
        IntegerDigits[code, 6, 4][[1 + 2 (i - 1) + (j - 1)]]]],
     {j, 2}], {i, 2}
   ]
}
