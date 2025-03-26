(* ::Package:: *)

Package["WolframInstitute`NetworkSystem`"]


PackageExport[NetworkSystemRule]
PackageExport[GetDepthOneRules]


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

NetworkSystemRule[index_, d_] := Module[{localpairs, c, base = (6^d)^2},
    localpairs = Tuples[Table[Range[1, 2^k], {k, d}]];
    c = Length[localpairs];
    If[index >= base^c,
        Return["Index out of range for depth=" <> ToString[d] <>". Max index is " <> ToString[base^c - 1]];
    ];
    {d, MapThread[If[IntegerQ[#], {#}, #] -> #2 &,
        {localpairs, decodeOneCaseIndex[#, d] & /@ IntegerDigits[index, base, c]}]
    }
]

GetDepthOneRules[code_Integer] := {1, Table[{i} -> Table[
     {{1}, {2}, {{1}, {1}}, {{1}, {2}}, {{2}, {1}}, {{2}, {2}}}[[1 +
        IntegerDigits[code, 6, 4][[1 + 2 (i - 1) + (j - 1)]]]],
     {j, 2}], {i, 2}
   ]
}
