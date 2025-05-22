(* ::Package:: *)

Package["WolframInstitute`NetworkSystem`"]


PackageExport[NetworkSystemEvolutionList]
PackageExport[NetworkSystemDisplay]
PackageExport[NetworkSystemEvolutionPlot]
PackageExport[CyclicNet]


NetworkSystemEvolutionList::badarg = "Arguments must be of the form:
    NetworkSystemEvolutionList[{depth_Integer, {{ints...} -> {List|Integer, List|Integer}, ... }}, {{i1_Integer, j1_Integer}, {i2_Integer, j2_Integer}, ...}, total_Integer, Options... ].
    NetworkSystemEvolutionList[code_Integer, {{i1_Integer, j1_Integer}, {i2_Integer, j2_Integer}, ...}, total_Integer, Options... ].";
NetworkSystemEvolutionPlot::badarg = "Arguments must be of the form:
    NetworkSystemEvolutionPlot[{depth_Integer, {{ints...} -> {List|Integer, List|Integer}, ... }}, {{i1_Integer, j1_Integer}, {i2_Integer, j2_Integer}, ...}, total_Integer, Options... ].
    NetworkSystemEvolutionPlot[code_Integer, {{i1_Integer, j1_Integer}, {i2_Integer, j2_Integer}, ...}, total_Integer, Options... ].";
NetworkSystemDisplay::badarg = "Argument must be a list of integer pairs. e.g., {{5, 2}, {1, 3}, {2, 4}, {3, 5}, {4, 1}}";
CyclicNet::argx = "CyclicNet called with an incorrect number or type of arguments. Expected CyclicNet[t_Integer]";


arrow[{a_, b_}, n_, tot_] := Module[
  {dir, rxy, r = 0.1, oset = 0.01, up = {0, Pi}, down = {Pi, 2 Pi}},
  dir = If[b > a, 1, -1];
  rxy = Abs[{(a - b)/2, 0.9*Sqrt[(a - b)/tot]}];

  If[a == b, Circle[{a, n/6}, 1/6],
  {
    Arrowheads[{{dir*Automatic, 0.5, Graphics[Line[n {{-r - oset, r}, {0, 0}, {-r - oset, -r}}]]}}],
    Arrow[Circle[{Mean[{a, b}], 0}, rxy, n /. {1 -> up, -1 -> down}]]
  }]
]

NetEvolutionStep[{depth_Integer, rules_List}, list_List] := Block[{new = {}},
    Join[Table[
        Map[NetEvolutionStep1[#, list, i]&, Replace[NeighborNumbers[list, i, depth], rules]],
        {i, Length[list]}
      ],
    new]
]

NetEvolutionStep1[s : {___Integer}, list_, i_] := FollowPath[list, i, s]
NetEvolutionStep1[{s1 : {___Integer}, s2 : {___Integer}}, list_List, i_Integer] := Module[{nodenumber},
    nodenumber = Length[list] + Length[AppendTo[new, {FollowPath[list, i, s1], FollowPath[list, i, s2]}]];
    AppendTo[nnmap, {i, nodenumber}]; nodenumber
]

FollowPath[list_, i_, s_] := Fold[(list[[#1]][[#2]]) &, i, s]

NeighborNumbers[list_, i_, d_] := (Length /@ NestList[Union[Flatten[list[[#]]]] &, Union[list[[i]]], d - 1])

ConnectedNodes[list_, i_] := Module[{seq, pl},
  seq = FixedPoint[DeleteDuplicates[Flatten[{#, list[[#]]}]] &, {1}];
  pl = Sort[If[MemberQ[Last /@ nnmap, #], Cases[nnmap, {_, #}][[1]], {#, #}] & /@ seq];
  seq = Last /@ pl;
  {First /@ pl, Map[Position[seq, #][[1, 1]] &, list[[seq]], {2}]}
]

CyclicNet[n_?IntegerQ] := RotateRight[Table[Mod[{i - 1, i + 1}, n] + 1, {i, n}]]
CyclicNet[___] := Failure["BadArg", <|"MessageTemplate" -> CyclicNet::argx|>];

Options[NetworkSystemEvolutionList] = {
    "Depth" -> 1,
    "SimpleNet" -> False
};
NetworkSystemEvolutionList[
    code_?IntegerQ,
    init: {{_Integer, _Integer} ..},
    tot_?IntegerQ,
    opts: OptionsPattern[]
] := Module[{rules, depth},
    depth = OptionValue["Depth"];
    rules = NetworkSystemRule[code, depth];
    If[
        FailureQ[rules],
        Return[rules]
    ];
    NetworkSystemEvolutionList[rules, init, tot, FilterRules[{opts}, Options[NetworkSystemEvolutionList]]]
]
NetworkSystemEvolutionList[
    rules: {_Integer, {({_Integer ..} -> {_List | _Integer, _List | _Integer}) ..}},
    init: {{_Integer, _Integer} ..},
    tot_?IntegerQ,
    opts: OptionsPattern[]
] := Module[{rmd},
  rmd = OptionValue["SimpleNet"];
  NestList[Block[{nnmap = {}},
     (rmd /. {False -> ConnectedNodes[#, 1],
          True -> {Union[Flatten[#]], #}}
        &)[NetEvolutionStep[rules, Last[#]]]
     ] &, {{}, init}, tot]
]
NetworkSystemEvolutionList[___] := Failure["BadArg", <|"MessageTemplate" -> NetworkSystemEvolutionList::badarg|>];

NetworkSystemDisplay[net: {{_Integer, _Integer} ..}] := Module[{i},
  Graphics[MapIndexed[{
      i = #2[[1]];
      GeometricTransformation[{
        AbsolutePointSize[2.5], AbsoluteThickness[.8], Point[{i, 0}],
        arrow[{#[[1]], i}, 1, Length[net]], arrow[{#[[2]], i}, -1, Length[net]]
        },
       RotationTransform[2 Pi, {i, 0}]
       ]
      } &, net]
  ]
]
NetworkSystemDisplay[___] := Failure["BadArg", <|"MessageTemplate" -> NetworkSystemDisplay::badarg|>];

Options[NetworkSystemEvolutionPlot] = {
    "Depth" -> 1,
    "SimpleNet" -> False
};
NetworkSystemEvolutionPlot[
    code_Integer,
    init: {{_Integer, _Integer} ..},
    tot_?IntegerQ,
    opts: OptionsPattern[]
] := Module[{depth, rules}, 
    depth = OptionValue["Depth"];
    rules = NetworkSystemRule[code, depth];
    If[
        FailureQ[rules],
        Return[rules],
        NetworkSystemEvolutionPlot[rules, init, tot, FilterRules[{opts}, Options[NetworkSystemEvolutionPlot]]]
    ]
]
NetworkSystemEvolutionPlot[
    rules: {_Integer, {({_Integer ..} -> {_List | _Integer, _List | _Integer}) ..}},
    init: {{_Integer, _Integer} ..},
    tot_?IntegerQ,
    opts: OptionsPattern[]
] := Module[{issimplenet, i, history, k = 2},
    issimplenet = OptionValue["SimpleNet"];
    history = NetworkSystemEvolutionList[rules, init, tot, "SimpleNet" -> issimplenet];
    Graphics[{
        {GrayLevel[0.6], AbsoluteThickness[2], MapIndexed[(
            i = First[#2];
            MapIndexed[Line[{{First[#2], -k*i}, {#1, k - k*i}}] &,
                First[#1]]
        ) &, history]},
        MapIndexed[
            Translate[First[#], {0, -k First[#2]}] &,
            Map[NetworkSystemDisplay[#[[2]]] &, history]
        ]
    },
    PlotRange -> {{0.5, 0.5 + Max[Length[#[[2]]] & /@ history]}, All}
    ]
]
NetworkSystemEvolutionPlot[___] := Failure["BadArg", <|"MessageTemplate" -> NetworkSystemEvolutionPlot::badarg|>];
