Package["WolframInstitute`NetworkSystem`"]

PackageExport[NDNetworkEvolutionList]
PackageExport[NDNetworkDisplay]
PackageExport[NDNetworkEvolutionPlot]
PackageExport[CyclicNet]



drawArrow[{a_, b_}, n_, tot_] := Module[
  {dir, rxy, r = 0.3, oset = 0.15, up = {0, Pi}, down = {Pi, 2 Pi}},
  dir = If[b > a, 1, -1];
  rxy = Abs[{(a - b)/2, 0.9*Sqrt[(a - b)/tot]}];

  If[a == b, Circle[{a, n/6}, 1/6],
  {
    Arrowheads[{{dir*Small, 0.5, Graphics[Line[n {{-r - oset, r}, {0, 0}, {-r - oset, -r}}]]}}],
    Arrow[Circle[{Mean[{a, b}], 0}, rxy, n /. {1 -> up, -1 -> down}]]
  }]
]

NDNetEvolutionStep[{depth_Integer, rules_List}, list_List] := Block[{new = {}},
    Join[Table[Map[NDNetEvolutionStep1[#, list, i] &, Replace[NeighborNumbers[list, i, depth], rules]], {i, Length[list]}], new]]

FollowPath[list_, i_, s_] := Fold[(list[[#1]][[#2]]) &, i, s]

NeighborNumbers[list_, i_, d_] := (Length /@ NestList[Union[Flatten[list[[#]]]] &, Union[list[[i]]], d - 1])

NDNetEvolutionStep1[s : {___Integer}, list_, i_] := FollowPath[list, i, s]

NDNetEvolutionStep1[{s1 : {___Integer}, s2 : {___Integer}}, list_List, i_Integer] := Module[{nodenumber},
  nodenumber = Length[list] + Length[AppendTo[new, {FollowPath[list, i, s1], FollowPath[list, i, s2]}]];
  AppendTo[nnmap, {i, nodenumber}]; nodenumber
]

ConnectedNodes[list_, i_] := Module[{seq, pl},
  seq = FixedPoint[DeleteDuplicates[Flatten[{#, list[[#]]}]] &, {1}];
  pl = Sort[If[MemberQ[Last /@ nnmap, #], Cases[nnmap, {_, #}][[1]], {#, #}] & /@ seq];
  seq = Last /@ pl;
  {First /@ pl, Map[Position[seq, #][[1, 1]] &, list[[seq]], {2}]}
]

CyclicNet[n_] := RotateRight[Table[Mod[{i - 1, i + 1}, n] + 1, {i, n}]]

Options[NDNetworkEvolutionList] = {"SimpleNet" -> False};
NDNetworkEvolutionList[rules_, init_, tot_Integer, OptionsPattern[]] := Module[{rmd},
  rmd = OptionValue["SimpleNet"];
  NestList[Block[{nnmap = {}},
     (rmd /. {False -> ConnectedNodes[#, 1],
          True -> {Union[Flatten[#]], #}}
        &)[NDNetEvolutionStep[rules, Last[#]]]
     ] &, {{}, init}, tot + 1]
]


NDNetworkDisplay[net_] := Module[{i},
  Graphics[MapIndexed[{
      i = #2[[1]];
      GeometricTransformation[{
        AbsolutePointSize[2.5], AbsoluteThickness[.8], Point[{i, 0}],
        drawArrow[{#[[1]], i}, 1, Length[net]], drawArrow[{#[[2]], i}, -1, Length[net]]
        },
       RotationTransform[2 Pi, {i, 0}]
       ]
      } &, net]
   ]
]

Options[NDNetworkEvolutionPlot] = {"SimpleNet" -> False};
NDNetworkEvolutionPlot[rules_, init_, tot_Integer, OptionsPattern[]] := Module[{issimplenet, i, history, k = 2},
  issimplenet = OptionValue["SimpleNet"];
  history = NDNetworkEvolutionList[rules, init, tot, "SimpleNet" -> issimplenet];
  Graphics[{
    {GrayLevel[0.6], AbsoluteThickness[1.5], MapIndexed[(
        i = First[#2];
        MapIndexed[Line[{{First[#2], -k*i}, {#1, k - k*i}}] &,
         First[#1]]
        ) &, history]},
    MapIndexed[
     Translate[First[#], {0, -k First[#2]}] &,
     Map[NDNetworkDisplay[#[[2]]] &, history]]
    },
   PlotRange -> {{0.5, 0.5 + Max[Length[#[[2]]] & /@ history]}, All}
  ]
]
