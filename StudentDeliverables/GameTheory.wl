(* ::Package:: *)

BeginPackage["GameTheory`"];                                                (* Makes GameTheory` and System` the only active contexts *)
                                                                               (* Symbols defined before `Private` are for Export *)


GameTheory::usage="Polymatrix Game";
GameTheoryPlot::usage="Polymatrix Games: 2x2, 2x3, and 2x2x2";

GameTheory::nodef = "Method `1` is not one of {\"Criteria\", \"Strategy\", \"Concept\"}"
GameTheory::optval = "The value of the option `1` is not one of `2`"
GameTheory::mixed2 = "Mixed strategy games are only supported for 2 players"
GameTheory::mixedc = "Mixed strategy games are only supported for \"NashEquilibrium\""
GameTheoryPlot::mixed2x2 = "Mixed strategy game Plot is only supported for \"NashEquilibrium\" and for dimensions: 2x2, 2x3, and 2x2x2"


Begin["`Private`"];


(* Symbols for Internal Use Only *)

Options[iGameTheory] = Options[GameTheory] = {
	Method->Automatic
}

GameTheory[args___] := With[{res = Catch[iGameTheory[args], $failureTag]},
	res /; res =!= $unevaluatedTag
]

iGameTheory[args___] := CompoundExpression[
	(*this will issue an appropriate message if there are too many, or too few, arguments*)
	System`Private`Arguments[GameTheory[args], {1, 1}],
	Throw[$unevaluatedTag, $failureTag]
]

iGameTheory[payoffMatrices:{__List}, OptionsPattern[]] := 
(* criterion:"Maximize"|"Minimize", type_,  solutionConcept:"NashEquilibrium"|"MaxMin"|"StackelbergEquilibrium" *)
Module[{method, crit, type, payoffM=payoffMatrices, concept,dimensions,numberOfPlayers},

	method = OptionValue[Method];

	{crit, type, concept} = If[method === Automatic,
		{"Maximize", "Pure", "NashEquilibrium"}
		,
		Check[
			Quiet[
				Check[
					OptionValue[
						{"Criteria"->"Maximize", "Strategy"->"Pure", "Concept"->"NashEquilibrium"},
						method,
						{"Criteria","Strategy","Concept"}
					],
					Message[
						GameTheory::nodef,
						First @ Keys @ FilterRules[method, Except[{"Criteria","Strategy","Concept"}]]
						],
					OptionValue::nodef
				],
				OptionValue::nodef
			],
			Throw[$unevaluatedTag, $failureTag]
		]
	];

	testOptionValue["Criteria"->crit, {"Maximize", "Minimize"}];
	testOptionValue["Strategy"->type, {"Pure", "Mixed"}];
	testOptionValue["Concept"->concept, {"NashEquilibrium", "MaxMin", "StackelbergEquilibrium"}];

	If[type === "Mixed",
		Which[
			Length[payoffM]>2,
			Message[GameTheory::rnimpl, GameTheory, "mixed strategies with more than 3 players"];
			Throw[$unevaluatedTag, $failureTag],
		
			MatchQ[concept, "MaxMin" | "StackelbergEquilibrium"],
			Message[GameTheory::rnimpl, GameTheory, "mixed strategies with the \"MaxMin\" or \"StackelbergEquilibrium\" concepts"];
			Throw[$unevaluatedTag, $failureTag]
		]
	];

	Which[
	   type == "Pure" && crit == "Maximize" && concept == "NashEquilibrium",
	   pureNashEquilibria[payoffM],
	   
	   type == "Mixed" && crit == "Maximize" && concept == "NashEquilibrium",
	   mixedNashEquilibria[payoffM],
	   	   
	   type == "Pure" && crit == "Maximize" && concept == "MaxMin",
	   maxMin[payoffM],
	   
	   type == "Pure" && crit == "Maximize" && concept == "StackelbergEquilibrium",
	   pureStackelbergEquilibria[payoffM]
	]
]

testOptionValue[lhs_->value_, options_List]:=If[!MemberQ[options, value],
	Message[GameTheory::optval, lhs->value, options];
	Throw[$unevaluatedTag, $failureTag]
]


Options[iGameTheoryPlot] = Options[GameTheoryPlot] = {
	Method->Automatic
}

GameTheoryPlot[args___] := With[{res = Catch[iGameTheoryPlot[args], $failureTag]},
	res /; res =!= $unevaluatedTag
]

iGameTheoryPlot[args___] := CompoundExpression[
	(*this will issue an appropriate message if there are too many, or too few, arguments*)
	System`Private`Arguments[GameTheoryPlot[args], {1, 1}],
	Throw[$unevaluatedTag, $failureTag]
]

iGameTheoryPlot[payoffMatrices:{__List}, OptionsPattern[]] := 
(* criterion:"Maximize"|"Minimize", type_,  solutionConcept:"NashEquilibrium"|"StackelbergEquilibrium" *)
Module[{method, crit, type, payoffM=payoffMatrices, concept,dimensions,numberOfPlayers},

	method = OptionValue[Method];

	{crit, type, concept} = If[method === Automatic,
		{"Maximize", "Mixed", "NashEquilibrium"}
		,
		Check[
			Quiet[
				Check[
					OptionValue[
						{"Criteria"->"Maximize", "Strategy"->"Mixed", "Concept"->"NashEquilibrium"},
						method,
						{"Criteria","Strategy","Concept"}
					],
					Message[
						GameTheory::nodef,
						First @ Keys @ FilterRules[method, Except[{"Criteria","Strategy","Concept"}]]
						],
					OptionValue::nodef
				],
				OptionValue::nodef
			],
			Throw[$unevaluatedTag, $failureTag]
		]
	];

	testOptionValue["Criteria"->crit, {"Maximize", "Minimize"}];
	testOptionValue["Strategy"->type, {"Mixed"}];
	testOptionValue["Concept"->concept, {"NashEquilibrium", "StackelbergEquilibrium"}];

	If[type === "Mixed",
		Which[
			!MatchQ[Dimensions[payoffM],{2,2}|{2,3}|{2,2,2}],
			Message[GameTheoryPlot::mixed2x2];
			Throw[$unevaluatedTag, $failureTag],
		
			MatchQ[concept, "MaxMin" | "StackelbergEquilibrium"],
			Message[GameTheoryPlot::mixed2x2];
			Throw[$unevaluatedTag, $failureTag]
		]
	];

	Which[
	   type == "Mixed" && crit == "Maximize" && concept == "NashEquilibrium",
	   Switch[Dimensions@payoffM[[1]],
			{2,2}, game2x2Plot[payoffM],
			{2,3}, game2x3Plot[payoffM],
			{2,2,2}, game2x2x2Plot[payoffM]
		],   
	   	   
		True,
		Message[GameTheoryPlot::minimize];
		Throw[$unevaluatedTag, $failureTag]
	]
]


(* ---- Pure Nash Equilibria ---- *)

indexSets[dims_List]:=With[{indices=Range/@dims},
	Table[
		ReplacePart[indices, i->{All}],
		{i,Length[dims]}
	]
]

maxPositions[list_]:=Pick[Range@Length@list, list,Max[list]]

bestResponse[m_]:=With[{ind=indexSets[Dimensions[m[[1]]]]},
	Intersection@@Table[
		Flatten[
			Thread/@Table[
				Replace[i, All:>maxPositions@m[[player,Sequence@@i]], {1}],
				{i, Tuples[ind[[player]]]}
			],
			1
		],
		{player, Length[m]}
	]
]

pureNashEquilibria[matr_]:=Module[{sNE=bestResponse[matr]},
                               Table[
                                 Apply[Association,
                                    Table[
                                          "Player "<>ToString[j]->sNE[[i,j]],
                                          {j,Length[matr]}
                                    ]
                                 ]->
                                    matr[[All,Sequence@@sNE[[i]]]],
                                    {i,Length[sNE]}
                               ]
]
(*"Player "<>ToString[player] -> <|"Strategy"->maxPositions[min],"Payoff"->min[[maxPositions[min]]][[1]]|>*)



(* ---- Mixed Nash Equilibria ---- *)

XRegion[b_,i_,j_,\[DoubleStruckCapitalI]_,\[DoubleStruckCapitalJ]_]:=Module[{comp\[DoubleStruckCapitalJ]=Complement[Range@Length@First@b,(\[DoubleStruckCapitalJ]\[Union]{j})],res},res=Inactive[RegionIntersection]@@Join[Table[Hyperplane[b[[All,k]]-b[[All,j]]],{k,\[DoubleStruckCapitalJ]}],Table[HalfSpace[b[[All,k]]-b[[All,j]]],{k,comp\[DoubleStruckCapitalJ]}],{Simplex[IdentityMatrix@Length@b]},{Parallelepiped[ConstantArray[0,Length@b],IdentityMatrix[Length@b][[\[DoubleStruckCapitalI]\[Union]{i}]]]}]]

X[b_,i_,j_,\[DoubleStruckCapitalI]_,\[DoubleStruckCapitalJ]_]:=With[{region=XRegion[b,i,j,\[DoubleStruckCapitalI],\[DoubleStruckCapitalJ]]},RegionMeasure@Activate@region]

XOut[b_,i_,j_,\[DoubleStruckCapitalI]_,\[DoubleStruckCapitalJ]_]:=With[{region=XRegion[b,i,j,\[DoubleStruckCapitalI],\[DoubleStruckCapitalJ]]},Solve[RegionMember[Activate@region,\[FormalX]]]]

YRegion[A_,i_,j_,\[DoubleStruckCapitalI]_,\[DoubleStruckCapitalJ]_]:=Module[{comp\[DoubleStruckCapitalI]=Complement[Range@Length@A,(\[DoubleStruckCapitalI]\[Union]{i})],res},res=Inactive[RegionIntersection]@@Join[Table[Hyperplane[A[[k]]-A[[i]]],{k,\[DoubleStruckCapitalI]}],Table[HalfSpace[A[[k]]-A[[i]]],{k,comp\[DoubleStruckCapitalI]}],{Simplex[IdentityMatrix@Length@First@A]},{Parallelepiped[ConstantArray[0,Length@First@A],IdentityMatrix[Length@First@A][[\[DoubleStruckCapitalJ]\[Union]{j}]]]}]]

Y[A_,i_,j_,\[DoubleStruckCapitalI]_,\[DoubleStruckCapitalJ]_]:=With[{res=YRegion[A,i,j,\[DoubleStruckCapitalI],\[DoubleStruckCapitalJ]]},RegionMeasure@Activate@res]

YOut[A_,i_,j_,\[DoubleStruckCapitalI]_,\[DoubleStruckCapitalJ]_]:=With[{res=YRegion[A,i,j,\[DoubleStruckCapitalI],\[DoubleStruckCapitalJ]]},Solve[RegionMember[Activate@res,\[FormalY]]]]


mixedNashEquilibria[m_]:=With[{a=m[[1]],b=m[[2]]},

  NESet={};

  Do[
     \[DoubleStruckCapitalU]=Range[i+1,Dimensions[a][[1]]];
        Do[
              Do[
                  If[X[b,i,j,\[DoubleStruckCapitalI],{}]==0,Break,Continue];
                  \[DoubleStruckCapitalV]=Range[j+1,Dimensions[a][[2]]];
                       Do[                                            
                            If[Y[a,i,j,\[DoubleStruckCapitalI],\[DoubleStruckCapitalJ]]!=0&&X[b,i,j,\[DoubleStruckCapitalI],\[DoubleStruckCapitalJ]]!=0,
                                      NESet=AppendTo[NESet,{XOut[b,i,j,\[DoubleStruckCapitalI],\[DoubleStruckCapitalJ]],YOut[a,i,j,\[DoubleStruckCapitalI],\[DoubleStruckCapitalJ]]}],
                                      Break
                             ],
                        {\[DoubleStruckCapitalJ],Subsets[\[DoubleStruckCapitalV]]}
                       ],
               {j,Dimensions[a][[2]]}
              ],
         {\[DoubleStruckCapitalI],Subsets[\[DoubleStruckCapitalU]]}
        ],
   {i,Dimensions[a][[1]]}
  ];
  DeleteDuplicates@NESet
]


(* ------ MaxMin strategies ------ *)

maxMin[matr_]:=Module[{dim=Dimensions[matr[[1]]], min, ind, tuples, payoffTuples},(*matr\[LeftDoubleBracket]1\[RightDoubleBracket] has the same dimensions as all other m\[LeftDoubleBracket]2\[RightDoubleBracket],...,m\[LeftDoubleBracket]n\[RightDoubleBracket]*)
	Table[
	    min=Table[0,dim[[player]]];
		Do[
		    ind=Table[Range[dim[[j]]],{j,Length[matr]}];                (* list of strategy sets *)
			ind[[player]]={str};                                         (* strategy set of the player is set simply formed by one element: {str} *)
			tuples=Tuples[ind];                                           (* generate the tuples *)
			payoffTuples=Table[matr[[player,Sequence@@t]],{t,tuples}];   (* the variable player gives the matrix of the player; other indeces give the payoff value *)
			min[[str]]=Min[payoffTuples],                                 (* for every player *)
		 {str,dim[[player]]}
		];
		"Player "<>ToString[player] -> <|"Strategy"->maxPositions[min],"Payoff"->min[[maxPositions[min]]][[1]]|>,
	 {player,Length[matr]}
	]
]


(* ------ Pure Stackelberg equilibria  ------ *)

pureStackelbergEquilibria[matr_]:=Module[{dim=Dimensions[matr[[1]]], min, ind, tuples, payoffTuples},(*matr\[LeftDoubleBracket]1\[RightDoubleBracket] has the same dimensions as all other m\[LeftDoubleBracket]2\[RightDoubleBracket],...,m\[LeftDoubleBracket]n\[RightDoubleBracket]*)
	Table[
	    min=Table[0,dim[[player]]];
		Do[
		    ind=Table[Range[dim[[j]]],{j,Length[matr]}];                (* list of strategy sets *)
			ind[[player]]={str};                                         (* strategy set of the player is set simply formed by one element: {str} *)
			tuples=Tuples[ind];                                           (* generate the tuples *)
			payoffTuples=Table[matr[[player,Sequence@@t]],{t,tuples}];   (* the variable player gives the matrix of the player; other indices give the payoff value *)
			min[[str]]=Max[payoffTuples],                                 (* for every player *)
		 {str,dim[[player]]}
		];
		"Player "<>ToString[player] -> <|"Strategy"->tuples[[maxPositions[min]]],"Payoff"->min[[maxPositions[min]]][[1]]|>,
	 {player,Length[matr],Length[matr],-1}
	]
]


StackelbergEquilibria[matr_]:=
With[{dim=Dimensions[matr[[1]]]},       (*  matr\[LeftDoubleBracket]1\[RightDoubleBracket] has the same dimensions as all other m\[LeftDoubleBracket]2\[RightDoubleBracket],..., m\[LeftDoubleBracket]n\[RightDoubleBracket]  *)
	Do[ 
	    Print["Player ",player],
	    min=Table[0,dim[[player]]];
	    Do[
	       ind=Table[Range[dim[[j]]],{j,Length[matr]}];
	       ind[[player]]={str};
	         tuples=Tuples[ind];
	         Print["For the fixed strategy ",str," the tuples are: \n",tuples];
	            payoffTuples=Table[matr[[player,Sequence@@t]],{t,tuples}];
	            Print[payoffTuples];
	            min[[str]]=Min[payoffTuples];
	            Print["For the strategy ",str," worst payoff value is: ",min[[str]]],
	       {str,dim[[player]]}
	      ];
	      Print["Player's ",player," MaxMin strategy -> ",maxPositions[min]," Guaranteed payoff value -> ",min[[maxPositions[min]]][[1]]],
	    {player,Length[matr]}
	  ]
]


(* ------ 2x2 Mixed Strategy Game - Graphs Intersection Metod ------ *)

g1[\[Alpha]_,\[Alpha]0_]:=\[Piecewise]{
 {Rectangle[{0,0},{1,1}], \[Alpha]==0&&\[Alpha]0==0},
 {Line[{{1,0},{1,1}}], (\[Alpha]>=0&&\[Alpha]0>0)||
  (\[Alpha]<0&&\[Alpha]+\[Alpha]0>0)},
 {Line[{{0,0},{0,1}}], (\[Alpha]<=0 &&\[Alpha]0<0)||
  (\[Alpha]>0&&\[Alpha]+\[Alpha]0<0)},
 {Line[{{0,0},{1,0},{1,1}}], \[Alpha]>0&&\[Alpha]0==0},
 {Line[{{0,0},{0,-(\[Alpha]0/\[Alpha])},{1,-(\[Alpha]0/\[Alpha])},{1,1}}], \[Alpha]>0&&\[Alpha]0<0&&
  \[Alpha]+\[Alpha]0>0},
 {Line[{{0,0},{0,1},{1,1}}], \[Alpha]>0&&\[Alpha]+\[Alpha]0==0},
 {Line[{{1,0},{0,0},{0,1}}], \[Alpha]<0&&\[Alpha]0==0},
 {Line[{{1,0},{1,-(\[Alpha]0/\[Alpha])},{0,-(\[Alpha]0/\[Alpha])},{0,1}}], \[Alpha]<0&&\[Alpha]0>0&&
  \[Alpha]+\[Alpha]0<0},
 {Line[{{1,0},{1,1},{0,1}}], \[Alpha]<0&&\[Alpha]+\[Alpha]0==0}
}
g2[\[Beta]_,\[Beta]0_]:=\[Piecewise]{
 {Rectangle[{0,0},{1,1}], \[Beta]==0&&\[Beta]0==0},
 {Line[{{0,1},{1,1}}], (\[Beta]>=0&&\[Beta]0>0)||
  (\[Beta]<0&&\[Beta]+\[Beta]0>0)},
 {Line[{{0,0},{1,0}}], (\[Beta]<=0&&\[Beta]0<0)||
  (\[Beta]>0&&\[Beta]+\[Beta]0<0)},
 {Line[{{0,0},{0,1},{1,1}}], \[Beta]>0&&\[Beta]0==0},
 {Line[{{0,0},{-(\[Beta]0/\[Beta]),0},{-(\[Beta]0/\[Beta]),1},{1,1}}], \[Beta]>0&&\[Beta]0<0&&
  \[Beta]+\[Beta]0>0},
 {Line[{{0,0},{1,0},{1,1}}], \[Beta]>0&&\[Beta]+\[Beta]0==0},
 {Line[{{0,1},{0,0},{1,0}}], \[Beta]<0&&\[Beta]0==0},
 {Line[{{0,1},{-(\[Beta]0/\[Beta]),1},{-(\[Beta]0/\[Beta]),0},{1,0}}], \[Beta]<0&&\[Beta]0>0&&
  \[Beta]+\[Beta]0<0},
 {Line[{{0,1},{1,1},{1,0}}], \[Beta]<0&&\[Beta]+\[Beta]0==0}
}
nes[\[Alpha]_,\[Alpha]0_,\[Beta]_,\[Beta]0_]:=\[Piecewise]{
 {{Point[{{0,0},{1,0},{1,1},{0,1}}],
  Rectangle[{0,0},{1,1}]}, (\[Alpha]==0&&\[Alpha]0==0)&&(\[Beta]==0&&\[Beta]0==0)(* 1 *)},
 {{Point[{{0,1},{1,1}}],
  Line[{{0,1},{1,1}}]}, ((\[Alpha]==0&&\[Alpha]0==0)&&((\[Beta]>=0&&\[Beta]0>0)||(\[Beta]<0&&\[Beta]+\[Beta]0>0)))||((\[Alpha]>0&&\[Alpha]+\[Alpha]0==0)&&((\[Beta]>=0&&\[Beta]0>0)||(\[Beta]<0&&\[Beta]+\[Beta]0>0)))||
  ((\[Alpha]>0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]+\[Beta]0==0))||
  ((\[Alpha]<0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0==0))||
  ((\[Alpha]<0&&\[Alpha]+\[Alpha]0==0)&&((\[Beta]>=0&&\[Beta]0>0)||(\[Beta]<0&&\[Beta]+\[Beta]0>0))) (* 2 *)},
 {{Point[{{0,0},{1,0}}],
  Line[{{0,0},{1,0}}]}, ((\[Alpha]==0&&\[Alpha]0==0)&&((\[Beta]<=0&&\[Beta]0<0)||(\[Beta]>0&&\[Beta]+\[Beta]0<0)))||((\[Alpha]>0&&\[Alpha]0==0)&&((\[Beta]<=0&&\[Beta]0<0)||(\[Beta]>0&&\[Beta]+\[Beta]0<0)))||
  ((\[Alpha]>0&&\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0==0))||
  ((\[Alpha]<0&&\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]+\[Beta]0==0))||
  ((\[Alpha]<0&&\[Alpha]0==0)&&((\[Beta]<=0&&\[Beta]0<0)||(\[Beta]>0&&\[Beta]+\[Beta]0<0))) (* 3 *)},
 {{Point[{{0,0},{0,1},{1,1}}],
  Line[{{0,0},{0,1},{1,1}}]}, ((\[Alpha]==0&&\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0==0))||
  ((\[Alpha]>0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]==0&&\[Beta]0==0))||
  ((\[Alpha]>0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0==0))  (* 4 *)},
 {{Point[g2[\[Beta],\[Beta]0][[1]]],g2[\[Beta],\[Beta]0]}, ((\[Alpha]==0&&\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0<0&&\[Beta]+\[Beta]0>0))||((\[Alpha]==0&&\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0>0&&\[Beta]+\[Beta]0<0)) (* 5 *)},
 {{Point[{{0,0},{1,0},{1,1}}],
  Line[{{0,0},{1,0},{1,1}}]}, ((\[Alpha]==0&&\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]+\[Beta]0==0))||((\[Alpha]>0&&\[Alpha]0==0)&&(\[Beta]==0&&\[Beta]0==0))||((\[Alpha]>0&&\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]+\[Beta]0==0)) (* 6*)},
 {{Point[{{0,1},{0,0},{1,0}}],
  Line[{{0,1},{0,0},{1,0}}]}, ((\[Alpha]==0&&\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0==0))||((\[Alpha]<0&&\[Alpha]0==0)&&(\[Beta]==0&&\[Beta]0==0))||((\[Alpha]<0&&\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0==0)) (* 7*)},
 {{Point[{{0,1},{1,1},{1,0}}],
  Line[{{0,1},{1,1},{1,0}}]}, ((\[Alpha]==0&&\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]+\[Beta]0==0))||((\[Alpha]<0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]==0&&\[Beta]0==0))||((\[Alpha]<0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]+\[Beta]0==0)) (* 8 *)},
 {{Point[{{1,0},{1,1}}],
  Line[{{1,0},{1,1}}]}, (((\[Alpha]>=0&&\[Alpha]0>0)||(\[Alpha]<0&&\[Alpha]+\[Alpha]0>0))&&(\[Beta]==0&&\[Beta]0==0))||
  (((\[Alpha]>=0&&\[Alpha]0>0)||(\[Alpha]<0&&\[Alpha]+\[Alpha]0>0))&&(\[Beta]>0&&\[Beta]+\[Beta]0==0))||(((\[Alpha]>=0&&\[Alpha]0>0)||(\[Alpha]<0&&\[Alpha]+\[Alpha]0>0))&&(\[Beta]<0&&\[Beta]+\[Beta]0==0))||((\[Alpha]>0&&\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]+\[Beta]0==0))||((\[Alpha]<0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]+\[Beta]0==0)) (* 9 *)},
 {{Point[{1,1}]}, (((\[Alpha]>=0&&\[Alpha]0>0)||(\[Alpha]<0&&\[Alpha]+\[Alpha]0>0))&&((\[Beta]>=0&&\[Beta]0>0)||(\[Beta]<0&&\[Beta]+\[Beta]0>0)))||
  (((\[Alpha]>=0&&\[Alpha]0>0)||(\[Alpha]<0&&\[Alpha]+\[Alpha]0>0))&&(\[Beta]>0&&\[Beta]0==0))||(((\[Alpha]>=0&&\[Alpha]0>0)||(\[Alpha]<0&&\[Alpha]+\[Alpha]0>0))&&(\[Beta]>0&&\[Beta]0<0&&\[Beta]+\[Beta]0>0))||((\[Alpha]>0&&\[Alpha]0==0)&&((\[Beta]>=0&&\[Beta]0>0)||(\[Beta]<0&&\[Beta]+\[Beta]0>0)))||((\[Alpha]>0&&\[Alpha]0<0&&\[Alpha]+\[Alpha]0>0)&&((\[Beta]>=0&&\[Beta]0>0)||(\[Beta]<0&&\[Beta]+\[Beta]0>0))) (* 10 *)},
 {{Point[{1,0}]}, (((\[Alpha]>=0&&\[Alpha]0>0)||(\[Alpha]<0&&\[Alpha]+\[Alpha]0>0))&&((\[Beta]<=0&&\[Beta]0<0)||(\[Beta]>0&&\[Beta]+\[Beta]0<0)))||
  (((\[Alpha]>=0&&\[Alpha]0>0)||(\[Alpha]<0&&\[Alpha]+\[Alpha]0>0))&&(\[Beta]<0&&\[Beta]0==0))||(((\[Alpha]>=0&&\[Alpha]0>0)||(\[Alpha]<0&&\[Alpha]+\[Alpha]0>0))&&(\[Beta]<0&&\[Beta]0>0&&\[Beta]+\[Beta]0<0))||((\[Alpha]<0&&\[Alpha]+\[Alpha]0==0)&&((\[Beta]<=0&&\[Beta]0<0)||(\[Beta]>0&&\[Beta]+\[Beta]0<0)))||((\[Alpha]<0&&\[Alpha]0>0&&\[Alpha]+\[Alpha]0<0)&&((\[Beta]<=0&&\[Beta]0<0)||(\[Beta]>0&&\[Beta]+\[Beta]0<0))) (* 11 *)},
 {{Point[{{0,0},{0,1}}],
  Line[{{0,0},{0,1}}]}, (((\[Alpha]<=0&&\[Alpha]0<0)||(\[Alpha]>0&&\[Alpha]+\[Alpha]0<0))&&(\[Beta]==0&&\[Beta]0==0))||
  (((\[Alpha]<=0&&\[Alpha]0<0)||(\[Alpha]>0&&\[Alpha]+\[Alpha]0<0))&&(\[Beta]>0&&\[Beta]0==0))||(((\[Alpha]<=0&&\[Alpha]0<0)||(\[Alpha]>0&&\[Alpha]+\[Alpha]0<0))&&(\[Beta]<0&&\[Beta]0==0))||((\[Alpha]>0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0==0))||((\[Alpha]<0&&\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0==0)) (* 12 *)},
 {{Point[{0,1}]}, (((\[Alpha]<=0&&\[Alpha]0<0)||(\[Alpha]>0&&\[Alpha]+\[Alpha]0<0))&&((\[Beta]>=0&&\[Beta]0>0)||(\[Beta]<0&&\[Beta]+\[Beta]0>0)))||
  (((\[Alpha]<=0&&\[Alpha]0<0)||(\[Alpha]>0&&\[Alpha]+\[Alpha]0<0))&&(\[Beta]<0&&\[Beta]0>0&&\[Beta]+\[Beta]0<0))||(((\[Alpha]<=0&&\[Alpha]0<0)||(\[Alpha]>0&&\[Alpha]+\[Alpha]0<0))&&(\[Beta]<0&&\[Beta]+\[Beta]0==0))||((\[Alpha]<0&&\[Alpha]0==0)&&((\[Beta]>=0&&\[Beta]0>0)||(\[Beta]<0&&\[Beta]+\[Beta]0>0)))||((\[Alpha]<0&&\[Alpha]0>0&&\[Alpha]+\[Alpha]0<0)&&((\[Beta]>=0&&\[Beta]0>0)||(\[Beta]<0&&\[Beta]+\[Beta]0>0))) (* 13 *)},
 {{Point[{0,0}]}, (((\[Alpha]<=0&&\[Alpha]0<0)||(\[Alpha]>0&&\[Alpha]+\[Alpha]0<0))&&(\[Beta]>0&&\[Beta]+\[Beta]0==0))||
  (((\[Alpha]<=0&&\[Alpha]0<0)||(\[Alpha]>0&&\[Alpha]+\[Alpha]0<0))&&((\[Beta]<=0&&\[Beta]0<0)||(\[Beta]>0&&\[Beta]+\[Beta]0<0)))||(((\[Alpha]<=0&&\[Alpha]0<0)||(\[Alpha]>0&&\[Alpha]+\[Alpha]0<0))&&(\[Beta]>0&&\[Beta]0<0&&\[Beta]+\[Beta]0>0))||((\[Alpha]>0&&\[Alpha]0<0&&\[Alpha]+\[Alpha]0>0)&&((\[Beta]<=0&&\[Beta]0<0)||(\[Beta]>0&&\[Beta]+\[Beta]0<0)))||((\[Alpha]>0&&\[Alpha]+\[Alpha]0==0)&&((\[Beta]<=0&&\[Beta]0<0)||(\[Beta]>0&&\[Beta]+\[Beta]0<0))) (* 14 *)},
 {{Point[{{0,0},{1,1}}]}, ((\[Alpha]>0&&\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0==0))||((\[Alpha]>0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]+\[Beta]0==0)) (* 15 *)},
 {{Point[{{0,0},{-(\[Beta]0/\[Beta]),0},{1,1}}],Line[{{0,0},{-(\[Beta]0/\[Beta]),0}}]}, (\[Alpha]>0&&\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0<0&&\[Beta]+\[Beta]0>0) (* 16*)},
 {{Point[{{-(\[Beta]0/\[Beta]),0},{1,0}}],Line[{{-(\[Beta]0/\[Beta]),0},{1,0}}]}, (\[Alpha]>0&&\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0>0&&\[Beta]+\[Beta]0<0) (* 17 *)},
 {{Point[g1[\[Alpha],\[Alpha]0][[1]]],g1[\[Alpha],\[Alpha]0]}, ((\[Alpha]>0&&\[Alpha]0<0&&\[Alpha]+\[Alpha]0>0)&&(\[Beta]==0&&\[Beta]0==0))||((\[Alpha]<0&&\[Alpha]0>0&&\[Alpha]+\[Alpha]0<0)&&(\[Beta]==0&&\[Beta]0==0)) (* 18 *)},
 {{Point[{{0,0},{0,-(\[Alpha]0/\[Alpha])},{1,1}}],Line[{{0,0},{0,-(\[Alpha]0/\[Alpha])}}]}, (\[Alpha]>0&&\[Alpha]0<0&&\[Alpha]+\[Alpha]0>0)&&(\[Beta]>0&&\[Beta]0==0) (* 19 *)},
 {{Point[{{0,0},{-(\[Beta]0/\[Beta]),-(\[Alpha]0/\[Alpha])},{1,1}}]}, (\[Alpha]>0&&\[Alpha]0<0&&\[Alpha]+\[Alpha]0>0)&&(\[Beta]>0&&\[Beta]0<0&& \[Beta]+\[Beta]0>0) (* 20 *)},
 {{Point[{{0,0},{1,-(\[Alpha]0/\[Alpha])},{1,1}}],Line[{{1,-(\[Alpha]0/\[Alpha])},{1,1}}]}, (\[Alpha]>0&&\[Alpha]0<0&&\[Alpha]+\[Alpha]0>0)&&(\[Beta]>0&& \[Beta]+\[Beta]0==0) (* 21 *)},
 {{Point[{{0,0},{0,-(\[Alpha]0/\[Alpha])}}],Line[{{0,0},{0,-(\[Alpha]0/\[Alpha])}}]}, (\[Alpha]>0&&\[Alpha]0<0&&\[Alpha]+\[Alpha]0>0)&&(\[Beta]<0&&\[Beta]0==0) (* 22 *)},
 {{Point[{{-(\[Beta]0/\[Beta]),-(\[Alpha]0/\[Alpha])}}]}, ((\[Alpha]>0&&\[Alpha]0<0&&\[Alpha]+\[Alpha]0>0)&&(\[Beta]<0&&\[Beta]0>0&&\[Beta]+\[Beta]0<0))||((\[Alpha]<0&&\[Alpha]0>0&&\[Alpha]+\[Alpha]0<0)&&(\[Beta]>0&&\[Beta]0<0&&\[Beta]+\[Beta]0>0))(*23*)},
 {{Point[{{1,-(\[Alpha]0/\[Alpha])},{1,1}}],Line[{{1,-(\[Alpha]0/\[Alpha])},{1,1}}]}, (\[Alpha]>0&&\[Alpha]0<0&&\[Alpha]+\[Alpha]0>0)&&(\[Beta]<0&&\[Beta]+\[Beta]0==0) (* 24 *)},
 {{Point[{{0,0},{-(\[Beta]0/\[Beta]),1},{1,1}}],Line[{{-(\[Beta]0/\[Beta]),1},{1,1}}]}, (\[Alpha]>0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0<0&&\[Beta]+\[Beta]0>0)(* 25 *)},
 {{Point[{{0,1},{-(\[Beta]0/\[Beta]),1}}],Line[{{0,1},{-(\[Beta]0/\[Beta]),1}}]}, (\[Alpha]>0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0>0&&\[Beta]+\[Beta]0<0) (* 26 *)},
 {{Point[{{0,0},{-(\[Beta]0/\[Beta]),0}}],Line[{{0,0},{-(\[Beta]0/\[Beta]),0}}]}, (\[Alpha]<0&&\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0<0&&\[Beta]+\[Beta]0>0) (* 27 *)},
 {{Point[{{-(\[Beta]0/\[Beta]),0}, {1,0},{0,1}}],Line[{{-(\[Beta]0/\[Beta]),0}, {1,0}}]}, (\[Alpha]<0&&\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0>0&&\[Beta]+\[Beta]0<0) (* 28 *)},
 {{Point[{{1,0},{0,1}}]}, ((\[Alpha]<0&&\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]+\[Beta]0==0))||
  ((\[Alpha]<0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0==0))  (* 29 *)},
 {{Point[{{0,-(\[Alpha]0/\[Alpha])},{0,1}}],Line[{{0,-(\[Alpha]0/\[Alpha])},{0,1}}]}, (\[Alpha]<0&&\[Alpha]0>0&&\[Alpha]+\[Alpha]0<0)&&(\[Beta]>0&&\[Beta]0==0) (* 30 *)},
 {{Point[{{1,-(\[Alpha]0/\[Alpha])},{1,0}}],Line[{{1,-(\[Alpha]0/\[Alpha])},{1,0}}]}, (\[Alpha]<0&&\[Alpha]0>0&&\[Alpha]+\[Alpha]0<0)&&(\[Beta]>0&&\[Beta]+\[Beta]0==0) (* 31 *)},
 {{Point[{{0,-(\[Alpha]0/\[Alpha])},{0,1},{1,0} }],Line[{{0,-(\[Alpha]0/\[Alpha])},{0,1}}]}, (\[Alpha]<0&&\[Alpha]0>0&&\[Alpha]+\[Alpha]0<0)&&(\[Beta]<0&&\[Beta]0==0) (* 32 *)},
 {{Point[{{0,1},{-(\[Beta]0/\[Beta]),-(\[Alpha]0/\[Alpha])},{1,0}}]}, (\[Alpha]<0&&\[Alpha]0>0&&\[Alpha]+\[Alpha]0<0)&&(\[Beta]<0&&\[Beta]0>0&&\[Beta]+\[Beta]0<0) (* 33 *)},
 {{Point[{{1,-(\[Alpha]0/\[Alpha])},{1,0},{0,1}}],Line[{{1,-(\[Alpha]0/\[Alpha])},{1,0}}]}, (\[Alpha]<0&&\[Alpha]0>0&&\[Alpha]+\[Alpha]0<0)&&(\[Beta]<0&&\[Beta]+\[Beta]0==0) (* 34 *)},
 {{Point[{{-(\[Beta]0/\[Beta]),1},{1,1}}],Line[{{-(\[Beta]0/\[Beta]),1},{1,1}}]}, (\[Alpha]<0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]>0&&\[Beta]0<0&&\[Beta]+\[Beta]0>0) (* 35 *)},
 {{Point[{{0,1},{-(\[Beta]0/\[Beta]),1},{1,0}}],Line[{{0,1},{-(\[Beta]0/\[Beta]),1}}]}, (\[Alpha]<0&&\[Alpha]+\[Alpha]0==0)&&(\[Beta]<0&&\[Beta]0>0&&\[Beta]+\[Beta]0<0) (* 36 *)}
}
game2x2Plot[m_]:=Module[{matr=m},
				 {{{\[DoubleStruckA]11,\[DoubleStruckA]12},{\[DoubleStruckA]21,\[DoubleStruckA]22}},{{\[DoubleStruckB]11,\[DoubleStruckB]12},{\[DoubleStruckB]21,\[DoubleStruckB]22}}}=matr;
			    Manipulate[
					Grid[{{Graphics[{Thick,
						Blue,g1[a11-a12-a21+a22,a12-a22],
						Green,g2[b11-b12-b21+b22,b21-b22],
						Red,PointSize[Large],nes[a11-a12-a21+a22,a12-a22,b11-b12-b21+b22,b21-b22]},
						PlotRange->{{0,1},{0,1}},Axes->True,AxesLabel->{"\!\(\*SubscriptBox[\(x\), \(1\)]\)","\!\(\*SubscriptBox[\(y\), \(1\)]\)"},
						ImageSize->{300,300}]},{" "},{Text@Style["Reference Nash Equilibria",Bold]},
						{Text@Style[nes[a11-a12-a21+a22,a12-a22,b11-b12-b21+b22,b21-b22][[1,1]],Bold]}},ItemSize->{Automatic,{10,1,1,3}},Alignment->{Center,Top}
					],
						Style["Matrix A",Bold],
						{{a11,\[DoubleStruckA]11,"\!\(\*SubscriptBox[\(a\), \(11\)]\)"},-10,10,1,Appearance-> "Labeled",ImageSize->Tiny},
						{{a12,\[DoubleStruckA]12,"\!\(\*SubscriptBox[\(a\), \(12\)]\)"},-10,10,1,Appearance-> "Labeled",ImageSize->Tiny},
						{{a21,\[DoubleStruckA]21,"\!\(\*SubscriptBox[\(a\), \(21\)]\)"},-10,10,1,Appearance-> "Labeled",ImageSize->Tiny},
						{{a22,\[DoubleStruckA]22,"\!\(\*SubscriptBox[\(a\), \(22\)]\)"},-10,10,1,Appearance-> "Labeled",ImageSize->Tiny},
						Delimiter,{{NonAntagonistic,True, "NonAntagonistic"},{True,False}},
						Delimiter,Style["Matrix B",Bold],
						{{b11,\[DoubleStruckB]11,"\!\(\*SubscriptBox[\(b\), \(11\)]\)"},-10,10,1,Enabled->NonAntagonistic,Appearance-> "Labeled",ImageSize->Tiny},
						{{b12,\[DoubleStruckB]12,"\!\(\*SubscriptBox[\(b\), \(12\)]\)"},-10,10,1,Enabled->NonAntagonistic,Appearance-> "Labeled",ImageSize->Tiny},
						{{b21,\[DoubleStruckB]21,"\!\(\*SubscriptBox[\(b\), \(21\)]\)"},-10,10,1,Enabled->NonAntagonistic,Appearance-> "Labeled",ImageSize->Tiny},
						{{b22,\[DoubleStruckB]22,"\!\(\*SubscriptBox[\(b\), \(22\)]\)"},-10,10,1,Enabled->NonAntagonistic,Appearance-> "Labeled",ImageSize->Tiny},
						Delimiter,
						Style["Matrices A and B",Bold],
					Dynamic[
						TableForm[
							{{ToString[a11]<>" , "<>ToString[If[NonAntagonistic,b11,b11=-a11]],
							ToString[a12]<>" , "<>ToString[If[NonAntagonistic,b12,b12=-a12]]},
							{ToString[a21]<>" , "<>ToString[If[NonAntagonistic,b21,b21=-a21]],
							ToString[a22]<>" , "<>ToString[If[NonAntagonistic,b22,b22=-a22]]}},
							TableHeadings->{{"1","2"},{"  1","  2"}},
						  TableSpacing->{2,2}
						]
					],
					SaveDefinitions->True
				]
]


End[];


EndPackage[];
