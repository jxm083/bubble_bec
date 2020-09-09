(* ::Package:: *)

(*Modified FitCurveHacked by using the geometric mean of the trapping frequencies for the fitting instead of the z-component. Look for instances of SingleTrapFrequency*)


(* ::Input::Initialization:: *)
Redistribute[value_,array_]:=Block[{
newarray,oldarray,pos,neg,posposition,negposition
},
newarray=#-value/Length[array]&/@array;
If[AllTrue[newarray,#>0&],
newarray,
oldarray=newarray;
pos=Select[oldarray,#>0&];
neg=Select[oldarray,#<=0&];
posposition=Flatten[Position[#>0&/@oldarray,True]];
negposition=Flatten[Position[#>0&/@oldarray,False]];
Redistribute[Total[neg],pos];
newarray[[posposition]]=Redistribute[-Total[neg],pos];
newarray[[negposition]]=Table[0,Length[negposition]];
newarray
]
]


(* ::Input::Initialization:: *)
Redistribute::usage="Redistribute[value_,array_] Takes aramp fraction value_ and evenly redistributes it over the ramp fractions array_. If any value of array_ goes negative, it is set to 0 and the ramp fraction that went to it is distributed over the remaining array elements, recursively.";


(* ::Input::Initialization:: *)
SetAttributes[BinaryFreqSearch,HoldAll];
BinaryFreqSearch[trap1_,trap2_,\[Omega]i_,\[Omega]f_,rampin_,nramps_:8,position_:1,fraction_:-3,round_:-5,modelForm_:"SackettTanh"]:=Module[{
newramp=rampin,
model,ramp,
lower=0,
upper=1,
roundTemp=round,
\[Delta],nr0,diff,head,foot,freqMean
},
If[modelForm=="SackettTanh",
model[t_]:=(\[Omega]i+\[Omega]f)/2+(\[Omega]f-\[Omega]i)/2 Tanh[5(2(t/2)^(1/4) -1)]/Tanh[5],
(* ELSE: *)
If[modelForm=="CorgierSin",
model[t_]:=Module[{tau=2*Pi*t},
\[Omega]i+((\[Omega]f-\[Omega]i)/(12*Pi))*(6*tau-8*Sin[tau]+Sin[2*tau])],
(* ELSE: *)
Print["The specified modelForm has not yet been implemented. Please choose either SackettTanh or CorgierSin."];
Return[];
]];

ramp:=Prepend[Table[{Sum[#[[1,i]],{i,n}],Sum[#[[2,i]],{i,n}],#[[2,n]]/#[[1,n]]},{n,nramps}]&[newramp],{0.,0.,0.}](*builds an array of (cumulative time, cumulative ramp fraction, and ramp slope)*);
\[Delta]:=GeometricMean[SingleTrapFrequency[trap1,trap2,newramp,nramps,position+1]]-model[ramp[[position+1,1]]];
freqMean:=\[Delta]+model[ramp[[position+1,1]]];
nr0=newramp[[2,position]];
upper=1-ramp[[position,2]];

While[Abs[\[Delta]]>10.^fraction*model[ramp[[position+1,1]]],
Print[{newramp[[2,position]],\[Delta],10.^fraction*model[ramp[[position+1,1]]],model[ramp[[position+1,1]]],freqMean,upper,lower}];
If[(upper-lower)<=10.^roundTemp,
Print["Fitting algorithm stalled. Increasing precision."];
roundTemp=roundTemp-1;
];
If[\[Delta]<0,
upper=newramp[[2,position]];
newramp[[2,position]]=Round[(upper+lower)/2,10.^roundTemp];,
(* ELSE: *)
lower=newramp[[2,position]];
newramp[[2,position]]=Round[(upper+lower)/2,10.^roundTemp];
]
];
diff=newramp[[2,position]]-nr0;
head=Take[newramp[[2]],position];
foot=Drop[newramp[[2]],position];
newramp[[2]]=Join[head,Redistribute[diff,foot]];
{newramp,\[Delta]}
]


(* ::Input::Initialization:: *)
BinaryFreqSearch::usage="BinaryFreqSearch[trap1_,trap2_,\[Omega]i_,\[Omega]f_,rampin_,nramps_:8,position_:1,fraction_:-3,round_:-5] takes an initial and final trap, initial and final trap fewquency, and ramp. The number of ramps, ramp position, and log10 of the final precision are all optional inputs. The output is a new ramp that has the appropriate ramp fraction to match the internal analytic model to within the specified precision (given by (10^fraction)*model[t]). It also includes the residual, with the output in the form {newramp, \[Delta]}.";


(* ::Input::Initialization:: *)
FindTrapEndpoints[trap1_,trap2_]:=Module[{
startpts,\[Omega]i,\[Omega]f
},
startpts=Table[SingleTrapFrequency[trap1,trap2,{{1},{1}},1,n],{n,2}];
\[Omega]i=GeometricMean[startpts[[1,All]]];
\[Omega]f=GeometricMean[startpts[[2,All]]];
{\[Omega]i,\[Omega]f}
]


(* ::Input::Initialization:: *)
FindTrapEndpoints::usage="FindTrapEndpoints[trap1_,trap2_] finds the initial and final values of the trap z frequency.";


(* ::Input::Initialization:: *)
GuessTimes[trap1_,trap2_,nramps_:8,round_:10^-4,last_:0.3,recursion_:0,freq_:0]:=Block[{
model,ramptimes,\[Omega]i,\[Omega]f,\[CapitalDelta]\[Omega],\[Omega]fnew,\[Omega]fstep,
ramps=nramps,
head
},
model[\[Omega]i_,\[Omega]f_,t_]:=(\[Omega]i+\[Omega]f)/2+(\[Omega]f-\[Omega]i)/2 Tanh[5(2(t/2)^(1/4) -1)]/Tanh[5];
{\[Omega]i,\[Omega]f}=FindTrapEndpoints[trap1,trap2];
\[Omega]fstep=\[Omega]f;
If[freq!=0,
\[Omega]fstep=freq
];
(*Print[{\[Omega]fstep,recursion,freq}];*)
\[CapitalDelta]\[Omega]=Log[\[Omega]fstep/\[Omega]i]/ramps;
ramptimes=Differences[Union[Round[Table[Solve[model[\[Omega]i,\[Omega]f,t]==\[Omega]i Exp[n \[CapitalDelta]\[Omega]],t][[1,1,2]],{n,nramps-1}],1.*round],{0.,1.*(1-last)^recursion}]](*this calculates equally spaced points, logarithmically, in the trap frequency, solves for the ramp time, then finds the differences to use as a ramp input*);
If[Last[ramptimes]>last,
\[Omega]fnew=model[\[Omega]i,\[Omega]f,(1-last)^(recursion+1)];
head=GuessTimes[trap1,trap2,ramps-1,round,last,recursion+1,\[Omega]fnew];
ramptimes=Join[head,{last*(1-last)^recursion}]
];
ramptimes
]


(* ::Input::Initialization:: *)
GuessTimes::usage="GuessTimes[trap1_,trap2_,nramps_:8,round_:\!\(\*SuperscriptBox[\(10\), \(-4\)]\),last_:0.3] makes a guess at what would be good time steps for a decompression ramp. The guess is times that create equally-spaced changes in frequency on a log scale over as many points as the user chooses. Generally, this doesn't seem to provide enough resolution for the ramp tail, so there is an option, 'last', to set the maximum size of the last time step. The default value for that is 0.3, which seems to work decently. Setting it to 1 will cause an error, but 0.99 is effectively turning this option off.";


(* ::Input::Initialization:: *)
FitCurveHacked[trap1_,trap2_,rampin_,nramps_:8,fraction_:-3,round_:-5,modelForm_:"SackettTanh"]:=Module[{
points=nramps,
ramp=rampin,
newramp,
position=1,
\[Omega]i,\[Omega]f,
pointTime,
totalTime=0.
},
{\[Omega]i,\[Omega]f}=FindTrapEndpoints[trap1,trap2];
(*\[Omega]f=0.9899925293586265*\[Omega]f;*)
While[position<points,
{pointTime,newramp}=Timing[BinaryFreqSearch[trap1,trap2,\[Omega]i,\[Omega]f,ramp,points,position,fraction,round,modelForm][[1]]];
totalTime+=pointTime;
Print["Completed point "<>ToString[position]<>" of "<>ToString[points-1]<>" in "<>ToString[pointTime]<>" s (Total time: "<>ToString[totalTime/60]<>" min)."];
position++;
ramp=newramp
];
ramp
]


(* ::Input::Initialization:: *)
FitCurve::usage="FitCurve[trap1_,trap2_,rampin_,nramps_:8,fraction_:-3,round_:-5] is a wrapper to loop BinaryFreqSearch over the points of a ramp sequentially. When it's done, it outputs a new ramp that matches the analytic curve in BinaryFreqSearch to 10^digits Hz in trap z frequency.";


(* ::Input::Initialization:: *)
(* Begin by defining the optional arguments (and their default values) for ExpansionPlotHacked that will be given as substitution rules. *)
Options[ExpansionPlotHacked]={axis->"z",trap->"1",usemodel->False,frequency->True,trapposition->True,gridlines->False,size->Medium,meanfrequency->False,meanmodel->False};
(* Then protect the names of these optional arguments so that they are not overwritten in whatever notebook or program calls uses this function. *)
Protect@Evaluate@(Options[ExpansionPlotHacked][[All,1]]);
(* Print a list of these variables when this package is run as a warning to the user. *)
Print["The following variables used by ExpansionPlotHacked are protected by this package: "<>ToString[Options[ExpansionPlotHacked][[All,1]]]];

ExpansionPlotHacked[movingtrap_,npoints_,ramp_:{{},{}},OptionsPattern[]]:=Block[{
dimension={OptionValue[axis]},
trapnumber=OptionValue[trap],
position,freq,model,meanFreq,meanFreqs,timeDomain,freqs,freqif,posif,gridif,modelif,meanfreqif,meanmodelif,
freqListLogPlot
},
model[\[Omega]i_,\[Omega]f_,t_]:=(\[Omega]i+\[Omega]f)/2+(\[Omega]f-\[Omega]i)/2 Tanh[5(2(t/2)^(1/4)-1)]/Tanh[5];
If[OptionValue[axis]=="x",
	dimension=Prepend[dimension,1],
	If[OptionValue[axis]=="y",
		dimension=Prepend[dimension,2],
		If[OptionValue[axis]=="z",
			dimension=Prepend[dimension,3];
		];
	];
];
If[OptionValue[trapposition],
freqif={Frame->{{False,True},{False,False}},FrameStyle-> {{Automatic,Red},{Automatic,Automatic}},FrameTicks->{{None,All},{None,None}}},
freqif=Frame->True
];(*sets options for frequency plot if not plotting position*)
If[OptionValue[frequency],
posif={Frame->{{True,False},{True,True}},FrameStyle-> {{Blue,Automatic},{Automatic,Automatic}}},
posif=Frame->True
];(*sets options for position plot if not plotting frequency*)
If[OptionValue[gridlines],
gridif=GridLines->{Table[Total[Take[#,n]],{n,Length[#]}],{}}&[ramp[[1]]],
gridif=GridLines->None
];(*sets options for gridlines*)
If[OptionValue[usemodel],
modelif=LogPlot[model[First[#],Last[#],t],{t,0.,1.},PlotRange->All,PlotStyle->Black]&[movingtrap[[All,2,dimension[[1]]]]],
modelif={}
];(*defines the plot of the model, and a case structure*)
If[OptionValue[meanfrequency],
meanfreqif={},
meanfreqif={}
];(*defines the plot of the geometric mean of the frequency.*)
position=ListPlot[{Table[((n-1)/npoints),{n,npoints+1}],movingtrap[[All,1,dimension[[1]]]]*10^6}\[Transpose],ImagePadding->40,posif,FrameLabel->{{StringJoin[dimension[[2]]," Trap Minimum (\[Mu]m)"],""},{"Ramp Time Fraction",StringJoin["Trap ",trapnumber," Expansion"]}},PlotRange->All,PlotStyle-> Blue,gridif,ImageSize->OptionValue[size]];

freqListLogPlot=ListLogPlot[{Table[((n-1)/npoints),{n,npoints+1}],movingtrap[[All,2,dimension[[1]]]]}\[Transpose],ImagePadding->40,freqif,FrameLabel->{{"",StringJoin[dimension[[2]],"-axis frequency (Hz)"]},{"Ramp Time Fraction",StringJoin["Trap ",trapnumber," Expansion"]}},PlotRange->All,PlotStyle-> Red,gridif];

freq=Show[freqListLogPlot,modelif,ImageSize->OptionValue[size]];

Print[Last@@AbsoluteOptions[freqListLogPlot,PlotRange]];
Print[E^(Last@@AbsoluteOptions[freqListLogPlot,PlotRange])[[2]]];


timeDomain=Table[((n-1)/npoints),{n,npoints+1}];
freqs=movingtrap[[All,2,All]];
meanFreqs=Table[GeometricMean[freqs[[n]]],{n,Length@freqs}];

If[OptionValue[meanmodel],
meanmodelif=LogPlot[model[First[#],Last[#],t],{t,0.,1.},PlotRange->All,PlotStyle->{Dashed,Black}]&[meanFreqs],
meanmodelif={}
];

meanFreq=Show[
ListLogPlot[Transpose@{timeDomain,meanFreqs},ImagePadding->40,PlotRange->All,gridif,freqif],
meanmodelif,
ImageSize->OptionValue[size]];

If[OptionValue[frequency]&&OptionValue[trapposition]&&OptionValue[meanfrequency],
	Overlay[{position,freq,meanFreq}],
	If[OptionValue[frequency]&&OptionValue[trapposition],
	Overlay[{position,freq}],
	If[OptionValue[frequency],
			freq,
			If[OptionValue[trapposition],
				position,
If[OptionValue[meanfrequency],
meanFreq,Print["Pick at least one of trap, frequency, or mean frequency."]
]
			]
		]
	]
]
]


plotFreqError[movingtrap_]:=Module[
{npoints,tdomain,freqs,model,freqsModel,
absError,absErrorPlot,
relError,relErrorPlot},
npoints=Length@movingtrap;

tdomain=Table[(n-1)/npoints,{n,npoints}];
freqs=movingtrap[[All,2,3]];

model[\[Omega]i_,\[Omega]f_,t_]:=(\[Omega]i+\[Omega]f)/2+(\[Omega]f-\[Omega]i)/2 Tanh[5(2(t/2)^(1/4)-1)]/Tanh[5];
freqsModel=Table[model[First[freqs],Last[freqs],t],{t,tdomain}];

(*absError=freqs-freqsModel;
absErrorPlot=ListPlot[{tdomain\[LeftDoubleBracket]#\[RightDoubleBracket],absError\[LeftDoubleBracket]#\[RightDoubleBracket]}&/@Range[npoints]];*)

relError=Table[(freqs[[n]]-freqsModel[[n]])/freqs[[n]],{n,npoints}];
relErrorPlot=ListPlot[{tdomain[[#]],relError[[#]]}&/@Range[npoints],PlotRange->All,PlotLabel->"Relative frequency error (\!\(\*SubscriptBox[\(\[Omega]\), \(ramp\)]\)-\!\(\*SubscriptBox[\(\[Omega]\), \(model\)]\))/\!\(\*SubscriptBox[\(\[Omega]\), \(ramp\)]\),
max abs. val. = "<>ToString[Max[Abs[relError]]]];

Show[relErrorPlot]
]


plotFreqError::usage=
"Takes a movingtrap object---the object returned by MovingTrap: 
{{{x1,y1,z1},{\[Omega]11,\[Omega]21,\[Omega]31},BMin1},{{x2,y2,z2},{\[Omega]12,\[Omega]22,\[Omega]32},BMin2},...}---
and plots the relative frequency error between the frequencies of the
calculated ramp and those of the ideal analytic ramp at each point in the ramp.";
