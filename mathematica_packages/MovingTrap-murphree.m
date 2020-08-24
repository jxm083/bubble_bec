(* ::Package:: *)

(* ::Input::Initialization:: *)
MovingTrap[trap1_,trap2_,rampin_,npoints_,nramps_:3]:=Block[{
deltatrap=#1-#2&[trap1,trap2],
ramp=Prepend[Table[{Sum[#[[1,i]],{i,n}],Sum[#[[2,i]],{i,n}],#[[2,n]]/#[[1,n]]},{n,nramps}]&[rampin],{0,0,0}],(*builds an array of (cumulative time, cumulative ramp fraction, and ramp slope)*)
ramplist,coordinates,joinedlist,movingBMin,freqlist
},
If[Total[rampin[[1]]]==1&&Total[rampin[[2]]]==1,(*the enclosing If checks that the ramp values in the time array and value array each sum to 1, and gives specific warnings if they do not*)
ramplist=Prepend[Drop[Table[{t,Piecewise[Table[{#3-(#1[[n-1,2]]+#1[[n,3]]*(t-#1[[n-1,1]]))*#2,#1[[n-1,1]]<t<=#1[[n,1]]},{n,2,nramps+1}]&[ramp,deltatrap,trap1]]},{t,0.,1.,1./npoints}],1],{0.,trap1}];(*generates the piecewise ramp and finds the values of each parameter at every point*)
coordinates=z0[#[[2,1]],#[[2,2]],#[[2,3]],#[[2,4]],#[[2,5]],#[[2,6]]][[All,2]]&/@ramplist;
joinedlist=Join[{coordinates},{ramplist[[All,2]]}]\[Transpose];
movingBMin=ChipTrapField[#[[1,1]],#[[1,2]],#[[1,3]],#[[2,1]],#[[2,2]],#[[2,3]],#[[2,4]],#[[2,5]],#[[2,6]]]&/@joinedlist;
freqlist=ChipTrapFrequencies[#[[2,1]],#[[2,2]],#[[2,3]],#[[2,4]],#[[2,5]],#[[2,6]]][[4]]&/@ramplist;
{coordinates,freqlist,movingBMin}\[Transpose],
If[Total[rampin[[1]]]!=1&&Total[rampin[[2]]]==1,
	Print["Ramp time error. Please make the total time (rampin\[LeftDoubleBracket]1\[RightDoubleBracket]) sum to 1."],
	If[Total[rampin[[1]]]==1&&Total[rampin[[2]]]!=1,
		Print["Ramp fraction error. Please make the total ramp (rampin\[LeftDoubleBracket]2\[RightDoubleBracket]) sum to 1."],
		Print["Ramp error. Please make the total time (rampin\[LeftDoubleBracket]1\[RightDoubleBracket]) and ramp (rampin\[LeftDoubleBracket]2\[RightDoubleBracket]) sum to 1."]
	]
]
]
]


(* ::Input::Initialization:: *)
MovingTrap::usage=
"MovingTrap[trap1_,trap2_,rampin_,npoints_,nramps_(optional)] takes two trap configurations (trap1, trap2) in the form {Ilb,Izb,Ih,Bx1,By1,Bz1}, a ramp (rampin) in the form {{1st_time_fraction,2nd_time_fraction,3rd_time_fraction},{1st_ramp_fraction,2nd_ramp_fraction,3rd_ramp_fraction}} (e.g. {{.2,.6,.2},{0.1,0.8,0.1}}), and a number of sample points (npoints). A number of ramp segments (nramps) is optional and will default to 3 if no parameter is given. It will output an array of {{{x1,y1,z1},{\[Omega]11,\[Omega]21,\[Omega]31},BMin1},{{x2,y2,z2},{\[Omega]12,\[Omega]22,\[Omega]32},BMin2},...}";


(* ::Input::Initialization:: *)
SingleTrapFrequency[trap1_,trap2_,rampin_,nramps_:3,position_:1]:=Block[{
deltatrap=#1-#2&[trap1,trap2],
ramp=Prepend[Table[{Sum[#[[1,i]],{i,n}],Sum[#[[2,i]],{i,n}],#[[2,n]]/#[[1,n]]},{n,nramps}]&[rampin],{0.,0.,0.}],(*builds an array of (cumulative time, cumulative ramp fraction, and ramp slope)*)
ramplist,freq
},
ramplist=Append[Prepend[Table[{t,Piecewise[Table[{#3-(#1[[n-1,2]]+#1[[n,3]]*(t-#1[[n-1,1]]))*#2,#1[[n-1,1]]<t<=#1[[n,1]]},{n,2,nramps+1}]&[ramp,deltatrap,trap1]]},{t,#[[2;;Length[#]-1,1]]&[ramp]}],{0.,trap1}],{1.,trap2}];(*generates the piecewise ramp and finds the values of each parameter at every point, not including zero or since those are the input traps*)
freq=ChipTrapFrequencies[#[[2,1]],#[[2,2]],#[[2,3]],#[[2,4]],#[[2,5]],#[[2,6]]][[4]]&[ramplist[[position]]]
]


(* ::Input::Initialization:: *)
SingleTrapFrequency::usage=
"SingleTrapFrequency[trap1_,trap2_,rampin_,nramps_:3,position_:1] is much like MovingTrap, but only outputs the 3 trap frequencies at a point, chosen by position_.";


SingleTrapCharacteristics[trap1_,trap2_,rampin_,nramps_:3,position_:1]:=Block[{
deltatrap=#1-#2&[trap1,trap2],
ramp=Prepend[Table[{Sum[#[[1,i]],{i,n}],Sum[#[[2,i]],{i,n}],#[[2,n]]/#[[1,n]]},{n,nramps}]&[rampin],{0.,0.,0.}],(*builds an array of (cumulative time, cumulative ramp fraction, and ramp slope)*)
ramplist,freq
},
ramplist=Append[Prepend[Table[{t,Piecewise[Table[{#3-(#1[[n-1,2]]+#1[[n,3]]*(t-#1[[n-1,1]]))*#2,#1[[n-1,1]]<t<=#1[[n,1]]},{n,2,nramps+1}]&[ramp,deltatrap,trap1]]},{t,#[[2;;Length[#]-1,1]]&[ramp]}],{0.,trap1}],{1.,trap2}];(*generates the piecewise ramp and finds the values of each parameter at every point, not including zero or since those are the input traps*)
freq=ChipTrapFrequenciesRawPrincipleAxes[#[[2,1]],#[[2,2]],#[[2,3]],#[[2,4]],#[[2,5]],#[[2,6]]]&[ramplist[[position]]]
]


SingleTrapCharacteristics::usage=
"SingleTrapCharacteristics[trap1_,trap2_,rampin_,nramps_:3,position_:1] is
defined identically to SingleTrapFrequency except instead of returning just the frequencies
it returns the trap principle axis as well in the form
{{eVec1,\!\(\*SubscriptBox[\(\[Omega]\), \(1\)]\)},{eVec2,\!\(\*SubscriptBox[\(\[Omega]\), \(2\)]\)},{eVec3,\!\(\*SubscriptBox[\(\[Omega]\), \(3\)]\)}}.";


(* ::Input::Initialization:: *)
RampList[trap1_,trap2_,rampin_,npoints_,nramps_:3,OptionsPattern[full->True]]:=Block[{
deltatrap=#1-#2&[trap1,trap2],
ramp=Prepend[Table[{Sum[#[[1,i]],{i,n}],Sum[#[[2,i]],{i,n}],#[[2,n]]/#[[1,n]]},{n,nramps}]&[rampin],{0,0,0}],
ramplist,coordinates,joinedlist,movingBMin,freqlist
},
If[OptionValue[full],
ramplist=Prepend[Drop[Table[{t,Piecewise[Table[{#3-(#1[[n-1,2]]+#1[[n,3]]*(t-#1[[n-1,1]]))*#2,#1[[n-1,1]]<t<=#1[[n,1]]},{n,2,nramps+1}]&[ramp,deltatrap,trap1]]},{t,0.,1.,1./npoints}],1],{0.,trap1}],
ramplist=Prepend[Drop[Table[{t,Piecewise[Table[{#3-(#1[[n-1,2]]+#1[[n,3]]*(t-#1[[n-1,1]]))*#2,#1[[n-1,1]]<t<=#1[[n,1]]},{n,2,nramps+1}]&[ramp,deltatrap,trap1]]},{t,ramp[[All,1]]}],1],{0.,trap1}]
]
]


(* ::Input::Initialization:: *)
RampList::usage=
"RampList[trap1_,trap2_,rampin_,npoints_,nramps(optional),OptionsPattern[full\[Rule]True]] takes the same inputs as MovingTrap and provides the trap physical value parameters at each time step. Time is given as a fraction of the total ramp time. The output format is {{t1,{trap_params1}},{t2,{trap_params2},...}. If full ->True (the default), the list will be for npoints points, but if full->False, it will output only the endpoints of the individual ramps. This is useful for generating tables for JPL.";


(* ::Input::Initialization:: *)
ExpansionPlot[movingtrap_,npoints_,ramp_:{{},{}},OptionsPattern[{axis->"z",trap->"1",usemodel->False,frequency->True,trapposition->True,gridlines->False,size->Medium}]]:=Block[{
dimension={OptionValue[axis]},
trapnumber=OptionValue[trap],
position,freq,model,freqif,posif,gridif,modelif
},
model[\[Omega]i_,\[Omega]f_,t_]:=(\[Omega]i+\[Omega]f)/2+(\[Omega]f-\[Omega]i)/2 Tanh[5(2t^(1/4)-1)]/Tanh[5];
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
position=ListPlot[{Table[((n-1)/npoints),{n,npoints+1}],movingtrap[[All,1,dimension[[1]]]]*10^6}\[Transpose],ImagePadding->40,posif,FrameLabel->{{StringJoin[dimension[[2]]," Trap Minimum (\[Mu]m)"],""},{"Ramp Time Fraction",StringJoin["Trap ",trapnumber," Expansion"]}},PlotRange->All,PlotStyle-> Blue,gridif,ImageSize->OptionValue[size]];
freq=Show[ListLogPlot[{Table[((n-1)/npoints),{n,npoints+1}],movingtrap[[All,2,dimension[[1]]]]}\[Transpose],ImagePadding->40,freqif,FrameLabel->{{"",StringJoin[dimension[[2]],"-axis frequency (Hz)"]},{"Ramp Time Fraction",StringJoin["Trap ",trapnumber," Expansion"]}},PlotRange->All,PlotStyle-> Red,gridif],modelif,ImageSize->OptionValue[size]];
If[OptionValue[frequency]&&OptionValue[trapposition],
	Overlay[{position,freq}],
	If[OptionValue[frequency],
		freq,
		If[OptionValue[trapposition],
			position,
			Print["Pick at lease one of trap and frequency."]
		]
	]
]
]


(* ::Input::Initialization:: *)
ExpansionPlot::usage=
"ExpansionPlot[movingtrap_,npoints_,ramp_:{{},{}},OptionsPattern[{axis\[Rule]'z',trap\[Rule]'1',usemodel\[Rule]False,frequency\[Rule]True,trapposition\[Rule]True,gridlines\[Rule]False,size\[Rule]Medium}]] takes the output of MovingTrap, a number of plot points, and optionally the axis to look at and the trap number, both of which must be in quotations. The axis defaults to z and the trap number (used only for chart labeling) defaults to 1. The output is two data sets plotted on independent axes in a single plot showing the trap position and trap frequency over the ramp.";
