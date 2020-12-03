(* ::Package:: *)

(* 
Where possible, these tables are drawn from D. Aveline's document 
CAL3A_chiptraps_v2.pdf, with the exception of ZHbC, where it is necessary to flip
the sign of the bias field Z value in order to realize a reasonable trap.

The corrected sign agress with the that of ZHbB1, which was based upon ZHbC.

ZHbB1 is currently based on the definition of trap parameters, but will eventually be
described by the values from a CAL table that implements the trap.
*)


(* ::Subsubsection:: *)
(*ZZH*)


With[{
AZ1selp=0,
AZ2selp=0,
AZ1p=0.57,
AZ2p=-0.57,
H1pH2p=0.4,

T1p=0.0047, (* Opposite sign relative to the value in CAL3A_chiptraps_v2.pdf *)
Yp=0,
Zp=0.1257
},
With[{T2p = T1p},
tableZZH={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];


(* ::Subsubsection:: *)
(*ZLHb*)


(* N.B. according to CAL3A_chiptraps_v2.pdf, only the x and y bias field parameters are 
multiplied by fdec; this was not the case for our slosh data, however. *)


With[{
AZ1selp=1,
AZ2selp=0,
AZ1p=0.243,
AZ2p=-0.686,
H1pH2p=0.46,

T1p=0.0275, (* Opposite sign relative to the value in CAL3A_chiptraps_v2.pdf *)
Yp=0.85,
Zp=-0.05
},
With[{T2p=T1p},
tableZLHb={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];


(* See note about fdec above. *)
With[{fdec=0.2},
With[{
AZ1selp=tableZLHb[[1]],
AZ2selp=tableZLHb[[2]],
AZ1p=tableZLHb[[3]],
AZ2p=tableZLHb[[4]],
H1pH2p=tableZLHb[[5]],

T1p=tableZLHb[[6]]*fdec, (* Opposite sign relative to the value in CAL3A_chiptraps_v2.pdf *)
Yp=tableZLHb[[10]]*fdec,
Zp=tableZLHb[[11]]*fdec
},
With[{T2p=T1p},
tableZLHbp2={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];];


(* See note about fdec above. *)
With[{fdec=0.25},
With[{
AZ1selp=tableZLHb[[1]],
AZ2selp=tableZLHb[[2]],
AZ1p=tableZLHb[[3]],
AZ2p=tableZLHb[[4]],
H1pH2p=tableZLHb[[5]],

T1p=tableZLHb[[6]]*fdec, (* Opposite sign relative to the value in CAL3A_chiptraps_v2.pdf *)
Yp=tableZLHb[[10]]*fdec,
Zp=tableZLHb[[11]]*fdec
},
With[{T2p=T1p},
tableZLHbp25={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];];


(* See note about fdec above. *)
With[{fdec=0.3},
With[{
AZ1selp=tableZLHb[[1]],
AZ2selp=tableZLHb[[2]],
AZ1p=tableZLHb[[3]],
AZ2p=tableZLHb[[4]],
H1pH2p=tableZLHb[[5]],

T1p=tableZLHb[[6]]*fdec, (* Opposite sign relative to the value in CAL3A_chiptraps_v2.pdf *)
Yp=tableZLHb[[10]]*fdec,
Zp=tableZLHb[[11]]*fdec
},
With[{T2p=T1p},
tableZLHbp3={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];];


(* ::Subsubsection:: *)
(*ZHbC*)


With[{
AZ1selp=1,
AZ2selp=0,
AZ1p=0.,
AZ2p=-0.914,
H1pH2p=0.26,

T1p=0.006125, (* Opposite sign relative to the value in CAL3A_chiptraps_v2.pdf *)
Yp=0.0677,
Zp=0.11367 (* This differs in sign from CAL3A_chiptraps_v2 *)
},
With[{T2p=T1p},
tableZHbC={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];


(* ::Subsubsection:: *)
(*ZHbB1*)


(* These values are taken from the table 700msZHB1_example1.xlsx sent by Dave in 
September 2020. *)
With[{
AZ1selp=1,
AZ2selp=0,
AZ1p=0.,
AZ2p=-0.742857,
H1pH2p=0.52,

T1p=0.006,
Yp=0.142384,
Zp=0.100095
},
With[{T2p=T1p},
tableZHbB1={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];


(* These values come from Nathan's bubble-4a notebook, upon which the 
now-official is based.*)
With[
{
currLa=0,
currZa=0,
currLb=0,
currZb=2.6,
currH = 2.6,

Bx1 = (-0.048)*40.625,
By1 = (0.43)*14.286,
Bz1 = (0.3003)*(-10.3675)
},
trapZHBates={currLa,currZa,currLb,currZb,currH,Bx1,By1,Bz1};
tableZHBates=convertTrapParametersToCALTable[trapZHBates];
];


(* This trap was created accidentally when D. Aveline made a table that ramped the bias
coils to ZHbB1 parameters but not the chip currents. *)
With[{
AZ1selp=1,
AZ2selp=0,
AZ1p=0.243,
AZ2p=-0.686,
H1pH2p=0.46,

T1p=-0.006,
Yp=0.142384,
Zp=0.100095
},
With[{T2p=T1p},
tableZHbB1Err={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];


(* This is a slight alteration of ZHbB1, moves it slightly closer to the center
and is expected to be slightly more uniform. *)
With[{
AZ1selp=1,
AZ2selp=0,
AZ1p=0.,
AZ2p=-0.742857,
H1pH2p=0.52,

T1p=0.00725,
Yp=0.142384,
Zp=0.125
},
With[{T2p=T1p},
tableZHbB2={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];


(* This trap should fall between ZHbB1 and ZHbB2, in an effort to land it on a bright
fringe of the HRC imaging beam. *)
With[{
AZ1selp = 1,
AZ2selp = 0,
AZ1p = 0.,
AZ2p = -0.742857,
H1pH2p = 0.52,

T1p = 0.0064,
Yp = 0.142384,
Zp = 0.107095
},
With[{T2p=T1p},
tableZHbB2b={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];


(* This trap is pulled far to the center and features small bias coil currents. *)
With[{
AZ1selp=1,
AZ2selp=0,
AZ1p=0.,
AZ2p=-0.692857,
H1pH2p=0.52,

T1p=0.006,
Yp=0.082384,
Zp=0.100095
},
With[{T2p=T1p},
tableZHbB3={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};
];];


(* ::Subsection:: *)
(*ZZHB1*)


(* This trap is modified from ZZH for better shell homogeneity. *)
(* It would require a two-stage transfer ramp in order to switch the current
to run through the second Z wire. *)
With[
{
AZ1sel = 0,
AZ2sel = 0,
AZ1 = 0.77,
AZ2 = -0.77,
H1pH2 = 0.5,
T1 = 0.,
T2 = 0.,
Y = 0.,
Z = 0.2157
},
tableZZHB1 = {AZ1sel, AZ2sel, AZ1, AZ2, H1pH2, T1, T2, X1p, X2p, Y, Z};
];
