(* ::Package:: *)

(* 
Where possible, these tables are drawn from D. Aveline's document 
CAL3A_chiptraps_v2.pdf, with the exception of ZHbC, where it is necessary to flip
the sign of the bias field Z value in order to realize a reasonable trap.

The corrected sign agress with the that of ZHbB1, which was based upon ZHbC.

ZHbB1 is currently based on the definition of trap parameters, but will eventually be
described by the values from a CAL table that implements the trap.
*)


(* ::Subsubsection::Closed:: *)
(*ZZH*)


AZ1selp=0;
AZ2selp=0;
AZ1p=0.57;
AZ2p=-0.57;
H1pH2p=0.4;

T1p=0.0047;
T2p = T1p;
Yp=0;
Zp=0.1257;

tableZZH={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};


(* ::Subsubsection::Closed:: *)
(*ZLHb*)


AZ1selp=1;
AZ2selp=0;
AZ1p=0.243;
AZ2p=-0.686;
H1pH2p=0.46;

T1p=-0.0275;
T2p = T1p;
Yp=0.85;
Zp=-0.05;

tableZLHb={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};


fdec=0.2

AZ1selp=1;
AZ2selp=0;
AZ1p=0.243;
AZ2p=-0.686;
H1pH2p=0.46;

T1p=-0.0275*fdec;
T2p = T1p;
Yp=0.85*fdec;
Zp=-0.05*fdec;

tableZLHbp2={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};


fdec=0.25

AZ1selp=1;
AZ2selp=0;
AZ1p=0.243;
AZ2p=-0.686;
H1pH2p=0.46;

T1p=-0.0275*fdec;
T2p = T1p;
Yp=0.85*fdec;
Zp=-0.05*fdec;

tableZLHbp25={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};


fdec=0.3

AZ1selp=1;
AZ2selp=0;
AZ1p=0.243;
AZ2p=-0.686;
H1pH2p=0.46;

T1p=-0.0275*fdec;
T2p = T1p;
Yp=0.85*fdec;
Zp=-0.05*fdec;

tableZLHbp3={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};


(* ::Subsubsection::Closed:: *)
(*ZHbC*)


AZ1selp=1;
AZ2selp=0;
AZ1p=0.;
AZ2p=-0.914;
H1pH2p=0.26;

T1p=-0.006125;
T2p = T1p;
Yp=0.0677;
Zp=0.11367; (* This differs in sign from CAL3A_chiptraps_v2 *)

tableZHbC={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};


(* ::Subsubsection::Closed:: *)
(*ZHbB1*)


(* These values currently come from Nathan's bubble-4a notebook. They will (hopefully)
be replaced by the table values from a sample SM3 Table ramping into ZHbB1,
once we get one. *)
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
]


(* This trap was created accidentally when D. Aveline made a table that ramped the bias
coils to ZHbB1 parameters but not the chip currents. *)
AZ1selp=1;
AZ2selp=0;
AZ1p=0.243;
AZ2p=-0.686;
H1pH2p=0.46;

T1p=-0.006;
T2p = T1p;
Yp=0.142384;
Zp=0.100095;

tableZHbB1Err={AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp};


(* Clear the variables that are internal to this package to prevent variables
from being renamed unexpectedly. *)
ClearAll[AZ1selp, AZ2selp, AZ1p, AZ2p, H1pH2p, T1p, T2p, X1p, X2p, Yp, Zp];
