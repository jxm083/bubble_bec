(* ::Package:: *)

(*BeginPackage["SMThreeChipModel`"]*)


(* ::Section:: *)
(*Chip trap*)


(* ::Subsection::Closed:: *)
(*Chip magnetic field*)


ChipTrapABVecField::usage = 
"ChipTrapABVecField[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]";


ChipTrapABField::usage = 
"ChipTrapABField[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]";


(* ::Subsection:: *)
(*Chip magnetic trap characterization*)


(* ::Subsubsection::Closed:: *)
(*Position of trap minimum*)


z0AB::usage = 
"z0AB[Ila_, Iza_, Ilb_, Izb_, Ih_, Bx_, By_, Bz_]";


(* ::Subsubsection::Closed:: *)
(*Trap frequencies and principle axes*)


ChipTrapABFrequencies::usage=
"ChipTrapABFrequencies[Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]";


(* ::Section:: *)
(*Begin private*)


(*Begin["`Private`"]*)


(* ::Section::Closed:: *)
(*Units and constants*)


(* ::Input::Initialization:: *)
\[Mu]m=10^-6;
Gauss=10^-4;
G= Gauss;
cm = .01;
mA=mm=10^-3;
A=1;
\[Mu]0=4\[Pi] 10^-7;
h = 6.62606957 10^-34;
hbar = h /(2 \[Pi]);
\[HBar]=hbar;
kb = 1.3806488*^-23;
mRb =1.443160648*^-25;
\[Mu]B=9.27400968*^-24;
gfactor=1/2;
gF = 1/2; (* not accurate to .0001 *) 
gF= gJ (F(F+1)-Inuc(Inuc+1)+J(J+1))/(2F(F+1))+gI (F(F+1)+Inuc(Inuc+1)-J(J+1))/(2F(F+1)) /. {F->2,J->1/2};
Inuc= 3/2;
Ahfs =  3.417341305*^9;
gI= -.0009951414 ;
gJ = 2.00233113;
nKPerHz = 1*^9(h/kb);


(* ::Section:: *)
(*CAL Table values to currents and magnetic fields*)


(* ::Subsection:: *)
(*Functions to print CAL Tables, current values, and trap parameters*)


printTableValues[table_, OptionsPattern[hideXs->True]] := Module[
{AZ1sel, AZ2sel, AZ1, AZ2, H1pH2, T1, T2, X1, X2, Y, Z,
labels = {"AZ1_sel", "AZ2_sel", "AZ1", "AZ2", "H1&H2", "T1", "T2", "X1", "X2", "Y", "Z"}},
If[OptionValue[hideXs],
(* THEN: *)
Print@TableForm@{labels[[1;;7]]~Join~labels[[10;;11]], table[[1;;7]]~Join~table[[10;;11]]};,
(* ELSE: *)
Print@TableForm@{labels, table};
];
];


printTableValues::usage = 
"printTableValues[table_] takes a CAL Table of the form 
{AZ1sel, AZ2sel, AZ1, AZ2, H1pH2, T1, T2, X1, X2, Y, Z}
and prints it to the notebook with the appropriate labels.
By default X1 and X2 are not printed, but this can be changed by setting hideXs->False.";


(* ::Subsection:: *)
(*Table values to current values*)


(** Current to field magnitude conversion factors for Bias **)
biasXCalib=40.625;(*[G/A]*)
biasYCalib=14.286;(*[G/A]*)
biasZCalib=-10.368;(*[G/A]*)

(** Max current values **)
(* Chip *)
maxCurrentAZ1 = 3.5; (*[A]*)
maxCurrentAZ2 = 3.5; (*[A]*)
maxCurrentH1pH2 = 5.; (*[A}*)

(* Bias coils *)
maxCurrentBiasX = 8.; (*[A]*)
maxCurrentBiasY = 3.02; (*[A]*) (* In some places it is reported as 3. A *)
maxCurrentBiasZ = 3.; (*[A]*)


convertCALTableToCurrents[table_, OptionsPattern[verbose->False]]:=Module[
{
AZ1sel = table[[1]],
AZ2sel = table[[2]],
AZ1 = table[[3]],
AZ2 = table[[4]],
H1pH2 = table[[5]],
T1 = table[[6]],
T2 = table[[7]],
X1 = table[[8]],
X2 = table[[9]],
Y = table[[10]],
Z = table[[11]],
currLa, currZa, currLb, currZb, currH, currBiasX, currBiasY, currBiasZ
},
If[AZ1sel==0,
currZa = AZ1*maxCurrentAZ1*(-1); (* The negative one ensures that a positive AZ1 value results in a current moving in the negative x direction, the convention used to define the chip model below. *)
currLb = 0.;,
(*Else:*)
currZa = 0;
currLb = AZ1*maxCurrentAZ1;
];

If[AZ2sel==0,
currZb = AZ2*maxCurrentAZ2*(-1); (* N.B. AZ2 only takes non-positive values, and so the current through Zb always travels in the positive x direction, if it exists. *)
currLa = 0.;,
(*Else:*)
currZb = 0.;
currLa = AZ2*maxCurrentAZ2; (* N.B. always travels in the negative x direction. *)
];

currH = H1pH2*maxCurrentH1pH2; (* Convention set by the chip model below stipulates that a postive current travels in the negative y direction. The chipdrivers_v3 document suggests (via its green arrows) that this is the convention used by the H driver as well. *)

currBiasX = T1*maxCurrentBiasX;
currBiasY = Y*maxCurrentBiasY;
currBiasZ = Z*maxCurrentBiasZ;

If[OptionValue[verbose],
Print["I evaluated!"];
Print[
"I_La = "<>ToString[currLa]<>"\n"
<>"I_Za = "<>ToString[currZa];
];
];

Return[{currLa, currZa, currLb, currZb, currH, currBiasX, currBiasY, currBiasZ}];
];


convertCALTableToTrapParameters[table_]:=Module[
{currLa, currZa, currLb, currZb, currH, currBiasX, currBiasY, currBiasZ,
fieldBiasX, fieldBiasY, fieldBiasZ},

{currLa, currZa, currLb, currZb, currH, currBiasX, currBiasY, currBiasZ} = 
convertCALTableToCurrents[table];

fieldBiasX = currBiasX*biasXCalib*(-1); (* The origin of this negative one factor is undetermined and troubling. *)
fieldBiasY = currBiasY*biasYCalib;
fieldBiasZ = currBiasZ*biasZCalib;

Return[{currLa, currZa, currLb, currZb, currH, fieldBiasX, fieldBiasY, fieldBiasZ}];
];


convertTrapParametersToCALTable[trapParameters_, OptionsPattern[verbose->False]] := Module[
{currLa, currZa, currLb, currZb, currH, currBiasX, currBiasY, currBiasZ,
fieldBiasX, fieldBiasY, fieldBiasZ,
currentCutoff = 10^(-9),
AZ1sel, AZ2sel, AZ1, AZ2, H1pH2, T1, T2, X1, X2, Y, Z,
table},

(* Name the individual trap parameters *)
{currLa, currZa, currLb, currZb, currH, fieldBiasX, fieldBiasY, fieldBiasZ} = trapParameters;

(* Determine which channel of driver AZ1 is being used, 0 (Za) or 1 (Lb). *)
If[Abs[currLb]<currentCutoff,
(* THEN: *)
AZ1sel = 0;
AZ1 = (-1)*currZa/maxCurrentAZ1;,
(* ELSE: *)
AZ1sel = 1;
AZ1 = currLb/maxCurrentAZ1;
];

(* Do the same for driver AZ2, where the possible channels are 0 (Zb) or 1 (La). *)
If[Abs[currLa]<currentCutoff,
(* THEN: *)
AZ2sel = 0;
AZ2 = (-1)*currZb/maxCurrentAZ2;,
(* ELSE: *)
AZ2sel = 1;
AZ2 = currLa/maxCurrentAZ2;
];

(* Setting the value of driver three, H, is straight-forward: *)
H1pH2 = currH/maxCurrentH1pH2;

(* As is setting the values of the bias fields: *)
T1 = fieldBiasX/(biasXCalib*maxCurrentBiasX*(-1));
T2 = T1;
Y = fieldBiasY/(biasYCalib*maxCurrentBiasY);
Z = fieldBiasZ/(biasZCalib*maxCurrentBiasZ);

table = {AZ1sel, AZ2sel, AZ1, AZ2, H1pH2, T1, T2, X1, X2, Y, Z};

If[OptionValue[verbose],
(* THEN: *)
printTableValues[table];
];

Return[table];
];


convertTrapParametersToCALTable::usage = 
"convertTrapParametersToCALTable[trapParameters] takes a list of the form 
{currLa, currZa, currLb, currZb, currH, biasFieldX, biasFieldY, biasFieldZ}, where the current
values are in Amps and are defined to be positive when travelling parallel to the positive
y axis and negative when travelling anti-parallel; and the bias field values are expressed in
terms of Gauss. It returns a list of CAL Table values of the form 
{AZ1sel, AZ2sel, AZ1, AZ2, H1pH2, T1, T2, X1, X2, Y, Z},
where presently T1=T2 and X1 and X2 are left undefined.";


(* ::Section:: *)
(*Chip trap*)


(* ::Subsection::Closed:: *)
(*Biot-Savart functions*)


(* ::Input::Initialization:: *)
(* Function FatXwire calculates the B-field at position (x0,y0,z0) for a conductor of width 2*hw at position y=yc running a finite length in x from x1\[Rule]x2. We assume the conductor lies in the plane z=0. *)
FatXwire[x0_,y0_,z0_,hw_,yc_,x1_,x2_]:=Module[{dx1,dx2,dyPlus,dyMinus},
dx1=-(x0-x1);
dx2=-(x0-x2);
dyPlus=-(y0-(yc+hw));
dyMinus=y0-(yc-hw);
Return[10^-3/(2hw) {0,ArcTan[(dx1 dyMinus)/(z0 Sqrt[dx1^2+dyMinus^2+z0^2])]-ArcTan[(dx2 dyMinus)/(z0 Sqrt[dx2^2+dyMinus^2+z0^2])]+ArcTan[(dx1 dyPlus)/(z0 Sqrt[dx1^2+dyPlus^2+z0^2])]-ArcTan[(dx2 dyPlus)/(z0 Sqrt[dx2^2+dyPlus^2+z0^2])],Log[(dx1+Sqrt[dx1^2+dyMinus^2+z0^2])/(dx2+Sqrt[dx2^2+dyMinus^2+z0^2]) (dx2+Sqrt[dx2^2+dyPlus^2+z0^2])/(dx1+Sqrt[dx1^2+dyPlus^2+z0^2])]}];
];


(* ::Input::Initialization:: *)
(* Function FatYwire calculates the magnetic field at position (x0,y0,z0) of a rectangular conductor with finite width 2*hw.  We assume: (1) Current flows in the y direction from y1 \[Rule] y2; (2) The conductor is centered at x=xc and has width 2*hw; (3) The conductor lies entirely in the x-y plane (i.e. z=0). *)
FatYwire[x0_,y0_,z0_,hw_,xc_,y1_,y2_]:=Module[{dxPlus,dxMinus,dy1,dy2},
dxPlus=hw+x0-xc;
dxMinus=hw-x0+xc;
dy1=y0-y1;
dy2=-y0+y2;
Return[10^-3/(2 hw) {ArcTan[(dxPlus dy1)/(z0 Sqrt[dxPlus^2+dy1^2+z0^2])]+ArcTan[(dxMinus dy1)/(z0 Sqrt[dxMinus^2+dy1^2+z0^2])]+ArcTan[(dxPlus dy2)/(z0 Sqrt[dxPlus^2+dy2^2+z0^2])]+ArcTan[(dxMinus dy2)/(z0 Sqrt[dxMinus^2+dy2^2+z0^2])],0,-Log[(-dy1+Sqrt[dxPlus^2+dy1^2+z0^2])/(-dy1+Sqrt[dxMinus^2+dy1^2+z0^2]) (dy2+Sqrt[dxMinus^2+dy2^2+z0^2])/(dy2+Sqrt[dxPlus^2+dy2^2+z0^2])]}];
];


(* ::Subsection:: *)
(*Chip magnetic field*)


XXGrad = 0.00005(* in G/cm *) ;
YYGrad=0.00005;
ZZGrad = 0.00005;
ZYGrad = 0.00005;
XGrad=XXGrad;
ZGrad=ZZGrad;
YGrad=YYGrad;

Hoffset = -425 \[Mu]m;
WireWidth = 50 \[Mu]m; (*half-width*)

ChipTrapABVecField[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:=Module[
{VecField,
ZLength,LLength,LegLength,deltaL,
ZAMainyc,ZAMainx0,ZAMainx1,
ZALeg1xc,ZALeg1y0,ZALeg1y1,
ZALeg2xc,ZALeg2y0,ZALeg2y1,
LAMainyc,LAMainx0,LAMainx1,
LALeg1xc,LALeg1y0,LALeg1y1,
LALeg2xc,LALeg2y0,LALeg2y1,
ZBMainyc,ZBMainx0,ZBMainx1,
ZBLeg1xc,ZBLeg1y0,ZBLeg1y1,
ZBLeg2xc,ZBLeg2y0,ZBLeg2y1,
LBMainyc,LBMainx0,LBMainx1,
LBLeg1xc,LBLeg1y0,LBLeg1y1,
LBLeg2xc,LBLeg2y0,LBLeg2y1},
ZLength=11.55mm;
LLength=5.875mm;
LegLength=5mm;
deltaL=150\[Mu]m;(* Distance between the centers of two adjacent wires *)

(* These definitions use the convention that current travels from x0\[Rule]x1, y0\[Rule]y1, 
and defines a positive current to be one that travels in the direction of the positve
x axis in the length of the wire that is
parallel to the x axis. *)

{LALeg1xc,LALeg1y0,LALeg1y1}={-deltaL/2-LLength, 2000\[Mu]m+deltaL+LegLength, 2000\[Mu]m+deltaL}; (* Took separation of lines before the bend, which is indicated in the diagram, to be the same after the bend. *)
{LAMainyc,LAMainx0,LAMainx1}={LALeg1y1,LALeg1xc,LALeg1xc+LLength};
{LALeg2xc,LALeg2y0,LALeg2y1}={LAMainx1,LAMainyc,LALeg1y0};

{ZAMainyc,ZAMainx0,ZAMainx1}={2000\[Mu]m,-deltaL/2-LLength, -deltaL/2-LLength+ZLength};
{ZALeg1xc,ZALeg1y0,ZALeg1y1}={ZAMainx0,ZAMainyc-LegLength-2000\[Mu]m,ZAMainyc}; (* Extended the leg waaay off the chip to be (roughly) flush with the legs from LB and ZB. *)
{ZALeg2xc,ZALeg2y0,ZALeg2y1}={ZAMainx1,ZAMainyc,ZAMainyc+LegLength};

{LBLeg1xc,LBLeg1y0,LBLeg1y1}={deltaL/2,-deltaL-LegLength,-deltaL};
{LBMainyc,LBMainx0,LBMainx1}={LBLeg1y1,LBLeg1xc,LBLeg1xc+LLength};
{LBLeg2xc,LBLeg2y0,LBLeg2y1}={LBMainx1,LBMainyc,LBLeg1y0};

{ZBMainyc,ZBMainx0,ZBMainx1}={0,deltaL/2+LLength-ZLength,deltaL/2+LLength};
{ZBLeg1xc,ZBLeg1y0,ZBLeg1y1}={ZBMainx0,ZBMainyc-LegLength,ZBMainyc};
{ZBLeg2xc,ZBLeg2y0,ZBLeg2y1}={ZBMainx1,ZBMainyc,ZBMainyc+LegLength+2000\[Mu]m};

 VecField=(
Iza FatXwire[x,y,z,WireWidth,ZAMainyc,ZAMainx0,ZAMainx1](* main section of ZA wire *)
+ Iza FatYwire[x,y,z,WireWidth,ZALeg1xc,ZALeg1y0,ZALeg1y1](* first leg of ZA wire *)
+Iza FatYwire[x,y,z,WireWidth,ZALeg2xc,ZALeg2y0,ZALeg2y1](* second leg of ZA wire *)

+Ila FatXwire[x,y,z,WireWidth,LAMainyc,LAMainx0,LAMainx1](* main section of LA wire *)
+Ila FatYwire[x,y,z,WireWidth,LALeg1xc,LALeg1y0,LALeg1y1](* first leg of LA wire *)
+Ila FatYwire[x,y,z,WireWidth,LALeg2xc,LALeg2y0,LALeg2y1](* second leg of LA wire *)

+Izb FatXwire[x,y,z,WireWidth,ZBMainyc,ZBMainx0,ZBMainx1](* main section of ZB wire *)
+ Izb FatYwire[x,y,z,WireWidth,ZBLeg1xc,ZBLeg1y0,ZBLeg1y1](* first leg of ZB wire *)
+Izb FatYwire[x,y,z,WireWidth,ZBLeg2xc,ZBLeg2y0,ZBLeg2y1](* second leg of ZB wire *)

+Ilb FatXwire[x,y,z,WireWidth,LBMainyc,LBMainx0,LBMainx1](* main section of LB wire *)
+Ilb FatYwire[x,y,z,WireWidth,LBLeg1xc,LBLeg1y0,LBLeg1y1](* first leg of LB wire *)
+Ilb FatYwire[x,y,z,WireWidth,LBLeg2xc,LBLeg2y0,LBLeg2y1](* second leg of LB wire *)

+Ih FatYwire[x,y,z-Hoffset,200\[Mu]m,-1.5mm,5mm,-5mm] (* first leg of H *)
+Ih FatYwire[x,y,z-Hoffset,200\[Mu]m,1.5mm,5mm,-5mm] (* second leg of H *)

+{Bx,By,Bz}+{100 XGrad x , 100 YGrad y ,100 ZGrad z+100ZYGrad y});(* Bias fields and field gradients *)
Return[VecField]
];


ChipTrapABField[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:=Module[
{VecField=ChipTrapABVecField[x, y, z, Ila, Iza, Ilb, Izb, Ih, Bx, By, Bz]},
Return[Sqrt[VecField.VecField]]
];


(* ::Subsection:: *)
(*Chip magnetic trap characterization*)


(* ::Subsubsection:: *)
(*Position of trap minimum*)


z0AB[Ila_, Iza_, Ilb_, Izb_, Ih_, Bx_, By_, Bz_]:=
NMinimize[{ChipTrapABField[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz],
-.4 mm<x<.4mm,-.4mm<y<1.5mm,2500\[Mu]m>z>20\[Mu]m},{x,y,z}][[2]];
(* These bounds may need to be changed for ZZH trap, whose center is >1.5 mm 
from the center of our coordinate system. *)


(* ::Subsubsection:: *)
(*Trap frequencies and principle axes*)


DABxx[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:=D[10^-4 ChipTrapABField[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz],{x,2}];
DABxy[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:=D[10^-4 ChipTrapABField[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz],x,y];
DABxz[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:=D[10^-4 ChipTrapABField[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz],x,z];
DAByy[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:=D[10^-4 ChipTrapABField[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz],{y,2}];
DAByz[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:=D[10^-4 ChipTrapABField[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz],y,z];
DABzz[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:=D[10^-4 ChipTrapABField[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz],{z,2}];
DABMatrix[x_,y_,z_,Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:=({
 {DABxx[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz], DABxy[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz], DABxz[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz]},
 {DABxy[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz], DAByy[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz], DAByz[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz]},
 {DABxz[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz], DAByz[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz], DABzz[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz]}
});


ChipTrapABFrequencies[Ila_,Iza_,Ilb_,Izb_,Ih_,Bx_,By_,Bz_]:= Module[
{x,y,z,DMatrixDiag,xmin,ymin,zmin,DMatrix0,e1,e2,e3},
{xmin,ymin,zmin}=z0AB[Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz];
xmin=xmin[[2]];
ymin=ymin[[2]];
zmin=zmin[[2]];
DMatrix0=SchurDecomposition[DABMatrix[x,y,z,Ila,Iza,Ilb,Izb,Ih,Bx,By,Bz]/.{x->xmin,y->ymin,z->zmin}];
DMatrixDiag=DMatrix0[[2]];
e1=DMatrix0[[1,1]];
e2=DMatrix0[[1,2]];
e3=DMatrix0[[1,3]];
Return[
 {{\[Phi],(180/Pi)ArcTan[e1[[2]],e1[[1]]]//Round,
\[Theta],(180/Pi)ArcCos[e1[[3]]/Sqrt[e1[[1]]^2+e1[[2]]^2+e1[[3]]^2]]//Round},

{\[Phi],(180/Pi)ArcTan[e2[[2]],e2[[1]]]//Round,
\[Theta],(180/Pi)ArcCos[e2[[3]]/Sqrt[e2[[1]]^2+e2[[2]]^2+e2[[3]]^2]]//Round},

{\[Phi],(180/Pi)ArcTan[e3[[2]],e3[[1]]]//Round,
\[Theta],(180/Pi)ArcCos[e3[[3]]/Sqrt[e3[[1]]^2+e3[[2]]^2+e3[[3]]^2]]//Round},


Diagonal[Sqrt[\[Mu]B Abs[DMatrixDiag]/mRb]/(2\[Pi]) ]}(* in Hz *)
]
];


(* ::Section:: *)
(*End package*)


(*End[]
EndPackage[]*)
