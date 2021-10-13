(* ::Package:: *)

(*Mathematica Package*)
BeginPackage["CALfunctions`"]
(*Exported symbols added here with SymbolName::usage*)

(* Constants *)
constants::usage="
constants[] = prints all the constants defined by this package.";
Bz::usage="
Bz[x_,y_,z_,coil_] = Biot-Savart of a square loop through the center of the coil. Calculated in Tesla.";
By::usage="
By[x_,y_,z_,coil_] = Biot-Savart of a square loop along the height of the coil. Calculated in Tesla.";
Bx::usage="
Bx[x_,y_,z_,coil_] = Biot-Savart of a square loop along the width of the coil. Calculated in Tesla.";
BxField::usage="
BxField[x_,y_,z_,coils_] = calculated magnetic field in the x-direction.";
ByField::usage="
BxField[x_,y_,z_,coils_] = calculated magnetic field in the y-direction.";
BzField::usage="
BxField[x_,y_,z_,coils_] = calculated magnetic field in the z-direction.";
BxSum::usage="
BxSum[x_,y_,z_,transfer_,MOT_,Ybias_,Zbias_,FF_] = Sum of all the fields from the square solenoids in 
the x-direction wrt the Feshbach coils.";
BySum::usage="
BySum[x_,y_,z_,transfer_,MOT_,Ybias_,Zbias_,FF_] = Sum of all the fields from the square solenoids in 
the y-direction wrt the Feshbach coils.";
BzSum::usage="
BzSum[x_,y_,z_,transfer_,MOT_,Ybias_,Zbias_,FF_] = Sum of all the fields from the square solenoids in 
the z-direction wrt the Feshbach coils.";
FieldMag::usage="
FieldMag[x_,y_,z_,transfer_,MOT_,Ybias_,Zbias_,FF_] = Magnitude of all coil fields with respect to 
their directions and their offsets from one another: Bmag=\[Sqrt](Bx^2 + By^2 + Bz^2).";
BiasCoils::usage="
BiasCoils[coils_] = This function takes the coil parameters defined above and calculates the positions of 
all the square wires for a square solenoid.  It returns a list of 5 parameters.  Each row describes one 
turn in HH configuration.  Use this function as a parameter when calculating the Bx, By, and Bz fields for 
the square solenoids. All of these functions are built into the total Chip Trap Field below.";
BiasCoilsAntiHH::usage="
BiasCoilsAntiHH[coils_] = This function takes the coil parameters defined above and calculates the positions of 
all the square wires for a square solenoid.  It returns a list of 5 parameters.  Each row describes one 
turn in anti-HH configuration.  Use this function as a parameter when calculating the Bx, By, and Bz fields for 
for a single pair of square solenoid coils. Note that only the MOT and Transfer coils can be made anti-HH.";
MagTrapField::usage="
MagTrapField[x_,y_,z_,currents_,trapparams_] = This function takes the currents for all of the magnetic cage and the
trap parameters and calculates the magnetic field of the cage at x,y,z. Provide a list for the currents 
and the trap parameters:
{I_driver1, I_driver2, I_driver3, I_transfercoils, I_MOTcoils, I_ybiascoils, I_zbiascoils, I_FFcoils};
{isotope,F,mF,D1,D2,D3}.";
Xwire::usage="
Xwire[x0_,y0_,z0_,hw_,wirecoord_] = Function Xwire calculates the magnetic field at position (x0, y0, z0) 
of a rectangular conductor with finite width 2*hw.  We assume : 
(1) Current flows in the x direction from x1 -> x2; 
(2) The conductor is centered at x = xc and has width 2*hw; 
(3) The conductor lies entirely in the x - y plane (i.e. z = 0). Really is 10um raised from the surface, 
but that is an ignorable factor.
Note that values are calculated in Tesla.";
Ywire::usage="
Ywire[x0_,y0_,z0_,hw_,wirecoord_] = Function Ywire calculates the magnetic field at position (x0, y0, z0) 
of a rectangular conductor with finite width 2*hw.  We assume : 
(1) Current flows in the y direction from y1 -> y2; 
(2) The conductor is centered at y = yc and has width 2*hw; 
(3) The conductor lies entirely in the x - y plane (i.e. z = 0). Really is 10um raised from the surface, 
but that is an ignorable factor.
Note that values are calculated in Tesla.";
ChipTrapField::usage="
ChipTrapField[x_,y_,z_,currents_,trapparams_] = Calculates the magnetic field in Tesla using calculated
magnetic fields based on coild geometry. Provide a list for the currents and the trap parameters:
{I_driver1, I_driver2, I_driver3, I_transfercoils, I_MOTcoils, I_ybiascoils, I_zbiascoils, I_FFcoils};
{isotope,F,mF,D1,D2,D3}.";
ChipTrapFieldFAST::usage="
ChipTrapFieldFAST[x_,y_,z_,currents_,trapparams_] = Calculates the magnetic field in Tesla using the static 
fields provided by CQ and JPL. Provide a list for the currents and the trap parameters:
{I_driver1, I_driver2, I_driver3, I_transfercoils, I_MOTcoils, I_ybiascoils, I_zbiascoils, I_FFcoils};
{isotope,F,mF,D1,D2,D3}.";
EnergyBR::usage="
EnergyBR[F_,M_,B_,isotope_] = Breit-Rabi correction to the energy of the system due to the Zeeman shift / Paschen-Back 
effect. This function calculates the energy changes due to the Zeeman effect in the presence of a magnetic field. 
Be sure to uncomment the correct isotope in the Constants chapter above. Here, |J-I| <= F <= J+I, so F=1,2; 
M is the -F <= \!\(\*SubscriptBox[\(m\), \(F\)]\) <= F state you are considering, B is the magnetic field at the atoms in Tesla.";
Gravity::usage="
Gravity[z_,isotope_] = calculates the GPE under gravity. Choose isotope. ";
PE::usage="
PE[x_,y_,z_,currents_,trapparams_,gravity_] = Potential energy calculation using full magnetic field calculation.
Uses Breit-Rabi to calculate traps.
Provide a list for the currents and the trap parameters:
{I_driver1, I_driver2, I_driver3, I_transfercoils, I_MOTcoils, I_ybiascoils, I_zbiascoils, I_FFcoils};
{isotope,F,mF,D1,D2,D3}
Gravity is true (1) or false (0).";
PEFAST::usage="
PEFAST[x_,y_,z_,currents_,trapparams_,gravity_] = Potential energy calculation using static magnetic fields.
Uses Breit-Rabi to calculate traps. 
Provide a list for the currents and the trap parameters:
{I_driver1, I_driver2, I_driver3, I_transfercoils, I_MOTcoils, I_ybiascoils, I_zbiascoils, I_FFcoils};
{isotope,F,mF,D1,D2,D3}
Gravity is true (1) or false (0).";
zmin::usage="
zmin[currents_,trapparams_,gravity_,ylim_] = Finds the minimum of the trap given the potential energy 
of the system using the full magnetic field calculations. Note that this function uses NMinimize and tends to pull values towards the edges if the potential is not deep 
enough. Provide a list for the currents and the trap parameters:
{I_driver1, I_driver2, I_driver3, I_transfercoils, I_MOTcoils, I_ybiascoils, I_zbiascoils, I_FFcoils};
{isotope,F,mF,D1,D2,D3}
Gravity is true (1) or false (0).
ylim is where you expect the trap bottom to be so that the minimize function can look in the correct 
region. For the b side of the chip, choose -1.0mm. For the a side, choose +1.0mm. If you expect your trap
to be in the center of the window, choose 0.0mm.";
zminFAST::usage="
zminFAST[currents_,trapparams_,gravity_,ylim_] = Finds the minimum of the trap given the potential 
energy of the system given the constant magnetic fields provided by CQ and JPL. Note that this function 
uses NMinimize and tends to pull values towards the edges if the potential is not deep enough. 
Provide a list for the currents and the trap parameters:
{I_driver1, I_driver2, I_driver3, I_transfercoils, I_MOTcoils, I_ybiascoils, I_zbiascoils, I_FFcoils};
{isotope,F,mF,D1,D2,D3}
Gravity is true (1) or false (0).
ylim is where you expect the trap bottom to be so that the minimize function can look in the correct 
region. For the b side of the chip, choose -1.0mm. For the a side, choose +1.0mm. If you expect your trap
to be in the center of the window, choose 0.0mm.
Use for quick checks.";
ChipTrapFreq::usage="
ChipTrapFrequencies[xmin_,ymin_,zmin_,currents_,trapparams_,gravity_] = This function takes the position z0 
where the trap is centered. This function takes into account the curvature of the external fields.  
Note that the addition of gravity should not change this function in regards to how the derivatives are calculated. 
The PE from gravity varies as z so the double partial derivative with respect to any combination of x, y and z 
yeilds zero. Use either Rb87, K39, or K41 for isotope.
Provide a list for the currents and the trap parameters:
{I_driver1, I_driver2, I_driver3, I_transfercoils, I_MOTcoils, I_ybiascoils, I_zbiascoils, I_FFcoils};
{isotope,F,mF,D1,D2,D3}
Gravity is true (1) or false (0).";
ChipTrapFreqFAST::usage="
ChipTrapFreqFAST[xmin_,ymin_,zmin_,currents_,trapparams_,gravity_] = This function takes the position z0 
where the trap is centered. This function does not take into account the curvature of the external fields.  
Note that the addition of gravity should not change this function in regards to how the derivatives are calculated. 
The PE from gravity varies as z so the double partial derivative with respect to any combination of x, y and z 
yeilds zero. Use either Rb87, K39, or K41 for isotope.
Provide a list for the currents and the trap parameters:
{I_driver1, I_driver2, I_driver3, I_transfercoils, I_MOTcoils, I_ybiascoils, I_zbiascoils, I_FFcoils};
{isotope,F,mF,D1,D2,D3}
Gravity is true (1) or false (0).";
ExtFieldFreq::usage="
ExtFieldFrequencies[xmin_,ymin_,zmin_,currents_,isotope_] = trap frequencies of the applied field given that 
the atom chip current is set to zero. Use either Rb87, K39, or K41 for isotope.";

(* Begin constants *)
\[Mu]0=4*\[Pi]*10^-7;
Gauss = 10^-4;
mm =mA=ms= 10^-3;
in = 25.4mm;
A = 1;
\[Mu]m=10^-6;
nK = 1*10^-9;
MHz = 1*10^6;
m2kg=1.6726231*10^-27; (*Mass conversion from u to kg*)
gravity = 9.81;
hbar = 6.626076/2/\[Pi]*10^-34;
hPlanck = hbar*2*\[Pi];
cLight = 299792458;
(*uproton = 1.66054*10^-27;*)
abohr = 5.2918*10^-11;
kB = 1.380658*10^-23; (* Boltzmann constant *)
muBohr = 9.2740154*10^-24;(* Bohr magneton *)
gj =1/2; 
mj = 2; (* 2 for the F=2 state, 1 for the F=1 state *)
gjmj = gj*mj;
mRb87 = 86.372836*m2kg;
mK39=38.96370668*m2kg;
Inuc = 3/2; (* Nuclear Spin for all three options *)
gs = 2.0023193043622; (* Subscript[g, s] factor constant *)
wiredia = 0.0253 in; (*0.0253 fr 22AWG wire,1.024 mm for 18 AWG which is 0.0403 inches , 1.45mm for 15 AWG*)

\[Epsilon] = 0*mm; 
(* Based on CQ's inner dimensions sent in Jason's 0421 2020 email *)
YbiasCoils[I_] := {wiredia,I,1.55in,2.33in,2.362in,11,11,(0.4525in-\[Epsilon])}; (* z\[Rule]x, x\[Rule]-z, y\[Rule]y *)
ZbiasCoils[I_] := {wiredia,I,2.15in,1.74in,3.228in,11,11,((3.228/2-1.319)in-\[Epsilon])};(* z\[Rule]y, x\[Rule]x, y\[Rule]-z *)
TransferCoils[I_]:= {wiredia,I,1.57in,1.84in,1.181in,13, 13,-\[Epsilon]};
MOTCoils[I_]:= {wiredia,I,1.57in,1.84in,2.0982in,13, 13,(0.591in-\[Epsilon])};
FFCoils[I_] := {wiredia,I,1.490in,1.770in,1.19105in, 1,2,-\[Epsilon]};

(* SM3 Atom Chip *)
(* 
Can only use 1 set of wires/coils per driver;
Driver 1: Za, Lb, Tb;
Driver 2: Zb, La, FF;
Driver 3: H-wires;
*)
C2Cwirespace = 0.150mm;
Hoffset = -425\[Mu]m;
WireWidth = 50\[Mu]m;
(*Driver 1 Options*)
Za = {{+5.55 mm,+6.55mm,+1.0mm},{+1.0mm,+5.55mm,-6.0mm},{-6.0mm,+1.0mm, -6.55mm}};
Lb = {{+C2Cwirespace/2,-6.55mm, -(1.0mm+C2Cwirespace) },{-(1.0mm+C2Cwirespace),+C2Cwirespace/2, +6.0mm},{+6.0mm,-(1.0mm+C2Cwirespace), -6.55mm}};
Tb = {{+6.0 mm, +6.55,-1.0mm},{-1.0 mm,+6.0mm, -C2Cwirespace/2},{-C2Cwirespace/2,-1.0mm, -6.55mm}};
(*Driver 2 Options*)
Zb = {{-5.55 mm,-6.55mm, -1.0 mm},{-1.0 mm,-5.55mm, +6.0 mm},{+6.0 mm,-1.0mm, +6.55 mm}};
(*Ta = {{-6.0 mm,-6.55mm,+1.0mm},{+1.0 mm,-6.0 mm, +C2Cwirespace/2},{+C2Cwirespace/2,+1.0mm, +6.55mm}};*)
La = {{-C2Cwirespace/2,+6.55mm, +(1.0mm+C2Cwirespace)},{+(1.0mm+C2Cwirespace), -C2Cwirespace/2, -6.0mm},{-6.0mm,+(1.0mm+C2Cwirespace),+6.55mm}};
(*Driver 3 Options*)
Ha = {{+1.5 mm, 5.0 mm, -5.0 mm}};
Hb = {{-1.5 mm, 5.0 mm, -5.0 mm}};

Begin["`Private`"] (*Begin Private Context*)
(* Constants *)

Bz[x_,y_,z_,coil_]:= Module[{a,b,c,d,r, curr,dx,dy,dz},
dx = x - coil[[3]];
dy = y-coil[[4]];
dz = z - coil[[5]];
a = coil[[1]]/2;
b = coil[[2]]/2;
c = {a+dx, a-dx, dx-a, -dx-a};
d = {dy+b, dy+b,dy-b, dy-b};
curr  = coil[[6]];
r={Sqrt[(a+dx)^2+(dy+b)^2+dz^2],Sqrt[(a-dx)^2+(dy+b)^2+dz^2],Sqrt[(a-dx)^2+(dy-b)^2+dz^2],
Sqrt[(a+dx)^2+(dy-b)^2+dz^2]} ;
\[Mu]0*curr/4/\[Pi]*Sum[((-1)^\[Alpha]*d[[\[Alpha]]])/(r[[\[Alpha]]]*(r[[\[Alpha]]]+(-1)^(\[Alpha]-1)*c[[\[Alpha]]]))-c[[\[Alpha]]]/(r[[\[Alpha]]]*(r[[\[Alpha]]]+d[[\[Alpha]]])),{\[Alpha],1,4}]
];

By[x_,y_,z_,coil_]:= Module[{a,b,c,d,r, curr,dx,dy,dz},
dx = x - coil[[3]];
dy = y-coil[[4]];
dz = z - coil[[5]];
a = coil[[1]]/2;
b = coil[[2]]/2;
c = {a+dx, a-dx, dx-a, -dx-a};
d = {dy+b, dy+b,dy-b, dy-b};
curr  = coil[[6]];
r={Sqrt[(a+dx)^2+(dy+b)^2+dz^2],Sqrt[(a-dx)^2+(dy+b)^2+dz^2],Sqrt[(a-dx)^2+(dy-b)^2+dz^2],
Sqrt[(a+dx)^2+(dy-b)^2+dz^2]} ;
\[Mu]0*curr/4/\[Pi]*Sum[((-1)^(\[Alpha]+1)*dz)/(r[[\[Alpha]]]*(r[[\[Alpha]]]+ (-1)^(\[Alpha]+1)*c[[\[Alpha]]])),{\[Alpha],1,4}]
];

Bx[x_,y_,z_,coil_]:= Module[{a,b,c,d,r, curr,dx,dy,dz},
dx = x - coil[[3]];
dy = y-coil[[4]];
dz = z - coil[[5]];
a = coil[[1]]/2;
b = coil[[2]]/2;
c = {a+dx, a-dx, dx-a, -dx-a};
d = {dy+b, dy+b,dy-b, dy-b};
curr  = coil[[6]];
r={Sqrt[(a+dx)^2+(dy+b)^2+dz^2],Sqrt[(a-dx)^2+(dy+b)^2+dz^2],Sqrt[(a-dx)^2+(dy-b)^2+dz^2],
Sqrt[(a+dx)^2+(dy-b)^2+dz^2]} ;
\[Mu]0*curr/4/\[Pi]*Sum[((-1)^(\[Alpha]+1)*dz)/(r[[\[Alpha]]]*(r[[\[Alpha]]]+d[[\[Alpha]]])),{\[Alpha],1,4}]
];

BxField[x_,y_,z_,coils_]:=Module[{},TempFunc[coil_]:=Bx[x,y,z,coil];
Plus@@Map[TempFunc,coils,{1}]];

ByField[x_,y_,z_,coils_]:=Module[{},TempFunc[coil_]:=By[x,y,z,coil];
Plus@@Map[TempFunc,coils,{1}]];

BzField[x_,y_,z_,coils_]:=Module[{},TempFunc[coil_]:=Bz[x,y,z,coil];
Plus@@Map[TempFunc,coils,{1}]];

BxSum[x_,y_,z_,transfer_,MOT_,Ybias_,Zbias_,FF_] :=  BxField[x,y-TransferCoils[0][[8]],z,transfer]+BxField[x,y-MOTCoils[0][[8]],z,MOT]+BzField[-z,y-YbiasCoils[0][[8]],x,Ybias]+BxField[x,y-FFCoils[0][[8]],z,FF]+BxField[x,-z,y-ZbiasCoils[0][[8]],Zbias];

BySum[x_,y_,z_,transfer_,MOT_,Ybias_,Zbias_,FF_] :=  ByField[x,y-TransferCoils[0][[8]],z,transfer]+ByField[x,y-MOTCoils[0][[8]],z,MOT]+ByField[-z,y-YbiasCoils[0][[8]],x,Ybias]+ByField[x,y-FFCoils[0][[8]],z,FF]+BzField[x,-z,y-ZbiasCoils[0][[8]],Zbias];

BzSum[x_,y_,z_,transfer_,MOT_,Ybias_,Zbias_,FF_] :=  BzField[x,y-TransferCoils[0][[8]],z,transfer]+BzField[x,y-MOTCoils[0][[8]],z,MOT]+BxField[-z,y-YbiasCoils[0][[8]],x,Ybias]+BzField[x,y-FFCoils[0][[8]],z,FF]+ByField[x,-z,y-ZbiasCoils[0][[8]],Zbias];

FieldMag[x_,y_,z_,transfer_,MOT_,Ybias_,Zbias_,FF_]:= Sqrt[BxSum[x,y,z,transfer,MOT,Ybias,Zbias,FF]^2+BySum[x,y,z,transfer,MOT,Ybias,Zbias,FF]^2+BzSum[x,y,z,transfer,MOT,Ybias,Zbias,FF]^2];

BiasCoils[coils_]:=Module[{length,h,r,hreal,heightreal,widthreal,biascoils,wiredia,current,width,height,coilspacing,hmax,rmax,distfromcenter},
biascoils={};
length=0;
wiredia = coils[[1]];
current = coils[[2]];
width = coils[[3]];
height = coils[[4]];
coilspacing = coils[[5]];
hmax = coils[[6]];
rmax = coils[[7]];
For[h=0,h<hmax,h++,
For[r=0,r<rmax,r++,
hreal=coilspacing/2+wiredia/2+h*wiredia; (*how many layers along the z axis direction *)
heightreal = height+2*(wiredia/2+r*wiredia);(*how many turns in one plane *)
widthreal = width + 2*(wiredia/2+r * wiredia);
AppendTo[biascoils,{widthreal, heightreal,0,0,hreal,current}];
AppendTo[biascoils,{widthreal,heightreal,0,0,-hreal,current}];
length = length + 2*(widthreal+heightreal);]];
Return[biascoils]
]

BiasCoilsAntiHH[coils_]:=Module[{length,h,r,hreal,heightreal,widthreal,biascoils,wiredia,current,width,height,coilspacing,hmax,rmax,distfromcenter},
biascoils={};
length=0;
wiredia = coils[[1]];
current = coils[[2]];
width = coils[[3]];
height = coils[[4]];
coilspacing = coils[[5]];
hmax = coils[[6]];
rmax = coils[[7]];
For[h=0,h<hmax,h++,
For[r=0,r<rmax,r++,
hreal=coilspacing/2+wiredia/2+h*wiredia; (*how many layers along the z axis direction *)
heightreal = height+2*(wiredia/2+r*wiredia);(*how many turns in one plane *)
widthreal = width + 2*(wiredia/2+r * wiredia);
AppendTo[biascoils,{widthreal, heightreal,0,0,hreal,current}];
AppendTo[biascoils,{widthreal,heightreal,0,0,-hreal,-current}];
length = length + 2*(widthreal+heightreal);]];
Return[biascoils]
]

MagTrapField[x_,y_,z_,currents_,trapparams_]:=Module[{d1,d2,d3,iD1,iD2,iD3,iT,iM,iY,iZ,iFF,trace1,trace2,VecField,transfer,MOT,Ybias,Zbias,FF,Gravity},
iT = currents[[4]];
iM = currents[[5]];
iY = currents[[6]];
iZ = currents[[7]];
iFF = currents[[8]];
transfer = BiasCoils[TransferCoils[iT]];
MOT = BiasCoils[MOTCoils[iM]];
Ybias = BiasCoils[YbiasCoils[iY]];
Zbias = BiasCoils[ZbiasCoils[iZ]];
FF=BiasCoils[FFCoils[iFF]]; 
VecField = 
 {BzSum[x,y,z,transfer,MOT,Ybias,Zbias,FF],BxSum[x,y,z,transfer,MOT,Ybias,Zbias,FF],BySum[x,y,z,transfer,MOT,Ybias,Zbias,FF]};
Return[Sqrt[VecField.VecField]]
]

Xwire[x0_,y0_,z0_,hw_,wirecoord_]:=Module[{dx1,dx2,dyPlus,dyMinus,yc,x1,x2},
yc = wirecoord[[1]];
x1 = wirecoord[[2]];
x2 = wirecoord[[3]];
dx1=-x0+x1;
dx2=-x0+x2;
dyPlus=hw-y0+yc;
dyMinus=hw+y0-yc;
Return[10^-7/(2hw) {0,ArcTan[(dx1 dyMinus)/(z0 Sqrt[dx1^2+dyMinus^2+z0^2])]-ArcTan[(dx2 dyMinus)/(z0 Sqrt[dx2^2+dyMinus^2+z0^2])]+ArcTan[(dx1 dyPlus)/(z0 Sqrt[dx1^2+dyPlus^2+z0^2])]-ArcTan[(dx2 dyPlus)/(z0 Sqrt[dx2^2+dyPlus^2+z0^2])],Log[(dx1+Sqrt[dx1^2+dyMinus^2+z0^2])/(dx2+Sqrt[dx2^2+dyMinus^2+z0^2]) (dx2+Sqrt[dx2^2 + dyPlus^2 + z0^2])/(dx1+Sqrt[dx1^2+dyPlus^2+z0^2])]}];
];

(* *************************
Ywire code edited on 0501 2019 in response to Nathan Lundblad email citing an error in the original NASA JPL code. Z component should have a negative sign prior to the "Log[...]".
************************** *)

Ywire[x0_,y0_,z0_,hw_,wirecoord_]:=Module[{dxPlus,dxMinus,dy1,dy2,xc,y1,y2},
xc = wirecoord[[1]];
y1 = wirecoord[[2]];
y2 = wirecoord[[3]];
dxPlus=hw+x0-xc;
dxMinus=hw-x0+xc;
dy1=y0-y1;
dy2=-y0+y2;
Return[10^-7/(2 hw) {ArcTan[(dxPlus dy1)/(z0 Sqrt[dxPlus^2+dy1^2+z0^2])]+ArcTan[(dxMinus dy1)/(z0 Sqrt[dxMinus^2+dy1^2+z0^2])]+ArcTan[(dxPlus dy2)/(z0 Sqrt[dxPlus^2+dy2^2+z0^2])]+ArcTan[(dxMinus dy2)/(z0 Sqrt[dxMinus^2+dy2^2+z0^2])],0,-Log[(-dy1+Sqrt[dxPlus^2+dy1^2+z0^2])/(-dy1+Sqrt[dxMinus^2+dy1^2+z0^2]) (dy2+Sqrt[dxMinus^2+dy2^2+z0^2])/(dy2+Sqrt[dxPlus^2+dy2^2+z0^2])]}];
];
                                  
ChipTrapField[x_,y_,z_,currents_,trapparams_]:=Module[{d1,d2,d3,iD1,iD2,iD3,iT,iM,iY,iZ,iFF,drive1,drive2,drive3,VecField,transfer,MOT,Ybias,Zbias,FF,Gravity},
iD1 = currents[[1]];
iD2 = currents[[2]];
iD3 = currents[[3]];
iT = currents[[4]];
iM = currents[[5]];
iY = currents[[6]];
iZ = currents[[7]];
iFF = currents[[8]];
d1 = trapparams[[4]];
d2 = trapparams[[5]];
d3 = trapparams[[6]];
transfer = BiasCoils[TransferCoils[iT]];
MOT = BiasCoils[MOTCoils[iM]];
Ybias = BiasCoils[YbiasCoils[iY]];
Zbias = BiasCoils[ZbiasCoils[iZ]];
FF=BiasCoils[FFCoils[iFF]];

drive1 =  Which[
d1=="Z1",(Ywire[x,y,z,WireWidth,Za[[1]]]+Xwire[x,y,z,WireWidth,Za[[2]]]+Ywire[x,y,z,WireWidth,Za[[3]]]),
d1=="L2",(Ywire[x,y,z,WireWidth,Lb[[2]]]+Xwire[x,y,z,WireWidth,Lb[[2]]]+Ywire[x,y,z,WireWidth,Lb[[3]]]),
d1=="T2",(Ywire[x,y,z,WireWidth,Tb[[1]]]+Xwire[x,y,z,WireWidth,Tb[[2]]]+Ywire[x,y,z,WireWidth,Tb[[3]]]),
d1=="Z2", Return["Z2 is not a valid option for driver 1. Please choose Z1, L2, T2 or None."],
d1=="L1", Return["L1 is not a valid option for driver 1. Please choose Z1, L2, T2 or None."],
d1=="FF", Return["FF is not a valid option for driver 1. Please choose Z1, L2, T2 or None."],
d1=="H", Return["H is not a valid option for driver 1. Please choose Z1, L2, T2 or None."],
d1!="Z1"\[Or]"L2"\[Or]"T2"\[Or]"Z2"\[Or]"L1"\[Or]"FF"\[Or]"H", Return["ERROR. The wire name you entered for 
driver 1 does not exist in this program. Valid wire names for driver 1 are Z1, L2, T2 or None."],
d1=="None",0];

drive2 = Which[
d2=="Z2",(Ywire[x,y,z,WireWidth,Zb[[1]]]+Xwire[x,y,z,WireWidth,Zb[[2]]]+Ywire[x,y,z,WireWidth,Zb[[3]]]),
d2=="L1",(Ywire[x,y,z,WireWidth,La[[1]]]+Xwire[x,y,z,WireWidth,La[[2]]]+Ywire[x,y,z,WireWidth,La[[3]]]),
d2=="FF",0,
d2=="Z1", Return["Z1 is not a valid option for driver 2. Please choose Z2, L1, FF or None."],
d2=="L2", Return["L2 is not a valid option for driver 2. Please choose Z2, L1, FF or None."],
d2=="T2", Return["T2 is not a valid option for driver 2. Please choose Z2, L1, FF or None."],
d2=="H", Return["H is not a valid option for driver 2. Please choose Z2, L1, FF or None."],
d2!="Z1"\[Or]"L2"\[Or]"T2"\[Or]"Z2"\[Or]"L1"\[Or]"FF"\[Or]"H", Return["ERROR. The wire name you entered for 
driver 2 does not exist in this program. Valid wire names for driver 2 are Z2, L1, FF or None."],
d2=="None",0];

drive3 = Which[
d3=="H",(Ywire[x,y,z-Hoffset,200\[Mu]m,Ha[[1]]]+Ywire[x,y,z-Hoffset,200\[Mu]m,Hb[[1]]]),
d3=="Z1", Return["Z1 is not a valid option for driver 3. Please choose H or None."],
d3=="L2", Return["L2 is not a valid option for driver 3. Please choose H or None."],
d3=="T2", Return["T2 is not a valid option for driver 3. Please choose H or None."],
d3=="Z2", Return["Z2 is not a valid option for driver 3. Please choose H or None."],
d3=="L1", Return["L1 is not a valid option for driver 3. Please choose H or None."],
d3=="FF", Return["FF is not a valid option for driver 3. Please choose H or None."],
d3!="Z1"\[Or]"L2"\[Or]"T2"\[Or]"Z2"\[Or]"L1"\[Or]"FF"\[Or]"H", Return["ERROR. The wire name you entered for 
driver 3 does not exist in this program. Valid wire names for driver 3 are H or None."],
d3=="None",0];

VecField = iD1*drive1 + iD2*drive2 + iD3*drive3 + {BzSum[x,y,z,transfer,MOT,Ybias,Zbias,FF],BxSum[x,y,z,transfer,MOT,Ybias,Zbias,FF],BySum[x,y,z,transfer,MOT,Ybias,Zbias,FF]};
Return[Sqrt[VecField.VecField]]
]

ChipTrapFieldFAST[x_,y_,z_,currents_,trapparams_]:=Module[{iD1,iD2,iD3,iT,iM,iY,iZ,iFF,d1,d2,d3,drive1,drive2,drive3,VecField},
iD1 = currents[[1]];
iD2 = currents[[2]];
iD3 = currents[[3]];
iT = currents[[4]];
iM=currents[[5]];
iY = currents[[6]];
iZ = currents[[7]];
iFF = currents[[8]];
d1 = trapparams[[4]];
d2 = trapparams[[5]];
d3 = trapparams[[6]];

drive1 =  Which[
d1=="Z1",(Ywire[x,y,z,WireWidth,Za[[1]]]+Xwire[x,y,z,WireWidth,Za[[2]]]+Ywire[x,y,z,WireWidth,Za[[3]]]),
d1=="L2",(Ywire[x,y,z,WireWidth,Lb[[2]]]+Xwire[x,y,z,WireWidth,Lb[[2]]]+Ywire[x,y,z,WireWidth,Lb[[3]]]),
d1=="T2",(Ywire[x,y,z,WireWidth,Tb[[1]]]+Xwire[x,y,z,WireWidth,Tb[[2]]]+Ywire[x,y,z,WireWidth,Tb[[3]]]),
d1=="Z2", Return["Z2 is not a valid option for driver 1. Please choose Z1, L2, T2 or None."],
d1=="L1", Return["L1 is not a valid option for driver 1. Please choose Z1, L2, T2 or None."],
d1=="FF", Return["FF is not a valid option for driver 1. Please choose Z1, L2, T2 or None."],
d1=="H", Return["H is not a valid option for driver 1. Please choose Z1, L2, T2 or None."],
d1!="Z1"\[Or]"L2"\[Or]"T2"\[Or]"Z2"\[Or]"L1"\[Or]"FF"\[Or]"H", Return["ERROR. The wire name you entered for 
driver 1 does not exist in this program. Valid wire names for driver 1 are Z1, L2, T2 or None."],
d1=="None",0];

drive2 = Which[
d2=="Z2",(Ywire[x,y,z,WireWidth,Zb[[1]]]+Xwire[x,y,z,WireWidth,Zb[[2]]]+Ywire[x,y,z,WireWidth,Zb[[3]]]),
d2=="L1",(Ywire[x,y,z,WireWidth,La[[1]]]+Xwire[x,y,z,WireWidth,La[[2]]]+Ywire[x,y,z,WireWidth,La[[3]]]),
d2=="FF",0,
d2=="Z1", Return["Z1 is not a valid option for driver 2. Please choose Z2, L1, FF or None."],
d2=="L2", Return["L2 is not a valid option for driver 2. Please choose Z2, L1, FF or None."],
d2=="T2", Return["T2 is not a valid option for driver 2. Please choose Z2, L1, FF or None."],
d2=="H", Return["H is not a valid option for driver 2. Please choose Z2, L1, FF or None."],
d2!="Z1"\[Or]"L2"\[Or]"T2"\[Or]"Z2"\[Or]"L1"\[Or]"FF"\[Or]"H", Return["ERROR. The wire name you entered for 
driver 2 does not exist in this program. Valid wire names for driver 2 are Z2, L1, FF or None."],
d2=="None",0];

drive3 = Which[
d3=="H",(Ywire[x,y,z-Hoffset,200\[Mu]m,Ha[[1]]]+Ywire[x,y,z-Hoffset,200\[Mu]m,Hb[[1]]]),
d3=="Z1", Return["Z1 is not a valid option for driver 3. Please choose H or None."],
d3=="L2", Return["L2 is not a valid option for driver 3. Please choose H or None."],
d3=="T2", Return["T2 is not a valid option for driver 3. Please choose H or None."],
d3=="Z2", Return["Z2 is not a valid option for driver 3. Please choose H or None."],
d3=="L1", Return["L1 is not a valid option for driver 3. Please choose H or None."],
d3=="FF", Return["FF is not a valid option for driver 3. Please choose H or None."],
d3!="Z1"\[Or]"L2"\[Or]"T2"\[Or]"Z2"\[Or]"L1"\[Or]"FF"\[Or]"H", Return["ERROR. The wire name you entered for 
driver 3 does not exist in this program. Valid wire names for driver 3 are H or None."],
d3=="None",0];

VecField = 
iD1*drive1 + iD2*drive2 + iD3*drive3 +{iT*40.625*Gauss+iM*21.1435*Gauss+iFF*0.6137*Gauss,iY*14.286*Gauss,iZ*10.3675*Gauss};
Return[Sqrt[VecField.VecField]]
]

EnergyBR[F_,M_,B_,isotope_]:= Module[{gI, ahf,xBR},
Which[isotope=="Rb87",gI=-0.000995141,
isotope=="K39", gI =\[Minus]0.00014193489,
isotope=="K41",gI =\[Minus]0.00007790600];
Which[isotope=="Rb87",ahf = 3.417341305452*10^9*hPlanck,
isotope=="K39", ahf = 230.8598601*hPlanck*MHz,
isotope=="K41",ahf = 127.0069352*hPlanck*MHz];
xBR = ((gs-gI)*muBohr)/(ahf*(Inuc+1/2))*B;
Which[
F==Inuc-1/2,-(ahf/4) +gI muBohr M B-(ahf*(Inuc+1/2))/2 Sqrt[1+(4 M)/(2 Inuc+1) xBR + xBR^2],
F==Inuc+1/2,
If[M==-(Inuc+1/2),-(ahf/4)+gI muBohr M B +(ahf*(Inuc+1/2))/2 (1-xBR),
If[M==(Inuc+1/2),-(ahf/4)+gI muBohr M B +(ahf*(Inuc+1/2))/2 (1+xBR),
-(ahf/4)+gI muBohr M B +(ahf*(Inuc+1/2))/2 Sqrt[1+(4 M)/(2 Inuc+1) xBR + xBR^2]]]]
];

Gravity[z_,isotope_]:= Module[{mass},
If[isotope=="Rb87",mass=mRb87,mass=mK39];
Return[mass*gravity*z]
];

PE[x_,y_,z_,currents_,trapparams_,gravity_]:= Module[{isotope,F,mF,d1,d2,U},
isotope = trapparams[[1]];
F = trapparams[[2]];
mF = trapparams[[3]];
U = EnergyBR[F,mF,ChipTrapField[x,y,z,currents,trapparams],isotope]-gravity*Gravity[z,isotope];
Return[U]
];

PEFAST[x_,y_,z_,currents_,trapparams_,gravity_]:= Module[{isotope,F,mF,d1,d2,PE},
isotope = trapparams[[1]];
F = trapparams[[2]];
mF = trapparams[[3]];
PE =EnergyBR[F,mF, ChipTrapFieldFAST[x,y,z,currents,trapparams],isotope]-gravity*Gravity[z,isotope];
Return[PE]
];

zmin[currents_,trapparams_,gravity_,ylim_] := Module[{xi,yi,zi,x,y,z,zlim},
Which[gravity==0,zlim=3mm,
gravity==1,zlim=0.5mm];
{xi,yi,zi} = NMinimize[{1000*PE[x,y,z,currents,trapparams,gravity]/kB,-0.5mm<x<0.5mm && ylim-0.5mm<y<ylim+0.5mm &&0.003mm<z<zlim},{x,y,z},WorkingPrecision->10][[2]];
xi = xi[[2]];
yi = yi[[2]];
zi = zi[[2]];
Return[{xi,yi,zi}]
];

zminFAST[currents_,trapparams_,gravity_,ylim_] :=Module[{xi,yi,zi,x,y,z,zlim},
Which[gravity==0,zlim=1.5*mm,
gravity==1,zlim=3*mm];
{xi,yi,zi} = NMinimize[{1000*PEFAST[x,y,z,currents,trapparams,gravity]/kB,-0.5mm<x<0.5mm && ylim-0.5mm<y<ylim+0.5mm &&0.003mm<z<zlim},{x,y,z},WorkingPrecision->10][[2]];
xi = xi[[2]];
yi = yi[[2]];
zi = zi[[2]];
Return[{xi,yi,zi}]
];

ChipTrapFreq[xmin_,ymin_,zmin_,currents_,trapparams_]:= Module[{x,y,z,d1,d2,isotope,iD1,iD2,iT,iM,iY,iZ,iFF,Dxx,Dxy,Dxz,Dyy,Dyz,Dzz,DMatrix,DMatrixDiag,mass,\[Omega]x,\[Omega]y,\[Omega]z,\[Omega]},
isotope = trapparams[[1]];
Which[isotope=="Rb87",mass=mRb87,
	isotope=="K39",mass=mK39,
	isotope=="K41",mass=mK39];
Dxx=D[PE[x,y,z,currents,trapparams,0],{x,2}];
Dxy=D[PE[x,y,z,currents,trapparams,0],x,y];
Dxz=D[PE[x,y,z,currents,trapparams,0],x,z];
Dyy=D[PE[x,y,z,currents,trapparams,0],{y,2}];
Dyz=D[PE[x,y,z,currents,trapparams,0],y,z];
Dzz=D[PE[x,y,z,currents,trapparams,0],{z,2}];
DMatrix=({
 {Dxx, Dxy, Dxz},
 {Dxy, Dyy, Dyz},
 {Dxz, Dyz, Dzz}
});
DMatrixDiag=Eigensystem[DMatrix/.{x->xmin,y->ymin,z->zmin}];
\[Omega]= Sqrt[Abs[DMatrixDiag[[1]]]/mass]/(2\[Pi]);(* in Hz *)
\[Omega]x = \[Omega][[3]];
\[Omega]y = \[Omega][[2]];
\[Omega]z = \[Omega][[1]];
Return[{\[Omega]x,\[Omega]y,\[Omega]z}];
];

ChipTrapFreqFAST[xmin_,ymin_,zmin_,currents_,trapparams_,gravity_]:= Module[{x,y,z,d1,d2,isotope,iD1,iD2,iT,iM,iY,iZ,iFF,Dxx,Dxy,Dxz,Dyy,Dyz,Dzz,DMatrix,DMatrixDiag,mass,\[Omega]x,\[Omega]y,\[Omega]z,\[Omega]},
isotope = trapparams[[1]];
Which[isotope=="Rb87",mass=mRb87,
	isotope=="K39",mass=mK39,
	isotope=="K41",mass=mK39];
Dxx=D[PEFAST[x,y,z,currents,trapparams,gravity],{x,2}];
Dxy=D[PEFAST[x,y,z,currents,trapparams,gravity],x,y];
Dxz=D[PEFAST[x,y,z,currents,trapparams,gravity],x,z];
Dyy=D[PEFAST[x,y,z,currents,trapparams,gravity],{y,2}];
Dyz=D[PEFAST[x,y,z,currents,trapparams,gravity],y,z];
Dzz=D[PEFAST[x,y,z,currents,trapparams,gravity],{z,2}];
DMatrix=({
 {Dxx, Dxy, Dxz},
 {Dxy, Dyy, Dyz},
 {Dxz, Dyz, Dzz}
});
DMatrixDiag=Eigensystem[DMatrix/.{x->xmin,y->ymin,z->zmin}];
\[Omega]= Sqrt[Abs[DMatrixDiag[[1]]]/mass]/(2\[Pi]);(* in Hz *)
\[Omega]x = \[Omega][[3]];
\[Omega]y = \[Omega][[2]];
\[Omega]z = \[Omega][[1]];
Return[{\[Omega]x,\[Omega]y,\[Omega]z}];
];

ExtFieldFreq[xmin_,ymin_,zmin_,currents_,isotope_]:= Module[{iT,iM,iY,iZ,iFF,mass,Dxx,Dyy,Dzz,x,y,z,\[Omega]x,\[Omega]y,\[Omega]z,transfer,MOT,Ybias,Zbias,FF},
iT = currents[[4]];
iM = currents[[5]];
iY = currents[[6]];
iZ = currents[[7]];
iFF = currents[[8]];
If[isotope=="Rb87",mass=mRb87,mass=mK39];
transfer = BiasCoils[TransferCoils[iT]];
MOT = BiasCoils[MOTCoils[iM]];
Ybias = BiasCoils[YbiasCoils[iY]];
Zbias = BiasCoils[ZbiasCoils[iZ]];
FF=BiasCoils[FFCoils[iFF]];
Dxx = D[FieldMag[x,ymin,zmin,transfer,MOT,Ybias,Zbias,FF],{x,2}]/.x-> ymin;
Dyy = D[FieldMag[xmin,y,zmin,transfer,MOT,Ybias,Zbias,FF],{y,2}]/.y-> zmin;
Dzz = D[FieldMag[xmin,ymin,z,transfer,MOT,Ybias,Zbias,FF],{z,2}]/.z-> xmin;
\[Omega]x = Sqrt[muBohr*gjmj*(Dxx)/mass];
\[Omega]y = Sqrt[muBohr*gjmj*(Dyy)/mass];
\[Omega]z = Sqrt[muBohr*gjmj*(Dzz)/mass];
Return[{{\[Omega]z,\[Omega]x,\[Omega]y}/(2\[Pi]),FieldMag[ymin,zmin,xmin,transfer,MOT,Ybias,Zbias,FF]/Gauss}]
];

End[] (*End Private Context*)

EndPackage[]













































