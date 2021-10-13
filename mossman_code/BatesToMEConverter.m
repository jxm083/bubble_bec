(* ::Package:: *)

(*Mathematica package*)
(* 
Used to convert between Bates and Mossman--Engels conventions.

General notes on the terminology:

Current driver names:
Driver 1 \[Rule] AZ1
Driver 2 \[Rule] AZ2
Driver 3 \[Rule] H1+H2
Transfer coils \[Rule] T1 AND(!) T2 [[NOT INDIVIDUALLY ADDRESSABLE??]]
MOT coils \[Rule] X1 AND(!) X2
Y bias coils \[Rule] Y
Z bias coils \[Rule] Z
FF coils \[Rule] AZ2 w/ AZ2_sel\[Rule]2

*)
BeginPackage["BatesToMEConverter`"]
(*Exported symbols added here with SymbolName::usage*)

(* Constants *)

(* Functions *)
convertCurrentsBToME::"convertCurrentsBTOME[batesCurrents_]";

convertCoorBToME::"convertCoorBToME[batesCoordinates_] [m]";

convertCoorMEToB::"convertCoorMEToB[meCoordinates_]";

Begin["`Private`"]
(* Begin Private context *)

convertCurrentsBToME[batesCurrents_]:=
Module[
{
(* Bates currents *)
currLa = batesCurrents[[1]],
currZa = batesCurrents[[2]],
currLb = batesCurrents[[3]],
currZb = batesCurrents[[4]],
currH = batesCurrents[[5]],
currBiasX = batesCurrents[[6]],
currBiasY = batesCurrents[[7]],
currBiasZ = batesCurrents[[8]],
(* ME currents *)
IDriver1,
IDriver2,
IDriver3,
ITransferCoils,
IMOTCoils,
IYBiasCoils,
IZBiasCoils,
IFFCoils,
(* Check input validity *)
validCurr1, validCurr2
},
(* Bates convention doesn't define currents for the
MOT or Fast-Feschbach coils, so we set those values to 0: *)
IMOTCoils = 0.;
IFFCoils = 0.;
(* The bias coils correspond directly: *)
ITransferCoils = currBiasX;
IYBiasCoils = currBiasY;
IZBiasCoils = currBiasZ;
(* Driver three is responsible only for the H wires, and so can
be assigned directly: *)
IDriver3 = currH;
(* Wires Za and Lb are both driven by driver 1 (AZ1) and
wires Zb and La are driven by driver 2 (AZ2). Thus one current
of each of those pairs should be 0 [OR UNDEFINED!]. *)
IDriver1 = currZa + currLb;
IDriver2 = currZb + currLa;
validCurr1 = (IDriver1==currZa Or IDriver1==currLb);
validCurr2 = (IDriver2==currZb Or IDriver2==currLa);
(* Check that the input currents are valid, accurately reflecting
from which driver they are driven. *)
If[Not@validCurr1,
Return["Either currZa (currents[[2]]) or currLb (currents[[3]])
must be 0, because they are driven with the same driver."]
];
If[Not@validCurr2,
Return["Either currZb (currents[[4]]) or currLa (currents[[1]])
must be 0, because they are driven with the same driver."]
];

Return[
{IDriver1, IDriver2, IDriver3, 
ITransferCoils, IMOTCoils, IYBiasCoils, IZBiasCoils, IFFCoils}];
];

convertCoorBToME[batesCoordinates_]:=
{#[[1]], #[[2]]-0.001, #[[3]]}&[batesCoordinates];

convertCoorMEToB[meCoordinates_]:=
{#[[1]], #[[2]]+0.001, #[[3]]}&[meCoordinates];

End[]

EndPackage[]
