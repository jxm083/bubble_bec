(* ::Package:: *)

(* Based on: https://mathematica.stackexchange.com/a/111134 *)
rearrange2DFourierArray[array_]:=Module[
{dim = Dimensions[array][[1]]},
Return[Transpose@RotateLeft[
Transpose@RotateLeft[array,dim/2],dim/2]];
];


opticalTransferFunction[delz_,k0_,kVec2D_]:=Module[
{},
Return[Exp[I*delz*Sqrt[k0^2-kVec2D.kVec2D]]];
];


propagateFreeSpace[electricField_,delz_,k0_]:=Module[{},Return[]];


propagateMedium[electricField_,delz_,k0_]:=Module[
{},
Return[];
];
