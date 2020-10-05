(* ::Package:: *)

(* Based on: https://mathematica.stackexchange.com/a/111134 *)
rearrange2DFourierArray[array_]:=Module[
{dim = Dimensions[array][[1]]},
Return[Transpose@RotateLeft[
Transpose@RotateLeft[array,dim/2],dim/2]];
];
