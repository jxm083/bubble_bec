(* ::Package:: *)

(* HamiltonianFunctions.m *)
(* Created on: 14 October 2021 *)
(* Created by: JDM (joseph.murphree@protonmail.com) *)

BeginPackage["HamiltonianFunctions`"]
(* A package containing functions to aid in working with the Hamiltonian. *)

groupLevels::usage = "groupLevels[\[CapitalDelta]_,\[CapitalOmega]_,x_,y_,z_,ham_] takes the values that are fed to a Hamiltonian function (
\[CapitalDelta] detuning [Hz]
\[CapitalOmega] Rabi frequency [Hz]
x, y, z, trap position [m]
along with the (numeric) Hamiltonian function ham which takes the argument ham[\[CapitalDelta],\[CapitalOmega],x,y,z] and uses Eigensystem to calculate the eigenvalues and eigenvectors of the Hamiltonian.

It then returns a list of the indices of these eigenvalues, grouped according to their energy and ordered from lowest to highest.

For our eight level system in the case of rf coupling, e.g., it returns
{{1,2,3},{8,7,6,5,4}},
where the sublists correspond to the F=1 and F=2 manifolds, respectively.

It is also useful for identifying our desired dressed energy level, which is the highest energy state of the highest energy dressed manifold for rf coupling and the lowest energy dressed manifold for uW coupling.
";


Begin["`Private`"]

groupLevels[\[CapitalDelta]_,\[CapitalOmega]_,x_,y_,z_,ham_]:=Module[
{evalFunc,evals,evalGroups,groupThreshold=1*10^9 (* [Hz] *),evalGroupLabels={},j,k},
evalFunc[\[CapitalDelta]t_,\[CapitalOmega]t_,xt_,yt_,zt_]:=Eigensystem[ham[\[CapitalDelta]t,\[CapitalOmega]t,xt,yt,zt]][[1]];(* Used Eigensystem instead of eigenvalues so that the ordering would be the same. Postfix "t" indicates "temporary" variable. *)
(* This function is defined in the same way that the AdiabaticEnergiesChip* functions. *)

(* Create a list of eigenvalues (energies/frequencies) as found at the position (x,y,z). *)
evals=evalFunc[\[CapitalDelta],\[CapitalOmega],x,y,z];

(* Group the eigenvalues that are within groupThreshold of each other. This will be something like 1 GHz, which is enough to encompass an energy manifold in either the rf or microwave regimes, but not two. *)
(* These groups are then sorted from lowest to highest frequency by their first elements. *)
evalGroups=SortBy[Gather[evals,Abs[#1-#2]<=groupThreshold&],First];
(* Sort the eigenvalues from lowest to highest within each manifold. *)
evalGroups=Table[Sort[evalGroups[[k]]],{k,Range@Length@evalGroups}];

(*Print["Length evals: "<>ToString@Length@evals];
Print["Length evalGroups: "<>ToString@Length@evalGroups];*)

evalGroupLabels=evalGroups; (* Make sure that both lists have the same dimensions. *)

For[j=1,j<=Length@evalGroups,j++,
For[k=1,k<=Length@evalGroups[[j]],k++,
evalGroupLabels[[j,k]]=FirstPosition[evals,evalGroups[[j,k]]][[1]];
];
];

Assert[Flatten[Length@evalGroupLabels]==Length[evals]]; (* Make sure that the returned list of group labels contains only as many elements as the list of eigenvalues. *)

Return[evalGroupLabels];
];

End[]


EndPackage[]
