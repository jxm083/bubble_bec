(* ::Package:: *)

(* BatesConstants.m *)
(* Created on: 15 September 2021 *)
(* Created by: JDM (joseph.murphree@protonmail.com) *)

BeginPackage["BatesConstants`"]
(* A package to contain all of the constants, mostly fundamental and rubidium related, used in the other notebooks. *)
(* 2018 CODATA values (https://physics.nist.gov/cuu/Constants/Table/allascii.txt) *)
kB::usage = "Boltzmann constant [J/K]";
h::usage = "Planck constant [J/Hz]";
hbar::usage = "Reduced Planck constant [J/(rad/s)]";
muB::usage = "Bohr magneton [J/T]";
mu0::usage = "vacuum magnetic permeability [N/A^2]";

(* From values collected in Daniel Adam Steck's "Rubidium 87 D Line Data" revision 2.2.2,  http://steck.us/alkalidata *)
mRb87::usage = "Mass of rubidium-87 [kg]";
ahfsS12::usage = "Magnetic dipole constant, 5 ^2S_{1/2} [J]";

gS::usage = "Electron spin g-factor";
gL::usage = "Electron orbital g-factor";
gJS12::usage = "Fine structure Lande g-factor, 5 ^2S_{1/2}";
gJP12::usage = "Fine structure Lande g-factor, 5 ^2P_{1/2}";
gJP32::usage = "Fine structure Lande g-factor, 5 ^2P_{3/2}";
gI::usage = "Nuclear g-factor";


Begin["`Private`"]
(* 2018 CODATA *)
kB = 1.380649*10^(-23); (* [J/K] *)
h = 6.62607015*10^(-34); (* [J/Hz] *)
hbar = h/(2*Pi); (* [J/(rad/s)] *)
muB = 9.2740100783*10^(-24); (* [J/T] *)
mu0 = 1.25663706212*10^(-6); (* [N/A^2] *)

(* Steck *)
mRb87 = 1.443160648*10^(-25); (* [kg] *)

ahfsS12 = h*(3.417341305452145*10^9); (* [J] *)

gS = 2.0023193043622;
gL = 0.99999369;
gJS12 = 2.00233113;
gJP12 = 0.666;
gJP32 = 1.3362;
gI = \[Minus]0.0009951414;

End[]


EndPackage[]
