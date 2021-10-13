(* ::Package:: *)

(* RbGroundStateAlgebra.m *)
(* Created on: 29 September 2021 *)
(* Created by: JDM (joseph.murphree@protonmail.com) *)

BeginPackage["RbGroundStateAlgebra`"]
(* A package to contain algebraic quantities and (possibly) functions related to the ground state
manifolds of rubidium. *)

(* NOTE! The package clears the definition of \[HBar] ! *)

(*
The order of the components is as follows for the coupled and uncoupled bases, respectively:
index: |F, m_F>
1: |2,2>
2: |2,1>
3: |2,0>
4: |2,-1>
5: |2,-2>
6: |1,1>
7: |1,0>
8: |1,-1>

index: |I=3/2, J=1/2, m_I, m_J>
1: |3/2, 1/2>
2: |3/2, -1/2>
3: |1/2, 1/2>
4: |1/2, -1/2>
5: |-1/2, 1/2>
6: |-1/2, -1/2>
7: |-3/2, 1/2>
8: |-3/2, -1/2>
*)

(* 
Helpful tips:
SparseArray[{{#}->1,{8}] constructs a list of length 8 where the #th element is set to 1 
and the rest are 0. It is a compact way of creating the #th unit vector. 

Most matrices are defined using the following convention:
first two letters identify the operator,
last one or two characters describe the basis.

e.g. fxij is the F_x operator expressed in the uncoupled (mI, mJ) basis, while
fyf is the F_y operator expressed int he coupled (F mF) basis.
*)

\[HBar]::usage = "Reduced Planck's constant. No numerical value is assigned within this package. The numerical value is in the package BatesConstants.m as hbar.";

fInds::usage = "A list of the spin indices {F, mF} for the coupled (F = I + J) basis.";
ijInds::usage = "A list of the spin indices {mI, mJ} for the uncoupled (I, J) basis.";

uftoij::usage = "A matrix for converting vectors in the coupled basis vF into vectors in
the uncoupled basis vIJ via vIJ = uftoij.vF.

As such, it can also be used, e.g. to convert matrices in the uncoupled basis aF to the 
uncoupled basis aIJ by aIJ = Transpose[uftoij].aIJ.uftoij .";

ipij::usage = "The raising ('plus') operator of the nuclear angular momentum expressed in the 
uncoupled (mI, mJ) basis.";

imij::usage = "The lowering ('minus') operator of the nuclear angular momentum expressed in the uncoupled
(mI, mJ) basis.";

ixij::usage = "The x component of the nuclear angular momentum operator expressed in the uncoupled (mI, mJ) basis.";

ixf::usage = "The x component of the nuclear angular momentum operator expressed in the coupled (F, mF) basis.";

iyij::usage = "The y component of the nuclear angular momentum operator expressed in the uncoupled (mI, mJ) basis.";

iyf::usage = "The y component of the nuclear angular momentum operator expressed in the coupled (F, mF) basis.";

izij::usage = "The z component of the nuclear angular momentum operator expressed in the uncoupled (mI, mJ) basis.";

izf::usage = "The z component of the nuclear angular momentum operator expressed in the coupled (F, mF) basis.";

ip1f::usage = "The +1 spherical tensor component of the nuclear angular momentum operator I_1 expressed in the coupled (F, mF) basis.";

im1f::usage = "The -1 spherical tensor component of the nuclear angular momentum operator I_-1 expressed in the coupled (F, mF) basis.";

isqrdij::usage = "The square of the nuclear angular momentum operator I^2 expressed in the uncoupled (mI, mJ) basis. It is identical in the other basis.";

isqrdf::usage = "The square of the nuclear angular momentum operator I^2 expressed in the coupled (F, mF) basis. It is identical in the other basis.";

jpij::usage = "The raising ('plus') operator of the net electronic angular momentum J_+ expressed in the 
uncoupled (mI, mJ) basis.";

jmij::usage = "The lowering ('minus') operator of the net electronic angular momentum J_- expressed in the uncoupled
(mI, mJ) basis.";

jxij::usage = "The x component of the net electronic angular momentum operator J_x expressed in the uncoupled (mI, mJ) basis.";

jxf::usage = "The x component of the net electronic angular momentum operator J_x expressed in the coupled (F, mF) basis.";

jyij::usage = "The y component of the net electronic angular momentum operator J_y expressed in the uncoupled (mI, mJ) basis.";

jyf::usage = "The y component of the net electronic angular momentum operator J_y expressed in the coupled (F, mF) basis.";

jzij::usage = "The z component of the net electronic angular momentum operator J_z expressed in the uncoupled (mI, mJ) basis.";

jzf::usage = "The z component of the net electronic angular momentum operator J_z expressed in the coupled (F, mF) basis.";

jp1f::usage = "The +1 spherical tensor component of the net electronic angular momentum operator J_1 expressed in the coupled (F, mF) basis.";

jm1f::usage = "The -1 spherical tensor component of the net electronic angular momentum operator J_-1 expressed in the coupled (F, mF) basis.";

idjij::usage = "The operator I dot J expressed in the uncoupled (mI, mJ) basis.";

idjf::usage = "The operator I dot J expressed in the coupled (F, mF) basis.";

fpf::usage = "The raising ('plus') operator of the net atomic angular momentum F_+ expressed in the 
coupled (F, mF) basis.";

fmf::usage = "The lowering ('minus') operator of the net atomic angular momentum F_- expressed in the uncoupled
(F, mF) basis.";

fxij::usage = "The x component of the net atomic angular momentum operator F_x expressed in the uncoupled (mI, mJ) basis.";

fxf::usage = "The x component of the net atomic angular momentum operator F_x expressed in the coupled (F, mF) basis.";

fyij::usage = "The y component of the net atomic angular momentum operator F_y expressed in the uncoupled (mI, mJ) basis.";

fyf::usage = "The y component of the net atomic angular momentum operator F_y expressed in the coupled (F, mF) basis.";

fzij::usage = "The z component of the net atomic angular momentum operator F_z expressed in the uncoupled (mI, mJ) basis.";

fzf::usage = "The z component of the net atomic angular momentum operator F_z expressed in the coupled (F, mF) basis.";



Begin["`Private`"]

ClearAll[\[HBar]];

fInds={{2,2},{2,1},{2,0},{2,-1},{2,-2},{1,1},{1,0},{1,-1}}; (* {F, mF} *)
ijInds={{3/2,1/2},{3/2,-1/2},{1/2,1/2},{1/2,-1/2},{-1/2,1/2},{-1/2,-1/2},{-3/2,1/2},{-3/2,-1/2}}; (* {mI, mJ} *)

(* A matrix to convert between the coupled and uncoupled bases, using Clebsch-Gordon coefficients from Griffiths[?] *)
uftoij=Transpose@{
{1,0,0,0,0,0,0,0},
{0,1/2,Sqrt[3]/2,0,0,0,0,0},
{0,0,0,1/Sqrt[2],1/Sqrt[2],0,0,0},
{0,0,0,0,0,-Sqrt[3]/2,-1/2,0},
{0,0,0,0,0,0,0,-1},
{0,Sqrt[3]/2,-1/2,0,0,0,0,0},
{0,0,0,1/Sqrt[2],-1/Sqrt[2],0,0,0},
{0,0,0,0,0,-1/2,Sqrt[3]/2,0}
};

(* Total[#^2]&/@uftoij (* Check that each row is normalized *) *)
(* uftoij.SparseArray[{{#}->1},{8}]&/@Range[8] (* Check that each of the basis vectors is
transformed correctly into the uncoupled basis. *) *)

ipij=\[HBar]*{
{0,0,Sqrt[3],0,0,0,0,0},
{0,0,0,Sqrt[3],0,0,0,0},
{0,0,0,0,2,0,0,0},
{0,0,0,0,0,2,0,0},
{0,0,0,0,0,0,Sqrt[3],0},
{0,0,0,0,0,0,0,Sqrt[3]},
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,0,0,0}
};

imij=\[HBar]*{
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,0,0,0},
{Sqrt[3],0,0,0,0,0,0,0},
{0,Sqrt[3],0,0,0,0,0,0},
{0,0,2,0,0,0,0,0},
{0,0,0,2,0,0,0,0},
{0,0,0,0,Sqrt[3],0,0,0},
{0,0,0,0,0,Sqrt[3],0,0}
};

ixij=(1/2)*(ipij+imij);

ixf=(Transpose[uftoij] . ixij . uftoij); (* Agrees with manual calculations *)

iyij=(1/(2*I))*(ipij-imij);

iyf=Transpose[uftoij] . iyij . uftoij;

izij=(\[HBar]/2)*{
{3,0,0,0,0,0,0,0},
{0,3,0,0,0,0,0,0},
{0,0,1,0,0,0,0,0},
{0,0,0,1,0,0,0,0},
{0,0,0,0,-1,0,0,0},
{0,0,0,0,0,-1,0,0},
{0,0,0,0,0,0,-3,0},
{0,0,0,0,0,0,0,-3}
};

izf=Transpose[uftoij] . izij . uftoij;

ip1f=(-1/Sqrt[2])*(ixf+I*iyf);

im1f=(1/Sqrt[2])*(ixf-I*iyf);

isqrdij = izij . izij+ixij . ixij+iyij . iyij;

isqrdf = izf . izf+ixf . ixf+iyf . iyf;

(* J operators *)

jpij=\[HBar]*{
{0,1,0,0,0,0,0,0},
{0,0,0,0,0,0,0,0},
{0,0,0,1,0,0,0,0},
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,1,0,0},
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,0,0,1},
{0,0,0,0,0,0,0,0}
};

(*jpij.SparseArray[{{#}->1},{8}]&/@Range[8] (* Check that it acts as expected on the basis states. *) *)

jmij=\[HBar]*{
{0,0,0,0,0,0,0,0},
{1,0,0,0,0,0,0,0},
{0,0,0,0,0,0,0,0},
{0,0,1,0,0,0,0,0},
{0,0,0,0,0,0,0,0},
{0,0,0,0,1,0,0,0},
{0,0,0,0,0,0,0,0},
{0,0,0,0,0,0,1,0}
};

(*jmij.SparseArray[{{#}->1},{8}]&/@Range[8] (* Check that it acts as expected on the basis states. *) *)

jxij=(1/2)*(jpij+jmij);

jxf=Transpose[uftoij] . jxij . uftoij;

jyij=(1/(2*I))*(jpij-jmij);

jyf=Transpose[uftoij] . jyij . uftoij;

jzij=(\[HBar]/2)*{
{1,0,0,0,0,0,0,0},
{0,-1,0,0,0,0,0,0},
{0,0,1,0,0,0,0,0},
{0,0,0,-1,0,0,0,0},
{0,0,0,0,1,0,0,0},
{0,0,0,0,0,-1,0,0},
{0,0,0,0,0,0,1,0},
{0,0,0,0,0,0,0,-1}
};

jzf=Transpose[uftoij] . jzij . uftoij;

jp1f=(-1/Sqrt[2])*(jxf+I*jyf);

jm1f=(1/Sqrt[2])*(jxf-I*jyf);

(* I dot J *)

idjij = ixij . jxij+iyij . jyij+izij . jzij;

idjf = ixf . jxf+iyf . jyf+izf . jzf;

(* F operators *)

fxf=(jxf+ixf);

fxij=(jxij+ixij);

fyf=(jyf+iyf);

fyij=(jyij+iyij);

fxf=(jxf+ixf);

fxij=(jxij+ixij);

fzf=(jzf+izf);

fzij=(jzij+izij);

fpf=fxf+I*fyf;

fmf=fxf-I*fyf;
End[]


EndPackage[]
