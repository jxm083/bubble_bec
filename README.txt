These programs model the potentials of the Cold Atom Laboratory, beginning with Science Module 2, and including both the potentials created by currents on the chip and those from externally applied radio frequency fields.

bubble-4a-murphree.nb is the primary Mathematica document and currently defines the functions which define the potentials.

Short_Ramps-murphree.nb is used to create ramps of the trap's currents that adiabatically transfer the atomic cloud from where the atoms are Bose condensed to where they are expanded into a dressed potential, following the protocol outlined by Sackett et al. in "Extreme adiabatic expansion in micro-gravity: modeling for the cold atomic laboratory," Microgravity-Science and Technology (2018).

Both notebooks depend upon packages stored in mathematica_packages.

Their text-based output is stored in the directory output.

NathanVersionCourtneyGP contains MATLAB code that takes the potentials predicted by bubble-4a and models the evolution of a Bose--Einstein condensate in such a potential according to the Gross--Pitaevskii equation.
