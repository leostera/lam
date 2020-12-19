---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_160838344487325000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_160838344487326000 == 
Permutations(const_160838344487325000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_160838344487327000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_160838344487328000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 14:10:44 CET 2020 by ostera
