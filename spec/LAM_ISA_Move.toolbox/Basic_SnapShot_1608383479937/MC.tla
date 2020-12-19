---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_160838347391730000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_160838347391731000 == 
Permutations(const_160838347391730000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_160838347391732000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_160838347391733000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 14:11:13 CET 2020 by ostera
