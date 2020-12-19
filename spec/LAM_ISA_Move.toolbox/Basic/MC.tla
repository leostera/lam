---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_160838467995417000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_160838467995418000 == 
Permutations(const_160838467995417000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_160838467995419000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_160838467995420000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 14:31:19 CET 2020 by ostera
