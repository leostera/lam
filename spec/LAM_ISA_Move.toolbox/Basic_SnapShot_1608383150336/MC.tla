---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_160838314430515000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_160838314430516000 == 
Permutations(const_160838314430515000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_160838314430517000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_160838314430518000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 14:05:44 CET 2020 by ostera
