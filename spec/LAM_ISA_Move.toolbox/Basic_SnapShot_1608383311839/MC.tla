---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_160838330580720000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_160838330580721000 == 
Permutations(const_160838330580720000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_160838330580722000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_160838330580723000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 14:08:25 CET 2020 by ostera
