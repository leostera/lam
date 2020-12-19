---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_160838446477612000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_160838446477613000 == 
Permutations(const_160838446477612000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_160838446477614000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_160838446477615000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 14:27:44 CET 2020 by ostera
