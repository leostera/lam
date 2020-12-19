---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_160838208029210000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_160838208029211000 == 
Permutations(const_160838208029210000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_160838208029212000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_160838208029213000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 13:48:00 CET 2020 by ostera
