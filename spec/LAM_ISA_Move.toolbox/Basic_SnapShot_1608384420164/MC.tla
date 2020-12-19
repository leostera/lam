---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_16083844141237000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_16083844141238000 == 
Permutations(const_16083844141237000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_16083844141239000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_160838441412310000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 14:26:54 CET 2020 by ostera
