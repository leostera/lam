---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_16083843723282000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_16083843723283000 == 
Permutations(const_16083843723282000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_16083843723284000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_16083843723285000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 14:26:12 CET 2020 by ostera
