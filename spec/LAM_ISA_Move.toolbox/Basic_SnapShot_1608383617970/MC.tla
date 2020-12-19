---- MODULE MC ----
EXTENDS LAM_ISA_Move, TLC

\* MV CONSTANT declarations@modelParameterConstants
CONSTANTS
a, b, c, d
----

\* MV CONSTANT definitions Literals
const_160838360174235000 == 
{a, b, c, d}
----

\* SYMMETRY definition
symm_160838360174236000 == 
Permutations(const_160838360174235000)
----

\* CONSTANT definitions @modelParameterConstants:2RegisterCount
const_160838360174237000 == 
8
----

\* CONSTANT definitions @modelParameterConstants:3InstrCount
const_160838360174238000 == 
10
----

=============================================================================
\* Modification History
\* Created Sat Dec 19 14:13:21 CET 2020 by ostera
