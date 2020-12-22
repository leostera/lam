The Move instruction operates on the Register Machine, and _copies_ values to
a local or global register.

Values can be literals, which are copied directly, or other registers. When the
value being moved is a register, we look up the literal inside it and copy that
instead.

The representation of literal values isn't important, so an implementation may
copy the actual value or just a heap pointer.

All registers begin zeroed out with the Nil value.

---------------------------- MODULE LAM_ISA_Move ----------------------------
EXTENDS Naturals, Sequences, TLC

CONSTANTS InstrCount, RegisterCount, Nil, Literals

RegisterKinds == {"local", "global"}
RegisterIdx == (0..RegisterCount)
ValueKinds == RegisterKinds \union { "literal" }
Values == RegisterIdx \union Literals

Registers == RegisterKinds \X RegisterIdx

(* --algorithm LAM_ISA_Move
variables
  registers = [ r \in Registers |-> Nil ],
  current_count = InstrCount,
  current_move = Nil
  ;

define

  AllRegistersAreValid == \/ current_move = Nil
                          \/ /\ current_move.dst[1] \in RegisterKinds
                             /\ current_move.dst[2] \in RegisterIdx
                             /\ \/ /\ current_move.src[1] \in RegisterKinds
                                   /\ current_move.src[2] \in RegisterIdx
                                \/ /\ current_move.src[1] \in { "literal" }
                                   /\ current_move.src[2] \in Literals

  TypeInvariant == /\ current_count \in Nat
                   /\ AllRegistersAreValid


end define;

procedure perform_move(move) begin
PerformMove:
  if move.src[1] = "literal" then registers[move.dst] := move.src[2]
  else registers[move.dst] := registers[move.src]
  end if;
  return;
end procedure;

begin
  Run:
    while current_count > 0 do
      current_count := current_count - 1;
      with src \in Registers,
           dst \in Registers,
           use_lit \in {TRUE, FALSE},
           lit \in Literals do
        (* Lets assert that the last move we did was actually carried out! *)
        assert (
               \/ current_move = Nil
               \/ /\ current_move.src[1] = "literal"
                  /\ registers[current_move.dst] = current_move.src[2]
               \/ registers[current_move.dst] = registers[current_move.src]
               ) = TRUE;
        if use_lit then
          current_move := [ src |-> <<"literal", lit>>,  dst |-> dst ];
        else
          current_move := [ src |-> src,  dst |-> dst ];
        end if;
        call perform_move(current_move);
      end with;
    end while;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "abda9f12" /\ chksum(tla) = "99d227dd")
CONSTANT defaultInitValue
VARIABLES registers, current_count, current_move, pc, stack

(* define statement *)
AllRegistersAreValid == \/ current_move = Nil
                        \/ /\ current_move.dst[1] \in RegisterKinds
                           /\ current_move.dst[2] \in RegisterIdx
                           /\ \/ /\ current_move.src[1] \in RegisterKinds
                                 /\ current_move.src[2] \in RegisterIdx
                              \/ /\ current_move.src[1] \in { "literal" }
                                 /\ current_move.src[2] \in Literals

TypeInvariant == /\ current_count \in Nat
                 /\ AllRegistersAreValid

VARIABLE move

vars == << registers, current_count, current_move, pc, stack, move >>

Init == (* Global variables *)
        /\ registers = [ r \in Registers |-> Nil ]
        /\ current_count = InstrCount
        /\ current_move = Nil
        (* Procedure perform_move *)
        /\ move = defaultInitValue
        /\ stack = << >>
        /\ pc = "Run"

PerformMove == /\ pc = "PerformMove"
               /\ IF move.src[1] = "literal"
                     THEN /\ registers' = [registers EXCEPT ![move.dst] = move.src[2]]
                     ELSE /\ registers' = [registers EXCEPT ![move.dst] = registers[move.src]]
               /\ pc' = Head(stack).pc
               /\ move' = Head(stack).move
               /\ stack' = Tail(stack)
               /\ UNCHANGED << current_count, current_move >>

perform_move == PerformMove

Run == /\ pc = "Run"
       /\ IF current_count > 0
             THEN /\ current_count' = current_count - 1
                  /\ \E src \in Registers:
                       \E dst \in Registers:
                         \E use_lit \in {TRUE, FALSE}:
                           \E lit \in Literals:
                             /\ Assert((
                                       \/ current_move = Nil
                                       \/ /\ current_move.src[1] = "literal"
                                          /\ registers[current_move.dst] = current_move.src[2]
                                       \/ registers[current_move.dst] = registers[current_move.src]
                                       ) = TRUE, 
                                       "Failure of assertion at line 62, column 9.")
                             /\ IF use_lit
                                   THEN /\ current_move' = [ src |-> <<"literal", lit>>,  dst |-> dst ]
                                   ELSE /\ current_move' = [ src |-> src,  dst |-> dst ]
                             /\ /\ move' = current_move'
                                /\ stack' = << [ procedure |->  "perform_move",
                                                 pc        |->  "Run",
                                                 move      |->  move ] >>
                                             \o stack
                             /\ pc' = "PerformMove"
             ELSE /\ pc' = "Done"
                  /\ UNCHANGED << current_count, current_move, stack, move >>
       /\ UNCHANGED registers

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == perform_move \/ Run
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 


=============================================================================
\* Modification History
\* Last modified Sat Dec 19 14:51:37 CET 2020 by ostera
\* Created Sat Dec 19 11:40:24 CET 2020 by ostera
