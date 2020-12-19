The Move instruction operates on the Register Machine, and _copies_ data from
one register into the other.

---------------------------- MODULE LAM_ISA_Move ----------------------------
EXTENDS Naturals, Sequences

CONSTANTS InstrCount, RegisterCount, Nil, Literals

RegisterKinds == {"local", "global"}
RegisterIdx == (0..RegisterCount)
Values == RegisterKinds \union { "literal" }

Registers == RegisterKinds \X RegisterIdx

(* --algorithm LAM_ISA_Move
variables
  global_registers = [ id \in RegisterIdx |-> Nil ],
  local_registers = [ id \in RegisterIdx |-> Nil ],
  current_count = InstrCount,
  current_move = Nil
  ;

define

  AllRegistersAreValid == \/ current_move = Nil
                          \/ /\ current_move.dst[1] \in RegisterKinds
                             /\ current_move.src[1] \in RegisterKinds
                             /\ current_move.dst[2] \in RegisterIdx
                             /\ current_move.src[2] \in RegisterIdx

  TypeInvariant == /\ current_count \in Nat
                   /\ AllRegistersAreValid


end define;

procedure perform_move(move) begin
PerformMove:
  if move.dst[1] = "global" /\ move.src[1] = "global" then
    global_registers[move.dst[2]] := global_registers[move.src[2]]
  elsif move.dst[1] = "local" /\ move.src[1] = "local" then
    local_registers[move.dst[2]] := local_registers[move.src[2]]
  elsif move.dst[1] = "global" /\ move.src[1] = "local" then
    global_registers[move.dst[2]] := local_registers[move.src[2]]
  elsif move.dst[1] = "local" /\ move.src[1] = "global" then
    local_registers[move.dst[2]] := global_registers[move.src[2]]
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
        if use_lit then
          current_move := [ src |-> lit,  dst |-> dst ];
        else
          current_move := [ src |-> src,  dst |-> dst ];
        end if;
        call perform_move(current_move);
      end with;
    end while;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "a9864123" /\ chksum(tla) = "18e17f2c")
CONSTANT defaultInitValue
VARIABLES global_registers, local_registers, current_count, current_move, pc, 
          stack

(* define statement *)
AllRegistersAreValid == \/ current_move = Nil
                        \/ /\ current_move.dst[1] \in RegisterKinds
                           /\ current_move.src[1] \in RegisterKinds
                           /\ current_move.dst[2] \in RegisterIdx
                           /\ current_move.src[2] \in RegisterIdx

TypeInvariant == /\ current_count \in Nat
                 /\ AllRegistersAreValid

VARIABLE move

vars == << global_registers, local_registers, current_count, current_move, pc, 
           stack, move >>

Init == (* Global variables *)
        /\ global_registers = [ id \in RegisterIdx |-> Nil ]
        /\ local_registers = [ id \in RegisterIdx |-> Nil ]
        /\ current_count = InstrCount
        /\ current_move = Nil
        (* Procedure perform_move *)
        /\ move = defaultInitValue
        /\ stack = << >>
        /\ pc = "Run"

PerformMove == /\ pc = "PerformMove"
               /\ IF move.dst[1] = "global" /\ move.src[1] = "global"
                     THEN /\ global_registers' = [global_registers EXCEPT ![move.dst[2]] = global_registers[move.src[2]]]
                          /\ UNCHANGED local_registers
                     ELSE /\ IF move.dst[1] = "local" /\ move.src[1] = "local"
                                THEN /\ local_registers' = [local_registers EXCEPT ![move.dst[2]] = local_registers[move.src[2]]]
                                     /\ UNCHANGED global_registers
                                ELSE /\ IF move.dst[1] = "global" /\ move.src[1] = "local"
                                           THEN /\ global_registers' = [global_registers EXCEPT ![move.dst[2]] = local_registers[move.src[2]]]
                                                /\ UNCHANGED local_registers
                                           ELSE /\ IF move.dst[1] = "local" /\ move.src[1] = "global"
                                                      THEN /\ local_registers' = [local_registers EXCEPT ![move.dst[2]] = global_registers[move.src[2]]]
                                                      ELSE /\ TRUE
                                                           /\ UNCHANGED local_registers
                                                /\ UNCHANGED global_registers
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
                             /\ IF use_lit
                                   THEN /\ current_move' = [ src |-> lit,  dst |-> dst ]
                                   ELSE /\ current_move' = [ src |-> src,  dst |-> dst ]
                             /\ /\ move' = current_move'
                                /\ stack' = << [ procedure |->  "perform_move",
                                                 pc        |->  "Run",
                                                 move      |->  move ] >>
                                             \o stack
                             /\ pc' = "PerformMove"
             ELSE /\ pc' = "Done"
                  /\ UNCHANGED << current_count, current_move, stack, move >>
       /\ UNCHANGED << global_registers, local_registers >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == perform_move \/ Run
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 


=============================================================================
\* Modification History
\* Last modified Sat Dec 19 14:07:16 CET 2020 by ostera
\* Created Sat Dec 19 11:40:24 CET 2020 by ostera
