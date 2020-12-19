The Move instruction operates on the Register Machine, and _copies_ data from
one register into the other.

---------------------------- MODULE LAM_ISA_Move ----------------------------
EXTENDS Naturals, Sequences

CONSTANTS InstrCount, RegisterCount, Nil, Literals

RegisterKinds == {"local", "global"}
RegisterIdx == (0..RegisterCount)

MoveKind == {"register", "literal"}

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

procedure make_move()
variables
  src = << CHOOSE k \in RegisterKinds : TRUE, CHOOSE n \in RegisterIdx : TRUE >>,
  lit = CHOOSE l \in Literals : TRUE,
  dst = << CHOOSE k \in RegisterKinds : TRUE, CHOOSE n \in RegisterIdx : TRUE >>
begin
MakeMove:
  either PutLiteral:
    current_move := [ src |-> lit,  dst |-> dst ];
  or CopyFromRegister:
    current_move := [ src |-> src,  dst |-> dst ];
  end either;
end procedure;

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
      Make: call make_move();
      Perform: call perform_move(current_move);
    end while;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "b4f1efbd" /\ chksum(tla) = "919f4a2e")
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

VARIABLES src, lit, dst, move

vars == << global_registers, local_registers, current_count, current_move, pc, 
           stack, src, lit, dst, move >>

Init == (* Global variables *)
        /\ global_registers = [ id \in RegisterIdx |-> Nil ]
        /\ local_registers = [ id \in RegisterIdx |-> Nil ]
        /\ current_count = InstrCount
        /\ current_move = Nil
        (* Procedure make_move *)
        /\ src = << CHOOSE k \in RegisterKinds : TRUE, CHOOSE n \in RegisterIdx : TRUE >>
        /\ lit = (CHOOSE l \in Literals : TRUE)
        /\ dst = << CHOOSE k \in RegisterKinds : TRUE, CHOOSE n \in RegisterIdx : TRUE >>
        (* Procedure perform_move *)
        /\ move = defaultInitValue
        /\ stack = << >>
        /\ pc = "Run"

MakeMove == /\ pc = "MakeMove"
            /\ \/ /\ pc' = "PutLiteral"
               \/ /\ pc' = "CopyFromRegister"
            /\ UNCHANGED << global_registers, local_registers, current_count, 
                            current_move, stack, src, lit, dst, move >>

PutLiteral == /\ pc = "PutLiteral"
              /\ current_move' = [ src |-> lit,  dst |-> dst ]
              /\ pc' = "Error"
              /\ UNCHANGED << global_registers, local_registers, current_count, 
                              stack, src, lit, dst, move >>

CopyFromRegister == /\ pc = "CopyFromRegister"
                    /\ current_move' = [ src |-> src,  dst |-> dst ]
                    /\ pc' = "Error"
                    /\ UNCHANGED << global_registers, local_registers, 
                                    current_count, stack, src, lit, dst, move >>

make_move == MakeMove \/ PutLiteral \/ CopyFromRegister

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
               /\ UNCHANGED << current_count, current_move, src, lit, dst >>

perform_move == PerformMove

Run == /\ pc = "Run"
       /\ IF current_count > 0
             THEN /\ current_count' = current_count - 1
                  /\ pc' = "Make"
             ELSE /\ pc' = "Done"
                  /\ UNCHANGED current_count
       /\ UNCHANGED << global_registers, local_registers, current_move, stack, 
                       src, lit, dst, move >>

Make == /\ pc = "Make"
        /\ stack' = << [ procedure |->  "make_move",
                         pc        |->  "Perform",
                         src       |->  src,
                         lit       |->  lit,
                         dst       |->  dst ] >>
                     \o stack
        /\ src' = << CHOOSE k \in RegisterKinds : TRUE, CHOOSE n \in RegisterIdx : TRUE >>
        /\ lit' = (CHOOSE l \in Literals : TRUE)
        /\ dst' = << CHOOSE k \in RegisterKinds : TRUE, CHOOSE n \in RegisterIdx : TRUE >>
        /\ pc' = "MakeMove"
        /\ UNCHANGED << global_registers, local_registers, current_count, 
                        current_move, move >>

Perform == /\ pc = "Perform"
           /\ /\ move' = current_move
              /\ stack' = << [ procedure |->  "perform_move",
                               pc        |->  "Run",
                               move      |->  move ] >>
                           \o stack
           /\ pc' = "PerformMove"
           /\ UNCHANGED << global_registers, local_registers, current_count, 
                           current_move, src, lit, dst >>

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == pc = "Done" /\ UNCHANGED vars

Next == make_move \/ perform_move \/ Run \/ Make \/ Perform
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(pc = "Done")

\* END TRANSLATION 


=============================================================================
\* Modification History
\* Last modified Sat Dec 19 13:47:57 CET 2020 by ostera
\* Created Sat Dec 19 11:40:24 CET 2020 by ostera
