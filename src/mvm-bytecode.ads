package MVM.ByteCode is
    type Opcode is
       (Halt, Noop, Load_Immediate, Push, Pop, Dup, Swap, Load_Top, Over, Inc, Dec,
        Add, Sub, Mul, Div, Eq, Neq, Lt, Lte, Gt, Gte, Inv, Jmp, Jump_If_Zero,
        Jump_If_Not_Zero, Call, Ret) with
       Size => 8;

    type Instruction_Immediate is range -32_768 .. 32_767 with
       Size => 16;
    type Instruction_Address is range 0 .. 1_024 with
       Size => 16;

    type Instruction (Op : Opcode := Halt) is record
        case Op is
            when Load_Immediate =>
                Immediate : Instruction_Immediate;
            when Jmp | Jump_If_Zero | Jump_If_Not_Zero | Call =>
                Address : Instruction_Address;
            when others =>
                null;
        end case;
    end record;
end MVM.ByteCode;
