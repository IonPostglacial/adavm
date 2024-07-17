package body MVM.Machine is
    function Machine_Stack_Pop (M : in out Machine) return Integer is
    begin
        M.SP := M.SP - 1;
        return M.Stack_Cells (M.SP);
    end Machine_Stack_Pop;

    procedure Machine_Stack_Push (M : in out Machine; Value : Integer) is
    begin
        M.Stack_Cells (M.SP) := Value;
        M.SP                 := M.SP + 1;
    end Machine_Stack_Push;

    procedure Machine_Execute_Instruction
       (M : in out Machine; Inst : Instruction)
    is
        Tmp : Integer;
    begin
        case Inst.Op is
            when Halt | Noop =>
                null;
            when Load_Immediate =>
                M.Acc := Integer (Inst.Immediate);
            when Push =>
                Machine_Stack_Push (M, M.Acc);
            when Pop =>
                M.Acc := Machine_Stack_Pop (M);
            when Dup =>
                Machine_Stack_Push (M, M.Stack_Cells (M.SP));
            when Swap =>
                Tmp                      := M.Stack_Cells (M.SP);
                M.Stack_Cells (M.SP)     := M.Stack_Cells (M.SP - 1);
                M.Stack_Cells (M.SP - 1) := Tmp;
            when Load_Top =>
                M.Acc := M.Stack_Cells (M.SP);
            when Over =>
                M.Acc := M.Stack_Cells (M.SP - 1);
            when Inc =>
                M.Acc := M.Acc + 1;
            when Dec =>
                M.Acc := M.Acc - 1;
            when Add =>
                Tmp   := Machine_Stack_Pop (M);
                M.Acc := M.Acc + Tmp;
            when Sub =>
                Tmp   := Machine_Stack_Pop (M);
                M.Acc := M.Acc - Tmp;
            when Mul =>
                Tmp   := Machine_Stack_Pop (M);
                M.Acc := M.Acc * Tmp;
            when Div =>
                Tmp   := Machine_Stack_Pop (M);
                M.Acc := M.Acc / Tmp;
            when Eq =>
                Tmp := Machine_Stack_Pop (M);
                if M.Acc = Tmp then
                    M.Acc := 1;
                else
                    M.Acc := 0;
                end if;
            when Neq =>
                Tmp := Machine_Stack_Pop (M);
                if M.Acc /= Tmp then
                    M.Acc := 1;
                else
                    M.Acc := 0;
                end if;
            when Lt =>
                Tmp := Machine_Stack_Pop (M);
                if M.Acc < Tmp then
                    M.Acc := 1;
                else
                    M.Acc := 0;
                end if;
            when Lte =>
                Tmp := Machine_Stack_Pop (M);
                if M.Acc <= Tmp then
                    M.Acc := 1;
                else
                    M.Acc := 0;
                end if;
            when Gt =>
                Tmp := Machine_Stack_Pop (M);
                if M.Acc > Tmp then
                    M.Acc := 1;
                else
                    M.Acc := 0;
                end if;
            when Gte =>
                Tmp := Machine_Stack_Pop (M);
                if M.Acc >= Tmp then
                    M.Acc := 1;
                else
                    M.Acc := 0;
                end if;
            when Jmp =>
                M.PC := Inst.Address;
                return;
            when Inv =>
                if M.Acc = 1 then
                    M.Acc := 0;
                else
                    M.Acc := 1;
                end if;
            when Jump_If_Zero =>
                if M.Acc = 0 then
                    M.PC := Inst.Address;
                end if;
                return;
            when Jump_If_Not_Zero =>
                if M.Acc /= 0 then
                    M.PC := Inst.Address;
                end if;
                return;
            when Call =>
                M.FP                      := M.FP + 1;
                M.Call_Stack_Cells (M.FP) := M.PC;
                M.PC                      := Inst.Address;
                return;
            when Ret =>
                M.FP := M.FP - 1;
                M.PC := M.Call_Stack_Cells (M.FP);
        end case;
        M.PC := M.PC + 1;
    end Machine_Execute_Instruction;

    procedure Machine_Run_Code (M : in out Machine; C : Code) is
        Inst : Instruction;
    begin
        loop
            Inst := C (M.PC);
            exit when Inst.Op = Halt;
            Machine_Execute_Instruction (M, Inst);
        end loop;
    end Machine_Run_Code;
end MVM.Machine;
