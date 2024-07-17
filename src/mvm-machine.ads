with MVM.ByteCode; use MVM.ByteCode;

package MVM.Machine is
    Stack_Size       : constant Integer := 16_000;
    Call_Stack_Size  : constant Integer := 128;

    type Stack_Index is range 0 .. Stack_Size;
    type Code is array (Instruction_Address) of Instruction;

    type Stack is array (Stack_Index) of Integer;
    type Call_Stack is array (Instruction_Address) of Instruction_Address;

    type Machine is record
        PC               : Instruction_Address;
        SP               : Stack_Index;
        FP               : Instruction_Address;
        Acc              : Integer;
        Stack_Cells      : Stack;
        Call_Stack_Cells : Call_Stack;
    end record;

    procedure Machine_Run_Code (M : in out Machine; C : Code);
end MVM.Machine;
