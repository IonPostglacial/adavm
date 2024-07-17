with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings;       use Ada.Strings;
with MVM.ByteCode;      use MVM.ByteCode;

package body MVM.ASM is

    procedure Load_Code_From_File
       (C : in out Code; Src_File : in out File_Type)
    is
        F          : Positive;
        L          : Natural;
        Line       : Instruction_Address    := 0;
        Immediate  : Instruction_Immediate;
        Address    : Instruction_Address;
        Whitespace : constant Character_Set := To_Set (' ');
    begin
        Open (Src_File, In_File, "sample.maf");
        while not End_Of_File (Src_File) loop
            declare
                Current_Line : String := Get_Line (Src_File);
            begin
                Find_Token
                   (Source => Current_Line, Set => Whitespace, From => 1,
                    Test   => Outside, First => F, Last => L);
                if Current_Line (F .. L) = "load" then
                    Find_Token
                       (Source => Current_Line, Set => Whitespace,
                        From => L + 1, Test => Outside, First => F, Last => L);
                    Immediate :=
                       Instruction_Immediate'Value (Current_Line (F .. L));
                    C (Line) := (Op => Load_Immediate, Immediate => Immediate);
                elsif Current_Line (F .. L) = "push" then
                    C (Line) := (Op => Push);
                elsif Current_Line (F .. L) = "pop" then
                    C (Line) := (Op => Pop);
                elsif Current_Line (F .. L) = "dup" then
                    C (Line) := (Op => Dup);
                elsif Current_Line (F .. L) = "swap" then
                    C (Line) := (Op => Swap);
                elsif Current_Line (F .. L) = "ldt" then
                    C (Line) := (Op => Load_Top);
                elsif Current_Line (F .. L) = "over" then
                    C (Line) := (Op => Over);
                elsif Current_Line (F .. L) = "inc" then
                    C (Line) := (Op => Inc);
                elsif Current_Line (F .. L) = "dec" then
                    C (Line) := (Op => Dec);
                elsif Current_Line (F .. L) = "add" then
                    C (Line) := (Op => Add);
                elsif Current_Line (F .. L) = "sub" then
                    C (Line) := (Op => Sub);
                elsif Current_Line (F .. L) = "mul" then
                    C (Line) := (Op => Mul);
                elsif Current_Line (F .. L) = "div" then
                    C (Line) := (Op => Div);
                elsif Current_Line (F .. L) = "eq" then
                    C (Line) := (Op => Eq);
                elsif Current_Line (F .. L) = "neq" then
                    C (Line) := (Op => Neq);
                elsif Current_Line (F .. L) = "lt" then
                    C (Line) := (Op => Lt);
                elsif Current_Line (F .. L) = "lte" then
                    C (Line) := (Op => Lte);
                elsif Current_Line (F .. L) = "gt" then
                    C (Line) := (Op => Gt);
                elsif Current_Line (F .. L) = "gte" then
                    C (Line) := (Op => Gte);
                elsif Current_Line (F .. L) = "not" then
                    C (Line) := (Op => Inc);
                elsif Current_Line (F .. L) = "jmp" then
                    Find_Token
                       (Source => Current_Line, Set => Whitespace,
                        From => L + 1, Test => Outside, First => F, Last => L);
                    Address  :=
                       Instruction_Address'Value (Current_Line (F .. L));
                    C (Line) := (Op => Jmp, Address => Address);
                elsif Current_Line (F .. L) = "jz" then
                    Find_Token
                       (Source => Current_Line, Set => Whitespace,
                        From => L + 1, Test => Outside, First => F, Last => L);
                    Address  :=
                       Instruction_Address'Value (Current_Line (F .. L));
                    C (Line) := (Op => Jump_If_Zero, Address => Address);
                elsif Current_Line (F .. L) = "jnz" then
                    Find_Token
                       (Source => Current_Line, Set => Whitespace,
                        From => L + 1, Test => Outside, First => F, Last => L);
                    Address  :=
                       Instruction_Address'Value (Current_Line (F .. L));
                    C (Line) := (Op => Jump_If_Not_Zero, Address => Address);
                elsif Current_Line (F .. L) = "call" then
                    Find_Token
                       (Source => Current_Line, Set => Whitespace,
                        From => L + 1, Test => Outside, First => F, Last => L);
                    Address  :=
                       Instruction_Address'Value (Current_Line (F .. L));
                    C (Line) := (Op => Call, Address => Address);
                elsif Current_Line (F .. L) = "ret" then
                    C (Line) := (Op => Ret);
                end if;
            end;
            Line := Line + 1;
        end loop;
        Close (Src_File);
    end Load_Code_From_File;
end MVM.ASM;
