with Ada.Text_IO; use Ada.Text_IO;
with MVM.Machine;  use MVM.Machine;
with MVM.ByteCode; use MVM.ByteCode;
with MVM.ASM; use MVM.ASM;

procedure Adavm is
   MMachine   : Machine;
   SampleCode : Code;
   F          : File_Type;
begin
   Load_Code_From_File(SampleCode, F);
   Machine_Run_Code (MMachine, SampleCode);
   Ada.Text_IO.Put_Line ("PC: " & MMachine.PC'Image);
   Ada.Text_IO.Put_Line ("SP: " & MMachine.SP'Image);
   Ada.Text_IO.Put_Line ("FP: " & MMachine.FP'Image);
   Ada.Text_IO.Put_Line ("Acc: " & MMachine.Acc'Image);
end Adavm;
