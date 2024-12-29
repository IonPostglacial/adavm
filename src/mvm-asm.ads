with Ada.Text_IO; use Ada.Text_IO;
with MVM.Machine; use MVM.Machine;

package MVM.ASM is

   procedure Load_Code_From_File
     (C : in out Code; Src_File : in out File_Type);

end MVM.ASM;
