with Ada.Numerics;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers; use Ada.Containers;

package body AOC2024 is

   package body D1 is

      type Input is record
         Left : Integer_Vectors.Vector;
         Right : Integer_Vectors.Vector;
      end record;

      function Read_Input return Input is
         F : File_Type;
         File_Name : constant String := "src/input/2024/d1q1.txt";
         Left_Number : Integer;
         Right_Number : Integer;
         Left_List : Integer_Vectors.Vector;
         Right_List : Integer_Vectors.Vector;
      begin
         Open (F, In_File, File_Name);
         while not End_Of_File(F) loop
            Ada.Integer_Text_IO.Get (F, Left_Number);
            Ada.Integer_Text_IO.Get (F, Right_Number);
            Left_List.Append(Left_Number);
            Right_List.Append(Right_Number);
         end loop;
         Close(F);
         return (Left => Left_List, Right => Right_List);
      end Read_Input;

      function Q1 return Integer is
         use Integer_Vectors_Sorting;
         Arrays : Input;
         Distance : Integer := 0;
      begin
         Arrays := Read_Input;
         Sort(Arrays.Left);
         Sort(Arrays.Right);
         
         for I in Arrays.Left.First_Index .. Arrays.Left.Last_Index loop
            Distance := Distance + abs (Arrays.Left.Element(I) - Arrays.Right.Element(I));
         end loop;

         return Distance;
      end Q1;

      function Q2 return Integer is
         use Integer_Ordered_Maps;
         Arrays : Input;
         Similarity : Integer := 0;
         Counts : Map;
      begin
         Arrays := Read_Input;
         for E of Arrays.Right loop
            if Counts.Contains(E) then
               Counts(E) := Counts(E) + 1;
            else
               Counts.Include(E, 1);
            end if;
         end loop;

         for E of Arrays.Left loop
            if Counts.Contains(E) then
               Similarity := Similarity + (E * Counts(E));
            end if;
         end loop;

         return Similarity;
      end Q2;

   end D1;

end AOC2024;