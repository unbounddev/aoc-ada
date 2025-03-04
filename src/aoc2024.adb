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

   package body d2 is
      function q1 return natural is
         package int_io renames ada.integer_text_io;
         f : file_type;
         file_name : constant string := "src/input/2024/d2q1.txt";
         safe_reports : natural := 0;
         prev_num : integer;
         curr_num : integer;
      begin
         -- How many reports are safe?
         -- Each line is a report
           -- Are all increasing or decreasing?
           -- Do all adjacent numbers only vary by 1-3?
         -- Check if numbers should be increasing or decreasing based on first two numbers
         -- If any number varies < 1 or > 3 then report fails 
         open(f, in_file, file_name);
         <<process_line>>
         while not end_of_file(f) loop
            declare
               line : string := get_line(f);
               last : natural := 0;
               count : natural := 0;
               increasing : boolean := false;
               decreasing : boolean := false;
            begin
               <<process_num>>
               while last < line'length loop
                  if count < 1 then
                     int_io.get(line(last+1..line'last), prev_num, last);
                     count := count + 1;
                     goto process_num;
                  elsif count < 2 then
                     int_io.get(line(last+1..line'last), curr_num, last);
                     if prev_num <= curr_num then
                        increasing := true;
                        decreasing := false;
                     else
                        increasing := false;
                        decreasing := true;
                     end if;
                  else
                     prev_num := curr_num;
                     int_io.get(line(last+1..line'last), curr_num, last);
                  end if;
                  count := count + 1;

                  if (prev_num <= curr_num and not increasing) or
                     (prev_num > curr_num and not decreasing) then
                     goto process_line;
                  end if;

                  if abs(curr_num-prev_num) < 1 or abs(curr_num-prev_num) > 3 then
                     goto process_line;
                  end if;
                  
               end loop;
               safe_reports := safe_reports + 1;
            end;
         end loop;
         close(f);

         return safe_reports;
      end q1;
   end d2;

end AOC2024;