with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

package AOC2024 is

   package Integer_Vectors is new
      Ada.Containers.Vectors
         (Index_Type => Natural, Element_Type => Integer);
   
   package Integer_Vectors_Sorting is new Integer_Vectors.Generic_Sorting;

   package Integer_Ordered_Maps is new
      Ada.Containers.Indefinite_Ordered_Maps
         (Key_Type => Integer,
          Element_Type => Natural);

   package D1 is
      function Q1 return Integer;
      function Q2 return Integer;
   end D1;

   package d2 is
      function q1 return natural;
      function q2 return natural;
   end d2;

   package d3 is
      function q1 return natural;
      function q2 return natural;
   end d3;

end AOC2024;