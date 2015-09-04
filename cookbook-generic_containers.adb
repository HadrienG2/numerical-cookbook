-- Copyright 2015 Hadrien Grasland
--
-- This file is part of Numerical Cookbook.
--
-- Numerical Cookbook is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Numerical Cookbook is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with Numerical Cookbook.  If not, see <http://www.gnu.org/licenses/>.


package body Cookbook.Generic_Containers is

   overriding function "=" (Left, Right : Matrix) return Boolean is
   begin
      for Row in Left'Range (1) loop
         for Col in Left'Range (2) loop
            if Left (Row, Col) /= Right (Row - Left'First (1) + Right'First (1), Col - Left'First (2) + Right'First (2)) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end "=";


   function "*" (Left : Item_Type; Right : Matrix) return Matrix is
   begin
      return Result : Matrix (Right'Range (1), Right'Range (2)) do
         for I in Right'Range (1) loop
            for J in Right'Range (2) loop
               Result (I, J) := Left * Right (I, J);
            end loop;
         end loop;
      end return;
   end "*";


   function "*" (Left : Item_Type; Right : Vector) return Vector is
   begin
      return Result : Vector (Right'Range) do
         for I in Right'Range loop
            Result (I) := Left * Right (I);
         end loop;
      end return;
   end "*";


   function "*" (Left, Right : Matrix) return Matrix is
   begin
      return Result : Matrix (Left'Range (1), Right'Range (2)) := (others => (others => Zero)) do
         for I in Left'Range (1) loop
            for K in Right'Range (2) loop
               for J in Left'Range (2) loop
                  Result (I, K) := Result (I, K) + Left (I, J) * Right (J - Left'First (2) + Right'First (1), K);
               end loop;
            end loop;
         end loop;
      end return;
   end "*";


   function "*" (Left : Matrix; Right : Vector) return Vector is
   begin
      return Result : Vector (Left'Range (1)) := (others => Zero) do
         for I in Left'Range (1) loop
            for J in Left'Range (2) loop
               Result (I) := Result (I) + Left (I, J) * Right (J - Left'First (2) + Right'First);
            end loop;
         end loop;
      end return;
   end "*";


   function "*" (Left : Vector; Right : Matrix) return Vector is
   begin
      return Result : Vector (Right'Range (2)) := (others => Zero) do
         for I in Left'Range loop
            for J in Right'Range (2) loop
               Result (J) := Result (J) + Left (I) * Right (I - Left'First + Right'First (1), J);
            end loop;
         end loop;
      end return;
   end "*";


   function "*" (Left, Right : Vector) return Item_Type is
   begin
      return Result : Item_Type := Zero do
         for I in Left'Range loop
            Result := Result + Left (I) * Right (I - Left'First + Right'First);
         end loop;
      end return;
   end "*";


   function Identity_Matrix (Size : Size_Type) return Matrix is
   begin
      return Result : Matrix (Index_Type'First .. Index_Type'Pred (Index_Type'First + Size),
                              Index_Type'First .. Index_Type'Pred (Index_Type'First + Size)) := (others => (others => Zero)) do
         for I in Result'Range (1) loop
            Result (I, I) := One;
         end loop;
      end return;
   end Identity_Matrix;

end Cookbook.Generic_Containers;
