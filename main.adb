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


with Cookbook;
with Cookbook.Float_Containers;
with Cookbook.Float_Utility;
-- with Cookbook.Linear_Equations;
-- with Cookbook.Linear_Equations.Gauss_Jordan;
-- with Cookbook.Linear_Equations.LU_Decomp;
-- with Cookbook.Linear_Equations.Tridiagonal;

procedure Main is
   -- We take float equality as equality up to a difference of one significant digit for nonzero numbers,
   -- and absolute value less than float prevision for comparison with zero.
   function Relatively_Equal (F1, F2 : Float) return Boolean is
      Comparison_Resolution : constant Float := 10.0**(-(Float'Digits - 1));
      Min_F : constant Float := (if (abs F1) < (abs F2) then F1 else F2);
      Max_F : constant Float := (if (abs F1) > (abs F2) then F1 else F2);
   begin
      if Min_F /= 0.0 then
         return abs ((Max_F - Min_F) / Min_F) < Comparison_Resolution;
      else
         return abs Max_F < Comparison_Resolution;
      end if;
   end Relatively_Equal;

   -- Instantiate the whole cookbook
   package Float_Cookbook is new Cookbook (Float_Type => Float, "=" => Relatively_Equal, Index_Type => Positive, Size_Type => Natural);
   package Float_Utility is new Float_Cookbook.Float_Utility with Unreferenced;
   package Float_Containers is new Float_Cookbook.Float_Containers with Unreferenced;
   -- package Linear_Equations is new Float_Cookbook.Linear_Equations (F_Containers => Float_Containers,
   --                                                                  F_Utility => Float_Utility);
   -- package Gauss_Jordan is new Linear_Equations.Gauss_Jordan with Unreferenced;
   -- package LU_Decomp is new Linear_Equations.LU_Decomp with Unreferenced;
   -- package Tridiagonal is new Linear_Equations.Tridiagonal with Unreferenced;
begin
   null;
end Main;
