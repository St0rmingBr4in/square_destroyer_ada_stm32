------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with STM32.Board;           use STM32.Board;
with HAL.Bitmap;            use HAL.Bitmap;
--with HAL.Touch_Panel;       use HAL.Touch_Panel;
with STM32.User_Button;     use STM32;
with BMP_Fonts;
with LCD_Std_Out;
with Ada.Numerics.discrete_Random;

package body Square_Destroyer is

procedure Init_Grid(g : out Grid) is
    package Rand_Int is new Ada.Numerics.Discrete_Random(Square);
    gen : Rand_Int.Generator;
begin
    Rand_Int.Reset(gen);
    for i in g'Range(1) loop
        for j in g'Range(2) loop
            g(i, j) := Square(Rand_Int.Random(gen));
        end loop;
    end loop;
end Init_Grid;


procedure Square_Destroyer
is
   type ColorMap is array(Square) of HAL.Bitmap.Bitmap_Color;
   m : constant ColorMap := (HAL.Bitmap.Blue, HAL.Bitmap.Green, HAL.Bitmap.Red,
                             HAL.Bitmap.Yellow, HAL.Bitmap.Magenta,
                             HAL.Bitmap.Cyan);
   BG : constant Bitmap_Color := (Alpha => 255, others => 0);

   Rect_Pos   : Point := (0, 0);
   r : Rect := (Rect_Pos, 39, 39);
   g : Grid;
begin
    Init_Grid(g);
   --  Initialize LCD
   Display.Initialize;
   Display.Initialize_Layer (1, ARGB_8888);

   --  Initialize touch panel
   Touch_Panel.Initialize;

   --  Initialize button
   User_Button.Initialize;

   LCD_Std_Out.Set_Font (BMP_Fonts.Font8x8);
   LCD_Std_Out.Current_Background_Color := BG;

   --  Clear LCD (set background)
   Display.Hidden_Buffer (1).Set_Source (BG);
   Display.Hidden_Buffer (1).Fill;

   LCD_Std_Out.Clear_Screen;
   Display.Update_Layer (1, Copy_Back => True);

   loop
    --  declare
    --     State : constant TP_State := Touch_Panel.Get_All_Touch_Points;
    --  begin
    --     case State'Length is
    --        when 1 =>
    --           Ball_Pos := (State (State'First).X, State (State'First).Y);
    --        when others => null;
    --     end case;
    --  end;

    for i in g'Range(1) loop
        for j in g'Range(2) loop
            Display.Hidden_Buffer (1).Set_Source (m(g(i,j)));
            Rect_Pos := ((i - g'First(1)) * 40, (j - g'First(2)) * 40);
            r := (Rect_Pos, 39, 39);
            Display.Hidden_Buffer (1).Fill_Rect (r);
        end loop;
    end loop;
     --  Update screen
      Display.Update_Layer (1, Copy_Back => True);

   end loop;
end Square_Destroyer;

end Square_Destroyer;
