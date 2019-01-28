with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with BMP_Fonts;
with HAL;                   use HAL;
with HAL.Touch_Panel;
with LCD_Std_Out;
with STM32.Board;           use STM32.Board;
with STM32.RNG.Interrupts;
with STM32.User_Button;     use STM32;

package body Square_Destroyer is

---------- Comparison function for PointSet -----------------------------------

   function Sort_By_Height (A : Point; B : Point) return Boolean is
   begin
      if A.Y = B.Y then
         return A.X > B.X;
      else
         return A.Y > B.Y;
      end if;
   end Sort_By_Height;

---------- Helper functions for contracts -------------------------------------

   function Is_Grid_Valid (G : Grid) return Boolean is
   begin
      for I in G'Range (1) loop
         for J in G'Range (2) loop
            if not G (I, J)'Valid then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Grid_Valid;

   function Is_In_Grid (P : Point) return Boolean is
   begin
      return P.X in 1 .. GRID_WIDTH and then P.Y in 1 .. GRID_HEIGHT;
   end Is_In_Grid;

---------- Random Wrapper -----------------------------------------------------

   function Get_Random_Square return Square is
   begin
      return Square'Val (RNG.Interrupts.Random mod
                         UInt32 (Square'Pos (Square'Last) + 1));
   end Get_Random_Square;

---------- Init procedures ----------------------------------------------------

   procedure Init_Grid (G : out Grid) is
   begin
      RNG.Interrupts.Initialize_RNG;

      for I in G'Range (1) loop
         for J in G'Range (2) loop
            loop
               G (I, J) := Get_Random_Square;
               exit when not Is_Match_3 (G, I, J);
            end loop;
         end loop;
      end loop;
   end Init_Grid;

   procedure Init_Board is
      BG : constant Bitmap_Color := (Alpha => 255, others => 0);
   begin
      --  Initialize LCD
      Display.Initialize;
      Display.Initialize_Layer (1, ARGB_8888);

      --  Initialize touch panel
      STM32.Board.Touch_Panel.Initialize;

      --  Initialize button
      User_Button.Initialize;

      LCD_Std_Out.Set_Font (BMP_Fonts.Font8x8);
      LCD_Std_Out.Current_Background_Color := BG;

      --  Clear LCD (set background)
      Display.Hidden_Buffer (1).Set_Source (BG);
      Display.Hidden_Buffer (1).Fill;

      LCD_Std_Out.Clear_Screen;
      Display.Update_Layer (1, Copy_Back => True);
   end Init_Board;

---------- Game loop procedures and functions ---------------------------------

   procedure Get_Input (Last_Square : out Optional_Point;
                        Cur_Square  : in out Optional_Point;
                        Just_Moved  : in out Boolean) is
      State : constant HAL.Touch_Panel.TP_State :=
         STM32.Board.Touch_Panel.Get_All_Touch_Points;
   begin
      case State'Length is
         when 0 =>
            Just_Moved := False;
         when 1 =>
            if not Just_Moved then
               Last_Square := Cur_Square;
               Cur_Square :=
               (Valid => True,
                P => ((State (State'First).X / SQUARE_SIZE) + Grid'First (1),
                      (State (State'First).Y / SQUARE_SIZE) + Grid'First (2)));
            end if;
         when others => null;
      end case;
   end Get_Input;

   procedure Blink (G : Grid; WorkListMove : PointSet.Set; Score : Natural) is
   begin
      for i in 1 .. 10 loop
         for S of WorkListMove loop
            declare
               R : Rect  := ((0, 0), SQUARE_SURFACE_SIZE, SQUARE_SURFACE_SIZE);
            begin
               Display.Hidden_Buffer (1).Set_Source (HAL.Bitmap.White);
               R.Position := ((S.X - G'First (1)) * SQUARE_SIZE,
               (S.Y - G'First (2)) * SQUARE_SIZE);
               Display.Hidden_Buffer (1).Fill_Rect (R);
            end;
         end loop;
         LCD_Std_Out.Put (0, 0, Score'Image);
         Display.Update_Layer (1, Copy_Back => True);
         delay Duration (0.01);
         Draw_Grid (G);
         LCD_Std_Out.Put (0, 0, Score'Image);
         Display.Update_Layer (1, Copy_Back => True);
      end loop;
   end Blink;

   procedure Process_Moves (G            : in out Grid;
                            WorkListMove : in out PointSet.Set;
                            Score        : in out Natural) is
      WorkListCombination : PointSet.Set;
      Multiplier          : Natural := 1;
   begin
      while not PointSet.Is_Empty (WorkListMove) loop
         Blink (G, WorkListMove, Score);
         Score := Score + ((MATCH_3_VALUE +
                           (Natural (PointSet.Length (WorkListMove)) - 3)
                              * BONUS_PER_SQUARE) * Multiplier);
         while not PointSet.Is_Empty (WorkListMove) loop
            declare
               Element : constant Point :=
                  PointSet.First_Element (WorkListMove);
                  Tmp_Y : Integer := Element.Y;
            begin
               while Tmp_Y > 0 loop
                  exit when not PointSet.Contains (WorkListMove,
                  (Element.X, Tmp_Y));
                  Tmp_Y := Tmp_Y - 1;
               end loop;
               if Tmp_Y = 0 then
                  G (Element.X, Element.Y) := Get_Random_Square;
               else
                  G (Element.X, Element.Y) := G (Element.X, Tmp_Y);
                  PointSet.Include (WorkListMove, (Element.X, Tmp_Y));
               end if;
               PointSet.Delete_First (WorkListMove);
               PointSet.Include (WorkListCombination, Element);
            end;
         end loop;
         while not PointSet.Is_Empty (WorkListCombination) loop
            Is_Move_Legal (G, PointSet.First_Element (WorkListCombination),
            WorkListMove);
            PointSet.Delete_First (WorkListCombination);
         end loop;
         Multiplier := Multiplier + 1;
      end loop;
   end Process_Moves;

   function Is_Unsolvable (G : in out Grid) return Boolean
   is
      Solution : PointSet.Set;
   begin
      for I in G'Range (1) loop
         for J in G'Range (2) loop
            if I > G'First (1) then
               Swap (G, (I, J), (I - 1, J));
               Is_Move_Legal (G, (I, J), Solution);
               Swap (G, (I, J), (I - 1, J));
            end if;
            if I < G'Last (1) then
               Swap (G, (I, J), (I + 1, J));
               Is_Move_Legal (G, (I, J), Solution);
               Swap (G, (I, J), (I + 1, J));
            end if;
            if J > G'First (2) then
               Swap (G, (I, J), (I, J - 1));
               Is_Move_Legal (G, (I, J), Solution);
               Swap (G, (I, J), (I, J - 1));
            end if;
            if J < G'Last (2) then
               Swap (G, (I, J), (I, J + 1));
               Is_Move_Legal (G, (I, J), Solution);
               Swap (G, (I, J), (I, J + 1));
            end if;
            if not PointSet.Is_Empty (Solution) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Unsolvable;

   procedure Update_Grid (G           : in out Grid;
                          Last_Square : in out Optional_Point;
                          Cur_Square  : in out Optional_Point;
                          Just_Moved  : in out Boolean;
                          Score       : in out Natural;
                          Solvable    : out Boolean) is
      WorkListMove : PointSet.Set;
   begin
      Solvable := True;
      if not Are_Adjacent (Cur_Square, Last_Square) then
         return;
      end if;
      Swap (G, Cur_Square.P, Last_Square.P);
      --  We want to evaluate both to fill our worklist
      Is_Move_Legal (G, Cur_Square.P, WorkListMove);
      Is_Move_Legal (G, Last_Square.P, WorkListMove);
      if not PointSet.Is_Empty (WorkListMove) then
         Last_Square.Valid := False;
         Cur_Square.Valid  := False;
         Just_Moved        := True;
         Process_Moves (G, WorkListMove, Score);
         Solvable := not Is_Unsolvable (G);
      else
         Swap (G, Cur_Square.P, Last_Square.P);
         return;
      end if;
   end Update_Grid;

   procedure Draw_Grid (G : Grid) is
      R : Rect  := ((0, 0), SQUARE_SURFACE_SIZE, SQUARE_SURFACE_SIZE);
   begin
      for I in G'Range (1) loop
         for J in G'Range (2) loop
            Display.Hidden_Buffer (1).Set_Source (CM (G (I, J)));
            R.Position := ((I - G'First (1)) * SQUARE_SIZE,
                           (J - G'First (2)) * SQUARE_SIZE);
            Display.Hidden_Buffer (1).Fill_Rect (R);
         end loop;
      end loop;
   end Draw_Grid;

   procedure Swap (G : in out Grid; A : Point; B : Point) is
      C : constant Square := G (A.X, A.Y);
   begin
      G (A.X, A.Y) := G (B.X, B.Y);
      G (B.X, B.Y) := C;
   end Swap;

   function Are_Adjacent (A : Optional_Point; B : Optional_Point)
   return Boolean is
      X_Diff : Integer;
      Y_Diff : Integer;
   begin
      if not A.Valid or else not B.Valid then
         return False;
      end if;
      X_Diff := A.P.X - B.P.X;
      Y_Diff := A.P.Y - B.P.Y;
      return ((abs X_Diff) + (abs Y_Diff)) = 1;
   end Are_Adjacent;

   procedure Get_Matching_Neighbourgs (
                                  G : Grid;
                                  X : Integer; Step_X : Integer;
                                  Y : Integer; Step_Y : Integer;
                                  S : Square;
                                  Matching_Squares : in out PointSet.Set) is
      Tmp_X : Integer := X;
      Tmp_Y : Integer := Y;
   begin
      while Tmp_X >= G'First (1) and then Tmp_X <= G'Last (1)
            and then Tmp_Y >= G'First (2) and then Tmp_Y <= G'Last (2)
            and then G (Tmp_X, Tmp_Y) = S loop
         PointSet.Include (Matching_Squares, (Tmp_X, Tmp_Y));
         Tmp_X := Tmp_X + Step_X;
         Tmp_Y := Tmp_Y + Step_Y;
      end loop;
   end Get_Matching_Neighbourgs;

   function Is_Match_3 (G : Grid; X : Natural; Y : Natural) return Boolean is
      S    : constant Square := G (X, Y);
      Line : PointSet.Set;
   begin
      Get_Matching_Neighbourgs (G, X, -1, Y, 0, S, Line);
      if PointSet.Length (Line) >= 3 then
         return True;
      end if;
      PointSet.Clear (Line);
      Get_Matching_Neighbourgs (G, X, 0, Y, -1, S, Line);
      return PointSet.Length (Line) >= 3;
   end Is_Match_3;

   procedure Is_Move_Legal (G : Grid; P : Point; Combinations : in out
                            PointSet.Set) is
      S                   : constant Square := G (P.X, P.Y);
      Matching_Horizontal : PointSet.Set;
      Matching_Vertical   : PointSet.Set;
      Legal               : Boolean := False;
   begin
      Get_Matching_Neighbourgs (G, P.X - 1, -1, P.Y, 0, S,
                                Matching_Horizontal);
      Get_Matching_Neighbourgs (G, P.X + 1, 1, P.Y, 0, S, Matching_Horizontal);
      Get_Matching_Neighbourgs (G, P.X, 0, P.Y - 1, -1, S, Matching_Vertical);
      Get_Matching_Neighbourgs (G, P.X, 0, P.Y + 1, 1, S, Matching_Vertical);
      if PointSet.Length (Matching_Horizontal) >= 2 then
         Combinations := PointSet.Union (Matching_Horizontal, Combinations);
         Legal := True;
      end if;
      if PointSet.Length (Matching_Vertical) >= 2 then
         Combinations := PointSet.Union (Matching_Vertical, Combinations);
         Legal := True;
      end if;
      if Legal then
         PointSet.Include (Combinations, P);
      end if;
   end Is_Move_Legal;

---------- Game main procedure ------------------------------------------------

   procedure Run is
      Score_Pos       : constant Point := (0, 0);
      Game_Over_Pos   : constant Point := (2 * SQUARE_SIZE,
                                           Natural (3.5 * SQUARE_SIZE));
      Final_Score_Pos : constant Point := (SQUARE_SIZE,
                                           Natural (4.25 * SQUARE_SIZE));

      G           : Grid;
      Last_Square : Optional_Point := (Valid => False, P => <>);
      Cur_Square  : Optional_Point := (Valid => False, P => <>);
      Just_Moved  : Boolean        := False;
      Score       : Natural        := 0;
      Solvable    : Boolean        := True;
   begin
      Init_Board;
      loop
         Score := 0;
         Init_Grid (G);
         loop
            Get_Input (Last_Square, Cur_Square, Just_Moved);
            Update_Grid (G, Last_Square, Cur_Square, Just_Moved, Score,
                         Solvable);
            Draw_Grid (G);

            if Solvable then
               --  Draw Score
               LCD_Std_Out.Put (Score_Pos.X, Score_Pos.Y, Score'Image);
            else
               --  Game Over
               LCD_Std_Out.Put (Game_Over_Pos.X, Game_Over_Pos.Y,
                                "GAME OVER");
               LCD_Std_Out.Put (Final_Score_Pos.X, Final_Score_Pos.Y,
                                "Final Score: " & Score'Image);
               delay Duration (5.0);
               exit;
            end if;
            --  Update screen
            Display.Update_Layer (1, Copy_Back => True);
         end loop;
      end loop;
   end Run;

-------------------------------------------------------------------------------

end Square_Destroyer;
