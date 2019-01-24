with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with HAL;                   use HAL;
with STM32.Board;           use STM32.Board;
with HAL.Touch_Panel;
with STM32.User_Button;     use STM32;
with BMP_Fonts;
with LCD_Std_Out;
with STM32.RNG.Interrupts;

package body Square_Destroyer is

    procedure Init_Grid(G : out Grid) is
    begin
        RNG.Interrupts.Initialize_RNG;

        for I in G'Range(1) loop
            for J in G'Range(2) loop
                G(I, J) := Square'Val(RNG.Interrupts.Random mod
                                      UInt32(Square'Pos(Square'Last) + 1));
            end loop;
        end loop;
    end Init_Grid;

    procedure Draw_Grid(G : Grid) is
        type ColorMap is array(Square) of HAL.Bitmap.Bitmap_Color;

        M : constant ColorMap := (HAL.Bitmap.Blue, HAL.Bitmap.Green,
                                  HAL.Bitmap.Red, HAL.Bitmap.Yellow,
                                  HAL.Bitmap.Magenta, HAL.Bitmap.Cyan);

        Rect_Pos : Point := (0, 0);
        R        : Rect  := (Rect_Pos, 39, 39);
    begin
        for I in G'Range(1) loop
            for J in G'Range(2) loop
                Display.Hidden_Buffer (1).Set_Source (M(G(I, J)));
                Rect_Pos := ((I - G'First(1)) * 40, (J - G'First(2)) * 40);
                R := (Rect_Pos, 39, 39);
                Display.Hidden_Buffer (1).Fill_Rect (R);
            end loop;
        end loop;
    end;

    procedure Swap(G : in out Grid; A : Point; B : Point) is
        C : Square;
    begin
        C := G(A.X, A.Y);
        G(A.X, A.Y) := G(B.X, B.Y);
        G(B.X, B.Y) := C;
    end;

    function Are_Adjacent(A : Optional_Point; B : Optional_Point)
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
    end;

    function Count_Dir(G : Grid; X : Integer; Step_X : Integer; Y : Integer;
        Step_Y : Integer; S : Square) return Integer is
        Count : Integer := 0;
        Tmp_X : Integer := X;
        Tmp_Y : Integer := Y;
    begin
        while Tmp_X >= G'First(1) and then Tmp_X <= G'Last(1)
              and then Tmp_Y >= G'First(2) and then Tmp_Y <= G'Last(2)
              and then G(Tmp_X, Tmp_Y) = S loop
            Count := Count + 1;
            Tmp_X := Tmp_X + Step_X;
            Tmp_Y := Tmp_Y + Step_Y;
        end loop;

        return Count;
    end;


    function Is_Move_Legal(G : Grid; P : Point) return Boolean is
        S : constant Square := G(P.X, P.Y);
    begin
        return ((Count_Dir(G, P.X - 1, -1, P.Y, 0, S) + Count_Dir(G, P.X + 1, 1,
            P.Y, 0, S)) >= 2) or else ((Count_Dir(G, P.X, 0, P.Y - 1, -1, S) +
            Count_Dir(G, P.X, 0, P.Y + 1, 1, S)) >= 2);
    end;

    procedure Square_Destroyer
    is
        BG : constant Bitmap_Color := (Alpha => 255, others => 0);

        G           : Grid;
        Last_Square : Optional_Point := (Valid => False, P => <>);
        Cur_Square  : Optional_Point := (Valid => False, P => <>);
        Just_Moved  : Boolean        := False;
    begin
        Init_Grid(G);

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

        loop
            declare
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
                               P => ((State (State'First).X / 40) + G'First(1),
                                    ((State (State'First).Y)/ 40) + G'FIrst(2)));
                        end if;
                    when others => null;
                end case;
            end;
            if Are_Adjacent(Cur_Square, Last_Square) then
                Swap(G, Cur_Square.P, Last_Square.P);
                if Is_move_Legal(G, Cur_Square.P)
                    or else Is_Move_Legal(G, Last_Square.P) then
                    Last_Square.Valid := False;
                    Cur_Square.Valid  := False;
                    Just_Moved        := True;
                else
                    Swap(G, Cur_Square.P, Last_Square.P);
                end if;
            end if;

            Draw_Grid(G);

            --  Update screen
            Display.Update_Layer (1, Copy_Back => True);
        end loop;
    end Square_Destroyer;

end Square_Destroyer;
