with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
--  The "last chance handler" is the user-defined routine that is called when
--  an exception is propagated. We need it in the executable, therefore it
--  must be somewhere in the closure of the context clauses.

with HAL; use HAL;
with STM32.Board; use STM32.Board;
with HAL.Bitmap;            use HAL.Bitmap;
with HAL.Touch_Panel;
with STM32.User_Button;     use STM32;
with BMP_Fonts;
with LCD_Std_Out;
with STM32.RNG.Interrupts;

package body Square_Destroyer is

    procedure Init_Grid(g : out Grid) is
    begin
        RNG.Interrupts.Initialize_RNG;

        for i in g'Range(1) loop
            for j in g'Range(2) loop
                g(i, j) := Square'Val(RNG.Interrupts.Random mod
                UInt32 (Square'Pos(Square'Last) + 1));
            end loop;
        end loop;
    end Init_Grid;

    procedure Draw_Grid(g : Grid) is
        type ColorMap is array(Square) of HAL.Bitmap.Bitmap_Color;

        m : constant ColorMap := (HAL.Bitmap.Blue, HAL.Bitmap.Green,
                                  HAL.Bitmap.Red, HAL.Bitmap.Yellow,
                                  HAL.Bitmap.Magenta, HAL.Bitmap.Cyan);
        Rect_Pos : Point := (0, 0);
        r        : Rect  := (Rect_Pos, 39, 39);
    begin
        for i in g'Range(1) loop
            for j in g'Range(2) loop
                Display.Hidden_Buffer (1).Set_Source (m(g(i,j)));
                Rect_Pos := ((i - g'First(1)) * 40, (j - g'First(2)) * 40);
                r := (Rect_Pos, 39, 39);
                Display.Hidden_Buffer (1).Fill_Rect (r);
            end loop;
        end loop;
    end;

    procedure Swap(g : in out Grid; a : Point; b : Point) is
        c : Square;
    begin
        c := g(a.X, a.Y);
        g(a.X, a.Y) := g(b.X, b.Y);
        g(b.X, b.Y) := c;
    end;

    function Are_Adjacent(a : Point; b : Point) return Boolean is
        x_diff : Integer;
        y_diff : Integer;
    begin
        x_diff := a.X - b.X;
        y_diff := a.Y - b.Y;
        return ((abs x_diff) + (abs y_diff)) = 1;
    end;


    procedure Square_Destroyer
    is
        BG : constant Bitmap_Color := (Alpha => 255, others => 0);
        g        : Grid;
        Last_Square : Point := (0,0);
        Cur_Square : Point := (0,0);
    begin
        Init_Grid(g);

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
                    when 1 =>
                        Last_Square := Cur_Square;
                        Cur_Square := ((State (State'First).X / 40) +
                        g'First(1), ((State (State'First).Y)/ 40) + g'FIrst(2));
                    when others => null;
                end case;
            end;
            -- if Cur_Square.X /= Last_Square.X or else Cur_Square.Y = Last
            if Are_Adjacent(Cur_Square, Last_Square) then
                Swap(g, Cur_Square, Last_Square);
                Last_Square := (0,0);
                Cur_Square := (0,0);
            end if;
            Draw_Grid(g);

            --  Update screen
            Display.Update_Layer (1, Copy_Back => True);
        end loop;
    end Square_Destroyer;

end Square_Destroyer;
