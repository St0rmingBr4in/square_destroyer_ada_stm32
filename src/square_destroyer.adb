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
with STM32.RNG.Interrupts;
with HAL; use HAL;

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


    procedure Square_Destroyer
    is
        BG : constant Bitmap_Color := (Alpha => 255, others => 0);
        g        : Grid;
    begin
        Init_Grid(g);

        --  Initialize LCD
        Display.Initialize;
        Display.Initialize_Layer (1, ARGB_8888);

        --  Initialize touch panel
        --HAL.Touch_Panel.Initialize;

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

            Draw_Grid(g);
            --  Update screen
            Display.Update_Layer (1, Copy_Back => True);

        end loop;
    end Square_Destroyer;

end Square_Destroyer;
