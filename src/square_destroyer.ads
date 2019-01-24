with HAL.Bitmap; use HAL.Bitmap;

package Square_Destroyer is

    procedure Square_Destroyer;

private

    GRID_WIDTH          : constant := 6;
    GRID_HEIGHT         : constant := 8;
    SQUARE_SIZE         : constant := 40;
    COLORED_SQUARE_SIZE : constant := SQUARE_SIZE - 1;

    type Square is (Blue, Green, Red, Yellow, Magenta, Cyan);
    type Grid   is
      array(Positive range 1..GRID_WIDTH, Positive range 1..GRID_HEIGHT)
      of Square;

    type ColorMap is array(Square) of HAL.Bitmap.Bitmap_Color;
    CM : constant ColorMap := (HAL.Bitmap.Blue, HAL.Bitmap.Green,
                               HAL.Bitmap.Red, HAL.Bitmap.Yellow,
                               HAL.Bitmap.Magenta, HAL.Bitmap.Cyan);

    type Optional_Point is
        record
            Valid : Boolean;
            P     : Point;
        end record;

    procedure Init_Grid(G : out Grid);
    procedure Draw_Grid(G : Grid);
    procedure Swap(G : in out Grid; A : Point; B : Point);
    function Are_Adjacent(A : Optional_Point; B : Optional_Point)
      return Boolean;
    function Count_Dir(G : Grid; X : Integer; Step_X : Integer; Y : Integer;
                       Step_Y : Integer; S : Square)
      return Integer;
    function Is_Move_Legal(G : Grid; P : Point) return Boolean;

end Square_Destroyer;
