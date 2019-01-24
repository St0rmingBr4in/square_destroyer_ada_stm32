with HAL.Bitmap; use HAL.Bitmap;

package Square_Destroyer is

    procedure Square_Destroyer;

private

    GRID_WIDTH  : constant := 6;
    GRID_HEIGHT : constant := 8;

    type Square is (Blue, Green, Red, Yellow, Magenta, Cyan);
    type Grid   is
      array(Positive range 1..GRID_WIDTH, Positive range 1..GRID_HEIGHT)
      of Square;
    type Optional_Point is
        record
            Valid : Boolean;
            P     : Point;
        end record;

    procedure Init_Grid(g : out Grid);

end Square_Destroyer;
