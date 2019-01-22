package Square_Destroyer is

GRID_WIDTH  : constant := 6;
GRID_HEIGHT : constant := 8;

type Square is (Blue, Green, Red, Yellow, Magenta, Cyan);
type Grid is array(Positive range 1..GRID_WIDTH, Positive range 1..GRID_HEIGHT)
             of Square;

procedure Init_Grid(g : out Grid);
procedure Square_Destroyer;

end Square_Destroyer;
