package Square_Destroyer is

type Square is (Blue, Green, Red, Yellow, Magenta, Cyan);
type Grid is array(Positive range 1..6, Positive range 1..8) of Square;

procedure Init_Grid(g : out Grid);
procedure Square_Destroyer;

end;
