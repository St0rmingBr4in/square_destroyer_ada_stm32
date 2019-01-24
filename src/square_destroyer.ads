with HAL.Bitmap; use HAL.Bitmap;

package Square_Destroyer is

    procedure Square_Destroyer with
        Global => null;

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

    function Is_In_Grid(P : Point) return Boolean with
        SPARK_MODE => On,
        Global     => null,
        Depends    => (Is_In_Grid'Result => P),
        Post       => (Is_In_Grid'Result = (P.X in 1..GRID_WIDTH and then
                                            P.Y in 1..GRID_HEIGHT));

    procedure Init_Grid(G : out Grid) with
        Global  => null,
        Depends => (G => null),
        Post    => (for all I in G'Range(1) =>
                        (for all J in G'Range(2) =>
                             G(I, J)'Valid));

    procedure Init_Board with
        Global => null;

    procedure Draw_Grid(G : Grid) with
        Global => null;

    procedure Swap(G : in out Grid; A : Point; B : Point) with
        SPARK_MODE => On,
        Global     => null,
        Depends    => (G =>+ (A, B)),
        Pre        => (Is_In_Grid(A) and then Is_In_Grid(B)),
        Post       => (G(A.X, A.Y) = G'Old(B.X, B.Y) and then
                       G(B.X, B.Y) = G'Old(A.X, A.Y));

    function Are_Adjacent(A : Optional_Point; B : Optional_Point)
    return Boolean with
        SPARK_MODE => On,
        Global     => null,
        Depends    => (Are_Adjacent'Result => (A, B)),
        Pre        => ((not A.Valid or else Is_In_Grid(A.P)) and then
                       (not B.Valid or else Is_In_Grid(B.P))),
        Post       => (if not A.Valid or else not B.Valid then
                           Are_Adjacent'Result = False
                       else
                           Are_Adjacent'Result =
                               (((abs (A.P.X - B.P.X)) + (abs (A.P.Y - B.P.Y)))
                                 = 1));

    function Count_Dir(G : Grid; X : Integer; Step_X : Integer; Y : Integer;
                       Step_Y : Integer; S : Square)
    return Integer with
        Global => null;

    function Is_Move_Legal(G : Grid; P : Point) return Boolean with
        Global => null;

end Square_Destroyer;
