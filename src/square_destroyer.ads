with HAL.Bitmap;             use HAL.Bitmap;
with Ada.Containers.Vectors; use Ada.Containers;

package Square_Destroyer is

---------- Game main procedure -------------------------------------------------

    procedure Run with
        Global => null;

--------------------------------------------------------------------------------

private

---------- Types and constants -------------------------------------------------

    GRID_WIDTH          : constant := 6;
    GRID_HEIGHT         : constant := 8;
    SQUARE_SIZE         : constant := 40;
    SQUARE_SURFACE_SIZE : constant := SQUARE_SIZE - 1;

    type Square is (Blue, Green, Red, Yellow, Magenta, Cyan);
    type Grid   is
      array(Positive range 1..GRID_WIDTH, Positive range 1..GRID_HEIGHT)
      of Square;

    type ColorMap is array(Square) of HAL.Bitmap.Bitmap_Color;
    CM : constant ColorMap := (HAL.Bitmap.Blue, HAL.Bitmap.Green,
                               HAL.Bitmap.Red, HAL.Bitmap.Yellow,
                               HAL.Bitmap.Magenta, HAL.Bitmap.Cyan);

    package PointVect is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Point);

    type Optional_Point is
        record
            Valid : Boolean;
            P     : Point;
        end record;

---------- Helper functions for contracts --------------------------------------

    function Is_Grid_Valid(G : Grid) return Boolean with
        SPARK_MODE => On,
        Global     => null,
        Depends    => (Is_Grid_Valid'Result => G),
        Post       => (Is_Grid_Valid'Result =
                          (for all I in G'Range(1) =>
                              (for all J in G'Range(2) =>
                                  G(I, J)'Valid)));

    function Is_In_Grid(P : Point) return Boolean with
        SPARK_MODE => On,
        Global     => null,
        Depends    => (Is_In_Grid'Result => P),
        Post       => (Is_In_Grid'Result = (P.X in 1..GRID_WIDTH and then
                                            P.Y in 1..GRID_HEIGHT));

---------- Init procedures -----------------------------------------------------

    procedure Init_Grid(G : out Grid) with
        Global  => null,
        Depends => (G => null),
        Post    => (Is_Grid_Valid(G));

    procedure Init_Board with
        Global => null;

---------- Game loop procedures and functions ----------------------------------

    procedure Get_Input(G           : Grid;
                        Last_Square : out Optional_Point;
                        Cur_Square  : in out Optional_Point;
                        Just_Moved  : in out Boolean) with
        Global     => null,
        Depends    => (Last_Square => (Just_Moved, Cur_Square),
                       Cur_Square  => (Just_Moved, G),
                       Just_Moved  => null);

    procedure Update_Grid(G           : in out Grid;
                          Last_Square : in out Optional_Point;
                          Cur_Square  : in out Optional_Point;
                          Just_Moved  : out Boolean) with
        Global     => null,
        Depends    => ((G, Last_Square, Cur_Square, Just_Moved) =>
                            (G, Cur_Square, Last_Square)),
        Pre        => (Is_Grid_Valid(G));

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

    procedure Get_Matching_Neighbourgs(
                                G : Grid;
                                X : Integer; Step_X : Integer;
                                Y : Integer; Step_Y : Integer;
                                S : Square;
                                Matching_Squares : in out PointVect.Vector) with
        SPARK_MODE => On,
        Global     => null,
        Depends    => (Matching_Squares =>+ (G, X, Step_X, Y, Step_Y, S)),
        Pre        => (Is_Grid_Valid(G) and then
                       (Step_X /= 0 or else Step_Y /= 0)),
        Post       => ((Is_Grid_Valid(G)) and then
                       (PointVect.Length(Matching_Squares) in
                        PointVect.Length(Matching_Squares'Old)..(
                          (GRID_WIDTH / Count_Type'Max(Count_Type(abs Step_X),
                                                       Count_Type(abs Step_Y)))
                          - 1 + PointVect.Length(Matching_Squares'Old)))
                        and then
                        (for all P of Matching_Squares => ((G(P.X, P.Y) = S))));

    function Is_Move_Legal(G : Grid; P : Point;
                           Combinations : in out PointVect.Vector)
    return Boolean with
        Global  => null,
        Pre     => (Is_Grid_Valid(G) and then Is_In_Grid(P));

    procedure Draw_Grid(G : Grid) with
        Global => null,
        Pre    => (Is_Grid_Valid(G));

    function Sort_By_Height(A : Point; B : Point) return Boolean with
        SPARK_MODE => On,
        Global     => null,
        Depends    => (Sort_By_Height'Result => (A, B)),
        Post       => (if a.Y = b.Y then
                          Sort_By_Height'Result = (a.X < b.X)
                       else
                          Sort_By_Height'Result = (a.Y < b.Y));

    package Sorter is new PointVect.Generic_Sorting ("<" => Sort_By_Height);

--------------------------------------------------------------------------------

end Square_Destroyer;
