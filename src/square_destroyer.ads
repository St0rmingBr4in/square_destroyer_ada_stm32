with Ada.Containers.Ordered_Sets; use Ada.Containers;
with HAL.Bitmap;                  use HAL.Bitmap;

package Square_Destroyer is

---------- Game main procedure ------------------------------------------------

   procedure Run with
      Global => null;

-------------------------------------------------------------------------------

private

---------- Types and constants ------------------------------------------------

   type Square is (Blue, Green, Red, Yellow, Magenta, Cyan);

   type ColorMap is array (Square) of HAL.Bitmap.Bitmap_Color;
   CM : constant ColorMap := (HAL.Bitmap.Blue, HAL.Bitmap.Green,
                              HAL.Bitmap.Red, HAL.Bitmap.Yellow,
                              HAL.Bitmap.Magenta, HAL.Bitmap.Cyan);

   type Optional_Point is
      record
         Valid : Boolean;
         P     : Point;
      end record;

   function Sort_By_Height (A : Point; B : Point) return Boolean with
      Global  => null,
      Depends => (Sort_By_Height'Result => (A, B)),
      Post    => (if A.Y = B.Y then
                     Sort_By_Height'Result = (A.X > B.X)
                  else
                     Sort_By_Height'Result = (A.Y > B.Y));

   package PointSet is new Ada.Containers.Ordered_Sets
      (Element_Type => Point,
       "<"          => Sort_By_Height);

   SCREEN_WIDTH        : constant := 240;
   SCREEN_HEIGHT       : constant := 340;
   SQUARE_SIZE         : constant := 40;

   --   SQUARE_SURFACE_SIZE := 40 - 1 = 39
   SQUARE_SURFACE_SIZE : constant := SQUARE_SIZE - 1;
   MATCH_3_VALUE       : constant := 100;
   BONUS_PER_SQUARE    : constant := 50;

   --   GRID_WIDTH  := 240 / 40 = 6
   GRID_WIDTH          : constant := SCREEN_WIDTH   / SQUARE_SIZE;
   --   GRID_HEIGHT := 320 / 40 = 8
   GRID_HEIGHT         : constant := SCREEN_HEIGHT  / SQUARE_SIZE;
   type Grid is
      array (Positive range 1 .. GRID_WIDTH, Positive range 1 .. GRID_HEIGHT)
      of Square;

---------- Helper functions for contracts -------------------------------------

   function Is_Grid_Valid (G : Grid) return Boolean with
      Global  => null,
      Depends => (Is_Grid_Valid'Result => G),
      Post    => (Is_Grid_Valid'Result =
                     (for all I in G'Range (1) =>
                        (for all J in G'Range (2) =>
                           G (I, J)'Valid)));

   function Is_In_Grid (P : Point) return Boolean with
      Global  => null,
      Depends => (Is_In_Grid'Result => P),
      Post    => (Is_In_Grid'Result = (P.X in 1 .. GRID_WIDTH and then
                                       P.Y in 1 .. GRID_HEIGHT));

---------- Random Wrapper -----------------------------------------------------

   function Get_Random_Square return Square with
      Global  => null,
      Depends => (Get_Random_Square'Result => null),
      Post    => (Get_Random_Square'Result'Valid);

---------- Helper function and procedure for Init_Grid ------------------------

   function Is_Match_3 (G : Grid; X : Natural; Y : Natural) return Boolean with
      Global     => null,
      Depends    => (Is_Match_3'Result => (G, X, Y)),
      Pre        => (X in 1 .. GRID_WIDTH and then Y in 1 .. GRID_HEIGHT),
      Post       => (Is_Match_3'Result =
                        (X >= 3 and then
                         G (X - 1, Y) = G (X, Y) and then
                         G (X - 2, Y) = G (X, Y)) or else
                           (Y >= 3 and then
                              G (X, Y - 1) = G (X, Y) and then
                              G (X, Y - 2) = G (X, Y)));

---------- Init procedures ----------------------------------------------------

   procedure Init_Grid (G : out Grid) with
      Global => null,
      Post   => (Is_Grid_Valid (G));

   procedure Init_Board with
      Global => null;

---------- Helper functions and procedures for Update_Grid --------------------

   function Are_Adjacent (A : Optional_Point; B : Optional_Point)
   return Boolean with
      Global  => null,
      Depends => (Are_Adjacent'Result => (A, B)),
      Pre     => ((not A.Valid or else Is_In_Grid (A.P)) and then
                  (not B.Valid or else Is_In_Grid (B.P))),
      Post    => (if not A.Valid or else not B.Valid then
                     Are_Adjacent'Result = False
                  else
                     Are_Adjacent'Result =
                        (((abs (A.P.X - B.P.X)) + (abs (A.P.Y - B.P.Y)))
                           = 1));

   procedure Swap (G : in out Grid; A : Point; B : Point) with
      Global  => null,
      Depends => (G =>+ (A, B)),
      Pre     => (Is_In_Grid (A) and then Is_In_Grid (B)),
      Post    => (G (A.X, A.Y) = G'Old (B.X, B.Y) and then
                  G (B.X, B.Y) = G'Old (A.X, A.Y));

   procedure Get_Matching_Neighbours (G : Grid;
                                       X : Integer; Step_X : Integer;
                                       Y : Integer; Step_Y : Integer;
                                       S : Square;
                                       Matching_Squares : in out PointSet.Set)
   with
      Global  => null,
      Depends => (Matching_Squares =>+ (G, X, Step_X, Y, Step_Y, S)),
      Pre     => (Is_Grid_Valid (G) and then
                  X in 0 .. GRID_WIDTH  + 1 and then
                  Y in 0 .. GRID_HEIGHT + 1 and then
                  (Step_X /= 0 or else Step_Y /= 0)),
      Post    => ((PointSet.Length (Matching_Squares) in
                     PointSet.Length (Matching_Squares'Old) .. (
                        (GRID_WIDTH / Count_Type'Max (
                                                Count_Type (abs Step_X),
                                                Count_Type (abs Step_Y)))
                        - 1 + PointSet.Length (Matching_Squares'Old)))
                  and then
                  (for all P of Matching_Squares =>
                     ((G (P.X, P.Y) = S) and then
                        (if not PointSet.Contains (Matching_Squares'Old, P)
                           then P.X = X or else P.Y = Y))));

   procedure Is_Move_Legal (G : Grid; P : Point;
                            Combinations : in out PointSet.Set) with
      Global  => null,
      Depends => (Combinations =>+ (G, P)),
      Pre     => (Is_Grid_Valid (G) and then Is_In_Grid (P)),
      Post    => (PointSet.Length (Combinations) >= PointSet.Length
                   (Combinations'Old));

   procedure Process_Moves (G            : in out Grid;
                            WorkListMove : in out PointSet.Set;
                            Score        : in out Natural) with
      Global  => null,
      Depends => ((G, WorkListMove, Score) => (G, WorkListMove, Score)),
      Pre     => (Is_Grid_Valid (G) and then
                     not PointSet.Is_Empty (WorkListMove)),
      Post    => (Is_Grid_Valid (G) and then PointSet.Is_Empty (WorkListMove)
                     and then Score >= Score'Old + MATCH_3_VALUE);

   procedure Blink (G : Grid; WorkListMove : PointSet.Set;
                    Score : Natural) with
      Global => null,
      Pre    => (Is_Grid_Valid (G)),
      Post   => (Is_Grid_Valid (G));

   function Is_Unsolvable (G : in out Grid) return Boolean with
      Global => null,
      Pre    => (Is_Grid_Valid (G)),
      Post   => (G = G'Old);

---------- Game loop procedures -----------------------------------------------

   procedure Get_Input (Last_Square : out Optional_Point;
                        Cur_Square  : in out Optional_Point;
                        Just_Moved  : in out Boolean) with
      Global  => null,
      Depends => (Last_Square => (Just_Moved, Cur_Square),
                  Cur_Square  => (Just_Moved),
                  Just_Moved  => null);

   procedure Update_Grid (G           : in out Grid;
                          Last_Square : in out Optional_Point;
                          Cur_Square  : in out Optional_Point;
                          Just_Moved  : in out Boolean;
                          Score       : in out Natural;
                          Solvable    : out Boolean) with
      Global  => null,
      Depends => ((G, Last_Square, Cur_Square, Just_Moved, Score, Solvable) =>
                     (G, Cur_Square, Last_Square, Just_Moved, Score)),
      Pre     => (Is_Grid_Valid (G)),
      Post    => (Is_Grid_Valid (G) and then
                  Solvable = not Is_Unsolvable (G) and then
                  Last_Square.P = Last_Square'Old.P and then
                  Cur_Square.P = Cur_Square'Old.P and then
                  (if Are_Adjacent (Cur_Square'Old, Last_Square'Old) and then
                      Just_Moved then
                      Score > Score'Old and then
                      Last_Square.Valid = False and then
                      Cur_Square.Valid = False and then
                      G /= G'Old
                   else
                      Score = Score'Old and then
                      Last_Square.Valid = Last_Square'Old.Valid and then
                      Cur_Square.Valid = Cur_Square'Old.Valid and then
                      G = G'Old));

   procedure Draw_Grid (G : Grid) with
      Global => null,
      Pre    => (Is_Grid_Valid (G)),
      Post   => (Is_Grid_Valid (G));

-------------------------------------------------------------------------------

end Square_Destroyer;
