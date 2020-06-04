--
-- "Ballfield"
--
--  (C) David Olofson <david@olofson.net>, 2002
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.Video.Rectangles;

package Ballfield is

   procedure Main;

   --  Definitions...
   BALLS  : constant := 200;
   COLORS : constant := 2;
   type Color_Type is range 0 .. Colors - 1;
   type Ball_Index is range 0 .. Balls - 1;

   type Point_Type is
      record
         X, Y, Z : Integer;  --  Position
         C       : Color_Type; -- Natural;  --  Color
      end record;

   --  Ballfield
   type Point_Array   is array (Ball_Index)  of Point_Type;
   type Surface_Array is array (Color_Type) of SDL.Video.Surfaces.Surface;
   type Texture_Array is array (Color_Type) of SDL.Video.Textures.Texture;
--   type Frame_Array   is array (Positive range <>) of SDL.Video.Rectangles.Rectangle;
--   type Frame_Access  is access all Frame_Array;
   type Rectangle_Array is array (Natural range <>) of SDL.Video.Rectangles.Rectangle;
   type Rectangle_Access is access all Rectangle_Array;

   type Ballfield_Type is
      record
        Points    : Point_Array;
        Frames    : Rectangle_Access; -- SDL.Video.Rectangles.Rectangle_Arrays;
        --Frames    : Frame_Access; --SDL.Video.Rectangles.Rectangle; --SDL_Rect   *frames;
        --Frames    : SDL.Video.Rectangles.Rectangle; --SDL_Rect   *frames;
        --Gfx       : Texture_Array;
        Gfx       : Surface_Array; --SDL_Surface        *gfx[COLORS];
        Use_Alpha : Boolean;
      end record;

   --  Size of the screen in pixels
   SCREEN_W : constant := 800;
   SCREEN_H : constant := 600;

   --  Size of the biggest ball image in pixels
   --
   --  Balls are scaled down and *packed*, one pixel
   --  smaller for each frame down to 1x1. The actual
   --  image width is (obviously...) the same as the
   --  width of the first frame.
   BALL_W : constant := 32;
   BALL_H : constant := 32;

end Ballfield;
