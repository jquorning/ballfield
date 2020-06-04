--
--  "Ballfield"
--
--  (C) David Olofson <david@olofson.net>, 2002, 2003
--
--  This software is released under the terms of the GPL.
--
--  Contact author for permission if you want to use this
--  software, or work derived from it, under other terms.

with Ada.Text_IO;

with Ada.Numerics.Elementary_Functions;
with Ada.Numerics.Discrete_Random;

with SDL.Video.Windows.Makers;
--with SDL.Video.Renderers.Makers;
--with SDL.Video.Textures.Makers;

with SDL.Images.IO;
with SDL.Events.Events;
with SDL.Events.Keyboards;

package body Ballfield is

--   Renderer : SDL.Video.Renderers.Renderer;

   procedure Debug (Text : String) is
   begin
      Ada.Text_IO.Put_Line (Text);
   end Debug;

   use SDL.Video.Surfaces;
--   use SDL.Video.Textures;

   ----------------------------
   -- General tool functions --
   ----------------------------

   -----------------
   -- Clean_Alpha --
   -----------------

   --  Bump areas of low and high alpha to 0% or 100%
   --  respectively, just in case the graphics contains
   --  "alpha noise".

--     procedure Clean_Alpha (Work : out surface;
--                            S    :     Surface)
--     is
--        use SDL.Video.Pixel_Formats;
--        subtype Unsigned_32 is Interfaces.Unsigned_32;
--        use type SDL.Video.Pixel_Formats.C.int;
--        --    SDL_Surface *work;
--        Pixels : Natural; --    Uint32*
--        Pp     : Integer;
--        X, Y   : Integer;
--        Size : constant SDL.Sizes := S.Size;
--     begin
--        SDL.Video.Surfaces.Makers.Create (Work,
--                                          -- SDL_SWSURFACE,
--                                          (S.Size.Width, S.Size.Height), 32,
--                                          16#FF_00_00_00#, 16#00_FF_00_00#,
--                                          16#00_00_FF_00#, 16#00_00_00_FF#);
--  --      Work := SDL_CreateRGBSurface (SDL_SWSURFACE, S.Size.Width, S.Size.Height, 32,
--  --                                    16#Ff_00_00_00#, 16#00_FF_00_00#,
--  --                                    16#00_00_FF_00#, 16#00_00_00_FF#);

--        --    if(!work)
--  --            return NULL;

--        declare
--           use SDL.Video.Rectangles;
--           R  : Rectangle := (0, 0, S.Size.Width, S.Size.Height);
--           SA : Rectangle := (0,0,0,0);
--        begin
--           Work.Blit (R, S, SA);
--        end;
--  --              if(SDL_BlitSurface(s, &r, work, NULL) < 0)
--  --      {
--  --              SDL_FreeSurface(work);
--  --              return NULL;
--  --      end if;

--        Work.Lock;

--        Pixels := Work.Pixels;
--        Pp := Work.Pitch / (Unsigned_32'Size / 8);

--        for Y in 0 .. Work.Size.Height - 1 loop
--           for X in 0 .. Work.Size.Width - 1 loop

--              declare
--                 Pix : Unsigned_32 := Pixels (Y * pp + X);
--              begin
--                 case Pix mod 16#100# / 2**4 is
--                    when 0      =>  pix := 16#00000000#;
--                    when others =>  null;
--                    when 15     =>  Pix := Pix or 16#FF#;
--                 end case;

--                 Pixels (Y * pp + X) := Pix;
--              end;
--           end loop;
--        end loop;

--        Work.Unlock;

--        --return work;
--     end Clean_Alpha;

   -----------------
   -- Load_Zoomed --
   -----------------

   --
   --  Load and convert an antialiazed, zoomed set of sprites.
   --
   procedure Load_Zoomed (Sprites   : out Surface; -- Texture; -- Surface;
                          File_Name :     String;
                          Alpha     :     Boolean)
   is
--      Sprites : SDL.Video.Surfaces.Surface;
      Temp    : SDL.Video.Surfaces.Surface;
   begin
      SDL.Images.IO.Create (Temp, File_Name);
--    if(!temp)
--            return NULL;

      Sprites := Temp;
      --SDL.Video.Textures.Set_Alpha (Sprites, 200);  -- SDL_RLEACCEL, 255);
--      Clean_Alpha (Temp, Sprites);

      --      Sprites.Finalize; -- SDL_FreeSurface (Sprites);

--      if(!temp)
--      {
--              fprintf(stderr, "Could not clean alpha!\n");
--              return NULL;
--      }

      if Alpha then
         null;
--           SDL.Video.Textures.Set_Alpha (Temp, 0); -- SDL_SRCALPHA or SDL_RLEACCEL, 0);
--           --Sprites := SDL_DisplayFormatAlpha (Temp);
      else
         null;
         --           SDL.Video.Surfaces.Set_Colour_Key
--             (Temp, -- SDL_SRCCOLORKEY or SDL_RLEACCEL,
--              Now    => SDL.Video.Pixel_Formats.To_Pixel (Temp.Pixel_Format,0,0,0),
--              --SDL_MapRGB (Temp.format, 0, 0, 0),
--              Enable => True);
--           --Sprites := SDL_DisplayFormat (Temp);
      end if;
    --SDL_FreeSurface(temp);

      -- return Sprites;
   end Load_Zoomed;

   ---------------
   -- Print_Num --
   ---------------

   procedure Print_Num (Dst   : in out Surface;
                        Font  :        Surface;
                        X, Y  :        Integer;
                        Value :        Float)
   is
      use SDL.Video.Rectangles;
      Buf  : array (0 .. 9) of Natural;
      P    : Natural := Buf'First;
      Val  : Integer := Integer (Value * 10.0);
      Pos  : Integer;
   begin
      -- Sign
      if Val < 0 then
         Buf (P) := 10;
         P := P + 1;
         Val := -Val;
      end if;

      -- Integer part
      -- Pos := 10_000_000;
      Pos := 1_000;
      while Pos > 1 loop
         declare
            Num : constant Integer := Val / Pos;
         begin
            Val := Val - Num * Pos;
            pos := Pos / 10;
            if P /= 0 or Num /= 0 then
               Buf (P) := Num;
               P := P + 1;
            end if;
         end;
      end loop;

      -- Decimals
      if Val / Pos /= 0 then

         Buf (P) := 11;
         P := P + 1;
         while Pos > 0 loop
            declare
               Num : constant Integer := val / pos;
            begin
               Val     := Val - Num * Pos;
               Pos     := Pos / 10;
               Buf (P) := Num;
               P       := P + 1;
            end;
         end loop;
      end if;

--      Buf := (1,3,2,11,4,7,6,9,8,4);

      -- Render!
      declare
          use SDL.C;
          From : Rectangle := (0, 0, Width => 7, Height => 10);
          To   : Rectangle;
      begin
         for pos in 0 .. P - 1 loop
            To.X := Int (X + Pos * 7);
            To.Y := Int (Y);
            From.X := Int (Buf (Pos) * 7);
            Dst.Blit (Source      => Font,
                      Source_Area => From,
                      Self_Area   => To);
         end loop;
      end;

   end Print_Num;

   ---------------------------
   -- ballfield_t functions --
   ---------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure ballfield_init (BF : out Ballfield_Type) is

      package Random_Integers
      is new Ada.Numerics.Discrete_Random (Integer);
      use Random_Integers;

      Gen : Generator;
   begin
      Reset (Gen);

      for I in Ball_Index loop

         Bf.Points (I).X := Random (Gen) mod 16#20000#;
         Bf.Points (I).Y := Random (Gen) mod 16#20000#;
         Bf.Points (I).Z := 16#20000# * Integer (I) / BALLS;

         if Random (Gen) mod 100 > 80 then
            Bf.Points (I).C := 1;
         else
            BF.Points (I).C := 0;
         end if;

      end loop;
   end Ballfield_Init;

   ----------
   -- Free --
   ----------

   procedure Ballfield_Free (BF : in out Ballfield_Type)
   is
      pragma Unreferenced (BF);
   begin
      for I in Color_Type loop
         null; --Bf.Gfx (I).Finalize;
      end loop;
   end Ballfield_Free;

   -----------------------
   -- Initialize_Frames --
   -----------------------

   procedure Ballfield_Init_Frames (BF : in out Ballfield_Type)
   is
      use SDL.C;

      J     : int          := 0;
      Width : constant int := Bf.Gfx (0).Size.Width;

      subtype Index_Range is Natural
      range 0 .. Natural (Width) - 1;

   begin
      --
      --  Set up source rects for all frames
      --
      Bf.Frames := new Rectangle_Array'(Index_Range => <>);
--      if(!bf->frames)
--      {
--              fprintf(stderr, "No memory for frame rects!\n");
--              return -1;
--      }
      for I in Index_Range loop

         Bf.Frames (I) := (X      => 0,
                           Y      => J,
                           Width  => Width - int (I),
                           Height => Width - int (I));

         J := J + Width - int (I);
      end loop;

   end Ballfield_Init_Frames;

   --------------
   -- Load_Gfx --
   --------------

   procedure Ballfield_Load_Gfx (Bf    : in out Ballfield_Type;
                                 Name  :        String;
                                 Color :        Color_Type) is
   begin
      Load_Zoomed (Bf.Gfx (Color),
                   File_Name => Name,
                   Alpha     => Bf.Use_Alpha);

--    if(!bf->gfx[color])
--            return -2;

--    if(!bf->frames)
--            return ballfield_init_frames(bf);
--      Ballfield_Init_Frames (Bf);

--    return 0;
   end Ballfield_Load_Gfx;

   type Koord_Type is new Integer; --mod 16#20000#;

   ----------
   -- Move --
   ----------

   procedure Ballfield_Move (BF : in out Ballfield_Type;
                             Dx, Dy, Dz : Koord_Type) is
   begin
      for Point of BF.Points loop
         Point := (X => (Point.X + Integer (Dx)) mod 16#20000#,
                   Y => (Point.Y + Integer (Dy)) mod 16#20000#,
                   Z => (Point.Z + Integer (Dz)) mod 16#20000#,
                   C => Point.C);
      end loop;
      -- Debug (BF.Points (0).X'Image & "  " & BF.Points (1).Y'Image & "   " & Dx'Image);
   end Ballfield_Move;

   ------------
   -- Render --
   ------------

   procedure Ballfield_Render (BF : in out Ballfield_Type;
                               Screen : in out Surface)
   is
      use SDL.C;
      J : Ball_Index;
      Z : Integer;
   begin
      --
      --  Find the ball with the highest Z.
      --
      Z := 0;
      J := 0;
      for I in Ball_Index loop
         if BF.Points (I).Z > Z then
            J := I;
            Z := Bf.Points (I).Z;
         end if;
      end loop;

      --
      --  Render all balls in back->front order.
      --
      for I in 0 .. BALLS - 1 loop
         declare
            R : SDL.Video.Rectangles.Rectangle;
            F : Integer;
         begin
            Z := Bf.Points (J).Z;
            Z := Z + 50;

            F := Integer ((Bf.Frames (0).Width / 2**12) + 100000) / Z;
            F := Integer (Bf.Frames (0).Width) - F;
            if F < 0 then
               F := 0;
            elsif F > Integer (Bf.Frames (0).Width) - 1 then
               F := Integer (Bf.Frames (0).Width) - 1;
            end if;

            Z := Z / 2**7;
            Z := Z + 1;
            R.X := Int (Bf.Points (J).X - 16#10000#) / int (Z);
            R.Y := Int (Bf.Points (J).Y - 16#10000#) / int (Z);
            R.X := R.X + (Screen.Size.Width  - Bf.Frames (F).Width) / 2;
            R.Y := R.Y + (Screen.Size.Height - Bf.Frames (F).Height) / 2;
            Screen.Blit (Source      => Bf.Gfx (Bf.Points (J).C),
                         Source_Area => Bf.Frames (F),
                         Self_Area   => R);
            if J > 0 then
               J := J - 1;
            else
               j := BALLS - 1;
            end if;
         end;
      end loop;
   end Ballfield_Render;

   -------------------------------
   -- Other rendering functions --
   -------------------------------

   ---------------
   -- Tile_Back --
   ---------------
   --
   --  Draw tiled background image with offset.
   --
   procedure Tiled_Back (Back   :        Surface;
                         Screen : in out Surface;
                         Xo, Yo :        Integer)
   is
      use SDL.C;
      Width  : constant Natural := Natural (Back.Size.Width);
      Height : constant Natural := Natural (Back.Size.Height);
      Xoc    : constant Natural := (Xo + Width  * ((-Xo) / Width + 1))  mod Width;
      Yoc    : constant Natural := (Yo + Height * ((-Yo) / Height + 1)) mod Height;
      X, Y   : Integer;
   begin
      Y  := -Yoc;
      while Y < Integer (Screen.Size.Height) loop
         X := -Xoc;
         while X < Integer (Screen.Size.Width) loop
            declare
               R  : SDL.Video.Rectangles.Rectangle := (int (X), int (Y), 0, 0);
               SA : SDL.Video.Rectangles.Rectangle := (0, 0, 0, 0);
            begin
               Screen.Blit (Source      => Back,
                            Source_Area => SA,
                            Self_Area   => R);
            end;
            X := X + Width;
         end loop;
         Y := Y + Height;
      end loop;

   end Tiled_Back;

   ----------
   -- Main --
   ----------

   procedure Main is

      Balls            : Ballfield_Type;
      Window           : SDL.Video.Windows.Window;
      Screen           : SDL.Video.Surfaces.Surface;
      Back, Logo, Font : SDL.Video.Surfaces.Surface;

      Event  : SDL.Events.Events.Events;

--      bpp    : Integer := 0;
--      flags  : Integer := SDL_DOUBLEBUF or SDL_SWSURFACE;
      Alpha  : constant Boolean := True;
      X_Offs : Integer := 0;
      Y_Offs : Integer := 0;

--        Tick          : Long_Integer;
--        Last_Tick     : Long_Integer;
--        Last_Avg_Tick : Long_Integer;

      T  : Long_Float := 0.000;
      Dt : Float;
--      I  : Integer;

      FPS : Float := 0.0;
--        FPS_Count : Integer := 0;
--        FPS_start : Integer := 0;

      X_Speed, Y_Speed, Z_Speed : Float;

   begin
      if not SDL.Initialise (SDL.Enable_Screen) then
         raise Program_Error with "Could not initialise SDL";
      end if;

--      for(i = 1; i < argc; ++i)
--      {
--              if(strncmp(argv[i], "-na", 3) == 0)
--                      alpha = 0;
--              else if(strncmp(argv[i], "-nd", 3) == 0)
--                      flags &= ~SDL_DOUBLEBUF;
--              else if(strncmp(argv[i], "-h", 2) == 0)
--              {
--                      flags |= SDL_HWSURFACE;
--                      flags &= ~SDL_SWSURFACE;
--              }
--              else if(strncmp(argv[i], "-f", 2) == 0)
--                      flags |= SDL_FULLSCREEN;
--              else
--                      bpp = atoi(&argv[i][1]);
--      end loop;

--      Screen := SDL_SetVideoMode (SCREEN_W, SCREEN_H, bpp, flags);
      SDL.Video.Windows.Makers.Create (Window,
                                       Title    => "Ballfield",
                                       Position => (100, 100),
                                       Size     => (SCREEN_W, SCREEN_H) -- ,
                                       -- Bpp,
                                       ); -- Flags => False); --Flags);
      Screen := Window.Get_Surface;
--      SDL.Video.Renderers.Makers.Create (Renderer, Window, SDL.Video.Renderers.Present_V_Sync);
      --      if(!screen)
--      {
--              fprintf(stderr, "Failed to open screen!\n");
--              exit(-1);
--      }

--      SDL_WM_SetCaption("Ballfield", "Ballfield");
--      if(flags & SDL_FULLSCREEN)
--              SDL_ShowCursor(0);

      Ballfield_Init (Balls);
--      if(!balls)
--      {
--              fprintf(stderr, "Failed to create ballfield!\n");
--              exit(-1);
--      }

      --
      --  Load and prepare balls...
      --
      Balls.Use_Alpha := Alpha;

      Ballfield_Load_Gfx (Balls, "assets/blueball.png", 0);
      Ballfield_Load_Gfx (Balls, "assets/redball.png",  1);
      Ballfield_Init_Frames (Balls);

      --  Load background image
      SDL.Images.IO.Create (Back, "assets/redbluestars.png");
--      Back := SDL_DisplayFormat(temp_image);

      --  Load logo
      SDL.Images.IO.Create (Logo, "assets/logo.bmp");
--      SDL_SetColorKey (temp_image, SDL_SRCCOLORKEY or SDL_RLEACCEL,
--                      SDL_MapRGB (Temp_Image.format, 255, 0, 255));
--      logo := SDL_DisplayFormat (temp_image);

      --  Load font
      SDL.Images.IO.Create (Font, "assets/font7x10.bmp");
--      SDL_SetColorKey (Temp_Image, SDL_SRCCOLORKEY or SDL_RLEACCEL,
--                       SDL_MapRGB (Temp_Image.format, 255, 0, 255));
--      font := SDL_DisplayFormat (temp_image);

      Debug ("##1-8");

--      Last_Avg_Tick := SDL_GetTicks;
--      Last_Tick     := SDL_GetTicks;
      loop
         declare
            use SDL.Events;
         begin
            if Events.Poll (Event) then
               exit when Event.Common.Event_Type = Quit;
               exit when Event.Common.Event_Type = Keyboards.Key_Down;
            end if;
         end;

--           --  Timing
--           Tick      := SDL_GetTicks;
--         Dt        := (Tick - Last_Tick) * 0.001;
--           Last_Tick := Tick;
         Dt := 0.010;

         --  Background image
         Tiled_Back (Back, Screen, X_Offs / 2**11, Y_Offs / 2**11);

         --  Ballfield
         Ballfield_Render (Balls, Screen);

         --  Logo
         declare
            use SDL.Video.Rectangles;
            Destin_Area : Rectangle := (2, 2, 0, 0);
            Source_Area : Rectangle := (0, 0, 0, 0);
         begin
            Screen.Blit (Source      => Logo,
                         Source_Area => Source_Area,
                         Self_Area   => Destin_Area);
         end;

--           --  FPS counter
--           if tick > fps_start + 500 then
--              FPS       := Float (FPS_Count) * 1000.0 / (Tick - Fps_Start);
--              FPS_Count := 0;
--              FPS_Start := tick;
--           end if;
         FPS := 987.32; -- JQ !!!
         Print_Num (Screen, Font, Integer (Screen.Size.Width) - 137, Integer (Screen.Size.Height) - 12, FPS);
--           FPS_Count := FPS_Count + 1;

         -- !!!
--         SDL_Flip (screen);
         Window.Update_Surface;
--         Renderer.Present;

         --  Animate
         declare
            use Ada.Numerics.Elementary_Functions;
            FT : constant Float := Float (T);
         begin
            X_Speed := 500.0 * Sin (FT * 0.37);
            Y_Speed := 500.0 * Sin (FT * 0.53);
            Z_Speed := 400.0 * Sin (FT * 0.21);
         end;

         Ballfield_Move (Balls,
                         Koord_Type (X_Speed),
                         Koord_Type (Y_Speed),
                         Koord_Type (Z_Speed));

         X_Offs := X_Offs - Integer (X_Speed);
         Y_Offs := Y_Offs - Integer (Y_Speed);

         T := T + Long_Float (Dt);
         delay 0.010;
      end loop;

      Ballfield_Free (Balls);

      Back.Finalize;
      Logo.finalize;
      Font.Finalize;

      SDL.Finalise;

      Debug ("##4-6");
   end Main;

end Ballfield;
