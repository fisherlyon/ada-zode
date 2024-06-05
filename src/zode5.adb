with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Zode5 is

   ----------------------
   -- Type Definitions --
   ----------------------

   type ExprC_Kind is (NumC, IdC, StrC, IfC, LamC, AppC);

   type ExprC;

   type ExprC_Acc is access all ExprC;

   type ExprC (Kind : ExprC_Kind) is record
      case Kind is
         when NumC =>
            Val : Float;
         when IdC =>
            Id : Unbounded_String;
         when StrC =>
            Str : Unbounded_String;
         when IfC =>
            Te : ExprC_Acc;
            Th : ExprC_Acc;
            El : ExprC_Acc;
         when LamC =>
            Param : Unbounded_String;
            Bod : ExprC_Acc;
         when AppC =>
            Fun : ExprC_Acc;
            Arg : ExprC_Acc;
      end case;
   end record;

   --  Takes a string and converts it to an unbounded string
   function Strify (S : String) return Unbounded_String is
   begin
      return To_Unbounded_String (S);
   end Strify;

   -----------------------
   -- Example Instances --
   -----------------------

   NumC_Ex : constant ExprC :=
      ExprC'(Kind => NumC,
         Val => 4.0);

   IdC_Ex : constant ExprC :=
      ExprC'(Kind => IdC,
         Id => Strify ("x"));

   StrC_Ex : constant ExprC :=
      ExprC'(Kind => StrC,
         Str => Strify ("Hello!"));

   IfC_Ex : constant ExprC :=
      ExprC'(Kind => IfC,
         Te => new ExprC'(Kind => IdC,
            Id => Strify ("true")),
         Th => new ExprC'(Kind => StrC,
            Str => Strify ("good")),
         El => new ExprC'(Kind => StrC,
            Str => Strify ("bad")));

begin
   Put_Line ("Example NumC Value : " & Float'Image (NumC_Ex.Val));
   Put_Line ("Example IdC Identifier : " & To_String (IdC_Ex.Id));
   Put_Line ("Example StrC String : " & To_String (StrC_Ex.Str));
   Put_Line ("Example IfC Test: " & To_String (IfC_Ex.Te.Id));
   Put_Line ("Example IfC Then: " & To_String (IfC_Ex.Th.Str));
   Put_Line ("Example IfC Else: " & To_String (IfC_Ex.El.Str));
end Zode5;