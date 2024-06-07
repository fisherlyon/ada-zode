with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure Zode5 is

   ----------------------
   -- Type Definitions --
   ----------------------

   type ExprC_Kind is (NumC, IdC, StrC, IfC, LamC, AppC);

   type ExprC;

   type ExprC_Acc is access all ExprC;

   type Value_kind is (NumV, BoolV, StrV, CLoV, PrimV);

   type Value;

   type Value_Acc is access all Value;

   type Binding is record
      Name : Unbounded_String;
      Val : Value_Acc;
   end record;

   type EnvNode;

   type EnvNode_Acc is access all EnvNode;

   type EnvNode is record
      Content : Binding;
      Next : EnvNode_Acc;
   end record;

   type Value (Kind : Value_Kind) is record
      case Kind is
         when NumV =>
            Val : Float;
         when BoolV =>
            Bool : Boolean;
         when strV => 
            Str : Unbounded_String;
         when CloV =>
            Param : Unbounded_String;
            Bod : ExprC_Acc;
            Env : EnvNode_Acc;
         when PrimV =>
            Op : Unbounded_String;
      end case;
   end record;

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

   --  Looks up an identifier in a environment and rerturns its value
   function Lookup
      (Id : Unbounded_String; Env : EnvNode_Acc) return Value_Acc is
   begin
      declare
         Cur : EnvNode_Acc := Env;
      begin
         while Cur /= null loop
            if Id = Cur.Content.Name then
               return Cur.Content.Val;
            end if;
            Cur := Cur.Next;
         end loop;
         raise Constraint_Error;
      end;
   end Lookup;

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

   Envr_Ex : aliased EnvNode :=
      EnvNode'(Content =>
         Binding'(Name => Strify ("nice"),
            Val => new Value'(Kind => NumV, Val => 6.9)),
         Next => new EnvNode'(Content =>
            Binding'(Name => Strify ("string"),
               Val => new Value'(Kind => StrV, Str => Strify ("cool"))),
            Next => null));

begin
   ----------------
   -- Test Cases --
   ----------------

   declare
      Lookup_Val : Value_Acc;
   begin
      Assert (NumC_Ex.Kind = NumC);
      Assert (NumC_Ex.Val = 4.0);
      Assert (IdC_Ex.Kind = IdC);
      Assert (IdC_Ex.Id = Strify ("x"));
      Assert (StrC_Ex.Kind = StrC);
      Assert (StrC_Ex.Str = Strify ("Hello!"));
      Assert (IfC_Ex.Kind = IfC);
      Assert (IfC_Ex.Te.Kind = IdC);
      Assert (IfC_Ex.Th.Kind = StrC);
      Assert (IfC_Ex.El.Kind = StrC);
      Assert (IfC_Ex.Te.Id = Strify ("true"));
      Assert (IfC_Ex.Th.Str = Strify ("good"));
      Assert (IfC_Ex.El.Str = Strify ("bad"));
      Lookup_Val := Lookup (Strify ("nice"), Envr_Ex'Access);
      Assert (Lookup_Val.Kind = NumV);
      Assert (Lookup_Val.Val = 6.9);
      begin
         Lookup_Val := Lookup (Strify ("missing"), Envr_Ex'Access);
         Assert (False);
      exception
         when Constraint_Error =>
            Assert (True);
      end;
   end;
end Zode5;