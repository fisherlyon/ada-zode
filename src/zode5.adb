with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Assertions; use Ada.Assertions;

procedure Zode5 is

   ----------------------
   -- Type Definitions --
   ----------------------

   type ExprC_Kind is (NumC, IdC, StrC, IfC, LamC, AppC);

   type ExprC;

   type ExprC_Acc is access all ExprC;

   type Value_Kind is (NumV, BoolV, StrV, CloV, PrimV);

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
         when StrV =>
            Str : Unbounded_String;
         when CloV =>
            Param1 : Unbounded_String;
            Param2 : Unbounded_String;
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
            Param1 : Unbounded_String;
            Param2 : Unbounded_String;
            Bod : ExprC_Acc;
         when AppC =>
            Fun : ExprC_Acc;
            Arg1 : ExprC_Acc;
            Arg2 : ExprC_Acc;
      end case;
   end record;

   --  Takes a string and converts it to an unbounded string
   function Strify (S : String) return Unbounded_String is
   begin
      return To_Unbounded_String (S);
   end Strify;

   --  Extends an environment
   function Extend_Env
      (Env : EnvNode_Acc; Param : Unbounded_String; Val : Value_Acc)
         return EnvNode_Acc is
      Cur : EnvNode_Acc := Env;
   begin
      while Cur.Next /= null loop
         Cur := Cur.Next;
      end loop;
      Cur.Next := new EnvNode'(Content =>
         Binding'(Name => Param, Val => Val),
      Next => null);
      return Env;
   end Extend_Env;

   --  Array of Primops, without '+'
   type Primops is array (1 .. 6) of Unbounded_String;

   Primops_Array : constant Primops :=
      (Strify ("-"),
      Strify ("*"),
      Strify ("/"),
      Strify ("<="),
      Strify ("equal?"),
      Strify ("error"));

   --  Creates the Top Level Environment
   function Create_TLE return EnvNode_Acc is
      TLE : EnvNode_Acc :=
            new EnvNode'(Content =>
               Binding'(Name => Strify ("+"),
               Val =>
                  new Value'(Kind => PrimV, Op => Strify ("+"))),
            Next => null);
   begin
      for J in Primops_Array'Range loop
         TLE := Extend_Env (TLE,
            Primops_Array (J), new Value'(Kind => PrimV,
               Op => Primops_Array (J)));
      end loop;
      TLE := Extend_Env (TLE, Strify ("true"),
            new Value'(Kind => BoolV, Bool => True));
      TLE := Extend_Env (TLE, Strify ("false"),
            new Value'(Kind => BoolV, Bool => False));
      return TLE;
   end Create_TLE;

   --  Looks up an identifier in a environment and rerturns its value
   function Lookup (Id : Unbounded_String; Env : EnvNode_Acc)
      return Value_Acc is
      Cur : EnvNode_Acc := Env;
   begin
         while Cur /= null loop
            if Id = Cur.Content.Name then
               return Cur.Content.Val;
            end if;
            Cur := Cur.Next;
         end loop;
         --  Put_Line ("variable not found: " & To_String (Id));
         raise Constraint_Error;
   end Lookup;

   --  Evaluates PrimV
   function Eval (Op : Unbounded_String; l : Value_Acc; r : Value_Acc)
      return Value_Acc is
   begin
      if Op = "+" then
         return new Value'(Kind => NumV, Val => (l.Val + r.Val));
      elsif Op = "-" then
         return new Value'(Kind => NumV, Val => (l.Val - r.Val));
      elsif Op = "*" then
         return new Value'(Kind => NumV, Val => (l.Val * r.Val));
      elsif Op = "/" then
         return new Value'(Kind => NumV, Val => (l.Val / r.Val));
      elsif Op = "<=" then
         return new Value'(Kind => BoolV, Bool => (l.Val <= r.Val));
      elsif Op = "equal?" then
         return new Value'(Kind => BoolV, Bool => (l.Val = r.Val));
      elsif Op = "error" then
         raise Program_Error;
      else
         raise Program_Error;
      end if;
   end Eval;

   --  Interprets an ExprC
   function Interp (Expr : ExprC_Acc; Env : EnvNode_Acc)
      return Value_Acc is
   begin
      case Expr.Kind is
         when NumC =>
            return new Value'(Kind => NumV, Val => Expr.Val);
         when IdC =>
            return Lookup (Expr.Id, Env);
         when StrC =>
            return new Value'(Kind => StrV, Str => Expr.Str);
         when IfC =>
            declare
               Interped_Test : constant Value_Acc := Interp (Expr.Te, Env);
            begin
               if Interped_Test.Bool = True then
                  return Interp (Expr.Th, Env);
               else
                  return Interp (Expr.El, Env);
               end if;
            end;
         when LamC =>
            return new Value'(Kind => CloV,
               Param1 => Expr.Param1,
               Param2 => Expr.Param2,
               Bod => Expr.Bod,
               Env => Env);
         when AppC =>
            declare
               Fun_Val : constant Value_Acc := Interp (Expr.Fun, Env);
               Arg_Val1 : constant Value_Acc := Interp (Expr.Arg1, Env);
               Arg_Val2 : constant Value_Acc := Interp (Expr.Arg2, Env);
            begin
               case Fun_Val.Kind is
                  when CloV =>
                     declare
                        Env2 : EnvNode_Acc :=
                           Extend_Env (Env, Fun_Val.Param1, Arg_Val1);
                     begin
                        Env2 :=
                           Extend_Env (Env2, Fun_Val.Param2, Arg_Val2);
                        return Interp (Fun_Val.Bod, Env2);
                     end;
                  when PrimV =>
                     return Eval (Fun_Val.Op, Arg_Val1, Arg_Val2);
                  when NumV =>
                     raise Program_Error;
                  when BoolV =>
                     raise Program_Error;
                  when StrV =>
                     raise Program_Error;
               end case;
            end;
      end case;
   end Interp;

   --  Takes in a Value, returns its string representation
   function Serialize (Value : Value_Acc)
      return String is
   begin
      case Value.Kind is
         when NumV =>
            return Float'Image (Value.Val);
         when BoolV =>
            if Value.Bool then
               return "true";
            else
               return "false";
            end if;
         when StrV =>
            return To_String (Value.Str);
         when CloV =>
            return "#<procedure>";
         when PrimV =>
            return "#<primop>";
      end case;
   end Serialize;

   --  Interprets and Serializes!
   function Top_Interp (Expr : ExprC_Acc)
      return String is
   begin
      return Serialize
         (Interp (Expr, Create_TLE));
   end Top_Interp;

   -----------------------
   -- Example Instances --
   -----------------------

   Envr_Ex : constant EnvNode_Acc :=
      new EnvNode'(Content =>
         Binding'(Name => Strify ("nice"),
            Val => new Value'(Kind => NumV, Val => 6.9)),
         Next => new EnvNode'(Content =>
            Binding'(Name => Strify ("string"),
               Val => new Value'(Kind => StrV, Str => Strify ("cool"))),
            Next => null));

   Envr_Ex2 : constant EnvNode_Acc :=
      new EnvNode'(Content =>
         Binding'(Name => Strify ("x"),
            Val => new Value'(Kind => NumV, Val => 1.0)),
         Next => null);

   LamC_Ex1 : constant ExprC_Acc :=
      new ExprC'(Kind => LamC,
         Param1 => Strify ("x"),
         Param2 => Strify ("y"),
         Bod =>
            new ExprC'(Kind => AppC,
               Fun => new ExprC'(Kind => IdC, Id => Strify ("<=")),
               Arg1 => new ExprC'(Kind => IdC, Id => Strify ("x")),
               Arg2 => new ExprC'(Kind => IdC, Id => Strify ("y"))));

begin

   ----------------
   -- Test Cases --
   ----------------

   declare
      Lookup_Val : Value_Acc;
      Extended_Env : EnvNode_Acc;
   begin
      --  Lookup tests
      Lookup_Val := Lookup (Strify ("nice"), Envr_Ex);
      Assert (Lookup_Val.Kind = NumV);
      Assert (Lookup_Val.Val = 6.9);
      begin
         Lookup_Val := Lookup (Strify ("missing"), Envr_Ex);
         Assert (False);
      exception
         when Constraint_Error =>
            Assert (True);
      end;

      --  Extend-Env tests
      Extended_Env :=
         Extend_Env (Envr_Ex2, Strify ("y"),
            new Value'(Kind => NumV, Val => 2.0));
      Assert (Extended_Env.Content.Name = Strify ("x"));
      Assert (Extended_Env.Content.Val.Val = 1.0);
      Assert (Extended_Env.Next.Content.Name = Strify ("y"));
      Assert (Extended_Env.Next.Content.Val.Val = 2.0);

      --  Serialize tests
      Assert (Serialize
         (new Value'(Kind => NumV, Val => 4.0)) = Float'Image (4.0));
      Assert (Serialize
         (new Value'(Kind => BoolV, Bool => True)) = "true");
      Assert (Serialize
         (new Value'(Kind => BoolV, Bool => False)) = "false");
      Assert (Serialize
         (new Value'(Kind => StrV, Str => Strify ("Hello"))) = "Hello");
      Assert (Serialize
         (new Value'(Kind => CloV,
            Param1 => Strify ("x"),
            Param2 => Strify ("y"),
            Bod => new ExprC'(Kind => NumC, Val => 4.0),
            Env => Envr_Ex)) = "#<procedure>");
      Assert (Serialize
         (new Value'(Kind => PrimV, Op => Strify ("+"))) = "#<primop>");

      --  Eval  tests
      Assert (Eval
         (Strify ("+"), new Value'(Kind => NumV, Val => 2.0),
         new Value'(Kind => NumV, Val => 2.0)).Val = 4.0);
      Assert (Eval
         (Strify ("-"), new Value'(Kind => NumV, Val => 7.0),
         new Value'(Kind => NumV, Val => 2.0)).Val = 5.0);
      Assert (Eval
         (Strify ("*"), new Value'(Kind => NumV, Val => 3.0),
         new Value'(Kind => NumV, Val => 2.0)).Val = 6.0);
      Assert (Eval
         (Strify ("/"), new Value'(Kind => NumV, Val => 6.0),
         new Value'(Kind => NumV, Val => 2.0)).Val = 3.0);

      --  Top Interp tests
      Assert (Top_Interp
         (new ExprC'(Kind => IdC, Id => Strify ("true"))) = "true");

      Assert (Top_Interp (new ExprC'(Kind => AppC,
         Fun => (new ExprC'(Kind => IdC, Id => (Strify ("+")))),
         Arg1 => (new ExprC'(Kind => NumC, Val => 3.0)),
         Arg2 => (new ExprC'(Kind => NumC, Val => 3.0))))
            = Float'Image (6.0));

      Assert (Top_Interp (new ExprC'(Kind => AppC,
         Fun => (new ExprC'(Kind => IdC, Id => (Strify ("*")))),
         Arg1 => (new ExprC'(Kind => NumC, Val => 2.0)),
         Arg2 => (new ExprC'(Kind => NumC, Val => 3.0))))
            = Float'Image (6.0));

      Assert (Top_Interp (new ExprC'(Kind => AppC,
         Fun => LamC_Ex1,
         Arg1 => (new ExprC'(Kind => NumC, Val => 0.0)),
         Arg2 => (new ExprC'(Kind => NumC, Val => 5.0)))) = "true");

   end;
end Zode5;