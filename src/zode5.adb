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

   --  Extends an environment
   function Extend_Env
      (Env : EnvNode_Acc; Param : Unbounded_String; Val : Value_Acc)
         return EnvNode_Acc is
   begin
      declare
         Cur : EnvNode_Acc := Env;
      begin
         while Cur.Next /= null loop
            Cur := Cur.Next;
         end loop;
         Cur.Next := new EnvNode'(Content =>
            Binding'(Name => Param, Val => Val),
         Next => null);
      end;
      return Env;
   end Extend_Env;

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
               Interped_Test : Value_Acc := Interp (Expr.Te, Env);
            begin
               if Interped_Test.Bool = True then
                  return Interp (Expr.Th, Env);
               else
                  return Interp (Expr.El, Env);
               end if;
            end;
         when LamC =>
            return new Value'(Kind => CloV,
               Param => Expr.Param,
               Bod => Expr.Bod,
               Env => Env);
         when AppC =>
            declare
               Fun_Val : Value_Acc := Interp (Expr.Fun, Env);
               Arg_Val : Value_Acc := Interp (Expr.Arg, Env);
               Env2 : EnvNode_Acc :=
                  Extend_Env (Env, Fun_Val.Param, Arg_Val);
            begin
               return Interp (Fun_Val.Bod, Env2);
            end;
      end case;
   end Interp;

   --  Interprets and Serializes!

   --  Takes a string and converts it to an unbounded string
   function Strify (S : String) return Unbounded_String is
   begin
      return To_Unbounded_String (S);
   end Strify;

   -----------------------
   -- Example Instances --
   -----------------------

   Envr_Ex : aliased EnvNode :=
      EnvNode'(Content =>
         Binding'(Name => Strify ("nice"),
            Val => new Value'(Kind => NumV, Val => 6.9)),
         Next => new EnvNode'(Content =>
            Binding'(Name => Strify ("string"),
               Val => new Value'(Kind => StrV, Str => Strify ("cool"))),
            Next => null));

   Envr_Ex2 : aliased EnvNode :=
      EnvNode'(Content =>
         Binding'(Name => Strify ("x"),
            Val => new Value'(Kind => NumV, Val => 1.0)),
         Next => null);

begin
   ----------------
   -- Test Cases --
   ----------------

   declare
      Lookup_Val : Value_Acc;
      Extended_Env : EnvNode_Acc;
   begin

      --  Lookup tests
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

      --  Extend-Env tests
      Extended_Env :=
         Extend_Env (Envr_Ex2'Access, Strify ("y"),
            new Value'(Kind => NumV, Val => 2.0));
      Assert (Extended_Env.Content.Name = Strify ("x"));
      Assert (Extended_Env.Content.Val.Val = 1.0);
      Assert (Extended_Env.Next.Content.Name = Strify ("y"));
      Assert (Extended_Env.Next.Content.Val.Val = 2.0);
   end;
end Zode5;