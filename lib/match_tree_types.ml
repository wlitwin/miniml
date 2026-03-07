(* Decision tree IR types for pattern match compilation.

   These types are parametric over the expression type 'expr to break the
   circular dependency between Typechecker and Match_tree. Typechecker
   instantiates 'expr with texpr. *)

(* ---- Map keys (shared by access paths and tests) ---- *)

type map_key =
  | MKInt of int
  | MKString of string
  | MKBool of bool
  | MKFloat of float
  | MKPin of string

(* ---- Access paths ---- *)

type access =
  | ATupleField of int
  | ARecordField of string * int (* field name, positional index *)
  | AVariantPayload (* unwrap variant constructor's payload *)
  | AConsHead
  | AConsTail
  | AArrayElem of int
  | AMapValue of map_key (* extract value from map by key *)

type occurrence = access list

(* ---- Tests ---- *)

type test =
  | TConstructor of string * int (* constructor name, tag *)
  | TPolyVariant of string * int (* tag name, hash tag *)
  | TBoolLit of bool
  | TIntLit of int
  | TFloatLit of float
  | TStringLit of string
  | TUnit
  | TNil
  | TCons
  | TArrayLen of int
  | TMapHasKey of map_key
  | TPin of string

(* ---- Bindings ---- *)

type binding = { var_name : string; bind_occ : occurrence; bind_ty : Types.ty }

(* ---- Decision tree ---- *)

type 'expr dtree =
  | DSwitch of {
      occ : occurrence;
      occ_ty : Types.ty;
      cases : (test * binding list * 'expr dtree) list;
      default : 'expr dtree option;
    }
  | DGuard of {
      arm_idx : int;
      guard : 'expr;
      bindings : binding list;
      on_true : 'expr dtree;
      on_false : 'expr dtree;
    }
  | DLeaf of { arm_idx : int; bindings : binding list }
  | DFail of Token.loc

(* ---- Compiled match ---- *)

type 'expr match_arm = { arm_body : 'expr }

type 'expr compiled_match = {
  scrutinee : 'expr;
  scrutinee_ty : Types.ty;
  match_arms : 'expr match_arm array;
  tree : 'expr dtree;
  loc : Token.loc;
  match_kind : Ast.match_kind;
}
