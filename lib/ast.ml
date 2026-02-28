type visibility = Public | Private | Opaque

type pv_annot_kind = PVExact | PVLower | PVUpper

type ty_annot =
  | TyName of string
  | TyVar of string
  | TyArrow of ty_annot * ty_annot * eff_annot option
    (* param, return, optional effect annotation — None = infer *)
  | TyRecord of (string * ty_annot) list * bool  (* fields, is_open *)
  | TyList of ty_annot
  | TyArray of ty_annot
  | TyTuple of ty_annot list
  | TyApp of ty_annot list * string
  | TyQualified of string list * string
  | TyPolyVariant of pv_annot_kind * (string * ty_annot option) list
  | TyWithEffect of ty_annot * eff_annot
    (* type / effect — only valid in return type position of let declarations *)

and eff_annot =
  | EffAnnotPure                          (* / pure *)
  | EffAnnotRow of eff_item list          (* / IO, State int, 'e *)

and eff_item =
  | EffLabel of string * ty_annot list    (* IO, State int *)
  | EffVar of string                      (* 'e — effect row variable *)

type pattern =
  | PatWild
  | PatVar of string
  | PatInt of int
  | PatFloat of float
  | PatBool of bool
  | PatString of string
  | PatUnit
  | PatTuple of pattern list
  | PatCons of pattern * pattern
  | PatNil
  | PatConstruct of string * pattern option
  | PatRecord of (string * pattern) list
  | PatAs of pattern * string
  | PatOr of pattern * pattern
  | PatArray of pattern list
  | PatMap of (pattern * pattern) list
  | PatPolyVariant of string * pattern option
  | PatPin of string
  | PatAnnot of pattern * ty_annot

type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Gt | Le | Ge
  | And | Or
  | Concat
  | Land | Lor | Lxor | Lsl | Lsr
  | Pipe

type unop =
  | Neg | Not | Lnot

type expr =
  | EInt of int
  | EFloat of float
  | EBool of bool
  | EString of string
  | EByte of int
  | ERune of int
  | EUnit
  | EVar of string
  | ELet of string * expr * expr
  | ELetRec of string * string list * expr * expr
  | EFun of param * expr
  | EApp of expr * expr
  | EIf of expr * expr * expr
  | EBinop of binop * expr * expr
  | EUnop of unop * expr
  | ETuple of expr list
  | ERecord of (string * expr) list
  | ERecordUpdate of expr * (string * expr) list
  | EField of expr * string
  | EIndex of expr * expr
  | ECons of expr * expr
  | ENil
  | EList of expr list
  | EArray of expr list
  | EConstruct of string * expr option
  | EMatch of expr * (pattern * expr option * expr) list * bool
  | ELetMut of string * expr * expr
  | EAssign of string * expr
  | EFieldAssign of expr * string * expr
  | ESeq of expr * expr
  | EAnnot of expr * ty_annot
  | EWhile of expr * expr
  | EFor of string * expr * expr
  | EForFold of string * expr * string * expr * expr
  | EWhileLet of pattern * expr * expr
  | EBreak of expr option
  | EContinueLoop
  | EReturn of expr
  | ELetRecAnd of (string * string list * expr) list * expr
  | EPerform of string * expr
  | EHandle of expr * handler_arm list
  | EResume of expr * expr
  | EMap of (expr * expr) list
  | EMapTyped of string * (expr * expr) list
  | ESet of expr list
  | ECollTyped of string * expr list
  | EPolyVariant of string * expr option
  | ECoerce of expr * ty_annot
  | ELocalOpen of string * expr
  | ELoc of Token.loc * expr

and handler_arm =
  | HReturn of string * expr
  | HOp of string * string * string * expr

and param = {
  name: string;
  annot: ty_annot option;
  is_generated: bool;
}

type type_def =
  | TDVariant of (string * ty_annot option * ty_annot option) list
    (* (name, arg_type, return_type) -- return_type = Some for GADT constructors *)
  | TDRecord of (bool * string * ty_annot) list
  | TDAlias of ty_annot
  | TDNewtype of string * ty_annot    (* constructor_name, underlying_type *)

type constraint_ = string * string list
  (* class_name, tyvar_names — e.g. ("Show", ["a"]) *)

type decl =
  | DLet of string * param list * ty_annot option * constraint_ list * expr
  | DLetMut of string * expr
  | DLetRec of string * string list * param list * ty_annot option * constraint_ list * expr
  | DLetRecAnd of (string * string list * param list * ty_annot option * constraint_ list * expr) list
  | DType of string list * string * type_def * string list
  | DTypeAnd of (string list * string * type_def * string list) list
    (* type_params, name, def, deriving_classes *)
  | DExpr of expr
  | DClass of string * string list * (string list * string list) list * (string * ty_annot) list
    (* class_name, tyvar_names, fundeps as [(from_tyvars, to_tyvars)], [(method_name, method_type)] *)
  | DInstance of string * ty_annot list * constraint_ list * (string * param list * expr) list
    (* class_name, instance_types, constraints, [(method_name, params, body)] *)
  | DEffect of string * string list * (string * ty_annot) list
    (* effect_name, type_params, [(op_name, op_type)] *)
  | DExtern of string * ty_annot
  | DModule of string * module_decl list
  | DOpen of string * string list option
    (* module_name, None = open all, Some = selective *)

and module_decl = {
  vis: visibility;
  decl: decl;
}

type program = decl list
