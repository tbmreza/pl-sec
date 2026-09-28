--- https://gist.github.com/kmicinski/bcdf71dcc8f4ee3d6766e52d76e67b5e

-- see cek.slog
def var := String

-- terms
inductive term : Type where
  | Ref : var → term
  | Lam : var → term → term
  | App : term → term → term
open term

-- values and environments
inductive value : Type where
  | Clo : term → environment → value
open value

-- environments, defunctionalized
inductive environment : Type where
  | Empty : environment
  | Ext_env : environment → var → value → environment
open environment

inductive env_map : environment → var → value → Prop where
  | Env_hit : ∀ env x clo, env_map (Ext_env env x clo) x clo
  | Env_miss : ∀ env x y clo, x ≠ y → env_map env y v → env_map (Ext_env env x clo) y v
open env_map

-- Continuations
inductive continuation : Type where
  | Halt : continuation
  | Ar_k : term → environment → continuation → continuation
  | Fn_k : value → continuation
open continuation

inductive state : Type where
  | Cek : term → environment → continuation → state
  | Ret : value → continuation → state
open state

inductive eval : (ς : state) → (v : value) → Prop where
  | Eval_ref : ∀ x env kont v, env_map env x v → eval (Ret v kont) v → eval (Cek (Ref x) env kont) v
  | Eval_lam : ∀ x e_body env kont v, eval (Ret (Clo (Lam x e_body) env) kont) v → eval (Ret (Clo (Lam x e_body) env) kont) v 
  | Eval_app : ∀ e_f e_a env kont v, eval (Cek e_f env (Ar-k e_a env kont)) v → eval (Cek (App e_f e_a) env kont) v
  | Eval_halt : ∀ v, eval (Ret v Halt) v
open eval

theorem id_ev : eval (Cek Empty (Lam "x" (Ref "x")) Halt) (Clo (Lam "x" (Ref "x")) Empty) :=
  by
    sorry
