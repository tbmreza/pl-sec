<!-- (* (lambda (cn) *) -->
<!-- (*   (lambda (f) *) -->
<!-- (*     (lambda (x) (f ((cn f) x))))) *) -->
<!-- let _cSUCC   = Abs (Abs (Abs (App (Ind 1, App (App (Ind 2, Ind 1), Ind 0))))) -->
<!--  -->
<!-- (* (lambda (cn) *) -->
<!-- (*   (lambda (ck) *) -->
<!-- (*     (lambda (f) *) -->
<!-- (*       (lambda (x) ((cn f) ((ck f) x)))))) *) -->
<!-- let _cPLUS   = Abs (Abs (Abs (Abs (App (App (Ind 3, Ind 1), App (App (Ind 2, Ind 1), Ind 0)))))) -->
<!--  -->
<!-- (* add1 *) -->
<!-- (* Abs (Abs (App (Ind 1, App (Ind 1, App (Ind 1, Ind 0))))) *) -->
<!-- (* PICKUP trace succ not using as_church *) -->
<!-- (* let () = assert_num_eq (App (cSUCC, Abs (Abs (App (Ind 1, App (Ind 1, App (Ind 1, Ind 0))))))) 4 *) -->
<!-- (* let () = assert_num_eq (App (cSUCC, as_church 9)) 10  (* fails *) *) -->
<!-- (* let _jj = App (App (cPLUS, as_church 3), as_church 2) *) -->
<!--  -->
<!-- (* (* [Clo (Abs (App (Ind 1, App (Ind 1, Ind 0))), Env [])] *) *) -->
<!-- (* Abs (App (App (Ind 3, Ind 1), App (App (Ind 2, Ind 1), Ind 0))) *) -->
<!-- (*  *) -->
<!-- (* [Clo (Abs (App (App (Ind 3, Ind 1), App (App (Ind 2, Ind 1), Ind 0))), *) -->
<!-- (*    Env *) -->
<!-- (*     [Clo (Abs (App (Ind 1, App (Ind 1, Ind 0))), Env []); *) -->
<!-- (*      Clo (Abs (App (Ind 1, App (Ind 1, App (Ind 1, Ind 0)))), Env [])])] *) -->
<!--  -->
dune exec cls

type env = Var -> Addr  -- in standard lc where appl is "replacing (binded) within body with arg, removing binder lambda"
type env = ?? -> Addr   -- in de bruijn notation where  "replacing 0 within body with arg, removing binder lambda (which means decr to other indices)"

type store = Addr -> Value

