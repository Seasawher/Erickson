/- # 掛け算 -/

/-- `k` に対して、`i + j = k` となるような `(i, j)` の組を全部生成する -/
def List.genDivide (k : Nat) : List (Nat × Nat) :=
  List.range (k + 1) |>.map (fun x => (x, k - x))

#guard List.genDivide 3 = [(0, 3), (1, 2), (2, 1), (3, 0)]

/-- 位取り記法に基づいて整数の積を計算するアルゴリズム

**この関数の実装は途中である**
-/
def fibonacciMultiply (x y : Array Nat) : Array Nat := Id.run do
  let mut hold := 0
  let n := x.size
  let m := y.size
  let mut z := Array.replicate (n + m) 0
  for k in [0 : n + m] do
    for (i, j) in List.genDivide k do
      if i ≥ n ∨ j ≥ m then
        continue
      hold := hold + x[i]! * y[j]!
    z := z.set! k (hold % 10)
    hold := hold / 10
  return z

-- #[1, 2, 8] が 821 を表している？
#eval fibonacciMultiply #[1, 2, 8] #[1]
#eval fibonacciMultiply #[8] #[8]
