# Exercise 2: Widening & Narrowing

## Concept
In domains with **infinite height** (like Intervals), standard Kleene iteration (⊥, f(⊥), f(f(⊥)), ...) might not terminate.
*   **Widening (∇)**: An operator used to "jump" up in the lattice to ensure termination. It usually extrapolates the growth of the range.
*   **Narrowing (△)**: An operator used after a fixpoint is approximated (post-widening) to refine the result and recover precision.

## Goal
Apply Widening and Narrowing to the **Interval Analysis** domain.

## Problem Description
Consider the Interval domain where elements are `[l, u]` with `l, u ∈ ℤ ∪ {-∞, +∞}`.
Consider the program loop:
```c
x = 0;
while (x < 100) {
    x = x + 1;
}
```
The transfer function for the loop body is effectively `F(I) = [0, 0] ⊔ (I + [1, 1]) ⊓ [-∞, 99]`.

1.  **Simulate Kleene Iteration**: Try to calculate the fixpoint of `F` starting from `⊥`. Does it terminate quickly?
2.  **Define a Widening Operator ∇**:
    *   Standard Interval Widening: If an upper bound is unstable (increasing), jump to `+∞`. If a lower bound is unstable (decreasing), jump to `-∞`.
3.  **Apply Widening**: Calculate the sequence `X₀ = ⊥`, `Xₙ₊₁ = Xₙ ∇ F(Xₙ)`. Find the limit `X^∇`.
4.  **Apply Narrowing**: Starting from `X^∇`, calculate `Y₀ = X^∇`, `Yₙ₊₁ = Yₙ △ F(Yₙ)`. Does this recover the bound 100?

## Implementation Task
Implement the Interval domain with Widening and Narrowing.
*   **Recommended Tool**: OCaml/Haskell for the solver logic.
*   **Hint**:
    ```haskell
    -- Haskell Hint
    data Interval = Bot | Intv (ExtendedInt, ExtendedInt)
    widen (Intv (l1, u1)) (Intv (l2, u2)) = ...
    ```