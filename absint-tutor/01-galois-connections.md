# Exercise 1: Galois Connections

## Concept
A **Galois Connection** is the formal framework relating a **concrete domain** (precise semantics) and an **abstract domain** (approximate semantics). It consists of two monotonic functions:
*   **Abstraction (α)**: Maps a set of concrete states to an abstract element.
*   **Concretization (γ)**: Maps an abstract element back to the set of concrete states it represents.

Formally, `(C, ⊆)` and `(A, ⊑)` form a Galois Connection `(C, ⊆) ⇄ (A, ⊑)` (via α, γ) if for all `c ∈ C` and `a ∈ A`:
`α(c) ⊑ a ⇔ c ⊆ γ(a)`

## Goal
Define a Galois Connection for the **Sign Analysis** domain.

## Problem Description
The concrete domain `C` is the powerset of integers, `𝒫(ℤ)`.
The abstract domain `A` is the set of signs: `Sign = { ⊥, -, 0, +, ⊤ }`.

1.  **Define the partial order ⊑ for `Sign`.** Draw the Hasse diagram (lattice structure).
2.  **Define the concretization function γ: Sign → 𝒫(ℤ).**
    *   Hint: What integers does `+` represent? What does `⊤` represent?
3.  **Derive the abstraction function α: 𝒫(ℤ) → Sign.**
    *   Ensure your definition satisfies the Galois Connection property: `α(S) ⊑ a ⇔ S ⊆ γ(a)`.

## Implementation Task (Optional)
Implement the `Sign` lattice in a language of your choice (e.g., OCaml, Haskell).
*   Define the type for `Sign`.
*   Implement functions for `join` (⊔), `meet` (⊓), and `leq` (⊑).
*   Implement `alpha` and `gamma`.

**Recommended Tool**: OCaml or Haskell.
```ocaml
(* OCaml Hint *)
type sign = Bot | Neg | Zero | Pos | Top
```
