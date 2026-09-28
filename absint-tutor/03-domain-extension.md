# Exercise 3: Domain Extension

## Concept
Simple domains like Signs or Intervals are often imprecise. We can construct more powerful domains using **Domain Combinators**.
*   **Direct Product**: Combining two domains independently.
*   **Reduced Product**: Sharing information between domains to increase precision (e.g., "x is positive" from Sign analysis might refine an interval `[-∞, 10]` to `[0, 10]`).
*   **Disjunctive Completion**: Allowing the abstract element to represent a set of abstract states (e.g., "x is [0,1] OR [5,6]"), useful for handling non-convex properties.

## Goal
Understand the benefit of the **Reduced Product** of Intervals and Parity.

## Problem Description
Let `A_Int` be the Interval domain and `A_Par` be the Parity domain (`{ ⊥, odd, even, ⊤ }`).
Consider the operation `x = x * 2` where initially `x ∈ [1, 5]`.

1.  **Analyze in A_Int**: `[1, 5] × 2 = [2, 10]`.
2.  **Analyze in A_Par**: `⊤ × even = even`.
3.  **Analyze in Direct Product**: result is `([2, 10], even)`.
4.  **Analyze with Reduction**:
    *   Suppose we have a condition `if (x == 3)`.
    *   In `A_Int`, `3 ∈ [2, 10]`, so the branch is taken.
    *   In `A_Par`, 3 is `odd`, but our value is `even`. `odd ⊓ even = ⊥`.
    *   The **Reduced Product** detects that this path is dead (unreachable).

## Task
Design a reduction operator `ρ: A_Int × A_Par → A_Int × A_Par`.
*   How can the Parity domain refine the Interval domain? (e.g. if interval is `[2, 2]` and parity is `odd`, result is `⊥`).
*   How can the Interval domain refine the Parity domain? (e.g. if interval is `[3, 3]`, parity must be `odd`).

## Implementation Task
Implement a generic `ProductDomain` functor/class that takes two domains and an optional reduction function.
*   **Recommended Tool**: OCaml Functors are perfect for this.
    ```ocaml
    module type DOMAIN = sig ... end
    module Product (D1 : DOMAIN) (D2 : DOMAIN) : DOMAIN = ...
    ```