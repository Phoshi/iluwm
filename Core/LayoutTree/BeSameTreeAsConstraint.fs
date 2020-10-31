namespace Twime

open NUnit.Framework.Constraints
open Twime

module BeSameTreeAs =
    open LayoutTree
    
    let dereferencedTree (tree: T) =
        let nullRef = TreeReference.zero
        
        cataTree
            (fun (_, wi) -> WindowNode (nullRef, wi))
            (fun (_, ci, n) -> ContainerNode (nullRef, ci, n))
            tree
        
    let dereferencedTree' tree =
        maybe {
            let! t = tree
            return dereferencedTree t
        }

    let eq (tree1: obj) (tree2: obj) =
        match (tree1, tree2) with
        | ((:? T as t1), (:? T as t2)) ->
           (dereferencedTree t1) = (dereferencedTree t2)
        | ((:? option<T> as t1), (:? option<T> as t2)) ->
            (dereferencedTree' t1) = (dereferencedTree' t2)
        | ((:? option<T> as t1), (:? T as t2)) ->
            (dereferencedTree' t1) = (Some (dereferencedTree t2))
        | ((:? T as t1), (:? option<T> as t2)) ->
            (Some (dereferencedTree t1)) = (dereferencedTree' t2)
        | _ -> false
    
    let someTree (tree: obj) =
        match tree with
        | ((:? T as t)) -> Some t
        | ((:? option<T> as t)) -> t
        | _ -> None
    
    type SameTreeConstraint(expected) =
        inherit Constraint()
        
        member this.Expected = expected
        
        override this.ApplyTo<'TActual> (actual: 'TActual) : ConstraintResult =
            if (eq this.Expected actual) then
                ConstraintResult(this, actual, true)
            else
                failwith (
                    "Expected: "
                    + (LayoutTreeWriter.writeTree expected 1)
                    + " / Actual: "
                    + (someTree actual |> Option.map (fun t -> LayoutTreeWriter.writeTree t 1) |> Option.defaultValue "None"))

    let beSameTreeAs = SameTreeConstraint

