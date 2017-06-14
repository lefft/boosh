(* example of coq comment style *)
(* link to nice tutorial: 
    https://coq.inria.fr/tutorial-nahas *)

Theorem my_first_proof : (forall A : Prop, A -> A).
Proof.
  intros A.
  intros proof_of_A.
  exact proof_of_A.
Qed.



Theorem forward_small : (forall A B : Prop, A -> (A->B) -> B).
Proof.
 intros A.
 intros B.
 intros proof_of_A.
 intros A_implies_B.
 pose (proof_of_B := A_implies_B proof_of_A).
 exact proof_of_B.
Qed.


Theorem backward_small : (forall A B : Prop, A -> (A->B)->B).
Proof.
 intros A B.
 intros proof_of_A A_implies_B.
 refine (A_implies_B _).
   exact proof_of_A.
Qed.


Theorem backward_large : (forall A B C : Prop, A -> (A->B) -> (B->C) -> C).
Proof.
 intros A B C.
 intros proof_of_A A_implies_B B_implies_C.
 refine (B_implies_C _).
   refine (A_implies_B _).
     exact proof_of_A.
Qed.


Theorem backward_huge : (forall A B C : Prop, A -> (A->B) -> (A->B->C) -> C).
Proof.
 intros A B C.
 intros proof_of_A A_implies_B A_imp_B_imp_C.
 refine (A_imp_B_imp_C _ _).
   exact proof_of_A.

   refine (A_implies_B _).
     exact proof_of_A.
Qed.



Theorem forward_huge : (forall A B C : Prop, A -> (A->B) -> (A->B->C) -> C).
Proof.
 intros A B C.
 intros proof_of_A A_implies_B A_imp_B_imp_C.
 pose (proof_of_B := A_implies_B proof_of_A).
 pose (proof_of_C := A_imp_B_imp_C proof_of_A proof_of_B).
 exact proof_of_C.
Show Proof.
Qed.





(* Just to get started, let's prove that 
   adding an element to a list increases it's length by 1. *)

Theorem cons_adds_one_to_length :
   (forall A:Type, 
   (forall (x : A) (lst : list A), 
   length (x :: lst) = (S (length lst)))).
Proof.
  intros A.
  intros x lst.
  simpl.
  exact (eq_refl (S (length lst))).
Qed.
