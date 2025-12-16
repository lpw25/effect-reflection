
# Section 1

- Expand on the motivation in Section 1 and give more surrounding
  context.

- Include an example from Effekt in Section 1 to illustrate lexical
  effect handlers. It should also explicitly discuss the trade-offs
  bewteen lexical and non-lexical handlers.

- Delay the full OxCaml state example until Section 2, rather than
  repeating it, to give us some more space.

- In Section 1, replace the overview of the paper with an explicit list
  of contributions based on the list from the rebuttal.

- Include the shift from countable to bounded Lawvere theories as a
  contribution.

# Section 3

- Add crisp version of rec.

- Add a modality nonterminal to the syntax ranging over box and an
  identity modality. Use this for bindings in the context. Then also
  use it for case and rec to avoid needing to duplicate so many things.

# Section 4

- Add additional citations to section 4. There are currently no
  citations for the general background material (e.g. Lawvere theories)
  or the semantics of algebraic effects.

- Add the following lemmas about Psh(Law^op):

    * It is a topos. This relies on coslices in Law being essentially small.
    * It has a natural number object
    * The Yoneda Lemma and Yoneda embedding work. I think this follows
      from Law being locally small.

  The proofs of these will be added to the appendices.

- Give more detailed descriptions of the following constructions:

    * The construction of the free algebras monad

    * The Yoneda Embedding

    * The Yoneda Lemma

    * Lemmas 4.7 and 4.8

- Add the semantics for the crisp version of case

- Add the semantics for the crisp induction on W types

# Section 5

- Add some sentences to Section (Implementation) discussing the
  performance of the implementation compared to directly using OCaml's
  algebraic effects.

# Section 6

- Rework parts of Section (Related Work) to improve the comparison with
  other work.

# Appendices

- Write the proof for subject reduction

- Write the proof for strong normalization

- Write proofs of the lemmas about Psh(Law^op)

- Write out full denotational semantics

- Write the proof for soundness

- Write the definition of reduction context

- Write definition of contextual equality

- Write the proof for compositionality

- Write the proof for adequacy

# Misc

- Add an artifact to the submission. Just the contents Figure 8 plus
  some build rules and instructions.

