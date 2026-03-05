-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Neurosym.Types: Core protocol types for the neurosymbolic inference server.
-- All types are closed sum types with total Show instances.

module Neurosym.Types

%default total

------------------------------------------------------------------------
-- InferenceMode
-- Determines how the server combines neural and symbolic subsystems
-- for a given inference request.
------------------------------------------------------------------------

||| The mode of inference to use when processing a request.
||| Neural uses learned representations, Symbolic uses formal rules,
||| Hybrid blends both simultaneously, and Cascade chains them in
||| sequence with fallback semantics.
public export
data InferenceMode : Type where
  ||| Pure neural network inference (embeddings, attention, generation).
  Neural   : InferenceMode
  ||| Pure symbolic reasoning (unification, resolution, rewriting).
  Symbolic : InferenceMode
  ||| Simultaneous blend of neural and symbolic subsystems.
  Hybrid   : InferenceMode
  ||| Sequential chaining: try one subsystem, fall back to the other.
  Cascade  : InferenceMode

export
Show InferenceMode where
  show Neural   = "Neural"
  show Symbolic = "Symbolic"
  show Hybrid   = "Hybrid"
  show Cascade  = "Cascade"

------------------------------------------------------------------------
-- SymbolicOp
-- The primitive operations available in the symbolic reasoning engine.
------------------------------------------------------------------------

||| A primitive operation in the symbolic reasoning subsystem.
public export
data SymbolicOp : Type where
  ||| Attempt to unify two terms under a substitution.
  Unify     : SymbolicOp
  ||| Apply resolution (e.g. SLD-resolution) to derive new clauses.
  Resolve   : SymbolicOp
  ||| Apply rewrite rules to transform terms.
  Rewrite   : SymbolicOp
  ||| Attempt to construct a proof of a proposition.
  Prove     : SymbolicOp
  ||| Search the knowledge base for matching facts or rules.
  Search    : SymbolicOp
  ||| Add a constraint to the constraint store.
  Constrain : SymbolicOp

export
Show SymbolicOp where
  show Unify     = "Unify"
  show Resolve   = "Resolve"
  show Rewrite   = "Rewrite"
  show Prove     = "Prove"
  show Search    = "Search"
  show Constrain = "Constrain"

------------------------------------------------------------------------
-- NeuralOp
-- The primitive operations available in the neural inference engine.
------------------------------------------------------------------------

||| A primitive operation in the neural inference subsystem.
public export
data NeuralOp : Type where
  ||| Produce a dense vector embedding of input data.
  Embed    : NeuralOp
  ||| Classify input into one of a set of categories.
  Classify : NeuralOp
  ||| Generate output tokens autoregressively.
  Generate : NeuralOp
  ||| Compute attention over input sequences.
  Attend   : NeuralOp
  ||| Retrieve nearest neighbours from a vector store.
  Retrieve : NeuralOp
  ||| Fine-tune model parameters on new data.
  Finetune : NeuralOp

export
Show NeuralOp where
  show Embed    = "Embed"
  show Classify = "Classify"
  show Generate = "Generate"
  show Attend   = "Attend"
  show Retrieve = "Retrieve"
  show Finetune = "Finetune"

------------------------------------------------------------------------
-- FusionStrategy
-- How the neurosymbolic server fuses neural and symbolic results.
------------------------------------------------------------------------

||| Strategy for fusing neural and symbolic inference results.
public export
data FusionStrategy : Type where
  ||| Run neural inference first, then refine with symbolic reasoning.
  NeuralThenSymbolic : FusionStrategy
  ||| Run symbolic reasoning first, then augment with neural inference.
  SymbolicThenNeural : FusionStrategy
  ||| Run both subsystems in parallel and merge results.
  Parallel           : FusionStrategy
  ||| Iterate between neural and symbolic until convergence.
  Iterative          : FusionStrategy
  ||| Use a learned gating function to blend outputs.
  Gated              : FusionStrategy

export
Show FusionStrategy where
  show NeuralThenSymbolic = "NeuralThenSymbolic"
  show SymbolicThenNeural = "SymbolicThenNeural"
  show Parallel           = "Parallel"
  show Iterative          = "Iterative"
  show Gated              = "Gated"

------------------------------------------------------------------------
-- ConfidenceLevel
-- The confidence the server assigns to an inference result.
------------------------------------------------------------------------

||| Confidence level assigned to an inference result, ranging from
||| formally proven down to actively contradicted.
public export
data ConfidenceLevel : Type where
  ||| Formally proven via symbolic reasoning.
  Proven         : ConfidenceLevel
  ||| High confidence from strong neural + symbolic agreement.
  HighConfidence : ConfidenceLevel
  ||| Moderate confidence; some supporting evidence.
  Moderate       : ConfidenceLevel
  ||| Low confidence; weak or sparse evidence.
  LowConfidence  : ConfidenceLevel
  ||| No confident determination could be made.
  Uncertain      : ConfidenceLevel
  ||| Actively contradicted by available evidence.
  Contradicted   : ConfidenceLevel

export
Show ConfidenceLevel where
  show Proven         = "Proven"
  show HighConfidence = "HighConfidence"
  show Moderate       = "Moderate"
  show LowConfidence  = "LowConfidence"
  show Uncertain      = "Uncertain"
  show Contradicted   = "Contradicted"

------------------------------------------------------------------------
-- KnowledgeType
-- Classification of entries in the knowledge base.
------------------------------------------------------------------------

||| Classification of a knowledge base entry by its epistemic status.
public export
data KnowledgeType : Type where
  ||| A foundational axiom accepted without proof.
  Axiom        : KnowledgeType
  ||| Knowledge acquired through training or fine-tuning.
  Learned      : KnowledgeType
  ||| Knowledge derived by the inference engine at runtime.
  Inferred     : KnowledgeType
  ||| Knowledge anchored to external data sources.
  Grounded     : KnowledgeType
  ||| A tentative hypothesis not yet confirmed.
  Hypothetical : KnowledgeType
  ||| Previously held knowledge that has been retracted.
  Retracted    : KnowledgeType

export
Show KnowledgeType where
  show Axiom        = "Axiom"
  show Learned      = "Learned"
  show Inferred     = "Inferred"
  show Grounded     = "Grounded"
  show Hypothetical = "Hypothetical"
  show Retracted    = "Retracted"
