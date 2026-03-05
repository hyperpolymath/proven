-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main: Entry point for the proven-neurosym server skeleton.
-- Prints the server identity, port, configuration constants,
-- and enumerates all protocol type constructors.

module Main

import Neurosym

%default total

------------------------------------------------------------------------
-- All constructors for each protocol type, collected as lists for
-- display purposes.
------------------------------------------------------------------------

||| All inference modes.
allInferenceModes : List InferenceMode
allInferenceModes = [Neural, Symbolic, Hybrid, Cascade]

||| All symbolic operations.
allSymbolicOps : List SymbolicOp
allSymbolicOps = [Unify, Resolve, Rewrite, Prove, Search, Constrain]

||| All neural operations.
allNeuralOps : List NeuralOp
allNeuralOps = [Embed, Classify, Generate, Attend, Retrieve, Finetune]

||| All fusion strategies.
allFusionStrategies : List FusionStrategy
allFusionStrategies = [NeuralThenSymbolic, SymbolicThenNeural, Parallel, Iterative, Gated]

||| All confidence levels.
allConfidenceLevels : List ConfidenceLevel
allConfidenceLevels = [Proven, HighConfidence, Moderate, LowConfidence, Uncertain, Contradicted]

||| All knowledge types.
allKnowledgeTypes : List KnowledgeType
allKnowledgeTypes = [Axiom, Learned, Inferred, Grounded, Hypothetical, Retracted]

------------------------------------------------------------------------
-- Main entry point
------------------------------------------------------------------------

main : IO ()
main = do
  putStrLn "proven-neurosym: Neurosymbolic Inference Server"
  putStrLn $ "  Port:               " ++ show neurosymPort
  putStrLn $ "  Max inference depth: " ++ show maxInferenceDepth
  putStrLn $ "  Default timeout:    " ++ show defaultTimeout ++ "s"
  putStrLn $ "  Max knowledge base: " ++ show maxKnowledgeBase
  putStrLn ""
  putStrLn $ "InferenceMode:   " ++ show allInferenceModes
  putStrLn $ "SymbolicOp:      " ++ show allSymbolicOps
  putStrLn $ "NeuralOp:        " ++ show allNeuralOps
  putStrLn $ "FusionStrategy:  " ++ show allFusionStrategies
  putStrLn $ "ConfidenceLevel: " ++ show allConfidenceLevels
  putStrLn $ "KnowledgeType:   " ++ show allKnowledgeTypes
