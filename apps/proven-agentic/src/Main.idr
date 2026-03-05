-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Main: Entry point for the proven-agentic server skeleton.
-- Prints the server identity, port, configuration constants,
-- and enumerates all protocol type constructors.

module Main

import Agentic

%default total

------------------------------------------------------------------------
-- All constructors for each protocol type, collected as lists for
-- display purposes.
------------------------------------------------------------------------

||| All agent states.
allAgentStates : List AgentState
allAgentStates = [Idle, Planning, Acting, Observing, Reflecting, Blocked, Terminated]

||| All tool call kinds.
allToolCalls : List ToolCall
allToolCalls = [Execute, Query, Transform, Communicate, Delegate, Escalate]

||| All plan step kinds.
allPlanSteps : List PlanStep
allPlanSteps = [Action, Condition, Loop, Branch, Parallel, Checkpoint, Rollback]

||| All coordination strategies.
allCoordinations : List Coordination
allCoordinations = [Solo, Collaborative, Competitive, Hierarchical, Swarm, Consensus]

||| All safety check outcomes.
allSafetyChecks : List SafetyCheck
allSafetyChecks = [Approved, Denied, Escalated, Timeout, Sandboxed, HumanRequired]

||| All memory types.
allMemoryTypes : List MemoryType
allMemoryTypes = [Working, Episodic, Semantic, Procedural, Shared]

------------------------------------------------------------------------
-- Main entry point
------------------------------------------------------------------------

main : IO ()
main = do
  putStrLn "proven-agentic: Multi-Agent Coordination Server"
  putStrLn $ "  Port:           " ++ show agenticPort
  putStrLn $ "  Max agents:     " ++ show maxAgents
  putStrLn $ "  Max plan depth: " ++ show maxPlanDepth
  putStrLn $ "  Safety timeout: " ++ show safetyTimeout ++ "s"
  putStrLn $ "  Max tool calls: " ++ show maxToolCalls
  putStrLn ""
  putStrLn $ "AgentState:   " ++ show allAgentStates
  putStrLn $ "ToolCall:     " ++ show allToolCalls
  putStrLn $ "PlanStep:     " ++ show allPlanSteps
  putStrLn $ "Coordination: " ++ show allCoordinations
  putStrLn $ "SafetyCheck:  " ++ show allSafetyChecks
  putStrLn $ "MemoryType:   " ++ show allMemoryTypes
