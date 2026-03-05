-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- Agentic.Types: Core protocol types for the multi-agent coordination
-- server. All types are closed sum types with total Show instances.

module Agentic.Types

%default total

------------------------------------------------------------------------
-- AgentState
-- The lifecycle state of an individual agent in the system.
------------------------------------------------------------------------

||| The current lifecycle state of an agent. Agents transition through
||| these states as they plan, act, observe results, and reflect on
||| outcomes.
public export
data AgentState : Type where
  ||| Agent is idle, awaiting a task or instruction.
  Idle       : AgentState
  ||| Agent is constructing a plan to achieve its goal.
  Planning   : AgentState
  ||| Agent is executing an action (tool call, delegation, etc.).
  Acting     : AgentState
  ||| Agent is observing the results of its most recent action.
  Observing  : AgentState
  ||| Agent is reflecting on observations to update its beliefs.
  Reflecting : AgentState
  ||| Agent is blocked, waiting on an external dependency.
  Blocked    : AgentState
  ||| Agent has terminated (successfully or otherwise).
  Terminated : AgentState

export
Show AgentState where
  show Idle       = "Idle"
  show Planning   = "Planning"
  show Acting     = "Acting"
  show Observing  = "Observing"
  show Reflecting = "Reflecting"
  show Blocked    = "Blocked"
  show Terminated = "Terminated"

------------------------------------------------------------------------
-- ToolCall
-- The kind of tool invocation an agent can make.
------------------------------------------------------------------------

||| Classification of tool calls an agent may issue during execution.
public export
data ToolCall : Type where
  ||| Execute a command or function with side effects.
  Execute     : ToolCall
  ||| Query a data source or knowledge base (read-only).
  Query       : ToolCall
  ||| Transform data from one representation to another.
  Transform   : ToolCall
  ||| Send a message to another agent or external system.
  Communicate : ToolCall
  ||| Delegate a subtask to another agent.
  Delegate    : ToolCall
  ||| Escalate a decision to a higher authority or human.
  Escalate    : ToolCall

export
Show ToolCall where
  show Execute     = "Execute"
  show Query       = "Query"
  show Transform   = "Transform"
  show Communicate = "Communicate"
  show Delegate    = "Delegate"
  show Escalate    = "Escalate"

------------------------------------------------------------------------
-- PlanStep
-- A node in an agent's execution plan.
------------------------------------------------------------------------

||| A single step in an agent's execution plan. Plans are trees of
||| these nodes, supporting sequential, conditional, and parallel
||| execution with checkpointing and rollback.
public export
data PlanStep : Type where
  ||| A concrete action to perform.
  Action     : PlanStep
  ||| A conditional branch based on a predicate.
  Condition  : PlanStep
  ||| A loop over a sequence of steps.
  Loop       : PlanStep
  ||| A branch point with multiple possible paths.
  Branch     : PlanStep
  ||| Execute multiple steps concurrently.
  Parallel   : PlanStep
  ||| Save a checkpoint for potential rollback.
  Checkpoint : PlanStep
  ||| Roll back to a previously saved checkpoint.
  Rollback   : PlanStep

export
Show PlanStep where
  show Action     = "Action"
  show Condition  = "Condition"
  show Loop       = "Loop"
  show Branch     = "Branch"
  show Parallel   = "Parallel"
  show Checkpoint = "Checkpoint"
  show Rollback   = "Rollback"

------------------------------------------------------------------------
-- Coordination
-- The coordination strategy used among multiple agents.
------------------------------------------------------------------------

||| Strategy for coordinating multiple agents working together.
public export
data Coordination : Type where
  ||| A single agent working alone.
  Solo          : Coordination
  ||| Agents cooperating toward a shared goal.
  Collaborative : Coordination
  ||| Agents competing (e.g. adversarial search, red-teaming).
  Competitive   : Coordination
  ||| Agents organised in a chain-of-command hierarchy.
  Hierarchical  : Coordination
  ||| Decentralised swarm behaviour with emergent coordination.
  Swarm         : Coordination
  ||| Agents reaching decisions via consensus protocols.
  Consensus     : Coordination

export
Show Coordination where
  show Solo          = "Solo"
  show Collaborative = "Collaborative"
  show Competitive   = "Competitive"
  show Hierarchical  = "Hierarchical"
  show Swarm         = "Swarm"
  show Consensus     = "Consensus"

------------------------------------------------------------------------
-- SafetyCheck
-- The outcome of a safety evaluation before an agent acts.
------------------------------------------------------------------------

||| Result of a safety check performed before an agent action is
||| allowed to proceed.
public export
data SafetyCheck : Type where
  ||| Action approved by the safety system.
  Approved      : SafetyCheck
  ||| Action denied by the safety system.
  Denied        : SafetyCheck
  ||| Action escalated for human review.
  Escalated     : SafetyCheck
  ||| Safety check timed out without a determination.
  Timeout       : SafetyCheck
  ||| Action permitted but confined to a sandbox.
  Sandboxed     : SafetyCheck
  ||| Action requires explicit human approval before proceeding.
  HumanRequired : SafetyCheck

export
Show SafetyCheck where
  show Approved      = "Approved"
  show Denied        = "Denied"
  show Escalated     = "Escalated"
  show Timeout       = "Timeout"
  show Sandboxed     = "Sandboxed"
  show HumanRequired = "HumanRequired"

------------------------------------------------------------------------
-- MemoryType
-- The kind of memory an agent uses to store information.
------------------------------------------------------------------------

||| Classification of agent memory systems by their cognitive role.
public export
data MemoryType : Type where
  ||| Short-term working memory for the current task.
  Working   : MemoryType
  ||| Episodic memory of specific past events and experiences.
  Episodic  : MemoryType
  ||| Semantic memory of general facts and knowledge.
  Semantic  : MemoryType
  ||| Procedural memory of how to perform tasks.
  Procedural : MemoryType
  ||| Memory shared across multiple agents.
  Shared    : MemoryType

export
Show MemoryType where
  show Working    = "Working"
  show Episodic   = "Episodic"
  show Semantic   = "Semantic"
  show Procedural = "Procedural"
  show Shared     = "Shared"
