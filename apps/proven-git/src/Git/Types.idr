-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

||| Core protocol types for the proven-git Git protocol server.
||| Defines closed sum types for Git transport commands, packet types,
||| ref types, protocol capabilities, and hook results.
module Git.Types

%default total

---------------------------------------------------------------------------
-- Command: Git smart transport commands.
---------------------------------------------------------------------------

||| The three smart transport services that a Git server can provide.
||| These correspond to the server-side commands invoked during fetch,
||| push, and archive operations.
public export
data Command
  = UploadPack    -- ^ Serve objects to a client performing a fetch/clone
  | ReceivePack   -- ^ Accept objects from a client performing a push
  | UploadArchive -- ^ Serve a tar/zip archive of a tree to the client

||| Display a human-readable label for each Git command.
public export
Show Command where
  show UploadPack    = "UploadPack"
  show ReceivePack   = "ReceivePack"
  show UploadArchive = "UploadArchive"

---------------------------------------------------------------------------
-- PacketType: Git protocol packet-line types.
---------------------------------------------------------------------------

||| Classifies the types of packets exchanged in the Git pack protocol
||| (both v1 and v2), including special control packets and sideband data.
public export
data PacketType
  = Flush          -- ^ 0000 flush packet (end of message boundary)
  | Delimiter      -- ^ 0001 delimiter packet (section separator, protocol v2)
  | ResponseEnd    -- ^ 0002 response-end packet (protocol v2)
  | Data           -- ^ Regular data packet with a length-prefixed payload
  | PktError       -- ^ ERR packet indicating a server-side error
  | SidebandData   -- ^ Sideband channel 1: pack data
  | SidebandProgress -- ^ Sideband channel 2: progress information
  | SidebandError  -- ^ Sideband channel 3: error messages

||| Display a human-readable label for each packet type.
public export
Show PacketType where
  show Flush           = "Flush"
  show Delimiter       = "Delimiter"
  show ResponseEnd     = "ResponseEnd"
  show Data            = "Data"
  show PktError        = "Error"
  show SidebandData    = "SidebandData"
  show SidebandProgress = "SidebandProgress"
  show SidebandError   = "SidebandError"

---------------------------------------------------------------------------
-- RefType: Categories of Git references.
---------------------------------------------------------------------------

||| Classifies the namespace of a Git reference (ref). Used when
||| advertising refs during the reference discovery phase.
public export
data RefType
  = Branch -- ^ refs/heads/* — a branch tip
  | Tag    -- ^ refs/tags/* — an annotated or lightweight tag
  | Head   -- ^ HEAD — the symbolic default branch reference
  | Remote -- ^ refs/remotes/* — a remote-tracking branch
  | Note   -- ^ refs/notes/* — a git-notes reference

||| Display a human-readable label for each ref type.
public export
Show RefType where
  show Branch = "Branch"
  show Tag    = "Tag"
  show Head   = "Head"
  show Remote = "Remote"
  show Note   = "Note"

---------------------------------------------------------------------------
-- Capability: Git protocol capabilities advertised during handshake.
---------------------------------------------------------------------------

||| Protocol capabilities that the server can advertise during the
||| initial reference discovery handshake. Clients select a subset
||| to negotiate the transfer parameters.
public export
data Capability
  = MultiAck     -- ^ Support multi_ack for efficient negotiation
  | ThinPack     -- ^ Allow thin packs (deltified against objects not in pack)
  | SideBand64k  -- ^ Support 64 KiB sideband for multiplexed output
  | OFSDelta     -- ^ Use OFS_DELTA for more compact pack encoding
  | Shallow      -- ^ Support shallow clones
  | DeepenSince  -- ^ Support deepen-since for time-based shallow boundary
  | DeepenNot    -- ^ Support deepen-not for ref-based shallow boundary
  | FilterSpec   -- ^ Support partial clone filter specifications
  | ObjectFormat -- ^ Advertise the object hash algorithm (SHA-1 / SHA-256)

||| Display a human-readable label for each capability.
public export
Show Capability where
  show MultiAck     = "MultiAck"
  show ThinPack     = "ThinPack"
  show SideBand64k  = "SideBand64k"
  show OFSDelta     = "OFSDelta"
  show Shallow      = "Shallow"
  show DeepenSince  = "DeepenSince"
  show DeepenNot    = "DeepenNot"
  show FilterSpec   = "FilterSpec"
  show ObjectFormat = "ObjectFormat"

---------------------------------------------------------------------------
-- HookResult: Outcome of a server-side Git hook invocation.
---------------------------------------------------------------------------

||| The result of running a server-side hook (pre-receive, update,
||| post-receive). Determines whether the push is accepted or rejected.
public export
data HookResult
  = Accept -- ^ Hook exited successfully; operation proceeds
  | Reject -- ^ Hook exited with an error; operation is refused

||| Display a human-readable label for each hook result.
public export
Show HookResult where
  show Accept = "Accept"
  show Reject = "Reject"
