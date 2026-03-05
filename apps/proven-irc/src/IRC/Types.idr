-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core IRC protocol types as closed sum types.
-- | Models commands (RFC 2812 Section 3), numeric replies (Section 5),
-- | and channel modes (Section 4).
module IRC.Types

%default total

-------------------------------------------------------------------------------
-- IRC Commands
-------------------------------------------------------------------------------

||| IRC protocol commands as defined in RFC 2812.
||| Each constructor corresponds to a distinct IRC command verb.
public export
data Command : Type where
  Nick    : Command
  User    : Command
  Join    : Command
  Part    : Command
  Privmsg : Command
  Notice  : Command
  Quit    : Command
  Ping    : Command
  Pong    : Command
  Mode    : Command
  Kick    : Command
  Topic   : Command
  Invite  : Command
  Names   : Command
  List    : Command
  Who     : Command
  Whois   : Command

||| Show instance for Command, rendering each as its IRC verb string.
export
Show Command where
  show Nick    = "NICK"
  show User    = "USER"
  show Join    = "JOIN"
  show Part    = "PART"
  show Privmsg = "PRIVMSG"
  show Notice  = "NOTICE"
  show Quit    = "QUIT"
  show Ping    = "PING"
  show Pong    = "PONG"
  show Mode    = "MODE"
  show Kick    = "KICK"
  show Topic   = "TOPIC"
  show Invite  = "INVITE"
  show Names   = "NAMES"
  show List    = "LIST"
  show Who     = "WHO"
  show Whois   = "WHOIS"

-------------------------------------------------------------------------------
-- Numeric Replies
-------------------------------------------------------------------------------

||| Selected numeric reply codes from RFC 2812 Section 5.
||| These represent server-to-client response numerics.
public export
data NumericReply : Type where
  Welcome         : NumericReply
  YourHost        : NumericReply
  Created         : NumericReply
  MyInfo          : NumericReply
  Bounce          : NumericReply
  NickInUse       : NumericReply
  NoSuchNick      : NumericReply
  NoSuchChannel   : NumericReply
  ChannelIsFull   : NumericReply
  InviteOnlyChan  : NumericReply
  BannedFromChan  : NumericReply

||| Show instance for NumericReply, rendering the 3-digit numeric code.
export
Show NumericReply where
  show Welcome        = "001 RPL_WELCOME"
  show YourHost       = "002 RPL_YOURHOST"
  show Created        = "003 RPL_CREATED"
  show MyInfo         = "004 RPL_MYINFO"
  show Bounce         = "005 RPL_BOUNCE"
  show NickInUse      = "433 ERR_NICKNAMEINUSE"
  show NoSuchNick     = "401 ERR_NOSUCHNICK"
  show NoSuchChannel  = "403 ERR_NOSUCHCHANNEL"
  show ChannelIsFull  = "471 ERR_CHANNELISFULL"
  show InviteOnlyChan = "473 ERR_INVITEONLYCHAN"
  show BannedFromChan = "474 ERR_BANNEDFROMCHAN"

-------------------------------------------------------------------------------
-- Channel Modes
-------------------------------------------------------------------------------

||| IRC channel modes as defined in RFC 2812 Section 4.
||| Each constructor corresponds to a single mode flag.
public export
data ChannelMode : Type where
  Op             : ChannelMode
  Voice          : ChannelMode
  Ban            : ChannelMode
  Limit          : ChannelMode
  InviteOnly     : ChannelMode
  Moderated      : ChannelMode
  NoExternalMsgs : ChannelMode
  TopicLock      : ChannelMode
  Secret         : ChannelMode
  Private        : ChannelMode

||| Show instance for ChannelMode, rendering the single-character mode flag.
export
Show ChannelMode where
  show Op             = "+o (Op)"
  show Voice          = "+v (Voice)"
  show Ban            = "+b (Ban)"
  show Limit          = "+l (Limit)"
  show InviteOnly     = "+i (InviteOnly)"
  show Moderated      = "+m (Moderated)"
  show NoExternalMsgs = "+n (NoExternalMsgs)"
  show TopicLock      = "+t (TopicLock)"
  show Secret         = "+s (Secret)"
  show Private        = "+p (Private)"
