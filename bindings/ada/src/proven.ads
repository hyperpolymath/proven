--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: 2025 Hyperpolymath

--  Proven: Formally verified safety primitives for Ada.
--
--  This library provides safe operations with bounds checking,
--  overflow detection, and security-focused string handling.
--
--  Version: 0.4.0
--  Module Count: 38
--
--  Categories:
--    Core (11): Safe_Math, Safe_String, Safe_Path, Safe_Email, Safe_Url,
--               Safe_Network, Safe_Crypto, Safe_UUID, Safe_Currency,
--               Safe_Phone, Safe_Hex
--    Data (7): Safe_Json, Safe_Datetime, Safe_Float, Safe_Version,
--              Safe_Color, Safe_Angle, Safe_Unit
--    Data Structures (5): Safe_Buffer, Safe_Queue, Safe_Bloom, Safe_LRU,
--                         Safe_Graph
--    Resilience (4): Safe_Rate_Limiter, Safe_Circuit_Breaker, Safe_Retry,
--                    Safe_Monotonic
--    State (2): Safe_State_Machine, Safe_Calculator
--    Algorithm (4): Safe_Geo, Safe_Probability, Safe_Checksum, Safe_Tensor
--    Security (2): Safe_Password, Safe_ML
--    HTTP (3): Safe_Header, Safe_Cookie, Safe_Content_Type

package Proven is
   pragma Pure;

   Version      : constant String := "0.4.0";
   Module_Count : constant := 38;

end Proven;
