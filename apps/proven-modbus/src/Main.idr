-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Entry point for the proven-modbus skeleton.
-- | Prints the server name and demonstrates type constructors.

module Main

import Modbus

%default total

||| All Modbus function code constructors for demonstration.
allFunctionCodes : List FunctionCode
allFunctionCodes =
  [ ReadCoils, ReadDiscreteInputs, ReadHoldingRegisters, ReadInputRegisters
  , WriteSingleCoil, WriteSingleRegister, WriteMultipleCoils
  , WriteMultipleRegisters, ReadWriteMultipleRegisters, MaskWriteRegister ]

||| All Modbus exception code constructors for demonstration.
allExceptionCodes : List ExceptionCode
allExceptionCodes =
  [ IllegalFunction, IllegalDataAddress, IllegalDataValue
  , SlaveDeviceFailure, Acknowledge, SlaveDeviceBusy
  , MemoryParityError, GatewayPathUnavailable, GatewayTargetDeviceFailed ]

||| All Modbus device role constructors for demonstration.
allDeviceRoles : List DeviceRole
allDeviceRoles = [Master, Slave]

main : IO ()
main = do
  putStrLn "proven-modbus: Modbus TCP Industrial Protocol"
  putStrLn $ "  Port:            " ++ show modbusPort
  putStrLn $ "  Max registers:   " ++ show maxRegisters
  putStrLn $ "  Max coils:       " ++ show maxCoils
  putStrLn $ "  Unit ID range:   " ++ show unitIDMin ++ "-" ++ show unitIDMax
  putStrLn $ "  Function codes:  " ++ show allFunctionCodes
  putStrLn $ "  Exception codes: " ++ show allExceptionCodes
  putStrLn $ "  Device roles:    " ++ show allDeviceRoles
