-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

-- | Core protocol types for Modbus TCP industrial protocol.
-- | Defines function codes, exception codes, and device roles
-- | as closed sum types with Show instances.

module Modbus.Types

%default total

||| Modbus function codes per Modbus Application Protocol Specification V1.1b3.
public export
data FunctionCode : Type where
  ReadCoils                    : FunctionCode
  ReadDiscreteInputs           : FunctionCode
  ReadHoldingRegisters         : FunctionCode
  ReadInputRegisters           : FunctionCode
  WriteSingleCoil              : FunctionCode
  WriteSingleRegister          : FunctionCode
  WriteMultipleCoils           : FunctionCode
  WriteMultipleRegisters       : FunctionCode
  ReadWriteMultipleRegisters   : FunctionCode
  MaskWriteRegister            : FunctionCode

public export
Show FunctionCode where
  show ReadCoils                  = "ReadCoils"
  show ReadDiscreteInputs         = "ReadDiscreteInputs"
  show ReadHoldingRegisters       = "ReadHoldingRegisters"
  show ReadInputRegisters         = "ReadInputRegisters"
  show WriteSingleCoil            = "WriteSingleCoil"
  show WriteSingleRegister        = "WriteSingleRegister"
  show WriteMultipleCoils         = "WriteMultipleCoils"
  show WriteMultipleRegisters     = "WriteMultipleRegisters"
  show ReadWriteMultipleRegisters = "ReadWriteMultipleRegisters"
  show MaskWriteRegister          = "MaskWriteRegister"

||| Modbus exception codes per Modbus Application Protocol Specification V1.1b3.
public export
data ExceptionCode : Type where
  IllegalFunction           : ExceptionCode
  IllegalDataAddress        : ExceptionCode
  IllegalDataValue          : ExceptionCode
  SlaveDeviceFailure        : ExceptionCode
  Acknowledge               : ExceptionCode
  SlaveDeviceBusy           : ExceptionCode
  MemoryParityError         : ExceptionCode
  GatewayPathUnavailable    : ExceptionCode
  GatewayTargetDeviceFailed : ExceptionCode

public export
Show ExceptionCode where
  show IllegalFunction           = "IllegalFunction"
  show IllegalDataAddress        = "IllegalDataAddress"
  show IllegalDataValue          = "IllegalDataValue"
  show SlaveDeviceFailure        = "SlaveDeviceFailure"
  show Acknowledge               = "Acknowledge"
  show SlaveDeviceBusy           = "SlaveDeviceBusy"
  show MemoryParityError         = "MemoryParityError"
  show GatewayPathUnavailable    = "GatewayPathUnavailable"
  show GatewayTargetDeviceFailed = "GatewayTargetDeviceFailed"

||| Modbus device roles.
public export
data DeviceRole : Type where
  Master : DeviceRole
  Slave  : DeviceRole

public export
Show DeviceRole where
  show Master = "Master"
  show Slave  = "Slave"
