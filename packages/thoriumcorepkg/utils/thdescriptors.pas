unit ThDescriptors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ThGlobals, Thorium;

function OperationInstructionDescription(const AInstruction: TThoriumInstruction;
  Op1: Integer = 0; Op2: Integer = 1; Target: Integer = 2): TThoriumOperationInstructionDescription;
function AccessDescription(const AInstruction: TThoriumInstruction;
  AValueRegister: Integer = -1; ATargetRegister: Integer = -1;
  AExtendedRegister: Integer = -1; AIndexRegister: Integer = -1): TThoriumAccess;
function CreationDescription(const AInstruction: TThoriumInstruction;
  ATargetRegister: Integer = -1): TThoriumCreateInstructionDescription;

implementation

function OperationInstructionDescription(const AInstruction: TThoriumInstruction;
  Op1: Integer = 0; Op2: Integer = 1; Target: Integer = 2): TThoriumOperationInstructionDescription;
begin
  Result.Instruction := TThoriumInstructionREG(AInstruction);
  Result.Value1RIOffset := Op1;
  Result.Value2RIOffset := Op2;
  Result.TargetRIOffset := Target;
end;

function AccessDescription(const AInstruction: TThoriumInstruction;
  AValueRegister: Integer = -1; ATargetRegister: Integer = -1;
  AExtendedRegister: Integer = -1; AIndexRegister: Integer = -1): TThoriumAccess;
begin
  Result.Allowed := True;
  Result.Instruction := TThoriumInstructionREG(AInstruction);
  Result.IndexRIOffset := AIndexRegister;
  Result.ValueRIOffset := AValueRegister;
  Result.ExtendedRIOffset := AExtendedRegister;
  Result.TargetRIOffset := ATargetRegister;
end;

function CreationDescription(const AInstruction: TThoriumInstruction;
  ATargetRegister: Integer = -1): TThoriumCreateInstructionDescription;
begin
  Result.Instruction := TThoriumInstructionREG(AInstruction);
  Result.TargetRegisterOffset := ATargetRegister;
end;

end.

