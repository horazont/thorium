unit ThOptimizeMoveCopy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, ThGlobals;

type

  { TThoriumOptimizeMoveXCopyR }

  TThoriumOptimizeMoveXCopyR = class (TThoriumCustomOptimizerPattern)
  public
    function Handle(const Instructions: TThoriumInstructions;
       const CurrInstruction: PThoriumInstruction; const StartIndex,
       Remaining: Integer; var Offset: Integer): Boolean; override;
  end;

  { TThoriumOptimizeMoveRCopyX }

  TThoriumOptimizeMoveRCopyX = class (TThoriumCustomOptimizerPattern)
  public
    function Handle(const Instructions: TThoriumInstructions;
       const CurrInstruction: PThoriumInstruction; const StartIndex,
       Remaining: Integer; var Offset: Integer): Boolean; override;
  end;

implementation

{ TThoriumOptimizeMoveXCopyR }

function TThoriumOptimizeMoveXCopyR.Handle(
  const Instructions: TThoriumInstructions;
  const CurrInstruction: PThoriumInstruction; const StartIndex,
  Remaining: Integer; var Offset: Integer): Boolean;
var
  Move, CopyMove: PThoriumInstruction;
  RegID: TThoriumRegisterID;
begin
  if Remaining < 2 then
    Exit(False);
  Move := @CurrInstruction[0];
  CopyMove := @CurrInstruction[1];

  if not (Move^.Instruction in [tiMOVEFG, tiMOVEG, tiMOVEL, tiMOVEP, tiMOVER, tiMOVEST])
    or not (CopyMove^.Instruction in [tiCOPYR, tiMOVER]) then
    Exit(False);

  RegID := TThoriumInstructionCOPYR(CopyMove^).SRI;
  case Move^.Instruction of
    tiMOVEFG:
    begin
      if TThoriumInstructionMOVEFG(Move^).TRI <> RegID then
        Exit(False);
      if CopyMove^.Instruction = tiCOPYR then
        Move^.Instruction := tiCOPYFG;
      TThoriumInstructionMOVEFG(Move^).TRI := TThoriumInstructionCOPYR(CopyMove^).TRI;
    end;
    tiMOVEG:
    begin
      if TThoriumInstructionMOVEG(Move^).TRI <> RegID then
        Exit(False);
      if CopyMove^.Instruction = tiCOPYR then
        Move^.Instruction := tiCOPYG;
      TThoriumInstructionMOVEG(Move^).TRI := TThoriumInstructionCOPYR(CopyMove^).TRI;
    end;
    tiMOVEL:
    begin
      if TThoriumInstructionMOVEL(Move^).TRI <> RegID then
        Exit(False);
      if CopyMove^.Instruction = tiCOPYR then
        Move^.Instruction := tiCOPYL;
      TThoriumInstructionMOVEL(Move^).TRI := TThoriumInstructionCOPYR(CopyMove^).TRI;
    end;
    tiMOVEP:
    begin
      if TThoriumInstructionMOVEP(Move^).TRI <> RegID then
        Exit(False);
      if CopyMove^.Instruction = tiCOPYR then
        Move^.Instruction := tiCOPYP;
      TThoriumInstructionMOVEP(Move^).TRI := TThoriumInstructionCOPYR(CopyMove^).TRI;
    end;
    tiMOVER:
    begin
      if TThoriumInstructionMOVER(Move^).TRI <> RegID then
        Exit(False);
      if CopyMove^.Instruction = tiCOPYR then
        Move^.Instruction := tiCOPYR;
      TThoriumInstructionMOVER(Move^).TRI := TThoriumInstructionCOPYR(CopyMove^).TRI;
    end;
    tiMOVEST:
    begin
      if TThoriumInstructionMOVEST(Move^).TRI <> RegID then
        Exit(False);
      if CopyMove^.Instruction = tiCOPYR then
        Move^.Instruction := tiCOPYST;
      TThoriumInstructionMOVEST(Move^).TRI := TThoriumInstructionCOPYR(CopyMove^).TRI;
    end;
  else
    raise EThoriumException.Create('Invalid instruction in optimizer.');
  end;
  Instructions.DeleteInstructions(StartIndex+1, 1);
  Offset := -1;
  Exit(True);
end;

{ TThoriumOptimizeMoveRCopyX }

function TThoriumOptimizeMoveRCopyX.Handle(
  const Instructions: TThoriumInstructions;
  const CurrInstruction: PThoriumInstruction; const StartIndex,
  Remaining: Integer; var Offset: Integer): Boolean;
var
  Move, CopyMove: PThoriumInstruction;
  RegID: TThoriumRegisterID;
begin
  if Remaining < 2 then
    Exit(False);
  Move := @CurrInstruction[0];
  CopyMove := @CurrInstruction[1];

  if not (Move^.Instruction in [tiMOVER])
    or not (CopyMove^.Instruction in [tiCOPYR, tiCOPYR_P, tiCOPYR_L, tiCOPYR_G,
      tiCOPYR_FG, tiCOPYR_ST, tiMOVER, tiMOVER_P, tiMOVER_L, tiMOVER_G,
      tiMOVER_FG, tiMOVER_ST]) then
    Exit(False);

  if (TThoriumInstructionMOVER(Move^).SRI >= THORIUM_REGISTER_C1)
    and not (CopyMove^.Instruction in [tiMOVER, tiCOPYR]) then
    Exit(False);

  RegID := TThoriumInstructionMOVER(Move^).TRI;
  case CopyMove^.Instruction of
    tiCOPYR, tiMOVER:
    begin
      if TThoriumInstructionCOPYR(CopyMove^).SRI <> RegID then
        Exit(False);
      TThoriumInstructionCOPYR(CopyMove^).SRI := TThoriumInstructionMOVER(Move^).SRI;
    end;
    tiCOPYR_P, tiMOVER_P:
    begin
      if TThoriumInstructionCOPYR_P(CopyMove^).SRI <> RegID then
        Exit(False);
      TThoriumInstructionCOPYR_P(CopyMove^).SRI := TThoriumInstructionMOVER(Move^).SRI;
    end;
    tiCOPYR_L, tiMOVER_L:
    begin
      if TThoriumInstructionCOPYR_L(CopyMove^).SRI <> RegID then
        Exit(False);
      TThoriumInstructionCOPYR_L(CopyMove^).SRI := TThoriumInstructionMOVER(Move^).SRI;
    end;
    tiCOPYR_G, tiMOVER_G:
    begin
      if TThoriumInstructionCOPYR_G(CopyMove^).SRI <> RegID then
        Exit(False);
      TThoriumInstructionCOPYR_G(CopyMove^).SRI := TThoriumInstructionMOVER(Move^).SRI;
    end;
    tiCOPYR_FG, tiMOVER_FG:
    begin
      if TThoriumInstructionCOPYR_FG(CopyMove^).SRI <> RegID then
        Exit(False);
      TThoriumInstructionCOPYR_FG(CopyMove^).SRI := TThoriumInstructionMOVER(Move^).SRI;
    end;
    tiCOPYR_ST, tiMOVER_ST:
    begin
      if TThoriumInstructionCOPYR_ST(CopyMove^).SRI <> RegID then
        Exit(False);
      TThoriumInstructionCOPYR_ST(CopyMove^).SRI := TThoriumInstructionMOVER(Move^).SRI;
    end;
  else
    raise EThoriumException.Create('Invalid instruction in optimizer.');
  end;
  Instructions.DeleteInstructions(StartIndex, 1);
  Offset := -1;
  Exit(True);
end;

end.

