unit Thorium_OptimizeJumps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals;

const
  INVERSE_CONDITION_JUMP : array [tiJE..tiJLE] of TThoriumInstructionCode = (
    tiJNE, tiJE, tiJLE, tiJLT, tiJGE, tiJGT
  );

type

  { TThoriumOptimizeConditionalJumps }

  TThoriumOptimizeConditionalJumps = class (TThoriumCustomOptimizerPattern)
  public
    function Handle(const Instructions: TThoriumInstructions; const CurrInstruction: PThoriumInstruction;
       const StartIndex, Remaining: Integer; var Offset: Integer): Boolean; override;
  end;

implementation

{ TThoriumOptimizeConditionalJumps }

function TThoriumOptimizeConditionalJumps.Handle(
  const Instructions: TThoriumInstructions;
  const CurrInstruction: PThoriumInstruction; const StartIndex,
  Remaining: Integer; var Offset: Integer): Boolean;
var
  Jump1, Jump2, Target: PThoriumInstruction;
  TargetOffset: Integer;
begin
  if Remaining <= 2 then
    Exit(False);
  Jump1 := @CurrInstruction[0];
  Jump2 := @CurrInstruction[1];
  if (Jump1^.Instruction in [tiJE, tiJGE, tiJLE, tiJLT, tiJGT, tiJNE])
    and (Jump2^.Instruction = INVERSE_CONDITION_JUMP[Jump1^.Instruction]) then
  begin
    Target := @CurrInstruction[2];
    TargetOffset := 2;
    if Target^.Instruction = tiEmbeddedHint then
    begin
      if Remaining <= 3 then
        Exit(False);
      Inc(Target);
      TargetOffset += 1;
    end;
    if TThoriumInstructionJMP(Jump1^).NewAddress = StartIndex+TargetOffset then
    begin
      Instructions.DeleteInstructions(StartIndex, 1);
      Offset := -1;
      Exit(True);
    end;
    if TThoriumInstructionJMP(Jump2^).NewAddress = StartIndex+TargetOffset then
    begin
      Jump2^ := Jump1^;
      Instructions.DeleteInstructions(StartIndex, 1);
      Offset := -1;
      Exit(True);
    end;
  end;
end;

end.

