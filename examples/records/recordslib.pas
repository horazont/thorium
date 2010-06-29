(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: recordlib.pas
** Last update: 2010-03-12
This file is part of the Thorium Scripting Language Project.

As this file is part of an example, you may do with it what you want, except
sue me for anything I've coded here. When you are going to redistribute this
file in its original functionallity (that is, the same effective code, removing
comments or this notice does not count), you must name the source where you
got it.

For feedback and questions about Thorium Scripting Language please mail me,
Jonas Wielicki:
j.wielicki@sotecware.net
*******************************************************************************)
unit recordslib;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals, Thorium_Utils;

type
  TSimpleRecord = record
    A: Byte;
    B: Word;
    C: DWord;
    D: QWord;
  end;
  PSimpleRecord = ^TSimpleRecord;

  TStringRecord = record
    A: Byte;
    S: String;
  end;

  TSimpleRecordRegistration = specialize TThoriumHostRecordType<TSimpleRecord>;
  TStringRecordRegistration = specialize TThoriumHostRecordType<TStringRecord>;

  { TThoriumLibExampleRecords }

  TThoriumLibExampleRecords = class (TThoriumLibrary)
  protected
    class function GetName: String; override;
    procedure InitializeLibrary; override;
  end;



implementation

procedure PrintSimpleRecord(ARec: PSimpleRecord);
begin
  WriteLn('host print');
  WriteLn(ARec^.A);
  WriteLn(ARec^.B);
  WriteLn(ARec^.C);
  WriteLn(ARec^.D);
end;

{ TThoriumLibExampleRecords }

class function TThoriumLibExampleRecords.GetName: String;
begin
  Result := 'example.records';
end;

procedure TThoriumLibExampleRecords.InitializeLibrary;
var
  SimpleRecordType, StringRecordType: TThoriumHostObjectType;
  TmpSimpleRecord: TSimpleRecord;
  TmpStringRecord: TStringRecord;
begin
  SimpleRecordType := RegisterFinishedObjectType(
    'TSimpleRecord',
    TSimpleRecordRegistration.Create(Self,
      [
        HostRecordField(HostVarType(htByte), 'a', Offset(TmpSimpleRecord, TmpSimpleRecord.A)),
        HostRecordField(HostVarType(htWord), 'b', Offset(TmpSimpleRecord, TmpSimpleRecord.B)),
        HostRecordField(HostVarType(htDWord), 'c', Offset(TmpSimpleRecord, TmpSimpleRecord.C)),
        HostRecordField(HostVarType(htQWord), 'd', Offset(TmpSimpleRecord, TmpSimpleRecord.D))
      ]),
    TypeInfo(TSimpleRecord)
  );
  StringRecordType := RegisterFinishedObjectType(
    'TStringRecord',
    TStringRecordRegistration.Create(Self,
      [
        HostRecordField(HostVarType(htByte), 'a', Offset(TmpStringRecord, TmpStringRecord.A)),
        HostRecordField(HostVarType(htString), 's', Offset(TmpStringRecord, TmpStringRecord.S))
      ]),
    TypeInfo(TStringRecord)
  );
  RegisterNativeCallFunction('PrintSimpleRecord', @PrintSimpleRecord, [
    htExt
  ], htNone, ncRegister).Parameters.ExtendedTypes[0] := SimpleRecordType;
end;

end.

