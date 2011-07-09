unit ThTypeHostRecord;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Thorium, ThTypeHost, ThUtils, ThGlobals;

type
  { TThoriumHostRecordType }

  generic TThoriumHostRecordType<RecordType> = class (TThoriumHostObjectType)
  public
    constructor Create(ALibrary: TThoriumLibrary); override;
    constructor Create(ALibrary: TThoriumLibrary;
      AFields: array of TThoriumHostRecordField);
    destructor Destroy; override;
  private
    FFields: TThoriumHostRecordFields;
  private
    function IndexOfFieldDefinition(const AFieldName: String): Integer;
  protected
    procedure CalcHash; override;
  public
    function CanAssignTo(var Assignment: TThoriumAssignmentDescription;
       const AnotherType: TThoriumType=nil): Boolean; override;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil): Boolean; override;
    procedure DisposeValue(var AValue: Pointer); override;
    function DuplicateInstance(const AValue: Pointer): Pointer; override;
    function GetFieldID(const FieldIdent: String; out ID: QWord): Boolean;
       override;
    function GetFieldStoring(const AFieldID: QWord): Boolean; override;
    procedure GetFieldType(const AFieldID: QWord; out
      TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition);
      override;
    function GetNewInstance: Pointer; override;
    function OpGetField(const AInstance: Pointer; const AFieldID: QWord
       ): TThoriumValue; override;
    procedure OpSetField(const AInstance: Pointer; const AFieldID: QWord;
       const NewValue: TThoriumValue); override;
  public
  end;
  TThoriumHostRecordClass = class of TThoriumHostRecordType;

implementation


{ TThoriumHostRecordType }

constructor TThoriumHostRecordType.Create(ALibrary: TThoriumLibrary);
begin
  raise EThoriumException.Create('TThoriumHostRecordType must not be created without field information.');
end;

constructor TThoriumHostRecordType.Create(ALibrary: TThoriumLibrary;
  AFields: array of TThoriumHostRecordField);
var
  I: Integer;
begin
  inherited Create(ALibrary);
  SetLength(FFields, High(AFields) + 1);
  for I := 0 to High(AFields) do
  begin
    FFields[I] := AFields[I];
    FFields[I].FieldName := ThoriumCase(FFields[I].FieldName);
  end;
end;

destructor TThoriumHostRecordType.Destroy;
begin
  inherited Destroy;
end;

function TThoriumHostRecordType.IndexOfFieldDefinition(const AFieldName: String
  ): Integer;
begin
  for Result := 0 to Length(FFields) - 1 do
  begin
    if FFields[Result].FieldName = AFieldName then
      Exit;
  end;
  Result := -1;
end;

procedure TThoriumHostRecordType.CalcHash;
var
  I: Integer;
  Buffer: PByte;
  BufferSize: Ptruint;
  FieldHash: TThoriumHash;
begin
  (*BufferSize := EntrySize * Length(FFields);
  Buffer := GetMem(BufferSize);
  try
    for I := 0 to Length(FFields) - 1 do
    begin
      if FFields[I].FieldType.HostType and (htTypeSection or htSizeSection) = htExt then
        FieldHash := FFields[I].FieldType.Extended.GetHash
      else
        FillByte(FieldHash, SizeOf(TThoriumHash), 0);
      Buffer[I*EntrySize] := FFields[I].FieldType.HostType;
      Move(FieldHash[0], Buffer[I*EntrySize + 1], SizeOf(TThoriumHash));
    end;
    FHash := TThoriumHash(MD5Buffer(Buffer, BufferSize));
  finally
    FreeMem(Buffer);
  end;*)
  raise Exception.Create('Needs to be reimplemented');
end;

function TThoriumHostRecordType.CanAssignTo(
  var Assignment: TThoriumAssignmentDescription; const AnotherType: TThoriumType
  ): Boolean;
begin
  Result := IsEqualTo(AnotherType);
  if not Result then
  begin
    Result := inherited;
    Exit;
  end;
  Assignment.Cast.Needed := False;
end;

function TThoriumHostRecordType.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType
  ): Boolean;
begin
  Result := False;
end;

procedure TThoriumHostRecordType.DisposeValue(var AValue: Pointer);
type
  PRecordType = ^RecordType;
begin
  Dispose(PRecordType(AValue));
  AValue := nil;
end;

function TThoriumHostRecordType.DuplicateInstance(const AValue: Pointer
  ): Pointer;
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

function TThoriumHostRecordType.GetFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
var
  FieldDefinitionIdx: Integer;
  FieldDefinition: PThoriumHostRecordField;
  Mask: Cardinal;
begin
(*  FieldDefinitionIdx := IndexOfFieldDefinition(FieldIdent);
  Result := FieldDefinitionIdx >= 0;
  if not Result then
    Exit;
  FieldDefinition := @FFields[FieldDefinitionIdx];
  case FieldDefinition^.FieldType.HostType of
    htByte, htShortInt: Mask := $FF;
    htWord, htSmallInt: Mask := $FFFF;
    htDWord, htLongInt: Mask := $FFFFFFFF;
  else
    Mask := 0;
  end;
  ID := Mask or (QWord(FieldDefinitionIdx) shl 32);*)
  raise EThoriumException.Create('Not reimplemented yet.');
end;

function TThoriumHostRecordType.GetFieldStoring(const AFieldID: QWord
  ): Boolean;
var
  FieldDefinition: PThoriumHostRecordField;
begin
  FieldDefinition := @FFields[Integer((AFieldID shr 32) and $FFFFFFFF)];
  Result := FieldDefinition^.FieldType.Storing;
end;

procedure TThoriumHostRecordType.GetFieldType(const AFieldID: QWord; out
  TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition);
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

function TThoriumHostRecordType.GetNewInstance: Pointer;
type
  PRecordType = ^RecordType;
begin
  New(PRecordType(Result));
end;

function TThoriumHostRecordType.OpGetField(const AInstance: Pointer;
  const AFieldID: QWord): TThoriumValue;
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

procedure TThoriumHostRecordType.OpSetField(const AInstance: Pointer;
  const AFieldID: QWord; const NewValue: TThoriumValue);
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

end.

