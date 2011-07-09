unit ThTypeStruct;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Thorium, ThGlobals;

type

  { TThoriumTypeStruct }

  TThoriumTypeStruct = class (TThoriumType)
  public
    constructor Create(AThorium: TThorium);
    destructor Destroy; override;
  private
    FCount: Integer;
    FFields: array of TThoriumStructFieldDefinition;

    procedure Expand;
  protected
    function GetTypeKind: TThoriumTypeKind; override;
  public
    function Add(const AName: String;
      const ValueType: TThoriumType): Integer;
    function Add(const AReference: TThoriumStructFieldDefinition): Integer;
    function CanPerformOperation(var Operation: TThoriumOperationDescription;
       const TheObject: TThoriumType=nil; const ExName: String = '';
       const ExType: PTypeInfo = nil): Boolean; override;
    procedure Delete(const AIndex: Integer);
    function IndexOf(const AName: String): Integer;
    function IsEqualTo(const AnotherType: TThoriumType): Boolean; override;
    function NeedsClear: Boolean; override;
    function UsesType(const AnotherType: TThoriumType; MayRecurse: Boolean=True
       ): Boolean; override;
  end;

implementation

{ TThoriumTypeStruct }

constructor TThoriumTypeStruct.Create(AThorium: TThorium);
begin
  inherited Create(AThorium);
  FCount := 0;
end;

destructor TThoriumTypeStruct.Destroy;
begin
  inherited Destroy;
end;

procedure TThoriumTypeStruct.Expand;
begin
  SetLength(FFields, Length(FFields) + 8);
end;

function TThoriumTypeStruct.GetTypeKind: TThoriumTypeKind;
begin
  Result := tkStruct;
end;

function TThoriumTypeStruct.Add(const AName: String; const ValueType: TThoriumType): Integer;
begin
  if ValueType.UsesType(Self) then
    raise EThoriumException.Create('Cannot cross-refer types in structs.');
  if FCount = Length(FFields) then
    Expand;
  FFields[FCount].Name := AName;
  FFields[FCount].ValueType := ValueType;
  FFields[FCount].Offset := SizeOf(TThoriumValue) * FCount;
  Result := FCount;
  Inc(FCount);
end;

function TThoriumTypeStruct.Add(const AReference: TThoriumStructFieldDefinition
  ): Integer;
begin
  Result := Add(AReference.Name, AReference.ValueType);
end;

function TThoriumTypeStruct.CanPerformOperation(
  var Operation: TThoriumOperationDescription; const TheObject: TThoriumType;
  const ExName: String; const ExType: PTypeInfo): Boolean;
begin
  Result := inherited CanPerformOperation(Operation, TheObject);
end;

procedure TThoriumTypeStruct.Delete(const AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EListError.CreateFmt('List index (%d) out of bounds (0…%d)', [AIndex, 0, FCount - 1]);
  FFields[AIndex].Name := '';
  FFields[AIndex].ValueType := nil;
  for I := AIndex + 1 to FCount - 1 do
  begin
    FFields[I-1].Name := FFields[I].Name;
    FFields[I-1].ValueType := FFields[I].ValueType;
  end;
  FFields[FCount-1].Name := '';
  FFields[FCount-1].ValueType := nil;
  Dec(FCount);
end;

function TThoriumTypeStruct.IndexOf(const AName: String): Integer;
begin
  for Result := 0 to FCount - 1 do
    if FFields[Result].Name = AName then
      Exit;
  Result := -1;
end;

function TThoriumTypeStruct.IsEqualTo(const AnotherType: TThoriumType): Boolean;
var
  OtherInstance: TThoriumTypeStruct;
  I: Integer;
begin
  if not (AnotherType is TThoriumTypeStruct) then
    Exit(False);
  OtherInstance := TThoriumTypeStruct(AnotherType);
  if OtherInstance.FCount <> FCount then
    Exit(False);
  for I := 0 to FCount - 1 do
  begin
    if OtherInstance.FFields[I].Name <> Self.FFields[I].Name then
      Exit(False);
    if OtherInstance.FFields[I].ValueType <> Self.FFields[I].ValueType then
      Exit(False);
    if OtherInstance.FFields[I].Offset <> Self.FFields[I].Offset then
      Exit(False);
  end;
  Result := True;
end;

function TThoriumTypeStruct.NeedsClear: Boolean;
begin
  Result := True;
end;

function TThoriumTypeStruct.UsesType(const AnotherType: TThoriumType;
  MayRecurse: Boolean): Boolean;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if FFields[I].ValueType.IsEqualTo(AnotherType) then
      Exit(True)
    else if MayRecurse and FFields[I].ValueType.UsesType(AnotherType) then
      Exit(True);
  end;
  Result := False;
end;

end.

