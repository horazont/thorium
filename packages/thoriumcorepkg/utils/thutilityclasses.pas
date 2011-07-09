unit ThUtilityClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TThoriumIntList }

  TThoriumIntList = class (TObject)
    constructor Create;
    destructor Destroy; override;
  private

    function GetItem(Index: Integer): Integer;
    procedure Expand;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure SetItem(Index: Integer; Value: Integer);
  protected
    FList: PInteger;
    FCapacity, FCount: Integer;
  public
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;
    property Count: Integer read FCount write SetCount;
    property Capacity: Integer read FCapacity write SetCapacity;

    function AddEntry(Value: Integer): Integer;
    function FindValue(AValue: Integer): Integer;
    procedure DeleteEntry(AIndex: Integer);
    procedure Clear;
  end;

  (* Only internally used. *)
  TThoriumIntStack = class (TThoriumIntList)
  public
    procedure Push(Value: Integer);
    function Pop: Integer;
  end;

implementation

{ TThoriumIntList }

constructor TThoriumIntList.Create;
begin
  inherited Create;
  FList := nil;
  FCapacity := 0;
  FCount := 0;
end;

destructor TThoriumIntList.Destroy;
begin
  if FList <> nil then
    FreeMem(FList, SizeOf(Integer) * FCapacity);
  inherited Destroy;
end;

function TThoriumIntList.GetItem(Index: Integer): Integer;
// Returns the value of a list item at the offset Index.
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt('List index out of bounds (%d)', [Index]);
  Result := FList[Index];
end;

procedure TThoriumIntList.Expand;
// Expands the list capacity by a special value.
begin
  if FCapacity = 0 then
    SetCapacity(8)
  else if FCapacity <= 256 then
    SetCapacity(FCapacity shl 2)
  else
    SetCapacity(FCapacity + 128);
end;

procedure TThoriumIntList.SetCapacity(NewCapacity: Integer);
// Set the capacity to a given value. This does not allow to decrease the amount
// of items currently stored in the list.
begin
  if (NewCapacity < FCount) or (NewCapacity = FCapacity) then
    Exit;
  ReAllocMem(FList, SizeOf(Integer)*NewCapacity);
  FCapacity := NewCapacity;
end;

procedure TThoriumIntList.SetCount(NewCount: Integer);
// Set the count of the item set to a given value. This does not allow to
// enlarge the list.
begin
  if NewCount >= FCount then
    Exit;
  ReAllocMem(FList, SizeOf(Integer)*NewCount);
  FCount := NewCount;
  FCapacity := FCount;
end;

procedure TThoriumIntList.SetItem(Index: Integer; Value: Integer);
// Sets the value of the item at Index to Value.
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt('List index out of bounds (%d)', [Index]);
  FList[Index] := Value;
end;

function TThoriumIntList.AddEntry(Value: Integer): Integer;
// Adds a new entry to the list and returns its index.
begin
  if FCapacity < FCount + 1 then
    Expand;
  FList[FCount] := Value;
  Result := FCount;
  Inc(FCount);
end;

function TThoriumIntList.FindValue(AValue: Integer): Integer;
// Searches for a value and returns its index. If it is not found, -1 is
// returned.
var
  I: Integer;
  LocalList: PInteger;
begin
  LocalList := FList;
  for I := 0 to FCount - 1 do
  begin
    if LocalList^ = AValue then
    begin
      Result := I;
      Exit;
    end;
    Inc(LocalList);
  end;
  Result := -1;
end;

procedure TThoriumIntList.DeleteEntry(AIndex: Integer);
// Delete the entry at AIndex.
begin
  if (AIndex >= FCount) or (AIndex < 0) then
    raise EListError.CreateFmt('List index out of bounds (%d)', [AIndex]);
  if (AIndex = FCount - 1) then
  begin
    Dec(FCount);
    Exit;
  end;
  Move(PInteger(ptruint(FList)+SizeOf(Integer)*Cardinal(AIndex+1))^,
    PInteger(ptruint(FList)+SizeOf(Integer)*Cardinal(AIndex))^,
    (FCount - (AIndex + 1))*SizeOf(Integer)
    );
    //(AIndex - (FCount - 1)));
  Dec(FCount);
end;

procedure TThoriumIntList.Clear;
begin
  SetCount(0);
  SetCapacity(FCapacity div 2);
end;

{ TThoriumIntStack }

procedure TThoriumIntStack.Push(Value: Integer);
begin
  AddEntry(Value);
end;

function TThoriumIntStack.Pop: Integer;
begin
  if FCount > 0 then
  begin
    Result := FList[FCount - 1];
    DeleteEntry(FCount-1);
  end
  else
    Result := 0;
end;

end.

