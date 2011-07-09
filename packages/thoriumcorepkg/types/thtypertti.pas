unit ThTypeRTTI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Thorium, ThTypeHost, ThGlobals, md5;

type

  { TThoriumRTTIObjectType }

  (* This is a derivate of TThoriumHostObjectType which uses the RTTI
     information of a class implementing IThoriumPersistent and some callbacks
     to publish a class to Thorium. Some optimizations have been made to
     reduce the overhead of RTTI as much as possible. *)
  TThoriumRTTIObjectType = class (TThoriumHostObjectType)
    constructor Create(ALibrary: TThoriumLibrary); override;
    constructor Create(ALibrary: TThoriumLibrary;
      ABaseClass: TThoriumPersistentClass; AbstractClass: Boolean = False);
    constructor Create(ALibrary: TThoriumLibrary; ABaseClass: TClass;
      MethodCallback: TThoriumRTTIMethodsCallback;
      StaticMethodCallback: TThoriumRTTIStaticMethodsCallback;
      AbstractClass: Boolean = False);
    destructor Destroy; override;
  private
    FBaseClass: TClass;
    FCanUsePersistent: Boolean;
    FPropCount: SmallInt;
    FPropList: PPropList;
    FStaticMethods: array of TThoriumHostFunctionBase;
    FMethods: array of TThoriumHostMethodBase;
    FStoringProperties: TStringList;
  protected
    procedure CalcHash; override;
  public
    procedure ApplyStoring(var AValue: Pointer; MayDecreaseReference: Boolean=
       True); override;
    procedure DisposeValue(var AValue: Pointer); override;
    function DuplicateInstance(const AValue: Pointer): Pointer; override;
    function FindMethod(const AMethodName: String): TThoriumHostMethodBase;
       override;
    function GetFieldID(const FieldIdent: String; out ID: QWord): Boolean;
       override;
    function GetFieldStoring(const AFieldID: QWord): Boolean; override;
    procedure GetFieldType(const AFieldID: QWord; out TypeSpec: TThoriumType;
       out Access: TThoriumAccessDefinition); override;
    function GetNewInstance: Pointer; override;
    function GetPropertyStoring(const PropInfo: PPropInfo): Boolean;
    function GetPropertyStoring(const PropertyName: String): Boolean;
    function GetStaticFieldID(const FieldIdent: String; out ID: QWord
       ): Boolean; override;
    procedure GetStaticFieldType(const AFieldID: QWord; out
       TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition); override;
    procedure DoEnableHostControl(const AValue: TThoriumValue); override;
    function DoEvaluate(const AValue: TThoriumValue): Boolean; override;
    function DoGetField(const AValue: TThoriumValue; const AFieldID: QWord
       ): TThoriumValue; override;
    procedure DoSetField(const AValue: TThoriumValue;
              const AFieldID: QWord; const NewValue: TThoriumValue); override;
    procedure SetPropertyStoring(const PropInfo: PPropInfo; const Storing: Boolean);
    procedure SetPropertyStoring(const PropertyName: String; const Storing: Boolean);
  public
    property BaseClass: TClass read FBaseClass;

    function NewNativeCallMethod(const AName: String;
      const ACodePointer: Pointer;
      const AParameters: array of PTypeInfo;
      const AReturnType: PTypeInfo = nil;
      const ACallingConvention: TThoriumNativeCallingConvention = ncRegister): TThoriumHostMethodNativeCall;
    function NewNativeCallStaticMethod(const AName: String;
      const ACodePointer: Pointer; const ADataPointer: Pointer;
      const AParameters: array of PTypeInfo;
      const AReturnType: PTypeInfo = nil;
      const ACallingConvention: TThoriumNativeCallingConvention = ncRegister): TThoriumHostMethodAsFunctionNativeCall;
    function NewNativeCallStaticFunction(const AName: String;
      const ACodePointer: Pointer;
      const AParameters: array of PTypeInfo;
      const AReturnType: PTypeInfo = nil;
      const ACallingConvention: TThoriumNativeCallingConvention = ncRegister): TThoriumHostFunctionNativeCall;
  end;

implementation

uses
  ThDescriptors;

{$I ThInstructionConstructors.inc}

{ TThoriumRTTIObjectType }

constructor TThoriumRTTIObjectType.Create(ALibrary: TThoriumLibrary);
begin
  raise EThoriumException.Create('TThoriumRTTIObjectType must not be created without BaseClass.');
end;

constructor TThoriumRTTIObjectType.Create(ALibrary: TThoriumLibrary;
  ABaseClass: TThoriumPersistentClass; AbstractClass: Boolean = False);
begin
  if ABaseClass = nil then
    raise EThoriumException.Create('TThoriumRTTIObjectType must not be created without BaseClass.');
  Create(ALibrary, ABaseClass, @ABaseClass.GetMethodList, @ABaseClass.GetStaticMethodList, AbstractClass);
  FCanUsePersistent := True;
end;

constructor TThoriumRTTIObjectType.Create(ALibrary: TThoriumLibrary;
  ABaseClass: TClass; MethodCallback: TThoriumRTTIMethodsCallback;
  StaticMethodCallback: TThoriumRTTIStaticMethodsCallback;
  AbstractClass: Boolean = False);
var
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  if not AbstractClass and not Supports(ABaseClass, IThoriumPersistent) then
    raise EThoriumException.CreateFmt('%s does not support IThoriumPersistent.', [ABaseClass.ClassName]);
  inherited Create(ALibrary);
  FCanUsePersistent := False;
  FBaseClass := ABaseClass;
  TypeInfo := FBaseClass.ClassInfo;
  if TypeInfo <> nil then
  begin
    FPropCount := GetTypeData(TypeInfo)^.PropCount;
    GetMem(FPropList, FPropCount * SizeOf(Pointer));
    GetPropInfos(TypeInfo, FPropList);
  end
  else
  begin
    FPropCount := 0;
    FPropList := nil;
  end;
  if MethodCallback <> nil then
  begin
    MethodCallback(Self, FMethods);
    for I := 0 to High(FMethods) do
    begin
      with FMethods[I] do
      begin
        SetType(Self);
      end;
      if FMethods[I] is TThoriumHostMethodNativeCall then
        TThoriumHostMethodNativeCall(FMethods[I]).Precompile;
    end;
  end;
  if StaticMethodCallback <> nil then
  begin
    StaticMethodCallback(Self, FStaticMethods);
    for I := 0 to High(FStaticMethods) do
    begin
      if FStaticMethods[I] is TThoriumHostFunctionNativeCall then
        TThoriumHostFunctionNativeCall(FStaticMethods[I]).Precompile;
    end;
  end;
  FStoringProperties := TStringList.Create;
end;

destructor TThoriumRTTIObjectType.Destroy;
var
  I: Integer;
begin
  Freemem(FPropList);
  for I := 0 to High(FMethods) do
    FMethods[I].Free;
  for I := 0 to High(FStaticMethods) do
    FStaticMethods[I].Free;
  FStoringProperties.Free;
  inherited Destroy;
end;

procedure TThoriumRTTIObjectType.ApplyStoring(var AValue: Pointer;
  MayDecreaseReference: Boolean);
var
  Intf: IThoriumPersistent;
begin
  if FCanUsePersistent then
  begin
    TThoriumPersistent(AValue).ThoriumReferenceImplementation.EnableHostControl;
    if MayDecreaseReference then
      TThoriumPersistent(AValue).ThoriumReferenceImplementation.FreeReference;
  end
  else
  begin
    TObject(AValue).GetInterface(IThoriumPersistent, Intf);
    Intf.EnableHostControl;
    if MayDecreaseReference then
      Intf.FreeReference;
    Intf := nil;
  end;
end;

procedure TThoriumRTTIObjectType.DisposeValue(var AValue: Pointer);
var
  Intf: IThoriumPersistent;
begin
  if FCanUsePersistent then
    TThoriumPersistent(AValue).ThoriumReferenceImplementation.FreeReference
  else
  begin
    TObject(AValue).GetInterface(IThoriumPersistent, Intf);
    Intf.FreeReference;
    Intf := nil;
  end;
  AValue := nil;
end;

function TThoriumRTTIObjectType.DuplicateInstance(const AValue: Pointer
  ): Pointer;
var
  Intf: IThoriumPersistent;
begin
  // Then assign the pointer from the source value. We can use the GetReference
  // method to automatically increase the stuff...

  if FCanUsePersistent then
    TThoriumPersistent(Result) := TThoriumPersistent(TThoriumPersistent(AValue).ThoriumReferenceImplementation.GetReference)
  else
  begin
    TObject(AValue).GetInterface(IThoriumPersistent, Intf);
    TObject(Result) := Intf.GetReference;
    Intf := nil;
  end;
  //TThoriumPersistent(Result.Extended.Value) := TThoriumPersistent(AValue.Value).GetReference;
end;

function TThoriumRTTIObjectType.FindMethod(const AMethodName: String): TThoriumHostMethodBase;
var
  I: Integer;
begin
  for I := 0 to High(FMethods) do
  begin
    Result := FMethods[I];
    if Result.Name = AMethodName then
      Exit;
  end;
  Result := nil;
end;

function TThoriumRTTIObjectType.GetFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
var
  FieldName: ShortString;
  I: Integer;
begin
  if Length(FieldIdent) > 255 then
  begin
    Result := False;
    Exit;
  end;
  FieldName := FieldIdent;
  for I := 0 to FPropCount - 1 do
  begin
    if (ShortCompareText(FieldName, UpCase(FPropList^[I]^.Name)) = 0) then
    begin
      ID := QWord(I);
      Result := True;
      Exit;
    end;
  end;
  for I := 0 to High(FMethods) do
  begin
    if FieldIdent = FMethods[I].Name then
    begin
      ID := QWord(I) or THORIUM_RTTI_METHOD_BIT;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TThoriumRTTIObjectType.GetFieldStoring(const AFieldID: QWord
  ): Boolean;
var
  PropInfo: PPropInfo;
begin
  if (AFieldID and THORIUM_RTTI_METHOD_BIT <> 0) then
  begin
    Result := False;
    Exit;
  end;
  PropInfo := FPropList^[AFieldID];
  Result := GetPropertyStoring(PropInfo);
end;

procedure TThoriumRTTIObjectType.GetFieldType(const AFieldID: QWord; out
  TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition);
var
  Info: PPropInfo;
begin
  if (AFieldID and THORIUM_RTTI_METHOD_BIT = THORIUM_RTTI_METHOD_BIT) then
  begin
    TypeSpec := FMethods[AFieldID xor THORIUM_RTTI_METHOD_BIT].Prototype;
    Access.ReadAccess.Allowed := True;
    Access.ReadAccess := AccessDescription(xmeth(FMethods[AFieldID xor THORIUM_RTTI_METHOD_BIT], 0, 0), -1, 4, 5);
    Access.WriteAccess.Allowed := False;
  end
  else
  begin
    Info := FPropList^[AFieldID];//PPropInfo(ptruint(AFieldID));
    Access.ReadAccess := AccessDescription(xfget(AFieldID, 0, 0), -1, 5, 4);
    if Info^.SetProc = nil then
    begin
      Access.WriteAccess.Allowed := False;
    end
    else
    begin
      Access.WriteAccess := AccessDescription(xfset(AFieldID, 0, 0), 5, -1, 4);
    end;
    TypeSpec := FLibrary.DeepFindTypeForHostType(Info^.PropType);
  end;
end;

function TThoriumRTTIObjectType.GetNewInstance: Pointer;
begin
  Result := nil;
end;

function TThoriumRTTIObjectType.GetPropertyStoring(const PropInfo: PPropInfo
  ): Boolean;
begin
  Result := FStoringProperties.IndexOfObject(TObject(PropInfo)) >= 0;
end;

function TThoriumRTTIObjectType.GetPropertyStoring(const PropertyName: String
  ): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(FBaseClass, PropertyName);
  if PropInfo = nil then
    raise EPropertyError.CreateFmt('Property ''%s'' not found.', [PropertyName]);
  Result := GetPropertyStoring(PropInfo);
end;

function TThoriumRTTIObjectType.GetStaticFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FStaticMethods) do
  begin
    if FStaticMethods[I].Name = FieldIdent then
    begin
      ID := QWord(I) or THORIUM_RTTI_METHOD_BIT;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TThoriumRTTIObjectType.GetStaticFieldType(const AFieldID: QWord; out
  TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition);
begin
  if AFieldID and THORIUM_RTTI_METHOD_BIT = 0 then
  begin
    raise EThoriumException.Create('Cannot do that.')
  end
  else
  begin
    Access.ReadAccess.Allowed := False;
    Access.WriteAccess.Allowed := False;
  end;
end;

procedure TThoriumRTTIObjectType.DoEnableHostControl(const AValue: TThoriumValue
  );
var
  Intf: IThoriumPersistent;
begin
  if FCanUsePersistent then
    TThoriumPersistent(AValue.HostObject).ThoriumReferenceImplementation.EnableHostControl
  else
  begin
    TObject(AValue.HostObject).GetInterface(IThoriumPersistent, Intf);
    Intf.EnableHostControl;
    Intf := nil;
  end;
end;

function TThoriumRTTIObjectType.DoEvaluate(const AValue: TThoriumValue): Boolean;
begin
  if AValue.HostObject <> nil then
    Exit(True);
  Result := False;
end;

function TThoriumRTTIObjectType.DoGetField(const AValue: TThoriumValue;
  const AFieldID: QWord): TThoriumValue;
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

procedure TThoriumRTTIObjectType.DoSetField(const AValue: TThoriumValue;
  const AFieldID: QWord; const NewValue: TThoriumValue);
begin
  raise EThoriumException.Create('Not reimplemented yet.');
end;

procedure TThoriumRTTIObjectType.SetPropertyStoring(const PropInfo: PPropInfo;
  const Storing: Boolean);
var
  Idx: Integer;
begin
  Idx := FStoringProperties.IndexOfObject(TObject(PropInfo));
  if Storing then
  begin
    if Idx < 0 then
      FStoringProperties.AddObject(PropInfo^.Name, TObject(PropInfo));
  end
  else
  begin
    if Idx >= 0 then
      FStoringProperties.Delete(Idx);
  end;
end;

procedure TThoriumRTTIObjectType.SetPropertyStoring(const PropertyName: String;
  const Storing: Boolean);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(FBaseClass, PropertyName);
  if PropInfo = nil then
    raise EPropertyError.CreateFmt('Property ''%s'' not found.', [PropertyName]);
  SetPropertyStoring(PropInfo, Storing);
end;

procedure TThoriumRTTIObjectType.CalcHash;
var
  Signature: String;
  SigLength: Integer;
  Offset: Integer;
  I: Integer;
begin
  SigLength := 12;
  for I := 0 to FPropCount - 1 do
    Inc(SigLength, SizeOf(TTypeKind) + Length(FPropList^[I]^.Name));
  for I := 0 to High(FMethods) do
    Inc(SigLength, 16);
  for I := 0 to High(FStaticMethods) do
    Inc(SigLength, 16);
  SetLength(Signature, SigLength);
  Move(FPropCount, Signature[1], SizeOf(Integer));
  Move(High(FMethods), Signature[5], SizeOf(Integer));
  Move(High(FStaticMethods), Signature[9], SizeOf(Integer));
  Offset := 13;
  for I := 0 to FPropCount - 1 do
  begin
    Move(FPropList^[I]^.PropType^.Kind, Signature[Offset], SizeOf(TTypeKind));
    Inc(Offset, SizeOf(TTypeKind));
    Move(FPropList^[I]^.Name[1], Signature[Offset], Length(FPropList^[I]^.Name));
    Inc(Offset, Length(FPropList^[I]^.Name));
  end;
  for I := 0 to High(FMethods) do
  begin
    Move(FMethods[I].GetHash, Signature[Offset], 16);
    Inc(Offset, 16);
  end;
  for I := 0 to High(FStaticMethods) do
  begin
    Move(FStaticMethods[I].GetHash, Signature[Offset], 16);
    Inc(Offset, 16);
  end;
  FHash := TThoriumHash(MD5Buffer(Signature[1], Length(Signature)));
end;

function TThoriumRTTIObjectType.NewNativeCallMethod(const AName: String;
  const ACodePointer: Pointer; const AParameters: array of PTypeInfo;
  const AReturnType: PTypeInfo;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostMethodNativeCall;
var
  I: Integer;
  ReturnTypeEx: TThoriumHostType;
begin
  Result := TThoriumHostMethodNativeCall.Create(Self.FLibrary);
  ReturnTypeEx.Storing := False;
  ReturnTypeEx.HostType := AReturnType;
  with Result do
  begin
    Name := AName;
    CodePointer := ACodePointer;
    for I := 0 to High(AParameters) do
      Parameters.Add(AParameters[I]);
    ReturnType := ReturnTypeEx;
    CallingConvention := ACallingConvention;
  end;
end;

function TThoriumRTTIObjectType.NewNativeCallStaticMethod(
  const AName: String; const ACodePointer: Pointer; const ADataPointer: Pointer;
  const AParameters: array of PTypeInfo;
  const AReturnType: PTypeInfo;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostMethodAsFunctionNativeCall;
var
  I: Integer;
  ReturnTypeEx: TThoriumHostType;
begin
  Result := TThoriumHostMethodAsFunctionNativeCall.Create(Self.FLibrary);
  ReturnTypeEx.Storing := False;
  ReturnTypeEx.HostType := AReturnType;
  with Result do
  begin
    Name := AName;
    CodePointer := ACodePointer;
    DataPointer := ADataPointer;
    for I := 0 to High(AParameters) do
      Parameters.Add(AParameters[I]);
    ReturnType := ReturnTypeEx;
    CallingConvention := ACallingConvention;
  end;
end;

function TThoriumRTTIObjectType.NewNativeCallStaticFunction(
  const AName: String; const ACodePointer: Pointer;
  const AParameters: array of PTypeInfo;
  const AReturnType: PTypeInfo;
  const ACallingConvention: TThoriumNativeCallingConvention
  ): TThoriumHostFunctionNativeCall;
var
  I: Integer;
  ReturnTypeEx: TThoriumHostType;
begin
  Result := TThoriumHostFunctionNativeCall.Create(Self.FLibrary);
  ReturnTypeEx.Storing := False;
  ReturnTypeEx.HostType := AReturnType;
  with Result do
  begin
    Name := AName;
    CodePointer := ACodePointer;
    for I := 0 to High(AParameters) do
      Parameters.Add(AParameters[I]);
    ReturnType := ReturnTypeEx;
    CallingConvention := ACallingConvention;
  end;
end;

end.

