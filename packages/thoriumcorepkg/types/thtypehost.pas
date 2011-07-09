unit ThTypeHost;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Thorium;

type

  { TThoriumHostObjectType }

  (* This is the base class for any class-alike type published by the host
     environment. You will need to override the methods to make it represent
     a type you want. *)
  TThoriumHostObjectType = class (TThoriumType)
    constructor Create(ALibrary: TThoriumLibrary); virtual;
    destructor Destroy; override;
  protected
    FLibrary: TThoriumLibrary;
  public
    procedure ApplyStoring(var AValue: Pointer; MayDecreaseReference: Boolean = True); virtual; abstract;
    procedure DisposeValue(var AValue: Pointer); virtual; abstract;
    function DuplicateInstance(const AValue: Pointer): Pointer; virtual; abstract;
    function FindMethod(const AMethodName: String): TThoriumHostMethodBase; virtual;
    function GetFieldID(const FieldIdent: String; out ID: QWord): Boolean; virtual;
    function GetFieldStoring(const AFieldID: QWord): Boolean; virtual; abstract;
    procedure GetFieldType(const AFieldID: QWord; out TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition); virtual; abstract;
    function GetIndexType(const IndexType: TThoriumType; out TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition): Boolean; virtual;
    function GetNewInstance: Pointer; virtual; abstract;
    function GetStaticFieldID(const FieldIdent: String; out ID: QWord): Boolean; virtual;
    function GetStaticFieldStoring(const AFieldID: QWord): Boolean; virtual; abstract;
    procedure GetStaticFieldType(const AFieldID: QWord; out TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition); virtual; abstract;
  end;
  TThoriumHostObjectTypeClass = class of TThoriumHostObjectType;

implementation

{ TThoriumHostObjectType }

constructor TThoriumHostObjectType.Create(ALibrary: TThoriumLibrary);
begin
  inherited Create(ALibrary.Thorium);
  FLibrary := ALibrary;
end;

destructor TThoriumHostObjectType.Destroy;
begin
  inherited Destroy;
end;

function TThoriumHostObjectType.FindMethod(const AMethodName: String
  ): TThoriumHostMethodBase;
begin
  Result := nil;
end;

function TThoriumHostObjectType.GetFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
begin
  Result := False;
end;

function TThoriumHostObjectType.GetIndexType(const IndexType: TThoriumType; out
  TypeSpec: TThoriumType; out Access: TThoriumAccessDefinition): Boolean;
begin
  Result := False;
end;

function TThoriumHostObjectType.GetStaticFieldID(const FieldIdent: String; out
  ID: QWord): Boolean;
begin
  Result := False;
end;

end.

