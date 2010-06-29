(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: libcustom.pas
** Last update: 2009-08-24
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
unit libcustom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals;

type

  { TUser }

  TUser = class (TThoriumPersistent)
  public
    constructor Create;
  private
    FAge: Cardinal;
    FBodyHeight: Double;
    FHobbies: String;
    FRealName: String;
    FUserName: String;
  protected
    class function Construct(AUserName: String): TUser;
    class procedure GetMethodList(Sender: TThoriumRTTIObjectType;
       var Methods: TThoriumRTTIMethods); override;
    class procedure GetStaticMethodList(Sender: TThoriumRTTIObjectType;
       var Methods: TThoriumRTTIStaticMethods); override;
  public
    procedure DumpUser;
  published
    property Age: Cardinal read FAge write FAge;
    property BodyHeight: Double read FBodyHeight write FBodyHeight;
    property Hobbies: String read FHobbies write FHobbies;
    property RealName: String read FRealName write FRealName;
    property UserName: String read FUserName write FUserName;
  end;

  { TLibCustom }

  TLibCustom = class (TThoriumLibrary)
  protected
    procedure InitializeLibrary; override;
    class function GetName: String; override;
  end;

implementation

function LibCustom_VerifyUserName(AUserName: String): Boolean;
const
  ValidChars = ['a'..'z', 'A'..'Z'];
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(AUserName) do
  begin
    if not (AUserName[I] in ValidChars) then
      Exit;
  end;
  Result := True;
end;

{ TUser }

constructor TUser.Create;
begin
  inherited;
  FUserName := '';
  FAge := 0;
  FBodyHeight := 0.0;
  FHobbies := '';
end;

class function TUser.Construct(AUserName: String): TUser;
// A note about this function. It is *neccessary* to use a static (class) method
// instead of the direct call to a constructor. This is due to pascal internas
// which seem to happen at constructor call. Maybe there will be an alternative
// for this later. A special type for constructors for example ;).
begin
  // The API of 1.0.3.0 forces you to separate host and script references. The
  // script references are counted, the host reference is a boolean which
  // indicates whether the host controls the object or not. To create an object
  // for the script only, you would call the constructor and get a new
  // reference:
  Result := TUser(TUser.Create.ThoriumReference.GetReference);
  // Initialize some stuff:
  Result.FUserName := AUserName;
  // And disable the host control.
  Result.ThoriumReference.DisableHostControl;
end;

class procedure TUser.GetMethodList(Sender: TThoriumRTTIObjectType;
  var Methods: TThoriumRTTIMethods);
begin
  SetLength(Methods, 1);
  Methods[0] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'dumpuser',
    @TUser.DumpUser,
    [],
    htNone,
    ncRegister
  );
end;

class procedure TUser.GetStaticMethodList(Sender: TThoriumRTTIObjectType;
  var Methods: TThoriumRTTIStaticMethods);
begin
  SetLength(Methods, 1);
  Methods[0] := TThoriumRTTIObjectType.NewNativeCallStaticMethod(
    'create',
    @TUser.Construct, TUser,
    [htString],
    htExt,
    ncRegister
  );
  Methods[0].ReturnTypeExtended := Sender;
end;

procedure TUser.DumpUser;
begin
  WriteLn('User:');
  WriteLn('  Name: ', FUserName);
  WriteLn('  Real name: ', FRealName);
  WriteLn('  Age: ', FAge);
  WriteLn('  Body height [m]: ', Format('%.2f', [FBodyHeight]));
  WriteLn('  Hobbies: ', FHobbies);
end;

{ TLibCustom }

procedure TLibCustom.InitializeLibrary;
begin
  RegisterRTTIType(TUser, False);
  RegisterNativeCallFunction(
    'verifyusername',
    @LibCustom_VerifyUserName,
    [htString],
    htByte,
    ncRegister
  );
end;

class function TLibCustom.GetName: String;
begin
  Result := 'libcustom';
end;

end.

