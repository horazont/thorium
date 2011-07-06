unit ThoriumTestNativeCall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals;

type

  { TTestNativeCall }

  TTestNativeCall = class (TThoriumLibrary)
  protected
    class function GetName: String; override;
    procedure InitializeLibrary; override;
  end;

implementation

procedure nctest(S: String);
begin
  WriteLn('S = ', S);
end;

{ TTestNativeCall }

class function TTestNativeCall.GetName: String;
begin
  Result := 'test.nativecall';
end;

procedure TTestNativeCall.InitializeLibrary;
begin
  AddDependency('thorium');
  RegisterNativeCallFunction('nctest', @nctest, [
    TypeInfo(String)
  ], nil, ncRegister);
  inherited InitializeLibrary;
end;

end.

