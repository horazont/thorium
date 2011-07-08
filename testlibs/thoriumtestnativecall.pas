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

procedure nctest(C: Single; D: Double);
begin
  WriteLn(Format('C = %.4f', [C]));
  WriteLn(Format('D = %.4f', [D]));
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
    TypeInfo(Single), TypeInfo(Double)
  ], nil, ncRegister);
  inherited InitializeLibrary;
end;

end.

