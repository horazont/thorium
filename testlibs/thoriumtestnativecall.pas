unit ThoriumTestNativeCall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, ThGlobals;

type

  { TTestNativeCall }

  TTestNativeCall = class (TThoriumLibrary)
  protected
    procedure InitializeLibrary; override;
  public
    class function GetName: String; override;
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

