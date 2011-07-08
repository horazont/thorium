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

procedure nctest(S: String; A: Integer; B: Int64; C: Extended; D: Double);
begin
  WriteLn('S = ', S);
  WriteLn('A = ', A);
  WriteLn('B = ', B);
  WriteLn(Format('C = %.4f', [C]));
  WriteLn(Format('D = %.4f', [D]));
//  WriteLn(Format('E = %.4f', [E]));
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
    TypeInfo(String), TypeInfo(Integer), TypeInfo(Int64), TypeInfo(Extended),
    TypeInfo(Double)
  ], nil, ncRegister);
  inherited InitializeLibrary;
end;

end.

