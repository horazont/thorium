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

procedure nctest(A: Int64; B: Integer);
begin
  WriteLn('A = ', A);
  WriteLn('B = ', B);
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
    TypeInfo(Int64), TypeInfo(Integer)
  ], nil, ncRegister);
  inherited InitializeLibrary;
end;

end.

