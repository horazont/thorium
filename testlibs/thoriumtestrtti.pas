unit ThoriumTestRTTI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium;

type

  { TTestClass }

  TTestClass = class (TThoriumPersistent)
  private
    FName: String;
    procedure SetName(const AValue: String);
  published
    property Name: String read FName write SetName;
  end;

  { TTestRTTI }

  TTestRTTI = class (TThoriumLibrary)
  protected
    procedure InitializeLibrary; override;
  public
    class function GetName: String; override;
  end;

implementation

{ TTestRTTI }

procedure TTestRTTI.InitializeLibrary;
begin
  inherited InitializeLibrary;
  RegisterRTTIType(TTestClass);
end;

class function TTestRTTI.GetName: String;
begin
  Result := 'test.rtti';
end;

{ TTestClass }

procedure TTestClass.SetName(const AValue: String);
begin
  if FName = AValue then exit;
  FName := AValue;
  WriteLn('Name set to ', FName);
end;

end.
