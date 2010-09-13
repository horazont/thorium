(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: thoriumlibstring.pas
** Last update: 2009-08-03
This file is part of the Thorium Scripting Language Project host library
package.
For more information and notes see the thorium.pas file delivered with the
Thorium Scripting Language Project main package.

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms
of the GNU General Public license (the  "GPL License"), in which case the
provisions of GPL License are applicable instead of those
above.

For feedback and questions about Thorium Scripting Language please mail me,
Jonas Wielicki:
j.wielicki@sotecware.net
*******************************************************************************)
unit ThoriumLibString;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals;

type

  { TThoriumLibString }

  TThoriumLibString = class (TThoriumLibrary)
  protected
    procedure InitializeLibrary; override;
  public
    class function GetName: String; override;
  end;

implementation

function LibString_Copy(const S: String; From, Count: Integer): String;
begin
  Result := Copy(S, From, Count);
end;

function LibString_Pos(const Substr, Str: String): Integer;
begin
  Result := Pos(Substr, Str);
end;

{ TThoriumLibString }

procedure TThoriumLibString.InitializeLibrary;
begin
  RegisterNativeCallFunction(
    'rightstr',
    @RightStr,
    [htString, htLongInt],
    htString,
    ncRegister
  );
  RegisterNativeCallFunction(
    'leftstr',
    @LeftStr,
    [htString, htLongInt],
    htString,
    ncRegister
  );
  RegisterNativeCallFunction(
    'copy',
    @LibString_Copy,
    [htString, htLongInt, htLongInt],
    htString,
    ncRegister
  );
  RegisterNativeCallFunction(
    'pos',
    @LibString_Pos,
    [htString, htString],
    htLongInt,
    ncRegister
  );
  inherited;
end;

class function TThoriumLibString.GetName: String;
begin
  Result:='thorium.strings';
end;

end.

