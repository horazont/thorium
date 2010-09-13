(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: thoriumlibstd.pas
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
unit ThoriumLibStd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, dateutils, Thorium_Globals;

type

  { TThoriumLibStd }

  TThoriumLibStd = class (TThoriumLibrary)
  protected
    procedure InitializeLibrary; override;
  public
    class function GetName: String; override;
  end;

implementation

function LibStd_length(Str: String): SizeInt;
begin
  Result := Length(Str);
end;

function LibStd_format(const Fmt: String; const Args: array of const): String;
begin
  Result := Format(Fmt, Args, DefaultFormatSettings);
end;

function LibStd_time: Int64;
begin
  Result := DateTimeToUnix(Now);
end;

{ TThoriumLibStd }

procedure TThoriumLibStd.InitializeLibrary;
begin
  RegisterNativeCallFunction(
    'length',
    @LibStd_length,
    [TypeInfo(String)],
    TypeInfo(PtrInt),
    ncRegister
  );
  RegisterNativeCallFunction(
    'format',
    @LibStd_format,
    [TypeInfo(String), TypeInfo(array of const)],
    TypeInfo(String),
    ncRegister
  );
  RegisterNativeCallFunction(
    'time',
    @LibStd_time,
    [],
    htInt64,
    ncRegister
  );
  inherited;
end;

class function TThoriumLibStd.GetName: String;
begin
  Result := 'thorium.std';
end;

end.

