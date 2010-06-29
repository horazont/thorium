(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: thoriumlibstdio.pas
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
unit ThoriumLibStdIO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals;

type

  { TThoriumLibStdIO }

  TThoriumLibStdIO = class (TThoriumLibrary)
  protected
    procedure InitializeLibrary; override;
  public
    class function GetName: String; override;
  end;

implementation

procedure LibStdIO_printf(const Fmt: String; const Args: array of const);
begin
  Write(Format(Fmt, Args, DefaultFormatSettings));
end;

function LibStdIO_readfloat: Double;
begin
  ReadLn(Result);
end;

function LibStdIO_readint: Int64;
begin
  ReadLn(Result);
end;

function LibStdIO_readstr: String;
begin
  ReadLn(Result);
end;

{ TThoriumLibStdIO }

procedure TThoriumLibStdIO.InitializeLibrary;
begin
  RegisterNativeCallFunction(
    'printf',
    @LibStdIO_printf,
    [htString, htArray or htAny],
    htNone,
    ncRegister
  );
  RegisterNativeCallFunction(
    'readfloat',
    @LibStdIO_readfloat,
    [],
    htDouble,
    ncRegister
  );
  RegisterNativeCallFunction(
    'readint',
    @LibStdIO_readint,
    [],
    htInt64,
    ncRegister
  );
  RegisterNativeCallFunction(
    'readstr',
    @LibStdIO_readstr,
    [],
    htString,
    ncRegister
  );
end;

class function TThoriumLibStdIO.GetName: String;
begin
  Result:='core.std.io';
end;

end.

