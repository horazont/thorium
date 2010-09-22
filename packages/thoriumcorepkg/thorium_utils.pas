(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: thorium_utils.pas
** Last update: 2010-03-12
This file is part of the Thorium Scripting Language Project.
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
unit Thorium_Utils;

{$mode objfpc}{$H+}

// Switch whether Thorium should be case-aware or not
{.$define CaseAware}

interface

uses
  Classes, SysUtils; 
  
type
  (* A record used to convert Double values bytewise to an Int64. *)
  TThoriumNativeConverterHelper = packed record
  case Byte of
    0: (Float: Double);
    {$IFDEF ENDIAN_LITTLE}
    1: (Lo, Hi: LongInt);
    {$ELSE}
    1: (Hi, Lo: LongInt);
    {$ENDIF}
    2: (I64: Int64);
  end;

  TThoriumFPCArrayHeader = packed record
    References: SizeInt;
    Len: SizeInt;
  end;
  PThoriumFPCArrayHeader = ^TThoriumFPCArrayHeader;

(* This function converts a Cardinal value to a UTF-8 encoded string. *)
function UTF8Chr(Value: Cardinal): String;

(* Calculate the offset of AField in ARec. *)
function Offset(var ARec; var AField): ptruint;

(* Handle the casing of identifiers. *)
function ThoriumCase(const Input: String): String;
function ThoriumCase(const Input: Char): Char;

function ColorCmd(A, B, C: Integer): String;

implementation

//  Cardinal:
//    543210zyxwvutsrqponmlkjihgfedcba
//  Stage 1: ($0000007F)
//    00000000000000000000000001111111
//    Mark bits: 0xxxxxxx = $00
//    Block 1: a-e (Value and $7F)
//  Stage 2: ($000007FF)
//    00000000000000000000011111111111
//    Mark bits: 110xxxxx = $C0
//    Block 1: a-f $80 or (Value and $3F)
//    Block 2: g-k $C0 or ((Value shr 6) and $1F)
//  Stage 3: ($0000FFFF)
//    00000000000000001111111111111111
//    Mark bits: 1110xxxx = $E0
//    Block 1: a-f $80 or (Value and $3F)
//    Block 2: g-l $80 or ((Value shr 6) and $3F)
//    Block 3: m-p $E0 or ((Value shr 10) and $0F)
//  Stage 4: ($001FFFFF)
//    00000000000111111111111111111111
//    Mark bits: 11110xxx = $F0
//    Block 1: a-f $80 or (Value and $3F)
//    Block 2: g-l $80 or ((Value shr 6) and $3F)
//    Block 3: m-r $80 or ((Value shr 12) and $3F)
//    Block 4: s-u $F0 or ((Value shr 15) and $07)

function UTF8Chr(Value: Cardinal): String;
// Converts a char code to UTF-8
begin
  if Value and $0000007F = Value then
    Result := Chr(Value)
  else if Value and $000007FF = Value then
  begin
    Result :=
      Chr($C0 or ((Value shr 6) and $1F))+
      Chr($80 or (Value and $3F));
  end
  else if Value and $0000FFFF = Value then
  begin
    Result :=
      Chr($E0 or ((Value shr 10) and $0F))+
      Chr($80 or ((Value shr 6) and $3F))+
      Chr($80 or (Value and $3F));
  end
  else if Value and $001FFFFF = Value then
  begin
    Result :=
      Chr($F0 or ((Value shr 15) and $07))+
      Chr($80 or ((Value shr 12) and $3F))+
      Chr($80 or ((Value shr 6) and $3F))+
      Chr($80 or (Value and $3F));
  end;
end;

function Offset(var ARec; var AField): ptruint;
begin
  Result := ptruint(@AField) - ptruint(@ARec);
end;

function ThoriumCase(const Input: String): String;
begin
  {$ifdef CaseAware}
  Result := Input;
  {$else}
  Result := LowerCase(Input);
  {$endif}
end;

function ThoriumCase(const Input: Char): Char;
begin
  {$ifdef CaseAware}
  Result := Input;
  {$else}
  Result := LowerCase(Input);
  {$endif}
end;

function ColorCmd(A, B, C: Integer): String;
begin
  if (A = 0) and (C = 0) then
  begin
    // Only set background
    Exit(#27 + '[;' + IntToStr(B) + 'm');
  end;
  Result := #27 + '[' + IntToStr(A);
  if B >= 0 then
  begin
    Result += ';'+IntToStr(B);
    if C >= 0 then
      Result += ';'+IntToStr(C);
  end;
  Result += 'm';
end;

end.

