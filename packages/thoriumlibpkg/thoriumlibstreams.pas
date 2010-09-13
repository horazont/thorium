(*******************************************************************************
*** THORIUM SCRIPTING LANGUAGE - by Jonas Wielicki
********************************************************************************
** File Name: thoriumlibstreams.pas
** Last update: 2009-08-24
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
unit ThoriumLibStreams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thorium, Thorium_Globals;

type
  { TThoriumFileStream }

  {$M+}
  TThoriumFileStream = class (TFileStream, IUnknown, IThoriumPersistent)
  public
    constructor Create(const AFileName: string; Mode: Word);
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal);
    destructor Destroy; override;
  protected
    FReferenceImplementation: TThoriumReferenceImplementation;
    FReference: IUnknown;
    FThoriumReference: IThoriumPersistent;
  protected
    property Reference: IUnknown read FReference implements IUnknown;
  protected
    class function Construct(const AFileName: String; Mode: Word): TThoriumFileStream;
    class procedure GetMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods);
    class procedure GetStaticMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIStaticMethods);
  public
    property ThoriumReference: IThoriumPersistent read FThoriumReference implements IThoriumPersistent;
  published
    property Position;
    property Size;
  end;
  {$M-}
  TThoriumFileStreamClass = class of TThoriumFileStream;

  { TThoriumLibStreams }

  TThoriumLibStreams = class (TThoriumLibrary)
  protected
    class procedure GetStreamMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods);
    procedure InitializeLibrary; override;
  public
    class function GetName: String; override;
  end;

implementation

function LibStreams_TStream_ReadQWord(AStream: TStream): QWord;
begin
  AStream.Read(Result, SizeOf(QWord));
end;

function LibStreams_TStream_ReadString(AStream: TStream; Length: Int64): String;
begin
  SetLength(Result, Length);
  SetLength(Result, AStream.Read(Result[1], Length));
end;

procedure LibStreams_TStream_WriteQWord(AStream: TStream; qw: QWord);
begin
  AStream.Write(qw, SizeOf(QWord));
end;

procedure LibStreams_TStream_WriteString(AStream: TStream; Str: String);
begin
  AStream.Write(Str[1], Length(Str));
end;

procedure LibStreams_TStream_WriteStringPart(AStream: TStream; Str: String; Start, Count: Int64);
begin
  AStream.Write(Str[Start+1], Count);
end;

{ TThoriumFileStream }

constructor TThoriumFileStream.Create(const AFileName: string; Mode: Word);
begin
  inherited;
  FReferenceImplementation := TThoriumReferenceImplementation.Create(Self);
  FReference := FReferenceImplementation;
  FThoriumReference := FReferenceImplementation;
  // The two above references must not be counted. Otherwise the object will
  // never get freed.
  FThoriumReference.FreeReference;
  FReference._Release;
end;

constructor TThoriumFileStream.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal);
begin
  inherited;
  FReferenceImplementation := TThoriumReferenceImplementation.Create(Self);
  FReference := FReferenceImplementation;
  FThoriumReference := FReferenceImplementation;
  // The two above references must not be counted. Otherwise the object will
  // never get freed.
  FThoriumReference.FreeReference;
  FReference._Release;
end;

destructor TThoriumFileStream.Destroy;
begin
  FReferenceImplementation.Free;
  inherited Destroy;
end;

class function TThoriumFileStream.Construct(const AFileName: String; Mode: Word
  ): TThoriumFileStream;
begin
  Result := TThoriumFileStream(TThoriumFileStream.Create(AFileName, Mode).ThoriumReference.GetReference);
  Result.ThoriumReference.DisableHostControl;
end;

class procedure TThoriumFileStream.GetMethodList(Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods);
begin
  SetLength(Methods, 13);
  Methods[0] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writebyte',
    @TThoriumFileStream.WriteByte,
    [htByte]
  );
  Methods[1] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writeword',
    @TThoriumFileStream.WriteWord,
    [htWord]
  );
  Methods[2] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writedword',
    @TThoriumFileStream.WriteDWord,
    [htDWord]
  );
  Methods[3] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writeqword',
    @LibStreams_TStream_WriteQWord,
    [htQWord]
  );
  Methods[4] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writestring',
    @LibStreams_TStream_WriteString,
    [htString]
  );
  Methods[5] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writestringpart',
    @LibStreams_TStream_WriteStringPart,
    [htString, htInt64, htInt64]
  );
  Methods[6] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writebinarystring',
    @TThoriumFileStream.WriteAnsiString,
    [htString]
  );
  Methods[7] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readbyte',
    @TThoriumFileStream.ReadByte,
    [],
    htByte
  );
  Methods[8] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readword',
    @TThoriumFileStream.ReadWord,
    [],
    htWord
  );
  Methods[9] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readdword',
    @TThoriumFileStream.ReadDWord,
    [],
    htDWord
  );
  Methods[10] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readqword',
    @LibStreams_TStream_ReadQWord,
    [],
    htQWord
  );
  Methods[11] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readstring',
    @LibStreams_TStream_ReadString,
    [htInt64],
    htString
  );
  Methods[12] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readbinarystring',
    @TThoriumFileStream.ReadAnsiString,
    [],
    htString
  );
end;

class procedure TThoriumFileStream.GetStaticMethodList(Sender: TThoriumRTTIObjectType;
  var Methods: TThoriumRTTIStaticMethods);
begin
  SetLength(Methods, 1);
  Methods[0] := TThoriumRTTIObjectType.NewNativeCallStaticMethod(
    'create',
    @TThoriumFileStream.Construct, TThoriumFileStream,
    [htString, htWord],
    htExt
  );
  Methods[0].ReturnTypeExtended := Sender;
end;

{ TThoriumLibStreams }

class procedure TThoriumLibStreams.GetStreamMethodList(
  Sender: TThoriumRTTIObjectType; var Methods: TThoriumRTTIMethods);
begin
  SetLength(Methods, 13);
  Methods[0] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writebyte',
    @TStream.WriteByte,
    [htByte]
  );
  Methods[1] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writeword',
    @TStream.WriteWord,
    [htWord]
  );
  Methods[2] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writedword',
    @TStream.WriteDWord,
    [htDWord]
  );
  Methods[3] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writeqword',
    @LibStreams_TStream_WriteQWord,
    [htQWord]
  );
  Methods[4] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writestring',
    @LibStreams_TStream_WriteString,
    [htString]
  );
  Methods[5] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writestringpart',
    @LibStreams_TStream_WriteStringPart,
    [htString, htInt64, htInt64]
  );
  Methods[6] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'writebinarystring',
    @TStream.WriteAnsiString,
    [htString]
  );
  Methods[7] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readbyte',
    @TStream.ReadByte,
    [],
    htByte
  );
  Methods[8] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readword',
    @TStream.ReadWord,
    [],
    htWord
  );
  Methods[9] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readdword',
    @TStream.ReadDWord,
    [],
    htDWord
  );
  Methods[10] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readqword',
    @LibStreams_TStream_ReadQWord,
    [],
    htQWord
  );
  Methods[11] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readstring',
    @LibStreams_TStream_ReadString,
    [htInt64],
    htString
  );
  Methods[12] := TThoriumRTTIObjectType.NewNativeCallMethod(
    'readbinarystring',
    @TStream.ReadAnsiString,
    [],
    htString
  );
end;

procedure TThoriumLibStreams.InitializeLibrary;
var
  Cls: TThoriumFileStreamClass;
begin
  AddDependency('thorium');
  RegisterRTTIType(TStream, @GetStreamMethodList, nil, True);
  Cls := TThoriumFileStream;
  RegisterRTTIType(TThoriumFileStream, @Cls.GetMethodList, @Cls.GetStaticMethodList, False);
  RegisterConstant('fmCreate', fmCreate);
  RegisterConstant('fmOpenRead', fmOpenRead);
  RegisterConstant('fmOpenReadWrite', fmOpenReadWrite);
  RegisterConstant('fmOpenWrite', fmOpenWrite);
  inherited;
end;

class function TThoriumLibStreams.GetName: String;
begin
  Result := 'thorium.streams';
end;

end.

