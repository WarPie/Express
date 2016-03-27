unit xpr.utils;
{
  Author: Jarl K. Holta  
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  utilities
}
{$I express.inc}
{$hints off}

interface

uses
  xpr.express;

const INone = High(Int32);

procedure WriteFancy(s:string);
function MarkTime(): Double; inline;

function NextPow2m1(n: Int32): Int32; inline;
function Modulo(X,Y:Double): Double; inline; overload;
function Modulo(X,Y:Single): Single; inline; overload;
function Modulo(X,Y:Int32): Int32; inline; overload;
function Modulo(X,Y:Int64): Int64; inline; overload;

function StrToFloatDot(const S: string): Extended; inline;
function FloatToStrDot(f:Extended): string; inline;

function StringContains(arr:array of string; v:string): Boolean;
function Slice(const S:string; Start:Int32=INone; Stop:Int32=INone; Step:Int32=1): string;
function StrPosEx(const SubStr, Text: string): TIntArray;
function StrExplode(const Text, Sep: string): TStringArray;
function StartsWith(const Prefix, Text:string):Boolean;

implementation

uses
  Math, SysUtils, DateUtils, {$IFDEF WINDOWS}Windows{$ELSE}BaseUnix,Unix{$ENDIF};

type
  TColorAttribute = record color:string; id:Int32; end;

const
  COLOR_ATTRIBUTES:Array [0..15] of TColorAttribute = (
    (color:'BLACK';  id:0),
    (color:'BLUE';   id:1),
    (color:'GREEN';  id:2),
    (color:'AQUA';   id:3),
    (color:'RED';    id:4),
    (color:'PURPLE'; id:5),
    (color:'YELLOW'; id:6),
    (color:'WHITE';  id:7),

    (color:'GRAY';    id:8),
    (color:'LBLUE';   id:9),
    (color:'LGREEN';  id:10),
    (color:'LAQUA';   id:11),
    (color:'LRED';    id:12),
    (color:'LPURPLE'; id:13),
    (color:'LYELLOW'; id:14),
    (color:'LWHITE';  id:15)
   );

procedure WriteFancy(s:string);
{$IFDEF WINDOWS}
var
  hConsole:HANDLE;
  consoleInfo:CONSOLE_SCREEN_BUFFER_INFO;
  saved_attributes:WORD;
  strings:TStringArray;
  i,j:Int32;
  didWrite:Boolean;
begin
  strings := StrExplode(s,#13#10);

  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(hConsole, &consoleInfo);
  for i:=0 to High(strings) do
  begin
    didWrite := False;
    saved_attributes := consoleInfo.wAttributes;
    for j:=0 to High(COLOR_ATTRIBUTES) do
      if StartsWith('['+COLOR_ATTRIBUTES[j].color+']', strings[i]) then
      begin
        SetConsoleTextAttribute(hConsole, COLOR_ATTRIBUTES[j].id);
        WriteLn(Slice(strings[i], Length(COLOR_ATTRIBUTES[j].color)+3));
        didWrite := True;
        break;
      end;
    if not didWrite then
      WriteLn(strings[i]);

    SetConsoleTextAttribute(hConsole, saved_attributes);
  end;
end;
{$ELSE}
begin
  WriteLn('...');
end;
{$ENDIF}


function MarkTime(): Double;
var
  {$IFDEF WINDOWS}
  frequency:Int64;
  count:Int64;
  {$ELSE}
  TV:TTimeVal;
  TZ:PTimeZone;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  QueryPerformanceFrequency(frequency);
  QueryPerformanceCounter(count);
  Result := count / frequency * 1000;
  {$ELSE}
  TZ := nil;
  fpGetTimeOfDay(@TV, TZ);
  count := Int64(TV.tv_sec) * 1000000 + Int64(TV.tv_usec);
  Result := count / 1000;
  {$ENDIF}
end;

function NextPow2m1(n: Int32): Int32;
begin
  n := n - 1;
  n := n or (n shr 1);
  n := n or (n shr 2);
  n := n or (n shr 4);
  n := n or (n shr 8);
  n := n or (n shr 16);
  n := n or (n shr 32);
  Result := n;
end;

{*
 "Real" modulus function as seen in: WolframAlpha, MatLab and Python, and other newer programming languages.
*}
function Modulo(X,Y:Double): Double;
begin Result := X - Floor(X / Y) * Y; end;

function Modulo(X,Y:Single): Single;
begin Result := X - Floor(X / Y) * Y; end;

function Modulo(X,Y:Int32): Int32;
begin Result := X - Floor(X / Y) * Y; end;

function Modulo(X,Y:Int64): Int64;
begin Result := X - Floor(X / Y) * Y; end;


function StrToFloatDot(const S: string): Extended;
begin
  Result := StrToFloat(StringReplace(S, '.', FormatSettings.DecimalSeparator, []));
end;

function FloatToStrDot(f:Extended): string;
begin
  Result := StringReplace(FloatToStr(f), FormatSettings.DecimalSeparator, '.', []);
end;

function StringContains(arr:array of string; v:string): Boolean;
var i:Int32;
begin
  Result := False;
  for i:=0 to High(arr) do
    if arr[i] = v then
      Exit(True);
end;

function Slice(const S:String; Start:Int32=High(Int32); Stop:Int32=High(Int32); Step:Int32=1): String;
var
  P,R:PChar;
  l:Int32;
  H:PtrUInt;
begin
  if (Start = High(Int32)) then
    if Step < 0 then Start := -1
    else Start := 1;
  if (Stop = High(Int32)) then
    if Step > 0 then Stop := -1
    else Stop := 1;

  h := Length(S);
  case (Step > 0) of
    True:  if (Stop > h) then Stop := h;
    False: if (Start > h) then Start := h;
  end;
  Start := Modulo(start,h+1);
  Stop  := Modulo(stop,h+1);

  if (Start > Stop) and (Step > 0) then
    Exit('');

  if (not InRange(Start,1,Length(S))) or (not InRange(Stop,1,Length(S))) then
    Exit('');

  SetLength(Result, ((Stop-Start) div step)+1);
  P := @S[start];
  R := @Result[1];
  L := PtrUInt(@Result[Length(Result)]);
  while PtrUInt(R) <= L do
  begin
    R^ := P^;
    Inc(R);
    Inc(P, step);
  end;
end;

{*
  Returns all positions of the given pattern/substring.
*}
function StrPosEx(const SubStr, Text:String): TIntArray;
var
  HitPos,LenSub,h,q,i: UInt32;
begin
  LenSub := Length(SubStr);
  if LenSub = 0 then Exit;
  HitPos := 1;
  h := 0;
  q := 1;
  SetLength(Result, q);
  for i:=1 to Length(Text) do
    if Text[i] <> SubStr[HitPos] then
      HitPos := 1
    else begin
      if (HitPos <> LenSub) then
        Inc(HitPos)
      else begin
        if q <= h then
        begin
          q := q+q;
          SetLength(Result, q);
        end;
        Result[h] := (i - HitPos) + 1;
        Inc(h);
        HitPos := 1;
      end;
    end;
  SetLength(Result, h);
end;

function StrExplode(const Text, Sep: String): TStringArray;
var
  Subs:TIntArray;
  Hi,i,Curr,Prev,HiSep: UInt32;
begin
  Hi := Length(Text);
  if Hi = 0 then Exit;

  Subs := StrPosEx(Sep, Text);
  if Length(Subs) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := Copy(Text, 1,Hi);
    Exit;
  end;
  HiSep := Length(Sep);
  Prev := 1;
  SetLength(Result, Length(Subs));
  for i:=0 to High(Subs) do
  begin
    Curr := Subs[i];
    Result[i] := Copy(Text, Prev, (Curr-Prev));
    Prev := Curr+HiSep;
  end;
  if Prev <= Hi then
  begin
    SetLength(Result, Length(Subs)+1);
    Result[Length(Subs)] := Copy(Text, Prev, Hi);
  end;
end;

function StartsWith(const Prefix, Text:String): Boolean;
begin
  Result := Copy(Text,1,Length(Prefix)) = Prefix;
end;

end.
