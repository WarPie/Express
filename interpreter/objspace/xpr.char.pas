{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Runtime datatype representations
}
unit xpr.char;
{$I express.inc}

interface

{$I objh.inc}

uses
  SysUtils,
  xpr.express, 
  xpr.objbase;


type
  TCharObject = class(TEpObject)
    value: epChar;
    
    constructor Create(AValue:epChar);
    function Copy(gcGen:Byte=0): TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    function AsBool: Boolean; override;
    function AsChar: epChar; override;
    function AsInt: epInt; override;
    function AsFloat: Double; override;

    procedure ASGN(other:TEpObject; var dest:TEpObject); override;

    procedure ADD(other:TEpObject; var dest:TEpObject); override;

    procedure EQ(other:TEpObject; var dest:TEpObject); override;
    procedure NE(other:TEpObject; var dest:TEpObject); override;
    procedure LT(other:TEpObject; var dest:TEpObject); override;
    procedure GT(other:TEpObject; var dest:TEpObject); override;
    procedure LE(other:TEpObject; var dest:TEpObject); override;
    procedure GE(other:TEpObject; var dest:TEpObject); override;
    procedure LOGIC_AND(other:TEpObject; var dest:TEpObject); override;
    procedure LOGIC_OR(other:TEpObject; var dest:TEpObject); override;
    procedure LOGIC_NOT(var dest:TEpObject); override;
  end;

procedure SetCharDest(var dest:TEpObject; constref value:epChar); inline;  
  
implementation

uses
  xpr.utils, 
  xpr.errors, 
  xpr.mmgr, 
  xpr.bool,
  xpr.str;

procedure SetCharDest(var dest:TEpObject; constref value:epChar);
var GC:TGarbageCollector;
begin
  Assert(dest <> nil, 'dest is nil');
  
  if dest.ClassType = TCharObject then
    TCharObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocChar(value);
  end;
end;

//Workaround for: `Internal error 2011010304`
procedure SetBoolDest(var dest:TEpObject; constref value:Boolean); inline;
var
  GC:TGarbageCollector;
begin
  Assert(dest <> nil, 'dest is nil');

  if dest.ClassType = TBoolObject then
    TBoolObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocBool(value);
  end;
end;


constructor TCharObject.Create(AValue:epChar);
begin
  self.Value := AValue;
end;

function TCharObject.Copy(gcGen:Byte=0): TEpObject;
begin
  Result := TGarbageCollector(GC).AllocChar(self.value, gcGen);
end;

function TCharObject.DeepCopy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocChar(self.value);
end;

function TCharObject.AsBool: Boolean;
begin
  Result := self.Value <> #0;
end;

function TCharObject.AsChar: epChar;
begin
  Result := self.Value;
end;

function TCharObject.AsInt: epInt;
begin
  Result := ord(self.Value);
end;

function TCharObject.AsFloat: Double;
begin
  Result := ord(self.Value);
end;

function TCharObject.AsString: epString;
begin
  Result := self.Value;
end;

procedure TCharObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other.ClassType = TCharObject then
    self.value := TCharObject(other).value
  else
    inherited;
end;

procedure TCharObject.ADD(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TCharObject) then
    SetStringDest(dest, self.value + TCharObject(other).value)
  else if (other.ClassType = TStringObject) then
    SetStringDest(dest, self.value + TStringObject(other).value)
  else
    inherited;
end;

procedure TCharObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TCharObject) then
    SetBoolDest(dest, self.value = TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) = other.AsInt)
end;

procedure TCharObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TCharObject) then
    SetBoolDest(dest, self.value <> TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) <> other.AsInt)
end;

procedure TCharObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TCharObject) then
    SetBoolDest(dest, self.value < TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) < other.AsInt)
end;

procedure TCharObject.GT(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TCharObject) then
    SetBoolDest(dest, self.value > TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) > other.AsInt)
end;

procedure TCharObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TCharObject) then
    SetBoolDest(dest, self.value <= TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) <= other.AsInt)
end;

procedure TCharObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TCharObject) then
    SetBoolDest(dest, self.value >= TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) >= other.AsInt)
end;

procedure TCharObject.LOGIC_AND(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TCharObject) then
    SetBoolDest(dest, (self.value <> #0) and (TCharObject(other).value <> #0))
  else if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, (self.value <> #0) and (TBoolObject(other).value))
  else
    SetBoolDest(dest, (self.value <> #0) and other.AsBool);
end;

procedure TCharObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TCharObject) then
    SetBoolDest(dest, (self.value <> #0) or (TCharObject(other).value <> #0))
  else if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, (self.value <> #0) or (TBoolObject(other).value))
  else
    SetBoolDest(dest, (self.value <> #0) or other.AsBool);
end;

procedure TCharObject.LOGIC_NOT(var dest:TEpObject);
begin
  SetBoolDest(dest, self.value = #0);
end;

end.
