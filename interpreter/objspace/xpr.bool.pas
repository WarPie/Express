{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Runtime datatype representations
}
unit xpr.bool;
{$I express.inc}

interface

{$I objh.inc}

uses
  SysUtils,
  xpr.express, 
  xpr.objbase;


type
  TBoolObject = class(TEpObject)
    value: Boolean;
    
    constructor Create(AValue:Boolean);
    function Copy(gcGen:Byte=0): TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsBool: Boolean; override;
    function AsInt: epInt; override;
    function AsFloat: Double; override;
    function AsString: epString; override;

    procedure ASGN(other:TEpObject; var dest:TEpObject); override;

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

procedure SetBoolDest(var dest:TEpObject; constref value:Boolean); inline;
  
implementation

uses
  xpr.utils, 
  xpr.errors, 
  xpr.mmgr,
  xpr.int,
  xpr.float;


procedure SetBoolDest(var dest:TEpObject; constref value:Boolean);
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


constructor TBoolObject.Create(AValue:Boolean);
begin
  self.Value := AValue;
end;

function TBoolObject.Copy(gcGen:Byte=0): TEpObject;
begin
  Result := TGarbageCollector(GC).AllocBool(self.value, gcGen);
end;

function TBoolObject.DeepCopy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocBool(self.value);
end;

function TBoolObject.AsBool: Boolean;
begin
  Result := self.Value;
end;

function TBoolObject.AsInt: epInt;
begin
  Result := ord(self.Value);
end;

function TBoolObject.AsFloat: Double;
begin
  Result := ord(self.Value);
end;

function TBoolObject.AsString: epString;
begin
  Result := BoolToStr(self.Value, True);
end;

procedure TBoolObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other.ClassType = TBoolObject then
    self.value := TBoolObject(other).value
  else
    inherited;
end;

procedure TBoolObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, self.value = TBoolObject(other).value)
  else
    SetBoolDest(dest, self.value = other.AsBool)
end;

procedure TBoolObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, self.value <> TBoolObject(other).value)
  else
    SetBoolDest(dest, self.value <> other.AsBool)
end;

procedure TBoolObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, Ord(self.value) < Ord(TBoolObject(other).value))
  else if (other.ClassType = TIntObject) then
    SetBoolDest(dest, Ord(self.value) < TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, Ord(self.value) < TFloatObject(other).value)
  else
    SetBoolDest(dest, Ord(self.value) < other.AsInt)
end;

procedure TBoolObject.GT(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, Ord(self.value) > Ord(TBoolObject(other).value))
  else if (other.ClassType = TIntObject) then
    SetBoolDest(dest, Ord(self.value) > TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, Ord(self.value) > TFloatObject(other).value)
  else
    SetBoolDest(dest, Ord(self.value) > other.AsInt)
end;

procedure TBoolObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, Ord(self.value) <= Ord(TBoolObject(other).value))
  else if (other.ClassType = TIntObject) then
    SetBoolDest(dest, Ord(self.value) <= TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, Ord(self.value) <= TFloatObject(other).value)
  else
    SetBoolDest(dest, Ord(self.value) <= other.AsInt)
end;

procedure TBoolObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, Ord(self.value) >= Ord(TBoolObject(other).value))
  else if (other.ClassType = TIntObject) then
    SetBoolDest(dest, Ord(self.value) >= TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, Ord(self.value) >= TFloatObject(other).value)
  else
    SetBoolDest(dest, Ord(self.value) >= other.AsInt)
end;

procedure TBoolObject.LOGIC_AND(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, self.value and TBoolObject(other).value)
  else
    SetBoolDest(dest, self.value and other.AsBool);
end;

procedure TBoolObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TBoolObject) then
    SetBoolDest(dest, self.value or TBoolObject(other).value)
  else
    SetBoolDest(dest, self.value or other.AsBool);
end;

procedure TBoolObject.LOGIC_NOT(var dest:TEpObject);
begin
  SetBoolDest(dest, not self.value);
end;

end.
