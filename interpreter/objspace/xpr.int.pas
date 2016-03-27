{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Runtime datatype representations
}
unit xpr.int;
{$I express.inc}

interface

{$I objh.inc}

uses
  SysUtils,
  xpr.express, 
  xpr.objbase;


type
  TIntObject = class(TEpObject)
    value: epInt;
    constructor Create(AValue:epInt);
    function Copy(gcGen:Byte=0): TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    function AsBool: Boolean; override;
    function AsInt: epInt; override;
    function AsFloat: Double; override;

    procedure ASGN(other:TEpObject; var dest:TEpObject); override;
    procedure INPLACE_ADD(other:TEpObject); override;
    procedure INPLACE_SUB(other:TEpObject); override;
    procedure INPLACE_MUL(other:TEpObject); override;
    procedure INPLACE_DIV(other:TEpObject); override;

    procedure PREINC(var dest:TEpObject); override;
    procedure PREDEC(var dest:TEpObject); override;
    procedure POSTINC(var dest:TEpObject); override;
    procedure POSTDEC(var dest:TEpObject); override;

    procedure UNARY_SUB(var dest:TEpObject); override;
    procedure UNARY_INV(var dest:TEpObject); override;

    procedure ADD(other:TEpObject; var dest:TEpObject); override;
    procedure SUB(other:TEpObject; var dest:TEpObject); override;
    procedure MUL(other:TEpObject; var dest:TEpObject); override;
    procedure IDIV(other:TEpObject; var dest:TEpObject); override;
    procedure FDIV(other:TEpObject; var dest:TEpObject); override;
    procedure MODULO(other:TEpObject; var dest:TEpObject); override;
    procedure POW(other:TEpObject; var dest:TEpObject); override;

    procedure EQ(other:TEpObject; var dest:TEpObject); override;
    procedure NE(other:TEpObject; var dest:TEpObject); override;
    procedure LT(other:TEpObject; var dest:TEpObject); override;
    procedure GT(other:TEpObject; var dest:TEpObject); override;
    procedure GE(other:TEpObject; var dest:TEpObject); override;
    procedure LE(other:TEpObject; var dest:TEpObject); override;

    procedure LOGIC_AND(other:TEpObject; var dest:TEpObject); override;
    procedure LOGIC_OR(other:TEpObject; var dest:TEpObject); override;
    procedure LOGIC_NOT(var dest:TEpObject); override;

    procedure BAND(other:TEpObject; var dest:TEpObject); override;
    procedure BOR(other:TEpObject; var dest:TEpObject); override;
    procedure BXOR(other:TEpObject; var dest:TEpObject); override;
  end;

procedure SetIntDest(var dest:TEpObject; constref value:epInt); inline;

implementation

uses
  xpr.utils, 
  xpr.errors, 
  xpr.mmgr,
  xpr.bool, 
  xpr.float;

procedure SetIntDest(var dest:TEpObject; constref value:epInt);
var GC:TGarbageCollector;
begin
  Assert(dest <> nil, 'dest is nil');
  
  if dest.ClassType = TIntObject then
    TIntObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocInt(value);
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


constructor TIntObject.Create(AValue:epInt);
begin
  self.Value := AValue;
end;

function TIntObject.Copy(gcGen:Byte=0): TEpObject;
begin
  Result := TGarbageCollector(GC).AllocInt(self.value, gcGen);
end;

function TIntObject.DeepCopy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocInt(self.value);
end;

function TIntObject.AsBool: Boolean;
begin
  Result := self.Value <> 0;
end;

function TIntObject.AsInt: epInt;
begin
  Result := self.Value;
end;

function TIntObject.AsFloat: Double;
begin
  Result := self.Value;
end;

function TIntObject.AsString: epString;
begin
  Result := IntToStr(self.Value);
end;

procedure TIntObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other.ClassType = TIntObject then
    self.value := TIntObject(other).value
  else
    inherited;
end;

procedure TIntObject.INPLACE_ADD(other:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    self.value += TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TIntObject.INPLACE_SUB(other:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    self.value -= TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TIntObject.INPLACE_MUL(other:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    self.value := self.value * TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TIntObject.INPLACE_DIV(other:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    self.value := self.value div TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TIntObject.PREINC(var dest:TEpObject);
begin
  self.value += 1;
  SetIntDest(dest, self.value);
end;

procedure TIntObject.PREDEC(var dest:TEpObject);
begin
  self.value -= 1;
  SetIntDest(dest, self.value);
end;

procedure TIntObject.POSTINC(var dest:TEpObject);
begin
  SetIntDest(dest, self.value);
  self.value += 1;
end;

procedure TIntObject.POSTDEC(var dest:TEpObject);
begin
  SetIntDest(dest, self.value);
  self.value -= 1;
end;

procedure TIntObject.UNARY_SUB(var dest:TEpObject);
begin
  SetIntDest(dest, -self.value);
end;

procedure TIntObject.UNARY_INV(var dest:TEpObject);
begin
  SetIntDest(dest, not self.value);
end;

procedure TIntObject.ADD(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetIntDest(dest, self.value + TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value + TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.SUB(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetIntDest(dest, self.value - TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value - TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.MUL(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetIntDest(dest, self.value * TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value * TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.IDIV(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetIntDest(dest, self.value div TIntObject(other).value)
  else
    inherited;
end;

procedure TIntObject.FDIV(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetFloatDest(dest, self.value / TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value / TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.MODULO(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetIntDest(dest, xpr.utils.modulo(self.value, TIntObject(other).value))
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, xpr.utils.modulo(self.value, TFloatObject(other).value))
  else
    inherited;
end;

procedure TIntObject.POW(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetIntDest(dest, self.value * TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value * TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value = TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value = TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value = other.AsInt)
end;

procedure TIntObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value <> TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value <> TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value <> other.AsInt)
end;

procedure TIntObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value < TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value < TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value < other.AsInt);
end;

procedure TIntObject.GT(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value > TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value > TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value > other.AsInt);
end;

procedure TIntObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value <= TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value <= TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value <= other.AsInt);
end;

procedure TIntObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value >= TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value >= TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value >= other.AsInt);
end;


procedure TIntObject.LOGIC_AND(other:TEpObject; var dest:TEpObject);
begin
  SetBoolDest(dest, (self.value <> 0) and other.AsBool);
end;

procedure TIntObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);
begin
  SetBoolDest(dest, (self.value <> 0) or other.AsBool);
end;

procedure TIntObject.LOGIC_NOT(var dest:TEpObject);
begin
  SetBoolDest(dest, self.value = 0);
end;

procedure TIntObject.BAND(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetIntDest(dest, self.value and TIntObject(other).value)
  else if (other.ClassType = TBoolObject) then
    SetIntDest(dest, self.value and Ord(TBoolObject(other).value))
  else
    inherited;
end;

procedure TIntObject.BOR(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetIntDest(dest, self.value or TIntObject(other).value)
  else if (other.ClassType = TBoolObject) then
    SetIntDest(dest, self.value or Ord(TBoolObject(other).value))
  else
    inherited;
end;

procedure TIntObject.BXOR(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetIntDest(dest, self.value xor TIntObject(other).value)
  else if (other.ClassType = TBoolObject) then
    SetIntDest(dest, self.value xor Ord(TBoolObject(other).value))
  else
    inherited;
end;

end.
