{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Runtime datatype representations
}
unit xpr.float;
{$I express.inc}

interface

{$I objh.inc}

uses
  SysUtils,
  xpr.express,
  xpr.objbase;


type
  TFloatObject = class(TEpObject)
    value: Double;
    constructor Create(AValue:Double);
    function Copy(gcGen:Byte=0): TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    function AsBool: Boolean; override;
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

    procedure ADD(other:TEpObject; var dest:TEpObject); override;
    procedure SUB(other:TEpObject; var dest:TEpObject); override;
    procedure MUL(other:TEpObject; var dest:TEpObject); override;
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
  end;

procedure SetFloatDest(var dest:TEpObject; constref value:Double); inline;  

implementation

uses
  xpr.utils,
  xpr.errors,
  xpr.mmgr,
  xpr.bool,
  xpr.int;

procedure SetFloatDest(var dest:TEpObject; constref value:Double); inline;
var GC:TGarbageCollector;
begin
  Assert(dest <> nil, 'dest is nil');
  
  if dest.ClassType = TFloatObject then
    TFloatObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocFloat(value);
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


constructor TFloatObject.Create(AValue:Double);
begin
  self.Value := AValue;
end;

function TFloatObject.Copy(gcGen:Byte=0): TEpObject;
begin
  Result := TGarbageCollector(GC).AllocFloat(self.value, gcGen);
end;

function TFloatObject.DeepCopy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocFloat(self.value);
end;

function TFloatObject.AsBool: Boolean;
begin
  Result := self.Value <> 0;
end;

function TFloatObject.AsFloat: Double;
begin
  Result := self.Value;
end;

function TFloatObject.AsString: epString;
begin
  Result := FloatToStr(self.Value);
end;

procedure TFloatObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other.ClassType = TFloatObject then
    self.value := TFloatObject(other).value
  else
    inherited;
end;

procedure TFloatObject.INPLACE_ADD(other:TEpObject);
begin
  if (other.ClassType = TFloatObject) then
    self.value += TFloatObject(other).value
  else if (other.ClassType = TIntObject) then
    self.value += TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TFloatObject.INPLACE_SUB(other:TEpObject);
begin
  if (other.ClassType = TFloatObject) then
    self.value -= TFloatObject(other).value
  else if (other.ClassType = TIntObject) then
    self.value -= TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TFloatObject.INPLACE_MUL(other:TEpObject);
begin
  if (other.ClassType = TFloatObject) then
    self.value := self.value * TFloatObject(other).value
  else if (other.ClassType = TIntObject) then
    self.value := self.value * TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TFloatObject.INPLACE_DIV(other:TEpObject);
begin
  if (other.ClassType = TFloatObject) then
    self.value := self.value / TFloatObject(other).value
  else if (other.ClassType = TIntObject) then
    self.value := self.value / TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TFloatObject.PREINC(var dest:TEpObject);
begin
  self.value += 1;
  SetFloatDest(dest, self.value);
end;

procedure TFloatObject.PREDEC(var dest:TEpObject);
begin
  self.value -= 1;
  SetFloatDest(dest, self.value);
end;

procedure TFloatObject.POSTINC(var dest:TEpObject);
begin
  SetFloatDest(dest, self.value);
  self.value += 1;
end;

procedure TFloatObject.POSTDEC(var dest:TEpObject);
begin
  SetFloatDest(dest, self.value);
  self.value -= 1;
end;

procedure TFloatObject.UNARY_SUB(var dest:TEpObject);
begin
  SetFloatDest(dest, -self.value);
end;

procedure TFloatObject.ADD(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetFloatDest(dest, self.value + TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value + TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.SUB(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetFloatDest(dest, self.value - TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value - TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.MUL(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetFloatDest(dest, self.value * TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value * TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.FDIV(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetFloatDest(dest, self.value / TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value / TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.MODULO(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetFloatDest(dest, xpr.utils.modulo(self.value, TIntObject(other).value))
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, xpr.utils.modulo(self.value, TFloatObject(other).value))
  else
    inherited;
end;

procedure TFloatObject.POW(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetFloatDest(dest, self.value * TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetFloatDest(dest, self.value * TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value = TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value = TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value = other.AsFloat);
end;

procedure TFloatObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value <> TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value <> TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value <> other.AsFloat);
end;

procedure TFloatObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value < TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value < TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value < other.AsFloat);
end;

procedure TFloatObject.GT(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value > TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value > TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value > other.AsFloat);
end;

procedure TFloatObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value <= TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value <= TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value <= other.AsFloat);
end;

procedure TFloatObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TIntObject) then
    SetBoolDest(dest, self.value >= TIntObject(other).value)
  else if (other.ClassType = TFloatObject) then
    SetBoolDest(dest, self.value >= TFloatObject(other).value)
  else
    SetBoolDest(dest, self.value >= other.AsFloat);
end;


procedure TFloatObject.LOGIC_AND(other:TEpObject; var dest:TEpObject);
begin
  SetBoolDest(dest, (self.value <> 0) and other.AsBool);
end;

procedure TFloatObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);
begin
  SetBoolDest(dest, (self.value <> 0) or other.AsBool);
end;

procedure TFloatObject.LOGIC_NOT(var dest:TEpObject);
begin
  SetBoolDest(dest, self.value = 0);
end;

end.
