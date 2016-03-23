{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Runtime datatype representations
}
unit datatypes;
{$I express.inc}

interface

{$DEFINE E_NOT_IMPLEMENTED := RuntimeError.Create(eNotImplemented)}

uses
  Classes, SysUtils, express;

const
  NO_REFERENCES = -1;
  CONSTANT_REF  = -2;

type
  TEpObject = class
    GC:Pointer;

    function Release: Boolean; virtual;
    function Copy: TEpObject; virtual; abstract;
    function DeepCopy: TEpObject; virtual; abstract;

    function AsBool: Boolean; virtual;
    function AsChar: epChar; virtual;
    function AsInt: epInt; virtual;
    function AsFloat: Double; virtual;
    function AsString: epString; virtual;

    procedure ASGN(other:TEpObject; var dest:TEpObject); virtual;
    procedure INPLACE_ADD(other:TEpObject); virtual;
    procedure INPLACE_SUB(other:TEpObject); virtual;
    procedure INPLACE_MUL(other:TEpObject); virtual;
    procedure INPLACE_DIV(other:TEpObject); virtual;

    procedure PREINC(var dest:TEpObject); virtual;
    procedure PREDEC(var dest:TEpObject); virtual;
    procedure POSTINC(var dest:TEpObject); virtual;
    procedure POSTDEC(var dest:TEpObject); virtual;

    procedure UNARY_SUB(var dest:TEpObject); virtual;
    procedure UNARY_INV(var dest:TEpObject); virtual;

    procedure ADD(other:TEpObject; var dest:TEpObject); virtual;
    procedure SUB(other:TEpObject; var dest:TEpObject); virtual;
    procedure MUL(other:TEpObject; var dest:TEpObject); virtual;
    procedure IDIV(other:TEpObject; var dest:TEpObject); virtual;
    procedure FDIV(other:TEpObject; var dest:TEpObject); virtual;
    procedure MODULO(other:TEpObject; var dest:TEpObject); virtual;
    procedure POW(other:TEpObject; var dest:TEpObject); virtual;

    procedure EQ(other:TEpObject; var dest:TEpObject); virtual;
    procedure NE(other:TEpObject; var dest:TEpObject); virtual;
    procedure LT(other:TEpObject; var dest:TEpObject); virtual;
    procedure GT(other:TEpObject; var dest:TEpObject); virtual;
    procedure GE(other:TEpObject; var dest:TEpObject); virtual;
    procedure LE(other:TEpObject; var dest:TEpObject); virtual;

    procedure LOGIC_AND(other:TEpObject; var dest:TEpObject); virtual;
    procedure LOGIC_OR(other:TEpObject; var dest:TEpObject); virtual;
    procedure LOGIC_NOT(var dest:TEpObject); virtual;

    procedure BAND(other:TEpObject; var dest:TEpObject); virtual;
    procedure BOR(other:TEpObject; var dest:TEpObject); virtual;
    procedure BXOR(other:TEpObject; var dest:TEpObject); virtual;

    procedure GET_ITEM(index:TEpObject; var dest:TEpObject); virtual;
    procedure SET_ITEM(index:TEpObject; other:TEpObject); virtual;
  end;
  TObjectArray = array of TEpObject;

  TNoneObject = class(TEpObject)
    function Release: Boolean; override;
    function Copy: TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsBool: Boolean; override;
    function AsString: epString; override;
    procedure ASGN(other:TEpObject; var dest:TEpObject); override;

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

  TBoolObject = class(TEpObject)
    value: Boolean;
    constructor Create(AValue:Boolean);
    function Copy: TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsBool: Boolean; override;
    function AsInt: epInt; override;
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

  TCharObject = class(TEpObject)
    value: epChar;
    constructor Create(AValue:epChar);
    function Copy: TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    function AsBool: Boolean; override;
    function AsChar: epChar; override;
    function AsInt: epInt; override;

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

  TIntObject = class(TEpObject)
    value: epInt;
    constructor Create(AValue:epInt);
    function Copy: TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    function AsBool: Boolean; override;
    function AsInt: epInt; override;

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

  TFloatObject = class(TEpObject)
    value: Double;
    constructor Create(AValue:Double);
    function Copy: TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    function AsBool: Boolean; override;
    function AsInt: epInt; override;

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

  TStringObject = class(TEpObject)
    value: epString;
    constructor Create(AValue:epString);

    function Copy: TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsChar: epChar; override;
    function AsString: epString; override;
    function AsBool: Boolean; override;
    function AsInt: epInt; override;

    procedure ASGN(other:TEpObject; var dest:TEpObject); override;
    procedure INPLACE_ADD(other:TEpObject); override;
    procedure ADD(other:TEpObject; var dest:TEpObject); override;
    procedure GET_ITEM(index:TEpObject; var dest:TEpObject); override;
    procedure SET_ITEM(index:TEpObject; other:TEpObject); override;
  end;

  TListObject = class(TEpObject)
    value: TObjectArray;

    constructor Create(AValue:TObjectArray);
    function Release: Boolean; override;
    function Copy: TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    function AsBool: Boolean; override;

    //temporary
    procedure INPLACE_ADD(other:TEpObject); override;

    //
    procedure ASGN(other:TEpObject; var dest:TEpObject); override;
    procedure GET_ITEM(index:TEpObject; var dest:TEpObject); override;
    procedure SET_ITEM(index:TEpObject; other:TEpObject); override;
  end;


  TFuncObject = class(TEpObject)
    Name: epString;
    CodePos: Int32;
    VarRange: TIntRange;

    constructor Create(AName:epString; ACodePos:Int32; AVarRange:TIntRange);
    function Copy: TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    procedure ASGN(other:TEpObject; var dest:TEpObject); override;
  end;


procedure SetBoolDest(var dest:TEpObject; constref value:Boolean); inline;
procedure SetCharDest(var dest:TEpObject; constref value:epChar); inline;
procedure SetIntDest(var dest:TEpObject; constref value:epInt); inline;
procedure SetFloatDest(var dest:TEpObject; constref value:Double); inline;
procedure SetStringDest(var dest:TEpObject; constref value:epString); inline;


implementation

uses
  utils, errors, mmgr;


{=============================================================================}
// Helper functions
// Releasing is technically not needed, but in certian cases we avoid
// executing garbage collection, "release" is only a hint tho, special
// objects wont get freed (only for simple data types)
{=============================================================================}
procedure SetBoolDest(var dest:TEpObject; constref value:Boolean);
var GC:TGarbageCollector;
begin
  assert(dest <> nil, 'dest is nil');
  if dest is TBoolObject then
    TBoolObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocBool(value);
  end;
end;

procedure SetCharDest(var dest:TEpObject; constref value:epChar);
var GC:TGarbageCollector;
begin
  assert(dest <> nil, 'dest is nil');
  if dest is TCharObject then
    TCharObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocChar(value);
  end;
end;

procedure SetIntDest(var dest:TEpObject; constref value:epInt);
var GC:TGarbageCollector;
begin
  assert(dest <> nil, 'dest is nil');
  if dest is TIntObject then
    TIntObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocInt(value);
  end;
end;

procedure SetFloatDest(var dest:TEpObject; constref value:Double);
var GC:TGarbageCollector;
begin
  assert(dest <> nil, 'dest is nil');
  if dest is TFloatObject then
    TFloatObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocFloat(value);
  end;
end;

procedure SetStringDest(var dest:TEpObject; constref value:epString);
var GC:TGarbageCollector;
begin
  assert(dest <> nil, 'dest is nil');
  if dest is TStringObject then
    TStringObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocString(value);
  end;
end;


{=============================================================================}
// Base object
{=============================================================================}
function TEpObject.Release: Boolean;
begin
  if (self <> nil) then
  begin
    Result := True;
    self.Destroy;
  end else
    Result := False;
end;

function TEpObject.AsBool: Boolean;    begin raise E_NOT_IMPLEMENTED; end;
function TEpObject.AsChar: epChar;     begin raise E_NOT_IMPLEMENTED; end;
function TEpObject.AsInt: epInt;       begin raise E_NOT_IMPLEMENTED; end;
function TEpObject.AsFloat: Double;    begin raise E_NOT_IMPLEMENTED; end;
function TEpObject.AsString: epString; begin raise E_NOT_IMPLEMENTED; end;

(*
  We are here because `dest` is incorrect type so it has to be re-allocated.
*)
procedure TEpObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  assert(other <> nil, '<other> is nil');
  assert(dest  <> nil, '<dest> is nil');
  TGarbageCollector(dest.GC).Release(dest);
  dest := other.Copy();
end;

procedure TEpObject.INPLACE_ADD(other:TEpObject);  begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.INPLACE_SUB(other:TEpObject);  begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.INPLACE_MUL(other:TEpObject);  begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.INPLACE_DIV(other:TEpObject);  begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.PREINC(var dest:TEpObject);    begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.PREDEC(var dest:TEpObject);    begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.POSTINC(var dest:TEpObject);   begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.POSTDEC(var dest:TEpObject);   begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.UNARY_SUB(var dest:TEpObject);               begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.UNARY_INV(var dest:TEpObject);               begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.ADD(other:TEpObject; var dest:TEpObject);    begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.SUB(other:TEpObject; var dest:TEpObject);    begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.MUL(other:TEpObject; var dest:TEpObject);    begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.IDIV(other:TEpObject; var dest:TEpObject);   begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.FDIV(other:TEpObject; var dest:TEpObject);   begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.MODULO(other:TEpObject; var dest:TEpObject); begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.POW(other:TEpObject; var dest:TEpObject);    begin raise E_NOT_IMPLEMENTED; end;

procedure TEpObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if (other is TNoneObject) then SetBoolDest(dest, False)
  else raise E_NOT_IMPLEMENTED;
end;

procedure TEpObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TNoneObject) then SetBoolDest(dest, True)
  else raise E_NOT_IMPLEMENTED;
end;

procedure TEpObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TNoneObject) then SetBoolDest(dest, False)
  else raise E_NOT_IMPLEMENTED;
end;

procedure TEpObject.GT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TNoneObject) then SetBoolDest(dest, True)
  else raise E_NOT_IMPLEMENTED;
end;

procedure TEpObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TNoneObject) then SetBoolDest(dest, True)
  else raise E_NOT_IMPLEMENTED;
end;

procedure TEpObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TNoneObject) then SetBoolDest(dest, False)
  else raise E_NOT_IMPLEMENTED;
end;

procedure TEpObject.LOGIC_AND(other:TEpObject; var dest:TEpObject); begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);  begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.LOGIC_NOT(var dest:TEpObject);                  begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.BAND(other:TEpObject; var dest:TEpObject);      begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.BOR(other:TEpObject; var dest:TEpObject);       begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.BXOR(other:TEpObject; var dest:TEpObject);      begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.GET_ITEM(index:TEpObject; var dest:TEpObject);  begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.SET_ITEM(index:TEpObject; other:TEpObject);     begin raise E_NOT_IMPLEMENTED; end;


{=============================================================================}
// None
{=============================================================================}
function TNoneObject.Release: Boolean;    begin Result := False; end;
function TNoneObject.Copy: TEpObject;     begin Result := self; end;
function TNoneObject.DeepCopy: TEpObject; begin Result := self; end;

function TNoneObject.AsBool: Boolean;    begin Result := False end;
function TNoneObject.AsString: epString; begin Result := 'None'; end;

procedure TNoneObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  //if (other is TNoneObject) then
  //  other := self
  //else
    inherited;
end;

procedure TNoneObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if other is TNoneObject then SetBoolDest(dest, True)
  else SetBoolDest(dest, False);
end;

procedure TNoneObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if other is TNoneObject then SetBoolDest(dest, False)
  else SetBoolDest(dest, True);
end;

procedure TNoneObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if other is TNoneObject then SetBoolDest(dest, False)
  else SetBoolDest(dest, True);
end;

procedure TNoneObject.GT(other:TEpObject; var dest:TEpObject);
begin
  SetBoolDest(dest, False);
end;

procedure TNoneObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if other is TNoneObject then SetBoolDest(dest, True)
  else SetBoolDest(dest, False);
end;

procedure TNoneObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if other is TNoneObject then SetBoolDest(dest, True)
  else SetBoolDest(dest, False);
end;

procedure TNoneObject.LOGIC_AND(other:TEpObject; var dest:TEpObject); begin SetBoolDest(dest, False and other.AsBool); end;
procedure TNoneObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);  begin SetBoolDest(dest, False  or other.AsBool); end;
procedure TNoneObject.LOGIC_NOT(var dest:TEpObject);                  begin SetBoolDest(dest, True); end;

{=============================================================================}
// Boolean object
{=============================================================================}
constructor TBoolObject.Create(AValue:Boolean);
begin
  self.Value := AValue;
end;

function TBoolObject.Copy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocBool(self.value);
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

function TBoolObject.AsString: epString;
begin
  Result := BoolToStr(self.Value, True);
end;

procedure TBoolObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other is TBoolObject then
    self.value := TBoolObject(other).value
  else
    inherited;
end;

procedure TBoolObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if (other is TBoolObject) then
    SetBoolDest(dest, self.value = TBoolObject(other).value)
  else
    SetBoolDest(dest, self.value = other.AsBool)
end;

procedure TBoolObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TBoolObject) then
    SetBoolDest(dest, self.value <> TBoolObject(other).value)
  else
    SetBoolDest(dest, self.value <> other.AsBool)
end;

procedure TBoolObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TBoolObject) then
    SetBoolDest(dest, Ord(self.value) < Ord(TBoolObject(other).value))
  else if (other is TIntObject) then
    SetBoolDest(dest, Ord(self.value) < TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, Ord(self.value) < TFloatObject(other).value)
  else
    SetBoolDest(dest, Ord(self.value) < other.AsInt)
end;

procedure TBoolObject.GT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TBoolObject) then
    SetBoolDest(dest, Ord(self.value) > Ord(TBoolObject(other).value))
  else if (other is TIntObject) then
    SetBoolDest(dest, Ord(self.value) > TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, Ord(self.value) > TFloatObject(other).value)
  else
    SetBoolDest(dest, Ord(self.value) > other.AsInt)
end;

procedure TBoolObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TBoolObject) then
    SetBoolDest(dest, Ord(self.value) <= Ord(TBoolObject(other).value))
  else if (other is TIntObject) then
    SetBoolDest(dest, Ord(self.value) <= TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, Ord(self.value) <= TFloatObject(other).value)
  else
    SetBoolDest(dest, Ord(self.value) <= other.AsInt)
end;

procedure TBoolObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TBoolObject) then
    SetBoolDest(dest, Ord(self.value) >= Ord(TBoolObject(other).value))
  else if (other is TIntObject) then
    SetBoolDest(dest, Ord(self.value) >= TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, Ord(self.value) >= TFloatObject(other).value)
  else
    SetBoolDest(dest, Ord(self.value) >= other.AsInt)
end;

procedure TBoolObject.LOGIC_AND(other:TEpObject; var dest:TEpObject);
begin
  if (other is TBoolObject) then
    SetBoolDest(dest, self.value and TBoolObject(other).value)
  else
    SetBoolDest(dest, self.value and other.AsBool);
end;

procedure TBoolObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);
begin
  if (other is TBoolObject) then
    SetBoolDest(dest, self.value or TBoolObject(other).value)
  else
    SetBoolDest(dest, self.value or other.AsBool);
end;

procedure TBoolObject.LOGIC_NOT(var dest:TEpObject);
begin
  SetBoolDest(dest, not self.value);
end;


{=============================================================================}
// Char object
{=============================================================================}
constructor TCharObject.Create(AValue:epChar);
begin
  self.Value := AValue;
end;

function TCharObject.Copy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocChar(self.value);
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

function TCharObject.AsString: epString;
begin
  Result := self.Value;
end;

procedure TCharObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other is TCharObject then
    self.value := TCharObject(other).value
  else
    inherited;
end;

procedure TCharObject.ADD(other:TEpObject; var dest:TEpObject);
begin
  if (other is TCharObject) then
    SetStringDest(dest, self.value + TCharObject(other).value)
  else if (other is TStringObject) then
    SetStringDest(dest, self.value + TStringObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) = other.AsInt)
end;

procedure TCharObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if (other is TCharObject) then
    SetBoolDest(dest, self.value = TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) = other.AsInt)
end;

procedure TCharObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TCharObject) then
    SetBoolDest(dest, self.value <> TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) <> other.AsInt)
end;

procedure TCharObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TCharObject) then
    SetBoolDest(dest, self.value < TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) < other.AsInt)
end;

procedure TCharObject.GT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TCharObject) then
    SetBoolDest(dest, self.value > TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) > other.AsInt)
end;

procedure TCharObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TCharObject) then
    SetBoolDest(dest, self.value <= TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) <= other.AsInt)
end;

procedure TCharObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TCharObject) then
    SetBoolDest(dest, self.value >= TCharObject(other).value)
  else
    SetBoolDest(dest, ord(self.value) >= other.AsInt)
end;

procedure TCharObject.LOGIC_AND(other:TEpObject; var dest:TEpObject);
begin
  if (other is TCharObject) then
    SetBoolDest(dest, (self.value <> #0) and (TCharObject(other).value <> #0))
  else if (other is TBoolObject) then
    SetBoolDest(dest, (self.value <> #0) and (TBoolObject(other).value))
  else
    SetBoolDest(dest, (self.value <> #0) and other.AsBool);
end;

procedure TCharObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);
begin
  if (other is TCharObject) then
    SetBoolDest(dest, (self.value <> #0) or (TCharObject(other).value <> #0))
  else if (other is TBoolObject) then
    SetBoolDest(dest, (self.value <> #0) or (TBoolObject(other).value))
  else
    SetBoolDest(dest, (self.value <> #0) or other.AsBool);
end;

procedure TCharObject.LOGIC_NOT(var dest:TEpObject);
begin
  SetBoolDest(dest, self.value = #0);
end;



{=============================================================================}
// Integer object
{=============================================================================}
constructor TIntObject.Create(AValue:epInt);
begin
  self.Value := AValue;
end;

function TIntObject.Copy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocInt(self.value);
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

function TIntObject.AsString: epString;
begin
  Result := IntToStr(self.Value);
end;

procedure TIntObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other is TIntObject then
    self.value := TIntObject(other).value
  else
    inherited;
end;

procedure TIntObject.INPLACE_ADD(other:TEpObject);
begin
  if (other is TIntObject) then
    self.value += TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TIntObject.INPLACE_SUB(other:TEpObject);
begin
  if (other is TIntObject) then
    self.value -= TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TIntObject.INPLACE_MUL(other:TEpObject);
begin
  if (other is TIntObject) then
    self.value := self.value * TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TIntObject.INPLACE_DIV(other:TEpObject);
begin
  if (other is TIntObject) then
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
  if (other is TIntObject) then
    SetIntDest(dest, self.value + TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value + TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.SUB(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetIntDest(dest, self.value - TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value - TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.MUL(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetIntDest(dest, self.value * TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value * TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.IDIV(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetIntDest(dest, self.value div TIntObject(other).value)
  else
    inherited;
end;

procedure TIntObject.FDIV(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetFloatDest(dest, self.value / TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value / TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.MODULO(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetIntDest(dest, utils.modulo(self.value, TIntObject(other).value))
  else if (other is TFloatObject) then
    SetFloatDest(dest, utils.modulo(self.value, TFloatObject(other).value))
  else
    inherited;
end;

procedure TIntObject.POW(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetIntDest(dest, self.value * TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value * TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value = TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value = TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value <> TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value <> TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value < TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value < TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.GT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value > TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value > TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value <= TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value <= TFloatObject(other).value)
  else
    inherited;
end;

procedure TIntObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value >= TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value >= TFloatObject(other).value)
  else
    inherited;
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
  if (other is TIntObject) then
    SetIntDest(dest, self.value and TIntObject(other).value)
  else
    inherited;
end;

procedure TIntObject.BOR(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetIntDest(dest, self.value or TIntObject(other).value)
  else
    inherited;
end;

procedure TIntObject.BXOR(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetIntDest(dest, self.value xor TIntObject(other).value)
  else
    inherited;
end;


{=============================================================================}
// Float object
{=============================================================================}
constructor TFloatObject.Create(AValue:Double);
begin
  self.Value := AValue;
end;

function TFloatObject.Copy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocFloat(self.value);
end;

function TFloatObject.DeepCopy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocFloat(self.value);
end;

function TFloatObject.AsBool: Boolean;
begin
  Result := self.Value <> 0;
end;

function TFloatObject.AsInt: epInt;
begin
  Result := Trunc(self.Value);
end;

function TFloatObject.AsString: epString;
begin
  Result := FloatToStr(self.Value);
end;

procedure TFloatObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other is TFloatObject then
    self.value := TFloatObject(other).value
  else
    inherited;
end;

procedure TFloatObject.INPLACE_ADD(other:TEpObject);
begin
  if (other is TFloatObject) then
    self.value += TFloatObject(other).value
  else if (other is TIntObject) then
    self.value += TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TFloatObject.INPLACE_SUB(other:TEpObject);
begin
  if (other is TFloatObject) then
    self.value -= TFloatObject(other).value
  else if (other is TIntObject) then
    self.value -= TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TFloatObject.INPLACE_MUL(other:TEpObject);
begin
  if (other is TFloatObject) then
    self.value := self.value * TFloatObject(other).value
  else if (other is TIntObject) then
    self.value := self.value * TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TFloatObject.INPLACE_DIV(other:TEpObject);
begin
  if (other is TFloatObject) then
    self.value := self.value / TFloatObject(other).value
  else if (other is TIntObject) then
    self.value := self.value / TIntObject(other).value
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TFloatObject.PREINC(var dest:TEpObject);
begin
  self.value += 1.0;
  SetFloatDest(dest, self.value);
end;

procedure TFloatObject.PREDEC(var dest:TEpObject);
begin
  self.value -= 1.0;
  SetFloatDest(dest, self.value);
end;

procedure TFloatObject.POSTINC(var dest:TEpObject);
begin
  SetFloatDest(dest, self.value);
  self.value += 1.0;
end;

procedure TFloatObject.POSTDEC(var dest:TEpObject);
begin
  SetFloatDest(dest, self.value);
  self.value -= 1.0;
end;

procedure TFloatObject.UNARY_SUB(var dest:TEpObject);
begin
  SetFloatDest(dest, -self.value);
end;

procedure TFloatObject.ADD(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetFloatDest(dest, self.value + TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value + TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.SUB(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetFloatDest(dest, self.value - TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value - TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.MUL(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetFloatDest(dest, self.value * TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value * TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.FDIV(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetFloatDest(dest, self.value / TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value / TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.MODULO(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetFloatDest(dest, utils.modulo(self.value, TIntObject(other).value))
  else if (other is TFloatObject) then
    SetFloatDest(dest, utils.modulo(self.value, TFloatObject(other).value))
  else
    inherited;
end;

procedure TFloatObject.POW(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetFloatDest(dest, self.value * TIntObject(other).value)
  else if (other is TFloatObject) then
    SetFloatDest(dest, self.value * TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value = TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value = TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value <> TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value <> TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value < TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value < TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.GT(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value > TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value > TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value <= TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value <= TFloatObject(other).value)
  else
    inherited;
end;

procedure TFloatObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if (other is TIntObject) then
    SetBoolDest(dest, self.value >= TIntObject(other).value)
  else if (other is TFloatObject) then
    SetBoolDest(dest, self.value >= TFloatObject(other).value)
  else
    inherited;
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


{=============================================================================}
// String object
{=============================================================================}
constructor TStringObject.Create(AValue:epString);
begin
  self.Value := AValue;
end;

function TStringObject.Copy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocString(self.value);
end;

function TStringObject.DeepCopy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocString(self.value);
end;

function TStringObject.AsChar: epChar;
begin
  if Length(self.value) = 1 then
    Result := self.Value[1]
  else
    inherited;
end;

function TStringObject.AsString: epString;
begin
  Result := self.value;
end;

function TStringObject.AsBool: Boolean;
begin
  Result := Length(self.value) > 0;
end;

function TStringObject.AsInt: epInt; begin raise E_NOT_IMPLEMENTED; end;

procedure TStringObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other is TStringObject then
    self.value := TStringObject(other).value
  else
    inherited;
end;

procedure TStringObject.INPLACE_ADD(other:TEpObject);
var l:Int32;
begin
  L := Length(self.value);
  if (other is TStringObject) then
  begin
    if (Length(TStringObject(other).value) > 0) and
       (Length(self.value) > 0) then
    begin
      SetLength(self.value, L+Length(TStringObject(other).value));
      Move(
        TStringObject(other).value[1],
        self.value[1+L],
        Length(TStringObject(other).value)*SizeOf(epChar)
      );
    end
    else
      self.value += TStringObject(other).value
  end
  else if (other is TCharObject) then
  begin
    SetLength(self.value, L+1);
    self.value[1+L] := TCharObject(other).value
  end
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TStringObject.ADD(other:TEpObject; var dest:TEpObject);
begin
  if (other is TStringObject) then
    SetStringDest(dest, self.value + TStringObject(other).value)
  else if (other is TCharObject) then
    SetStringDest(dest, self.value + TCharObject(other).value)
  else
    inherited;
end;

procedure TStringObject.GET_ITEM(index:TEpObject; var dest:TEpObject);
begin
  if index is TIntObject then
    SetCharDest(dest, self.value[1+TIntObject(index).value])
  else
    SetCharDest(dest, self.value[1+index.AsInt]);
end;

procedure TStringObject.SET_ITEM(index:TEpObject; other:TEpObject);
begin
  self.value[1+index.AsInt] := TCharObject(other).AsChar;
end;


{=============================================================================}
// TListObject
{=============================================================================}
constructor TListObject.Create(AValue:TObjectArray);
begin
  self.Value := AValue;
end;

function TListObject.Release: Boolean;
begin
  Result := False;
end;

function TListObject.Copy: TEpObject;
begin
  Result := self; //refobject, ya know
end;

function TListObject.DeepCopy: TEpObject;
var
  i:Int32;
  tmp:TObjectArray;
begin
  SetLength(tmp, Length(self.value));
  for i:=0 to High(self.value) do
    tmp[i] := self.value[i].DeepCopy();

  Result := TGarbageCollector(GC).AllocList(tmp);
end;

function TListObject.AsString: epString;
var i:Int32;
begin
  Result := '[';
  for i:=0 to High(self.value) do
  begin
    if (Pointer(self.value[i]) = Pointer(self)) then
      Result += '[...]'
    else
      Result += self.value[i].AsString;
    if i <> High(self.value) then Result += ',';
  end;
  Result += ']';
end;

function TListObject.AsBool: Boolean;
begin
  Result := Length(self.value) > 0;
end;

procedure TListObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  //if other is TListObject then
  //  self.value := TListObject(other).value
  //else
    inherited;
end;

procedure TListObject.INPLACE_ADD(other:TEpObject);
var
  l:Int32;
begin
  l := Length(self.value);
  SetLength(self.value, l+1);
  self.value[l] := other.Copy();
end;


procedure TListObject.GET_ITEM(index:TEpObject; var dest:TEpObject);
var
  idx,real_idx,len:SizeInt;
begin
  real_idx := index.AsInt;

  len := Length(self.value);
  if real_idx < 0 then
    idx := len-real_idx
  else
    idx := real_idx;
  if (idx < 0) or (idx >= len) then
    raise RuntimeError.CreateFmt(eIndexOutOfRange, [real_idx, len]);

  dest.ASGN(self.value[idx], dest);
end;

procedure TListObject.SET_ITEM(index:TEpObject; other:TEpObject);
var
  idx,real_idx,len:SizeInt;
begin
  real_idx := index.AsInt;

  len := Length(self.value);
  if real_idx < 0 then
    idx := len-real_idx
  else
    idx := real_idx;
  if (idx < 0) or (idx >= len) then
    raise RuntimeError.CreateFmt(eIndexOutOfRange, [real_idx, len]);

  self.value[idx].ASGN(other, self.value[idx]);
end;


{=============================================================================}
// Function object
{=============================================================================}
constructor TFuncObject.Create(AName:epString; ACodePos:Int32; AVarRange:TIntRange);
begin
  self.Name := AName;
  self.CodePos := ACodePos;
  self.VarRange := AVarRange;
end;

function TFuncObject.Copy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocFunc(self.Name, self.CodePos, self.VarRange);
end;

function TFuncObject.DeepCopy: TEpObject;
begin
  Result := TGarbageCollector(GC).AllocFunc(self.Name, self.CodePos, self.VarRange);
end;

function TFuncObject.AsString: epString;
begin
  Result := Lowercase('Func('+self.Name+') @ '+IntToHex(self.CodePos, 8));
end;

procedure TFuncObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if other is TFuncObject then
  begin
    self.Name     := TFuncObject(other).Name;
    self.CodePos  := TFuncObject(other).CodePos;
    self.VarRange := TFuncObject(other).VarRange;
  end
  else
    inherited;
end;

end.
