{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Runtime datatype representations
}
unit xpr.objbase;
{$I express.inc}

interface

{$I objh.inc}

uses
  SysUtils,
  xpr.express;


type
  PEpObject = ^TEpObject;
  TEpObject = class
    Handle: UInt32;
    GC: Pointer;

    function Release: Boolean; virtual;
    function Copy(gcGen:Byte=0): TEpObject; virtual; abstract;
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

    procedure GET_ITEM(constref index:TEpObject; var dest:TEpObject); virtual;
    procedure SET_ITEM(constref index:TEpObject; constref other:TEpObject); virtual;
  end;
  TObjectArray = array of TEpObject;
  
  
  TFuncObject = class(TEpObject)
    Name: epString;
    CodePos: Int32;
    VarRange: TIntRange;

    constructor Create(AName:epString; ACodePos:Int32; AVarRange:TIntRange);
    function Copy(gcGen:Byte=0): TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    procedure ASGN(other:TEpObject; var dest:TEpObject); override;
  end;


implementation

uses
  xpr.utils,
  xpr.errors,
  xpr.mmgr;


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

procedure TEpObject.INPLACE_ADD(other:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.INPLACE_SUB(other:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.INPLACE_MUL(other:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.INPLACE_DIV(other:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.PREINC(var dest:TEpObject);   begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.PREDEC(var dest:TEpObject);   begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.POSTINC(var dest:TEpObject);  begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.POSTDEC(var dest:TEpObject);  begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.UNARY_SUB(var dest:TEpObject);              begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.UNARY_INV(var dest:TEpObject);              begin raise E_NOT_IMPLEMENTED; end;
procedure TEpObject.ADD(other:TEpObject; var dest:TEpObject);   begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.SUB(other:TEpObject; var dest:TEpObject);   begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.MUL(other:TEpObject; var dest:TEpObject);   begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.IDIV(other:TEpObject; var dest:TEpObject);  begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.FDIV(other:TEpObject; var dest:TEpObject);  begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.MODULO(other:TEpObject; var dest:TEpObject);begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.POW(other:TEpObject; var dest:TEpObject);   begin raise E_NOT_COMPATIBLE1; end;

procedure TEpObject.EQ(other:TEpObject; var dest:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.NE(other:TEpObject; var dest:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.LT(other:TEpObject; var dest:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.GT(other:TEpObject; var dest:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.GE(other:TEpObject; var dest:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.LE(other:TEpObject; var dest:TEpObject); begin raise E_NOT_COMPATIBLE1; end;

procedure TEpObject.LOGIC_AND(other:TEpObject; var dest:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);  begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.LOGIC_NOT(var dest:TEpObject);                  begin raise E_NOT_IMPLEMENTED; end;

procedure TEpObject.BAND(other:TEpObject; var dest:TEpObject); begin raise E_NOT_COMPATIBLE1; end;
procedure TEpObject.BOR(other:TEpObject; var dest:TEpObject);  begin raise E_NOT_COMPATIBLE1; end;

procedure TEpObject.BXOR(other:TEpObject; var dest:TEpObject); begin raise E_NOT_COMPATIBLE1; end;

procedure TEpObject.GET_ITEM(constref index:TEpObject; var dest:TEpObject);
begin
  raise E_NOT_IMPLEMENTED;
end;

procedure TEpObject.SET_ITEM(constref index:TEpObject; constref other:TEpObject); 
begin
  raise E_NOT_IMPLEMENTED;
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

function TFuncObject.Copy(gcGen:Byte=0): TEpObject;
begin
  Result := TGarbageCollector(GC).AllocFunc(self.Name, self.CodePos, self.VarRange, gcGen);
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
  if other.ClassType = TFuncObject then
  begin
    self.Name     := TFuncObject(other).Name;
    self.CodePos  := TFuncObject(other).CodePos;
    self.VarRange := TFuncObject(other).VarRange;
  end
  else
    inherited;
end;


end.
