{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Runtime datatype representations
}
unit xpr.str;
{$I express.inc}

interface

{$I objh.inc}

uses
  SysUtils,
  xpr.express, 
  xpr.objbase;


type
  TStringObject = class(TEpObject)
    value: epString;
    
    constructor Create(AValue:epString);
    
    function Copy(gcGen:Byte=0): TEpObject; override;
    function DeepCopy: TEpObject; override;
    
    function AsChar: epChar; override;
    function AsString: epString; override;
    function AsBool: Boolean; override;
    function AsInt: epInt; override;
    
    procedure ASGN(other:TEpObject; var dest:TEpObject); override;
    procedure INPLACE_ADD(other:TEpObject); override;
    procedure ADD(other:TEpObject; var dest:TEpObject); override;
    procedure GET_ITEM(constref index:TEpObject; var dest:TEpObject); override;
    procedure SET_ITEM(constref index:TEpObject; constref other:TEpObject); override;
  end;

procedure SetStringDest(var dest:TEpObject; constref value:epString); inline;  

implementation

uses
  xpr.utils, 
  xpr.errors, 
  xpr.mmgr,
  xpr.bool,
  xpr.int,  
  xpr.char;

procedure SetStringDest(var dest:TEpObject; constref value:epString);
var GC:TGarbageCollector;
begin
  assert(dest <> nil, 'dest is nil');
  if dest.ClassType = TStringObject then
    TStringObject(dest).value := value
  else
  begin
    GC := TGarbageCollector(dest.gc);
    GC.Release(dest);
    dest := GC.AllocString(value);
  end;
end;


constructor TStringObject.Create(AValue:epString);
begin
  self.Value := AValue;
end;

function TStringObject.Copy(gcGen:Byte=0): TEpObject;
begin
  Result := TGarbageCollector(GC).AllocString(self.value, gcGen);
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
  if other.ClassType = TStringObject then
    self.value := TStringObject(other).value
  else
    inherited;
end;

procedure TStringObject.INPLACE_ADD(other:TEpObject);
var l:Int32;
begin
  L := Length(self.value);
  if (other.ClassType = TStringObject) then
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
  else if (other.ClassType = TCharObject) then
  begin
    SetLength(self.value, L+1);
    self.value[1+L] := TCharObject(other).value
  end
  else
    raise E_NOT_IMPLEMENTED;
end;

procedure TStringObject.ADD(other:TEpObject; var dest:TEpObject);
begin
  if (other.ClassType = TStringObject) then
    SetStringDest(dest, self.value + TStringObject(other).value)
  else if (other.ClassType = TCharObject) then
    SetStringDest(dest, self.value + TCharObject(other).value)
  else
    inherited;
end;

procedure TStringObject.GET_ITEM(constref index:TEpObject; var dest:TEpObject);
begin
  if index.ClassType = TIntObject then
    SetCharDest(dest, self.value[1+TIntObject(index).value])
  else
    SetCharDest(dest, self.value[1+index.AsInt]);
end;

procedure TStringObject.SET_ITEM(constref index:TEpObject; constref other:TEpObject);
begin
  self.value[1+index.AsInt] := TCharObject(other).AsChar;
end;

end.
