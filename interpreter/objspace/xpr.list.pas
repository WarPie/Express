{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Runtime datatype representations
}
unit xpr.list;
{$I express.inc}

interface

{$I objh.inc}

uses
  SysUtils,
  xpr.express, 
  xpr.objbase;


type
  TListObject = class(TEpObject)
    value: TObjectArray;

    constructor Create(AValue:TObjectArray);
    function Release: Boolean; override;
    function Copy(gcGen:Byte=0): TEpObject; override;
    function DeepCopy: TEpObject; override;

    function AsString: epString; override;
    function AsBool: Boolean; override;

    //temporary
    procedure INPLACE_ADD(other:TEpObject); override;

    //
    procedure ASGN(other:TEpObject; var dest:TEpObject); override;
    procedure GET_ITEM(constref index:TEpObject; var dest:TEpObject); override;
    procedure SET_ITEM(constref index:TEpObject; constref other:TEpObject); override;
  end;


implementation

uses
  xpr.utils, 
  xpr.errors, 
  xpr.mmgr,
  xpr.bool,
  xpr.int;

constructor TListObject.Create(AValue:TObjectArray);
begin
  self.Value := AValue;
end;

function TListObject.Release: Boolean;
begin
  Result := False;
end;

function TListObject.Copy(gcGen:Byte=0): TEpObject;
begin
  Result := self; //meh
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


procedure TListObject.GET_ITEM(constref index:TEpObject; var dest:TEpObject);
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

procedure TListObject.SET_ITEM(constref index:TEpObject; constref other:TEpObject);
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


end.
