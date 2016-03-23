{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  still thinking..
}
unit mmgr;
{$I express.inc}

interface

uses
  SysUtils,
  express,
  dictionary,
  datatypes;

const
  DEFAULT_TEMP_SIZE = 4096;

type
  TGCGeneration = specialize TDictionary<PtrUInt, Boolean>;
  
  TGarbageCollector = class
    Threshold: Array [0..2] of Int32;
    CountDown: Array [0..2] of Int32;
    Gen:       Array [0..2] of TGCGeneration;
    
    temp:TObjectArray;
    tempPos: Int32;

    constructor Create();
    destructor Destroy(); override;

    procedure Mark(genId:Int32; root:TObjectArray; top:Int32);
    procedure Sweep(genId:Int32);
    
    (* alloc and free *)
    function AllocNone(): TEpObject; //inline;
    function AllocBool(constref v:Boolean): TEpObject; //inline;
    function AllocChar(constref v:epChar): TEpObject; //inline;
    function AllocInt(constref v:epInt): TEpObject; //inline;
    function AllocFloat(constref v:Double): TEpObject; //inline;
    function AllocString(constref v:epString): TEpObject; //inline;
    function AllocList(constref v:TObjectArray): TEpObject; //inline;
    function AllocFunc(n:epString; p:Int32; r:TIntRange): TEpObject; //inline;
    procedure Release(var T:TEpObject); inline;
  end;


implementation

uses utils;

constructor TGarbageCollector.Create();
begin
  THRESHOLD[0] := 9000; //`n` allocations before G0 checked (allocations - deallocations)
  THRESHOLD[1] := 30;   //`n` times of G0 before G1 checked (survivors are moved to G2)
  THRESHOLD[2] := 30;   //`n` times of G1 before G2 checked (objects stay here)

  Gen[0] := TGCGeneration.Create(@HashPointer);
  Gen[0].SetSize(8000);

  Gen[1] := TGCGeneration.Create(@HashPointer);
  Gen[1].SetSize(4000);
  
  Gen[2] := TGCGeneration.Create(@HashPointer);
  Gen[2].SetSize(1000);
  
  CountDown[0] := THRESHOLD[0];
  CountDown[1] := THRESHOLD[1];
  CountDown[2] := THRESHOLD[2];
  
  tempPos := -1;
  SetLength(temp, DEFAULT_TEMP_SIZE);
end;

destructor TGarbageCollector.Destroy();
var g,i,j:Int32;
begin
  for g:=0 to High(Gen) do
  begin
    for i:=0 to High(Gen[g].Items) do
      for j:=0 to High(Gen[g].Items[i]) do
        TEpObject(Gen[g].Items[i][j].key).Destroy();
    Gen[g].Destroy;
  end;

  inherited;
end;

procedure TGarbageCollector.Mark(genId:Int32; root:TObjectArray; top:Int32);
var
  i:Int32;
begin
  for i:=0 to top do
  begin
    if Gen[genId].Remove(PtrUInt(root[i])) then
    begin
      if tempPos = High(temp) then
        SetLength(temp, Length(temp) * 2);
      Inc(tempPos);
      temp[tempPos] := root[i];
    end;

    // if current is a container object (object with children), it may contain
    // items in any other generation, so it should always be checked, this slows
    // down the GC a bit (notably so if it's a large list)
    if (root[i] is TListObject) then
      Mark(genId, TListObject(root[i]).value, High(TListObject(root[i]).value));
  end;
end;

procedure TGarbageCollector.Sweep(genId:Int32);
var
  curr:TGCGeneration;
  i,j:Int32;
begin
  curr := Gen[genId];
  for i:=0 to High(curr.Items) do
  begin
    for j:=0 to High(curr.Items[i]) do
      TEpObject(curr.Items[i][j].key).Destroy();
    curr.Size := curr.Size - Length(curr.Items[i]);
    SetLength(curr.Items[i], 0);
  end;

  if genId < High(gen) then curr := Gen[genId+1];
  for i:=0 to tempPos do curr[PtrUInt(temp[i])] := True;

  CountDown[genId] := THRESHOLD[genId];
  if genId < High(gen) then
    Dec(CountDown[genId+1]);

  tempPos := -1;
  SetLength(temp, DEFAULT_TEMP_SIZE);
end;


(*
  allocation routines
*)
function TGarbageCollector.AllocNone(): TEpObject;
begin
  Result := TNoneObject.Create();
  Result.gc := Pointer(self);
  Gen[0].Add(PtrUInt(Result), True);
  Dec(CountDown[0]);
end;

function TGarbageCollector.AllocBool(constref v:Boolean): TEpObject;
begin
  Result := TBoolObject.Create(v);
  Result.gc := Pointer(self);
  Gen[0].Add(PtrUInt(Result), True);
  Dec(CountDown[0]);
end;

function TGarbageCollector.AllocChar(constref v:epChar): TEpObject;
begin
  Result := TCharObject.Create(v);
  Result.gc := Pointer(self);
  Gen[0].Add(PtrUInt(Result), True);
  Dec(CountDown[0]);
end;

function TGarbageCollector.AllocInt(constref v:epInt): TEpObject;
begin
  Result := TIntObject.Create(v);
  Result.gc := Pointer(self);
  Gen[0].Add(PtrUInt(Result), True);
  Dec(CountDown[0]);
end;

function TGarbageCollector.AllocFloat(constref v:Double): TEpObject;
begin
  Result := TFloatObject.Create(v);
  Result.gc := Pointer(self);
  Gen[0].Add(PtrUInt(Result), True);
  Dec(CountDown[0]);
end;

function TGarbageCollector.AllocString(constref v:epString): TEpObject;
begin
  Result := TStringObject.Create(v);
  Result.gc := Pointer(self);
  Gen[0].Add(PtrUInt(Result), True);
  Dec(CountDown[0]);
end;

function TGarbageCollector.AllocList(constref v:TObjectArray): TEpObject;
begin
  Result := TListObject.Create(v);
  Result.gc := Pointer(self);
  Gen[0].Add(PtrUInt(Result), True);
  Dec(CountDown[0]);
end;

function TGarbageCollector.AllocFunc(n:epString; p:Int32; r:TIntRange): TEpObject;
begin
  Result := TFuncObject.Create(n,p,r);
  Result.gc := Pointer(self);
  Gen[0].Add(PtrUInt(Result), True);
  Dec(CountDown[0]);
end;

procedure TGarbageCollector.Release(var T:TEpObject);
begin
  if (T = nil) or (not T.Release) then Exit;
  if Gen[0].Remove(PtrUInt(T)) or Gen[1].Remove(PtrUInt(T)) or Gen[2].Remove(PtrUInt(T)) then
    Inc(CountDown[0]);

  T := nil;
end;


end.
