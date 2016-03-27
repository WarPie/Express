unit xpr.mmgr;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  still thinking..
}
{$I express.inc}

interface

uses
  SysUtils,
  xpr.express,
  xpr.dictionary,
  {$I objects.inc};

const
  //base array size of a temp storage used when moving objects between generations.
  DEFAULT_TEMP_SIZE = $1000; //4K

  POOL_MIN_SIZE   = $10000; //65k
  POOL_MULTIPLIER = 2;


type
  TGCLookup = specialize TDictionary<PtrUInt, Boolean>;

  PMemoryPool = ^TMemoryPool;
  TMemoryPool = record
    Pool: array of TEpObject;
    PoolPos: Int32;
    procedure Init;
    procedure Add(T: TEpObject); inline;
    procedure Unset(H:UInt32); inline;
    procedure Reset; inline;
    function Contains(T: TEpObject): Boolean; inline;
  end;

  TGarbageCollector = class
    Threshold: Array [0..2] of Int32;
    CountDown: Array [0..2] of Int32;
    Gen:       Array [0..2] of TMemoryPool;
    Lookup, Marked: TGCLookup;

    Temp:TObjectArray;
    TempPos: Int32;

    constructor Create();
    destructor Destroy(); override;
    function ShouldEnter: Boolean; inline;

    procedure PrepareCollection(genId:Int8); inline;
    procedure Mark(genId:Int8; root:TObjectArray; top:Int32);
    procedure Sweep(genId:Int8);
    
    (* alloc and free *)
    function AllocNone(gcGen:Int8=0): TEpObject; //inline;
    function AllocBool(constref v:Boolean; gcGen:Int8=0): TEpObject; //inline;
    function AllocChar(constref v:epChar; gcGen:Int8=0): TEpObject; //inline;
    function AllocInt(constref v:epInt; gcGen:Int8=0): TEpObject; //inline;
    function AllocFloat(constref v:Double; gcGen:Int8=0): TEpObject; //inline;
    function AllocString(constref v:epString; gcGen:Int8=0): TEpObject; //inline;
    function AllocList(constref v:TObjectArray; gcGen:Int8=0): TEpObject; //inline;
    function AllocFunc(n:epString; p:Int32; r:TIntRange; gcGen:Int8=0): TEpObject; //inline;
    procedure Release(var T:TEpObject); //inline;
  end;

implementation

uses xpr.utils;


procedure TMemoryPool.Init;
begin
  SetLength(Pool, POOL_MIN_SIZE);
  PoolPos := -1;
end;

procedure TMemoryPool.Add(T: TEpObject);
begin
  Assert(T <> nil);

  if PoolPos = High(Pool) then
    SetLength(Pool, Length(Pool) * POOL_MULTIPLIER);

  Inc(PoolPos);
  Pool[PoolPos] := T;
  T.Handle := PoolPos;
end;

procedure TMemoryPool.Unset(H:UInt32);
begin
  Pool[H] := Pool[PoolPos];
  Pool[H].Handle := H;
  Dec(PoolPos);
end;

procedure TMemoryPool.Reset;
begin
  SetLength(Pool, POOL_MIN_SIZE);
  PoolPos := -1;
end;

function TMemoryPool.Contains(T: TEpObject): Boolean;
begin
  Result := (T.Handle <= PoolPos) and (PtrUInt(Pool[T.Handle]) = PtrUInt(T));
end;



constructor TGarbageCollector.Create();
var i:Int32;
begin
  THRESHOLD[0] := 8000;  //items must be allocated before G0 is checked
  THRESHOLD[1] := 10;    //`n` times of G0 before G1 is checked (survivors are moved to G2)
  THRESHOLD[2] := 10;    //`n` times of G1 before G2 is checked (objects stay here)

  for i:=0 to 2 do
    Gen[i].Init;

  CountDown[0] := THRESHOLD[0];
  CountDown[1] := THRESHOLD[1];
  CountDown[2] := THRESHOLD[2];

  Lookup := TGCLookup.Create(@HashPointer, POOL_MIN_SIZE-1);
  Marked := TGCLookup.Create(@HashPointer);

  tempPos := -1;
  SetLength(temp, DEFAULT_TEMP_SIZE);
end;

destructor TGarbageCollector.Destroy();
var g,i,j:Int32;
begin
  for g:=0 to High(Gen) do
  begin
    for i:=0 to Gen[g].PoolPos do
      if (Gen[g].Pool[i] <> nil) then
        Gen[g].Pool[i].Destroy();
    Gen[g].Reset;
  end;
  inherited;
end;

function TGarbageCollector.ShouldEnter: Boolean;
begin
  Result := CountDown[0] <= 0;
end;

procedure TGarbageCollector.PrepareCollection(genId:Int8);
var i:Int32;
begin
  Lookup.Clear;
  if POOL_MIN_SIZE > Gen[genId].PoolPos then
    Lookup.SetSize(POOL_MIN_SIZE)
  else
    Lookup.SetSize(Gen[genId].PoolPos);

  for i:=0 to gen[genId].PoolPos do
    if (gen[genId].Pool[i] <> nil) then
      Lookup[PtrUInt(gen[genId].Pool[i])] := True;
end;

(*
  Fix me: Infinite recursion.
   Properly mark objects that has been seen once already.
   Could use a dictionary for that as well.
*)
procedure TGarbageCollector.Mark(genId:Int8; root:TObjectArray; top:Int32);
var
  i: Int32;
begin
  for i:=0 to top do
  begin
    if Lookup.Remove( PtrUInt(root[i]) ) then
    begin
      if tempPos = High(temp) then
        SetLength(temp, Length(temp) * 2);
      Inc(tempPos);
      temp[tempPos] := root[i];
    end;

    // if current is a container object (object with children), it may contain
    // items in any other generation, so it should be checked even tho the list
    // itself is not in our current generation.
    if (root[i] is TListObject) and (not marked.Add(PtrUInt(root[i]), True)) then
      Mark(genId, TListObject(root[i]).value, High(TListObject(root[i]).value));
  end;
end;

procedure TGarbageCollector.Sweep(genId:Int8);
var
  nextGen,i,j: Int32;
begin
  for i:=0 to High(Lookup.Items) do
    for j:=0 to High(Lookup.Items[i]) do
      TEpObject(Lookup.Items[i][j].key).Destroy();

  Gen[genId].Reset;
  if genId < High(gen) then
  begin
    nextGen := genId+1;
    Dec(CountDown[nextGen]);
  end else
    nextGen := genId;

  for i:=0 to tempPos do
    Gen[nextGen].Add(temp[i]);

  CountDown[genId] := THRESHOLD[genId];
  tempPos := -1;
  SetLength(temp, DEFAULT_TEMP_SIZE);

  marked.Clear;
end;


(*
  allocation routines
*)
function TGarbageCollector.AllocNone(gcGen:Int8=0): TEpObject;
begin
  if gcGen < 0 then gcGen := 0;
  Result := TNoneObject.Create();
  Result.gc := Pointer(self);
  Gen[gcGen].Add(Result);
  if gcGen = 0 then Dec(CountDown[0]);
end;

function TGarbageCollector.AllocBool(constref v:Boolean; gcGen:Int8=0): TEpObject;
begin
  if gcGen < 0 then gcGen := 0;
  Result := TBoolObject.Create(v);
  Result.gc := Pointer(self);
  Gen[gcGen].Add(Result);
  if gcGen = 0 then Dec(CountDown[0]);
end;

function TGarbageCollector.AllocChar(constref v:epChar; gcGen:Int8=0): TEpObject;
begin
  if gcGen < 0 then gcGen := 0;
  Result := TCharObject.Create(v);
  Result.gc := Pointer(self);
  Gen[gcGen].Add(Result);
  if gcGen = 0 then Dec(CountDown[0]);
end;

function TGarbageCollector.AllocInt(constref v:epInt; gcGen:Int8=0): TEpObject;
begin
  if gcGen < 0 then gcGen := 0;
  Result := TIntObject.Create(v);
  Result.gc := Pointer(self);
  Gen[gcGen].Add(Result);
  if gcGen = 0 then Dec(CountDown[0]);
end;

function TGarbageCollector.AllocFloat(constref v:Double; gcGen:Int8=0): TEpObject;
begin
  if gcGen < 0 then gcGen := 0;
  Result := TFloatObject.Create(v);
  Result.gc := Pointer(self);
  Gen[gcGen].Add(Result);
  if gcGen = 0 then Dec(CountDown[0]);
end;

function TGarbageCollector.AllocString(constref v:epString; gcGen:Int8=0): TEpObject;
begin
  if gcGen < 0 then gcGen := 0;
  Result := TStringObject.Create(v);
  Result.gc := Pointer(self);
  Gen[gcGen].Add(Result);
  if gcGen = 0 then Dec(CountDown[0]);
end;

function TGarbageCollector.AllocList(constref v:TObjectArray; gcGen:Int8=0): TEpObject;
begin
  if gcGen < 0 then gcGen := 0;
  Result := TListObject.Create(v);
  Result.gc := Pointer(self);
  Gen[gcGen].Add(Result);
  if gcGen = 0 then Dec(CountDown[0]);
end;

function TGarbageCollector.AllocFunc(n:epString; p:Int32; r:TIntRange; gcGen:Int8=0): TEpObject;
begin
  if gcGen < 0 then gcGen := 0;
  Result := TFuncObject.Create(n,p,r);
  Result.gc := Pointer(self);
  Gen[gcGen].Add(Result);
  if gcGen = 0 then Dec(CountDown[0]);
end;

procedure TGarbageCollector.Release(var T:TEpObject);
var H:UInt32;
begin
  if (T = nil) then
    Exit;

  H := T.Handle;
  if Gen[0].Contains(T) then
  begin
    if T.Release then
    begin
      Gen[0].Unset(H);
      Inc(CountDown[0]);
    end;
  end

  else if Gen[1].Contains(T) then
  begin
    if T.Release then Gen[1].Unset(H);
  end

  else if Gen[2].Contains(T) then
  begin
    if T.Release then Gen[2].Unset(H);
  end;

  T := nil;
end;


end.
