{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  stuff
}
unit bytecode;
{$I express.inc}

interface

uses
  Classes, SysUtils,
  express,
  dictionary,
  datatypes,
  utils,
  {lexer,}
  opcodes,
  mmgr;

type
  TObjectArray = array of TEpObject;
  
  TLoopInfo = record
    Pos,Id:Int32;
  end;
  TLoopInfoArray = array of TLoopInfo;
  
  TStringToIntMap = specialize TDictionary<string, Int32>; 
  TIntToStringMap = specialize TDictionary<Int32, string>;

  (*
    Bytecode
  *)
  PBytecode = ^TBytecode;
  TBytecode = class
    Code: TOperationArray;
    DocPos: TDocPosArray;
    Variables: TObjectArray;
    Constants: TObjectArray;
    GC: TGarbageCollector;

    NumVars: Int32;

    VarNames: TStringArray;
    DebugHints: TIntToStringMap;

    constructor Create(ACode:TOperationArray;
                       AConstants:TObjectArray;
                       ANumVars:Int32;
                       ADocPos:TDocPosArray;
                       MemMgr:TGarbageCollector); virtual;
    destructor Destroy; override;
    function ToString: string; override;
  end;

  (* "Variable context" *)
  TVarContext = record
    Names: TStringArray;             //Names can contain duplicates (different scopes and such)
    NamesToNumbers: TStringToIntMap; //The map belongs to a single scope (no duplicates)
  end;

  (*
    Compiler context
  *)
  TCompilerContext = class(TObject)
    Code: TOperationArray;
    DocPos: TDocPosArray;
    GC: TGarbageCollector;

    Constants: TObjectArray;
    Vars: TVarContext;
    DestVars: TIntArray;

    PatchCount: SizeInt;
    PatchPositions: TLoopInfoArray;
    DebugHints: TIntToStringMap;

    constructor Create();
    destructor Destroy; override;

    function CodeSize: SizeInt; inline;
    function GetNone: Int32;

    function RegisterConst(value:TEpObject): Int32; inline;
    function RegisterVar(name:String; Redeclare:Boolean=True): Int32; inline;
    function getTempVar: Int32; inline;
    function LookupVar(name:String; Pos:TDocPos): Int32; inline;
    function Emit(bc:EBytecode; arg:Int32; Pos:TDocPos): Int32; inline;
    procedure Patch(pos:Int32; bc:EBytecode; arg:Int32=0); inline;
    procedure PatchArg(pos:Int32; arg:Int32); inline;

    procedure PushDestVar(varId:Int32); inline;
    function PopDestVar: Int32; inline;
    function RemoveDestVarIf(location:Int32): Boolean; inline;
    procedure DebugHint(hint:string); inline;

    procedure PreparePatch(AddDebugSym:Boolean=True; DEBUG_SYM:String='LOOP_START'); inline;
    procedure PopPatch(AddDebugSym:Boolean=True; DEBUG_SYM:String='LOOP_END'); inline;
    procedure RunPatch(oldOp:EBytecode; newOp:EBytecode=JUMP; newArg:Int32=0);

    function Bytecode: TBytecode;
  end;
  
  TStringToOpcode = specialize TDictionary<string, EBytecode>;
  
var
  OperatorToOpcode: TStringToOpcode; 

const
  INPLACE_OPERATIONS = [INPLACE_ADD, INPLACE_SUB, INPLACE_MUL, INPLACE_DIV];


implementation

uses
  errors;


(*============================================================================
  Program bytecode representation
  ============================================================================*)
constructor TBytecode.Create(ACode:TOperationArray;
                             AConstants:TObjectArray;
                             ANumVars:Int32;
                             ADocPos:TDocPosArray;
                             MemMgr:TGarbageCollector);
var i:Int32;
begin
  Code      := ACode;
  DocPos    := ADocPos;
  Constants := AConstants;
  NumVars   := ANumVars;
  GC        := MemMgr;

  SetLength(Variables, NumVars);
  for i:=0 to NumVars-1 do
    Variables[i] := Constants[0]; //None
end;

destructor TBytecode.Destroy;
begin
  if DebugHints <> nil then
    DebugHints.Destroy;

  GC.Destroy; //this will clean up whatever's left
  inherited;
end;

function TBytecode.ToString: string;
var
  i,x:Int32;
  t,tmpstr:string;
begin
  Result := '[LGREEN]PROGRAM:'+#13#10;
  for i:=0 to High(self.code) do
  begin
    WriteStr(t, Code[i].code);
    x := Length(t);

    if DebugHints.Contains(i) then
      Result += '[LYELLOW]'+DebugHints[i]+#13#10;

    Result += Format('  #%-6d %s %'+IntToStr(20-x)+'d', [i, t, Code[i].arg]);

    if (Code[i].code = LOAD_CONST) then
    begin
      tmpstr := Constants[Code[i].arg].AsString;
      if Length(tmpstr) > 10 then
      begin
        SetLength(tmpstr, 10);
        tmpstr += '..';
      end;
      Result += ' ['+ tmpstr +']';
    end
    else if (Code[i].code = LOAD) then
    begin
      Result += ' ['+self.VarNames[Code[i].arg]+']';
    end;

    Result +=  #13#10;
  end;
end;


(*============================================================================
  Compiler context for the AST .
  ============================================================================*)
constructor TCompilerContext.Create();
begin
  GC := TGarbageCollector.Create();

  Vars.NamesToNumbers := TStringToIntMap.Create(@HashStr);
  DebugHints := TIntToStringMap.Create(@HashInt32);
  Self.RegisterConst(GC.AllocNone());
end;

destructor TCompilerContext.Destroy;
begin
  //DebugHints is handled by TBytecode
  Vars.NamesToNumbers.Destroy;
  inherited;
end;

function TCompilerContext.CodeSize: SizeInt;
begin
  Result := Length(Code);
end;

function TCompilerContext.GetNone: Int32;
begin
  Result := 0; //None should always be `0`
end;

function TCompilerContext.RegisterConst(value:TEpObject): Int32;
begin
  Result := Length(Constants);
  SetLength(Constants, Result + 1);
  Constants[Result] := value;
end;

function TCompilerContext.RegisterVar(Name:String; Redeclare:Boolean=True): Int32;
var id:Int32;
begin
  if (not Redeclare) and Vars.NamesToNumbers.Get(Name, id) then
    Exit(id);

  Result := Length(Vars.Names);
  SetLength(Vars.Names, Result + 1);
  Vars.Names[Result] := Name;
  Vars.NamesToNumbers[name] := Result;
end;

function TCompilerContext.getTempVar: Int32;
begin
  Result := Length(Vars.Names);
  SetLength(Vars.Names, Result + 1);
end;

function TCompilerContext.LookupVar(Name:String; Pos:TDocPos): Int32;
begin
  Result := Vars.NamesToNumbers.GetDef(Name, -1);
  if Result = -1 then
    RaiseExceptionFmt(eSyntaxError, eUndefinedIdentifier, [Name], Pos);
end;

function TCompilerContext.Emit(Bc:EBytecode; Arg:Int32; Pos:TDocPos): Int32;
begin
  Result := Length(Code);
  SetLength(Code, Result+1);
  Code[Result].code := Bc;
  Code[Result].arg  := Arg;

  SetLength(DocPos, Result+1);
  DocPos[Result] := Pos;
end;

procedure TCompilerContext.Patch(pos:Int32; bc:EBytecode; arg:Int32=0);
begin
  Code[pos].code := bc;
  Code[pos].arg  := arg;
end;

procedure TCompilerContext.PatchArg(pos:Int32; arg:Int32);
begin
  Code[pos].arg := arg;
end;

// Add a new destination variable
procedure TCompilerContext.PushDestVar(varId:Int32);
var L:Int32;
begin
  L := Length(DestVars);
  SetLength(DestVars, L+1);
  DestVars[L] := varId;
end;

//get the topmost destvar, and remove it
function TCompilerContext.PopDestVar: Int32;
var top:Int32;
begin
  top := High(DestVars);
  if top < 0 then
    Result := -1
  else
  begin
    Result := DestVars[top];
    SetLength(DestVars, top);
  end;
end;

// remove the topmost destvar if it matches the given location
function TCompilerContext.RemoveDestVarIf(location:Int32): Boolean;
var top:Int32;
begin
  top := High(DestVars);
  Result := False;
  if (top >= 0) and (location = DestVars[top]) then
  begin
    SetLength(DestVars, top);
    Result := True;
  end;
end;

//debugging hint
procedure TCompilerContext.DebugHint(hint:string);
begin
  DebugHints.Add(Length(Code), hint);
end;

// push right before a loop starts
// it's needed for "continue" and "break" statements.
procedure TCompilerContext.PreparePatch(AddDebugSym:Boolean=True; DEBUG_SYM:String='LOOP_START');
var L:Int32;
begin
  L := Length(PatchPositions);
  SetLength(PatchPositions, L+1);
  PatchPositions[L].Pos := Length(Code);
  PatchPositions[L].ID  := PatchCount;

  if AddDebugSym then
    DebugHint(DEBUG_SYM+' #'+IntToStr(PatchCount));
  Inc(PatchCount);
end;

//pop it off at the end of a loop
//but only after you've called "PatchContinue" and "PatchBreak" with the new jump pos
procedure TCompilerContext.PopPatch(AddDebugSym:Boolean=True; DEBUG_SYM:String='LOOP_END');
var L:Int32;
begin
  L := High(PatchPositions);
  if AddDebugSym then
    DebugHint(DEBUG_SYM+' #'+IntToStr(PatchPositions[L].ID));
  SetLength(PatchPositions, L);
end;


// patches all the jumps of the type `__CONTINUE` / `__BREAK`
// swaps it out with a normal "JUMP", and replaces the argument
procedure TCompilerContext.RunPatch(oldOp:EBytecode; newOp:EBytecode=JUMP; newArg:Int32=0);
var
  i,patchStart:Int32;
begin
  patchStart := PatchPositions[High(PatchPositions)].Pos;
  if patchStart < 0 then patchStart := 0;

  for i:=patchStart to High(Code) do
    if Code[i].code = oldOp then
    begin
      Code[i].code := newOp;
      Code[i].arg  := newArg;
    end;
end;


// builds the bytecode which is what's executed at runtime.
function TCompilerContext.Bytecode: TBytecode;
begin
  Result := TBytecode.Create(Code, Constants, Length(Vars.Names), DocPos, GC);
  Result.DebugHints := DebugHints;
  Result.VarNames := Vars.Names;
end;



procedure InitModule();
begin
  OperatorToOpcode := TStringToOpcode.Create(@HashStr);

  (* assign operations *)
  OperatorToOpcode.Add(':=',   ASGN);
  OperatorToOpcode.Add('+=',   INPLACE_ADD);
  OperatorToOpcode.Add('-=',   INPLACE_SUB);
  OperatorToOpcode.Add('*=',   INPLACE_MUL);
  OperatorToOpcode.Add('/=',   INPLACE_DIV);

  (* arithmetic operations *)
  OperatorToOpcode.Add('+',   BIN_ADD);
  OperatorToOpcode.Add('-',   BIN_SUB);
  OperatorToOpcode.Add('*',   BIN_MUL);
  OperatorToOpcode.Add('div', BIN_DIV);
  OperatorToOpcode.Add('/',   BIN_FDIV);
  OperatorToOpcode.Add('%',   BIN_MOD);

  (* equality operations *)
  OperatorToOpcode.Add('=',   BIN_EQ);
  OperatorToOpcode.Add('!=',  BIN_NE);
  OperatorToOpcode.Add('<',   BIN_LT);
  OperatorToOpcode.Add('>',   BIN_GT);
  OperatorToOpcode.Add('<=',  BIN_LE);
  OperatorToOpcode.Add('>=',  BIN_GE);

  (* logical operations *)
  OperatorToOpcode.Add('and', BIN_AND);
  OperatorToOpcode.Add('or',  BIN_OR);

  (* binary operations *)
  OperatorToOpcode.Add('&',   BIN_BAND);
  OperatorToOpcode.Add('|',   BIN_BOR);
  OperatorToOpcode.Add('^',   BIN_BXOR);
end;

initialization
  InitModule();
  
end.
