unit xpr.AST;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Abstract syntax tree
}
{$I express.inc}

interface

uses
  Classes, SysUtils, 
  xpr.express, 
  xpr.bytecode;

type
  (* 
    Abstract base node
  *)
  TBaseNode = class(TObject)
    FDocPos: TDocPos;
    constructor Create(DocPos:TDocPos); virtual;
    function ToString: string; reintroduce; virtual;
    procedure Compile(ctx: TCompilerContext); virtual;
  end;

  TNodeArray = array of TBaseNode;

  (* 
    A list of statements or expressions
  *)
  TBlock = class(TBaseNode)
    List: TNodeArray;
    constructor Create(AList:TNodeArray; DocPos: TDocPos); reintroduce;
    constructor Create(AStmt:TBaseNode; DocPos: TDocPos); overload;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  TReturn = class(TBaseNode)
    Expr:TBaseNode;
    constructor Create(AExpr:TBaseNode; DocPos: TDocPos); overload;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  TContinue = class(TBaseNode)
    procedure Compile(ctx: TCompilerContext); override;
  end;

  TBreak = class(TBaseNode)
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (* 
    A single statements
  *)
  TStatement = class(TBaseNode)
    Expr: TBaseNode;
    constructor Create(AExpr:TBaseNode; DocPos: TDocPos); reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (* 
    A stub for all constants
  *)
  TConstant = class(TBaseNode)
    StrVal:String;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  TConstNone = class(TConstant)
    constructor Create(DocPos: TDocPos); reintroduce;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  TConstBool = class(TConstant)
    Value: Boolean;
    constructor Create(AValue:String; DocPos: TDocPos); reintroduce;
    procedure Compile(ctx: TCompilerContext); override;
  end;
  
  TConstChar = class(TConstant)
    Value: epChar;
    constructor Create(AValue:String; DocPos: TDocPos); reintroduce;
    procedure Compile(ctx: TCompilerContext); override;
  end;
  
  TConstInt = class(TConstant)
    Value: Int64;
    constructor Create(AValue:String; DocPos: TDocPos); reintroduce;
    procedure Compile(ctx: TCompilerContext); override;
  end;
  
  TConstFloat = class(TConstant)
    Value: Extended;
    constructor Create(AValue:String; DocPos: TDocPos); reintroduce;
    procedure Compile(ctx: TCompilerContext); override;
  end;
  
  TConstString = class(TConstant)
    Value: String;
    constructor Create(AValue:String; DocPos: TDocPos); reintroduce;
    procedure Compile(ctx: TCompilerContext); override;
  end;
  
  (* 
    A variable
  *)
  TVariable = class(TBaseNode)
    Name: string;
    constructor Create(AName:String; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    function VarId(ctx: TCompilerContext): Int32;
    procedure Compile(ctx: TCompilerContext); override;
  end;
  TVarArray = array of TVariable;

  (*
    For declaring variables
  *)
  TVarDecl = class(TBaseNode)
    Variables: TVarArray;
    Expr:TBaseNode;
    constructor Create(AVariables:TVarArray; AExpr:TBaseNode; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (*
    A function
  *)
  TFunction = class(TBaseNode)
    Name: string;
    Params: TVarArray;
    Prog: TBlock;

    constructor Create(AName:string; AParams:TVarArray; AProg:TBlock; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (*
    Call a callable object
  *)
  TCall = class(TBaseNode)
    Callable: TBaseNode;
    Args: TNodeArray;
    constructor Create(ACallable:TBaseNode; AArgs:TNodeArray; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (*
    Index an indexable object
  *)
  TIndex = class(TBaseNode)
    Indexable: TBaseNode;
    Args: TNodeArray;
    constructor Create(AIndexable:TBaseNode; AArgs:TNodeArray; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (*
    A list expression
  *)
  TListExpr = class(TConstant)
    Expressions: TNodeArray;
    constructor Create(AExpressions:TNodeArray; DocPos: TDocPos); reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (*
    Unary operation
  *)
  TUnaryOp = class(TBaseNode)
    Left: TBaseNode;
    Op: TToken;
    PostFix: Boolean;
    constructor Create(Operation:TToken; ALeft:TBaseNode; APostFix:Boolean; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (*  Binary operation - EG: `5/2` *)
  TBinOp = class(TBaseNode)
    Left, Right: TBaseNode;
    Op: TToken;
    constructor Create(Operation:TToken; ALeft, ARight:TBaseNode; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;
  
  (*  simple assignments - EG: `z := 5/2` *)
  TAssignment = class(TBaseNode)
    Left: TBaseNode;
    Right: TBaseNode;
    Op: TToken;
    constructor Create(Operation:TToken; ALeft:TBaseNode; ARight:TBaseNode; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (* If-Expression *)
  TIfExpression = class(TBaseNode)
    Condition: TBaseNode;
    Left, Right: TBaseNode;
    constructor Create(ACond:TBaseNode; ALeft, ARight:TBaseNode; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (* if statement *)
  TIf = class(TBaseNode)
    Condition: TBaseNode;
    Body: TBlock;
    ElseBody: TBlock;
    constructor Create(ACond:TBaseNode; ABody, AElseBody:TBlock; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (* while loop *)
  TWhile = class(TBaseNode)
    Condition: TBaseNode;
    Body: TBlock;
    constructor Create(ACond:TBaseNode; ABody:TBlock; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (* repeat loop *)
  TRepeat = class(TBaseNode)
    Condition: TBaseNode;
    Body: TBlock;
    constructor Create(ACond:TBaseNode; ABody:TBlock; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (* for loop *)
  TFor = class(TBaseNode)
    Stmt1,Stmt2,Stmt3: TBaseNode;
    Body: TBlock;
    constructor Create(AStmt1,AStmt2,AStmt3:TBaseNode; ABody:TBlock; DocPos: TDocPos); virtual; reintroduce;
    function ToString: string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (* print statement *)
  TPrint = class(TBaseNode)
    Exprs: TNodeArray;
    constructor Create(AExprs:TNodeArray; DocPos: TDocPos); virtual; reintroduce;
    function ToString:string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

  (* temporary time function / variable thingy *)
  TTimeNow = class(TBaseNode)
    constructor Create(DocPos: TDocPos); virtual; reintroduce;
    function ToString:string; override;
    procedure Compile(ctx: TCompilerContext); override;
  end;

function CompileAST(astnode:TBaseNode; doFree:Boolean = True): TBytecode;
  
implementation

uses
  xpr.interpreter, 
  xpr.utils, 
  xpr.opcodes,
  xpr.errors,
  {$I objects.inc};

function CompileAST(astnode:TBaseNode; doFree:Boolean = True): TBytecode;
var
  ctx:TCompilerContext;
begin
  ctx := TCompilerContext.Create();
  astnode.Compile(ctx);
  ctx.Emit(RETURN, 0, astnode.FDocPos);
  Result := ctx.Bytecode;
  ctx.Free;
  //if doFree then
  //  astNode.Free;
end;


constructor TBaseNode.Create(DocPos: TDocPos);
begin
  FDocPos := DocPos;
end;

function TBaseNode.ToString: string;
begin
  if Self.ClassType = TBaseNode then Result := 'BaseNode'
  else if Self is TContinue then     Result := 'Continue'
  else if Self is TBreak then        Result := 'Break'
  else                               Result := Self.ToString;
end;

procedure TBaseNode.Compile(ctx: TCompilerContext);
begin
  Assert(ctx  <> nil, 'compiler context is `nil`');

  if Self.ClassType = TBaseNode then
    RaiseException(eSyntaxError, eNotImplemented, FDocPos);
  Self.Compile(ctx);
end;

(*
  Everything should be inside a block..
*)
constructor TBlock.Create(AList:TNodeArray; DocPos: TDocPos);
begin
  List := AList;
end;

constructor TBlock.Create(AStmt:TBaseNode; DocPos: TDocPos);
begin
  SetLength(List, 1);
  List[0] := AStmt;
end;

function TBlock.ToString: AnsiString;
var i:Int32;
begin
  Result := 'Block([';
  for i:=0 to High(List) do
  begin
    Result += List[i].ToString;
    if i <> High(List) then
      Result += ', ';
  end;
  Result += '])';
end;

procedure TBlock.Compile(ctx: TCompilerContext);
var i:Int32;
begin
  for i:=0 to High(List) do
    if List[i] = nil then
      continue
    else
      List[i].Compile(ctx);
end;

(* branching statements *)
constructor TReturn.Create(AExpr:TBaseNode; DocPos: TDocPos);
begin
  Expr := AExpr;
  FDocPos := DocPos;
end;

procedure TReturn.Compile(ctx: TCompilerContext);
begin
  if Expr <> nil then
    Expr.Compile(ctx)
  else
    ctx.Emit(LOAD_CONST, ctx.GetNone(), FDocPos);

  ctx.Emit(RETURN, 0, FDocPos);
end;

procedure TContinue.Compile(ctx: TCompilerContext);
begin
  ctx.Emit(__CONTINUE, 0, FDocPos);
end;

procedure TBreak.Compile(ctx: TCompilerContext);
begin
  ctx.Emit(__BREAK, 0, FDocPos);
end;


(*
  A statement, for example a function-call.
*)
constructor TStatement.Create(AExpr:TBaseNode; DocPos: TDocPos);
begin
  Expr := AExpr;
  FDocPos := DocPos;
end;

function TStatement.ToString: string;
begin
  Result := 'Statement('+Expr.ToString+')';
end;

procedure TStatement.Compile(ctx: TCompilerContext);
begin
  Expr.Compile(ctx);
  ctx.Emit(DISCARD_TOP, 0, FDocPos);
end;

(*
  Variables that's considered constants, for example `1.0` in a script is a constant.
*)
function TConstant.ToString: string;
begin
  Result := 'Constant('+StrVal+')';
end;

procedure TConstant.Compile(ctx:TCompilerContext);
begin
  if (Self is TConstBool) then
    (Self as TConstBool).Compile(ctx)
  else if (Self is TConstChar) then
    (Self as TConstChar).Compile(ctx)
  else if (Self is TConstInt) then
    (Self as TConstInt).Compile(ctx)
  else if (Self is TConstFloat) then
    (Self as TConstFloat).Compile(ctx)
  else if (Self is TConstString) then
    (Self as TConstString).Compile(ctx)
  else
    RaiseException(eSyntaxError, eNotImplemented, FDocPos);
end;

(*
  A None
*)
constructor TConstNone.Create(DocPos: TDocPos);
begin
  StrVal := 'None';
  FDocPos := DocPos;
end;

procedure TConstNone.Compile(ctx: TCompilerContext);
begin
  ctx.Emit(LOAD_CONST, ctx.GetNone, FDocPos);
end;

(*
  A boolean
*)
constructor TConstBool.Create(AValue:String; DocPos: TDocPos);
begin
  StrVal := AValue;
  Value  := StrToBool(AValue);
  FDocPos := DocPos;
end;

procedure TConstBool.Compile(ctx: TCompilerContext);
var obj:TEpObject;
begin
  obj := ctx.GC.AllocBool(Value, 2);
  ctx.Emit(LOAD_CONST, ctx.RegisterConst(obj), FDocPos);
end;

(*
  A single character
*)
constructor TConstChar.Create(AValue:String; DocPos: TDocPos);
begin
  StrVal := AValue;
  Value  := #0;
  if Length(Value) > 0 then
    Value := AValue[1];
  FDocPos := DocPos;
end;

procedure TConstChar.Compile(ctx: TCompilerContext);
var obj:TEpObject;
begin
  obj := ctx.GC.AllocChar(Value, 2);
  ctx.Emit(LOAD_CONST, ctx.RegisterConst(obj), FDocPos);
end;

(*
  An integer
*)
constructor TConstInt.Create(AValue:String; DocPos: TDocPos);
begin
  StrVal := AValue;
  Value  := StrToInt(AValue);
  FDocPos := DocPos;
end;

procedure TConstInt.Compile(ctx: TCompilerContext);
var obj:TEpObject;
begin
  obj := ctx.GC.AllocInt(Value, 2);
  ctx.Emit(LOAD_CONST, ctx.RegisterConst(obj), FDocPos);
end;

(*
  A floating point numbers
*)
constructor TConstFloat.Create(AValue:String; DocPos: TDocPos);
begin
  StrVal  := AValue;
  Value   := StrToFloatDot(AValue);
  FDocPos := DocPos;
end;

procedure TConstFloat.Compile(ctx: TCompilerContext);
var obj:TEpObject;
begin
  obj := ctx.GC.AllocFloat(Value, 2);
  ctx.Emit(LOAD_CONST, ctx.RegisterConst(obj), FDocPos);
end;

(*
  strings
*)
constructor TConstString.Create(AValue:String; DocPos: TDocPos);
begin
  StrVal := AValue;
  FDocPos := DocPos;
end;

procedure TConstString.Compile(ctx: TCompilerContext);
var obj:TEpObject;
begin
  obj := ctx.GC.AllocString(StrVal, 2);
  ctx.Emit(LOAD_CONST, ctx.RegisterConst(obj), FDocPos);
end;

(*
  Variables
*)
constructor TVariable.Create(AName:String; DocPos: TDocPos);
begin
  Name := AName;
  FDocPos := DocPos;
end;

function TVariable.ToString: string;
begin
  Result := 'Variable('+Name+')';
end;

function TVariable.VarId(ctx: TCompilerContext): Int32;
begin
  Result := ctx.LookupVar(self.Name, FDocPos);
end;

procedure TVariable.Compile(ctx: TCompilerContext);
begin
  ctx.Emit(LOAD, ctx.LookupVar(self.Name, FDocPos), FDocPos);
end;

(*
  Declaring variables
*)
constructor TVarDecl.Create(AVariables:TVarArray; AExpr:TBaseNode; DocPos: TDocPos);
begin
  Variables := AVariables;
  Expr := AExpr;
  FDocPos := DocPos;
end;

function TVarDecl.ToString: string;
begin
  Result := 'VarDecl(...)';
end;

procedure TVarDecl.Compile(ctx: TCompilerContext);
var
  dest, i:Int32;
begin
  for i:=0 to High(Variables) do
  begin
    dest := ctx.RegisterVar(TVariable(Variables[i]).Name, True);
    if Expr <> nil then
    begin
      Expr.Compile(ctx);
      Variables[i].Compile(ctx);
      ctx.Emit(RASGN, dest, FDocPos);
    end;
  end;
end;

(*
  Functions
  To-do:
    Fix for recursion, every time the function is called
    the local vars has to be re-allocated (somehow)..
    Sadly this will add a major overhead -.-'

    Edit: I've made temp patch that leaks A LOT, but whatever..
    New To-do: Check for a more proper solution.
*)
constructor TFunction.Create(AName:string; AParams:TVarArray; AProg:TBlock; DocPos: TDocPos);
begin
  Name   := AName;
  Params := AParams;
  Prog   := AProg;
  FDocPos := DocPos;
end;

function TFunction.ToString: string;
begin
  Result := 'Function('+Name+')';
end;

procedure TFunction.Compile(ctx: TCompilerContext);
var
  dest,funcStart,i,funcID:Int32;
  funcObj: TFuncObject;
  varRange: TIntRange;
var
  oldNameMap, funcNameMap: TStringToIntMap;
begin
  funcID := ctx.RegisterVar(Name);

  //store current context state
  oldNameMap  := ctx.Vars.NamesToNumbers;

  funcNameMap := oldNameMap.Copy();


  //set context to our function and compile
  begin
    varRange.Low := Length(ctx.Vars.Names);
    ctx.Vars.NamesToNumbers := funcNameMap;

    ctx.PreparePatch(True, 'FUNCTION['+Name+']');
    ctx.Emit(__FUNC, -1, FDocPos);
    funcStart := ctx.CodeSize;

    (* The caller has pushed arguments onto the stack, we need to store them *)
    for i:=0 to High(Params) do
    begin
      dest := ctx.RegisterVar(Params[High(Params)-i].Name);
      ctx.Emit(LOAD, dest, Params[High(Params)-i].FDocPos);
      ctx.Emit(RASGN, dest, Params[High(Params)-i].FDocPos);
    end;

    Prog.Compile(ctx);
    ctx.Emit(LOAD_CONST, ctx.GetNone(), FDocPos);
    ctx.Emit(RETURN, 0, FDocPos);

    varRange.High := High(ctx.Vars.Names);
    funcObj := ctx.GC.AllocFunc(Name, funcStart, varRange, 2) as TFuncObject;

    ctx.RunPatch(__FUNC, JMP_FORWARD, ctx.CodeSize());
    ctx.PopPatch(True, 'END_FUNCTION');
  end;

  //reset context
  ctx.Vars.NamesToNumbers := oldNameMap;

  ctx.Emit(LOAD_CONST, ctx.RegisterConst(funcObj), FDocPos);
  ctx.Emit(STORE_FAST, funcID, FDocPos);
end;

(*
  For calling a function, or a function like object
*)
constructor TCall.Create(ACallable:TBaseNode; AArgs:TNodeArray; DocPos: TDocPos);
begin
  Callable := ACallable;
  Args := AArgs;
  FDocPos := DocPos;
end;

function TCall.ToString: string;
begin
  Result := 'Call('+Callable.ToString+')';
end;

procedure TCall.Compile(ctx: TCompilerContext);
var i:Int32;
begin
  for i:=0 to High(Args) do
    Args[i].Compile(ctx);

  Callable.Compile(ctx);
  ctx.Emit(CALL, Length(Args), FDocPos);
end;


(*
  For calling a function, or a function like object
*)
constructor TIndex.Create(AIndexable:TBaseNode; AArgs:TNodeArray; DocPos: TDocPos);
begin
  Indexable := AIndexable;
  Args := AArgs;
  FDocPos := DocPos;
end;

function TIndex.ToString: string;
begin
  Result := 'Index('+Indexable.ToString+')';
end;

procedure TIndex.Compile(ctx: TCompilerContext);
var dest:Int32;
begin
  dest := ctx.PopDestVar;
  if dest < 0 then dest := ctx.getTempVar;

  Args[0].Compile(ctx);

  Indexable.Compile(ctx);
  ctx.Emit(GET_ITEM, dest, FDocPos);
end;

(*
  list expression
*)
constructor TListExpr.Create(AExpressions:TNodeArray; DocPos: TDocPos);
begin
  FDocPos := DocPos;
  Expressions := AExpressions;
end;

function TListExpr.ToString: string;
begin
  Result := 'ListExpr(...)';
end;

procedure TListExpr.Compile(ctx: TCompilerContext);
var dest,i,l:Int32;
begin
  dest := ctx.PopDestVar;
  if dest < 0 then dest := ctx.getTempVar;

  l := Length(Expressions);
  for i:=0 to l-1 do
    Expressions[i].Compile(ctx);

  ctx.Emit(LOAD_CONST, ctx.RegisterConst(ctx.GC.AllocInt(l, 2)), FDocPos);
  ctx.Emit(BUILD_LIST, dest, FDocPos);
end;


(*
  Handling of unary operations, eg "-value"
*)
constructor TUnaryOp.Create(Operation:TToken; ALeft:TBaseNode; APostFix:Boolean; DocPos: TDocPos);
begin
  Left := ALeft;
  Op   := Operation;
  PostFix := APostFix;
  FDocPos := DocPos;
end;

function TUnaryOp.ToString: string;
begin
  if PostFix then
    Result := 'UnaryOp('+Left.ToString+'`'+ Op.Value +'`)'
  else
    Result := 'UnaryOp('+Left.ToString+'`'+ Op.Value +'`)'
end;

procedure TUnaryOp.Compile(ctx: TCompilerContext);
var dest:Int32;
begin
  dest := ctx.PopDestVar;
  if dest < 0 then dest := ctx.getTempVar;

  Left.Compile(ctx);
  if ((Op.Value = '++') or (Op.Value = '--')) then
  begin
    if not(Left is TVariable) then
      RaiseException(eSyntaxError, eExpectedVar, Left.FDocPos);
    if PostFix then
    begin
      if      Op.Value = '++' then ctx.Emit(UNARY_POSTINC, dest, FDocPos)
      else if Op.Value = '--' then ctx.Emit(UNARY_POSTDEC, dest, FDocPos);
    end else
      if      Op.Value = '++' then ctx.Emit(UNARY_PREINC, dest, FDocPos)
      else if Op.Value = '--' then ctx.Emit(UNARY_PREDEC, dest, FDocPos);
  end
  else
  begin
    if      Op.Value = '-'   then ctx.Emit(UNARY_SUB, dest, FDocPos)
    else if Op.Value = '+'   then (* nothing *)
    else if Op.Value = 'not' then ctx.Emit(UNARY_NOT, dest, FDocPos)
    else if Op.Value = '~'   then ctx.Emit(UNARY_BINV, dest, FDocPos)
    else RaiseException(eSyntaxError, eNotImplemented, Left.FDocPos);
  end;
end;

(*
  Handling of binary operations, eg "var1 * var2"
*)
constructor TBinOp.Create(Operation:TToken; ALeft, ARight:TBaseNode; DocPos: TDocPos);
begin
  Left  := ALeft;
  Right := ARight;
  Op := Operation;
  FDocPos := DocPos;
end;

function TBinOp.ToString: string;
begin
  Result := 'BinOp(`'+ Op.Value +'`, '+Left.ToString+', '+ Right.ToString +')';
end;

procedure TBinOp.Compile(ctx: TCompilerContext);
var
  dest:Int32;
begin
  dest := ctx.PopDestVar;
  if dest < 0 then dest := ctx.getTempVar;

  Left.Compile(ctx);
  Right.Compile(ctx);

  ctx.Emit(OperatorToOpcode[op.value], dest, FDocPos)
end;


(*
  Handle all simple assignment operations
*)
constructor TAssignment.Create(Operation:TToken; ALeft:TBaseNode; ARight:TBaseNode; DocPos: TDocPos);
begin
  Left  := ALeft;
  Right := ARight;
  Op    := Operation;
  FDocPos := DocPos;
end;

function TAssignment.ToString: string;
begin
  Result := 'Assignment(`'+ Op.Value +'`, '+Left.ToString+', '+ Right.ToString +')';
end;

procedure TAssignment.Compile(ctx: TCompilerContext);
var
  dest:Int32;
  isVar,isAsgn:Boolean;
begin
  isVar := (Left is TVariable);
  isAsgn := (op.value = ':=');

  if (isVar) then
  begin
    dest := ctx.LookupVar(TVariable(Left).Name, Left.FDocPos);
    if isAsgn then
    begin
      if (Right is TBinOp) or (Right is TUnaryOp) or (Right is TIndex) or (Right is TListExpr) then
      begin
        ctx.PushDestVar(dest);
        Right.Compile(ctx); //compile with a destination = left: avoids an assignment
        ctx.Emit(DISCARD_TOP, 0, FDocPos);
      end else
      begin
        Left.Compile(ctx);
        Right.Compile(ctx);
        ctx.Emit(ASGN, dest, FDocPos)
      end;
    end else
    begin
      Left.Compile(ctx);
      Right.Compile(ctx);
      ctx.Emit(OperatorToOpcode[op.value], dest, FDocPos);
    end;
  end
  else if (Left is TIndex) then
  begin
    Right.Compile(ctx);
    TIndex(Left).Indexable.Compile(ctx);  //a tad hacky? ;P
    TIndex(Left).Args[0].Compile(ctx);
    if isAsgn then
      ctx.Emit(SET_ITEM, 0, FDocPos)
    else
      ctx.Emit(OperatorToOpcode[op.value], 0, FDocPos);
  end else
    RaiseException(eSyntaxError, eUnexpected, Left.FDocPos);
end;


(*
  <stmt> if (condition) else <stmt>
*)
constructor TIfExpression.Create(ACond:TBaseNode; ALeft, ARight:TBaseNode; DocPos: TDocPos);
begin
  Condition := ACond;
  Left  := ALeft;
  Right := ARight;
  FDocPos := DocPos;
end;

function TIfExpression.ToString: string;
begin
  Result := 'IfExpr('+Condition.ToString+', '+ Left.ToString +':'+ Right.ToString +')';
end;

procedure TIfExpression.Compile(ctx: TCompilerContext);
var
  after,afterElse:Int32;
begin
  Condition.Compile(ctx);
  after := ctx.Emit(JMP_IF_FALSE, 0, Condition.FDocPos);
  Left.Compile(ctx);
  afterElse := ctx.Emit(JMP_FORWARD, 0, Condition.FDocPos);
  ctx.PatchArg(after, ctx.CodeSize());     //jump here if false
  Right.Compile(ctx);
  ctx.PatchArg(afterElse, ctx.CodeSize()); //jump here to skip else
end;


(*
  if (condition) then <stmts> end
  if (condition) <stmt>

  if (condition) then <stmts> else <stmts> end
  if (condition) <stmt> else <stmt>

  NOT ALLOWED: if (condition) <stmt> else <stmts> end
*)
constructor TIf.Create(ACond:TBaseNode; ABody, AElseBody:TBlock; DocPos: TDocPos);
begin
  Condition := ACond;
  Body     := Abody;
  ElseBody := AElseBody;
  FDocPos  := DocPos;
end;

function TIf.ToString: string;
begin
  if ElseBody = nil then
    Result := 'If('+Condition.ToString+', '+ Body.ToString +')'
  else
    Result := 'If('+Condition.ToString+', '+ Body.ToString +''+ ElseBody.ToString +')';
end;

procedure TIf.Compile(ctx: TCompilerContext);
var
  after,afterElse:Int32;
begin
  Condition.Compile(ctx);

  after := ctx.Emit(JMP_IF_FALSE, 0, Condition.FDocPos);
  Body.Compile(ctx);
  if (elseBody <> nil) then
    afterElse := ctx.Emit(JMP_FORWARD, 0, Condition.FDocPos);
  ctx.PatchArg(after, ctx.CodeSize()); //jump here if false

  if (elseBody <> nil) then
  begin
    ElseBody.Compile(ctx);
    ctx.PatchArg(afterElse, ctx.CodeSize()); //jump here to skip else
  end;
end;


(*
  while (condition) do <stmts> end
  while (condition) <stmt>
*)
constructor TWhile.Create(ACond:TBaseNode; ABody:TBlock; DocPos: TDocPos);
begin
  Condition := ACond;
  Body    := Abody;
  FDocPos := DocPos;
end;

function TWhile.ToString: string;
begin
  Result := 'While('+Condition.ToString+', '+ Body.ToString +')';
end;

procedure TWhile.Compile(ctx: TCompilerContext);
var
  before,after:Int32;
begin
  ctx.PreparePatch();

  before := ctx.CodeSize();
  //while -->
  Condition.Compile(ctx);
  after := ctx.Emit(JMP_IF_FALSE,0,Condition.FDocPos);
  //<-- do -->
  Body.Compile(ctx);
  ctx.Emit(JMP_BACK, before, Condition.FDocPos);
  //<--
  ctx.PatchArg(after, ctx.CodeSize());

  ctx.RunPatch(__CONTINUE, JMP_FORWARD, before);
  ctx.RunPatch(__BREAK, JMP_FORWARD, ctx.CodeSize());
  ctx.PopPatch();
end;

(*
  while (condition) do <stmts> end
  while (condition) <stmt>
*)
constructor TRepeat.Create(ACond:TBaseNode; ABody:TBlock; DocPos: TDocPos);
begin
  Condition := ACond;
  Body    := Abody;
  FDocPos := DocPos;
end;

function TRepeat.ToString: string;
begin
  Result := 'Repeat('+ Body.ToString +', '+ Condition.ToString +')';
end;

procedure TRepeat.Compile(ctx: TCompilerContext);
var
  before,after:Int32;
begin
  ctx.PreparePatch();

  before := ctx.CodeSize();
  //repeat -->
  Body.Compile(ctx);
  //<-- until -->
  Condition.Compile(ctx);
  after := ctx.Emit(JMP_IF_TRUE,0,Condition.FDocPos);
  ctx.Emit(JMP_BACK, before, Condition.FDocPos);
  //<--
  ctx.PatchArg(after, ctx.CodeSize());

  ctx.RunPatch(__CONTINUE, JMP_FORWARD, before);
  ctx.RunPatch(__BREAK, JMP_FORWARD, ctx.CodeSize());
  ctx.PopPatch();
end;

(*
  for (stmt1,stmt2,stmt3) do <stmts> end
  for (stmt1,stmt2,stmt3) <stmt>
*)
constructor TFor.Create(AStmt1,AStmt2,AStmt3:TBaseNode; ABody:TBlock; DocPos: TDocPos);
begin
  Stmt1 := AStmt1;
  Stmt2 := AStmt2;
  Stmt3 := AStmt3;
  Body  := Abody;
  FDocPos := DocPos;
end;

function TFor.ToString: string;
begin
  Result := 'For(..., '+ Body.ToString +')';
end;

procedure TFor.Compile(ctx: TCompilerContext);
var
  before,after,incPos:Int32;
begin
  if Stmt1 <> nil then
    Stmt1.Compile(ctx);    //before the loop

  ctx.PreparePatch();
  before := ctx.CodeSize();
  if Stmt2 <> nil then
  begin
    Stmt2.Compile(ctx);    //the beginning of the body
    after := ctx.Emit(JMP_IF_FALSE, 0, Stmt2.FDocPos);
  end;
  Body.Compile(ctx);
  incPos := ctx.CodeSize;

  if Stmt3 <> nil then
     Stmt3.Compile(ctx); //after the body

  ctx.Emit(JMP_BACK, before, FDocPos);

  if Stmt2 <> nil then
    ctx.PatchArg(after, ctx.CodeSize());

  ctx.RunPatch(__CONTINUE, JMP_FORWARD, incPos);
  ctx.RunPatch(__BREAK, JMP_FORWARD, ctx.CodeSize());
  ctx.PopPatch();
end;

(*
  print <something>
*)
constructor TPrint.Create(AExprs:TNodeArray; DocPos: TDocPos);
begin
  Exprs := AExprs;
  FDocPos := DocPos;
end;

function TPrint.ToString: string;
begin
  Result := 'Print(...)';
end;

procedure TPrint.Compile(ctx: TCompilerContext);
var i:Int32;
begin
  for i:=0 to High(Exprs) do
    Exprs[i].Compile(ctx);
  ctx.Emit(PRINT, Length(Exprs), FDocPos);
end;


(*
  print time //used for testing atm
*)
constructor TTimeNow.Create(DocPos: TDocPos);
begin
  FDocPos := DocPos;
end;

function TTimeNow.ToString: string;
begin
  Result := 'TimeNow(...)';
end;

procedure TTimeNow.Compile(ctx: TCompilerContext);
begin
  ctx.Emit(TIMENOW, 0, FDocPos);
end;
        

end.
