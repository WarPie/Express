{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Interpreter, for the execution of code
}
unit interpreter;
{$I express.inc}

interface

uses
  Classes,
  SysUtils,
  datatypes,
  express,
  bytecode,
  opcodes;

type
  TFrame = record
    Stack: TObjectArray;
    StackPos: Int32;
    procedure Init;

    procedure Push(value: TEpObject); inline;
    function Pop: TEpObject; inline;
    procedure Popn(n:Int32; var dest:TObjectArray); inline;
    function Top: TEpObject; inline;
    procedure SetTop(value: TEpObject); inline;

    function StackToString: String;
  end;

  TCallStack = record
  public
    Stack: TIntArray;
    StackPos: Int32;
    procedure Init;

    procedure Push(constref pc: Int32); inline;
    procedure Reset(var pc: Int32); inline;
  end;

  TInterpreter = class(TObject)
  public
    Frame: TFrame;
    CallStack: TCallStack;
    Bytecode: TBytecode;
    Pc: Int32;
  public
    constructor Create(bc:TBytecode);
    destructor Destroy; override;
    procedure BuildList(var dest:TEpObject); inline;
    procedure ExecuteSafe;
    procedure Execute;
  end;

const
  STACK_MIN_SIZE   = 16;
  STACK_MULTIPLIER = 2;


implementation

uses
  utils, errors;


procedure TFrame.Init;
begin
  SetLength(Stack, STACK_MIN_SIZE);
  StackPos := -1;
end;

procedure TFrame.Push(value: TEpObject);
begin
  if StackPos = High(Stack) then
    SetLength(Stack, Length(stack) * STACK_MULTIPLIER);
  Inc(StackPos);
  Stack[StackPos] := value;
end;

function TFrame.Pop: TEpObject;
begin
  Result := Stack[StackPos];
  Dec(StackPos);
end;

procedure TFrame.Popn(n:Int32; var dest:TObjectArray);
begin
  if n > 0 then
  begin
    Move(Stack[StackPos-n+1], dest[0], n*SizeOf(Pointer));
    Dec(StackPos, n);
  end;
end;

function TFrame.Top: TEpObject;
begin
  Result := Stack[StackPos];
end;

procedure TFrame.SetTop(value: TEpObject);
begin
  Stack[StackPos] := value;
end;

function TFrame.StackToString: String;
var i,size:Int32;
begin
  Result := '[';
  if StackPos > 10 then size := 10 else size := StackPos;
  for i:=0 to size do
  begin
    Result += stack[i].AsString;
    if i <> StackPos then
      Result += ', ';
  end;
  if StackPos > 10 then Result += '...';
  Result += ']';
end;

(*
  Call stack
*)
procedure TCallStack.Init;
begin
  SetLength(Stack, STACK_MIN_SIZE);
  StackPos := -1;
end;

procedure TCallStack.Push(constref pc: Int32);
begin
  if StackPos = High(Stack) then
    SetLength(Stack, Length(stack) * STACK_MULTIPLIER);

  Inc(StackPos);
  Stack[StackPos] := pc;
end;

procedure TCallStack.Reset(var pc: Int32);
begin
  pc := Stack[StackPos];
  Dec(StackPos);
end;



(*
  An executable object
*)
constructor TInterpreter.Create(bc:TBytecode);
begin
  Bytecode := bc;
  Pc := 0;
  Frame.Init;
  CallStack.Init;
end;

destructor TInterpreter.Destroy;
var i:Int32;
begin
  //for i:=0 to High(Frame.Vars) do
  //  Ep_DecRef(Frame.Vars[i]);
  if not Bytecode.IsChild then
    Bytecode.Free;

  inherited;
end;

procedure TInterpreter.BuildList(var dest:TEpObject);
var
  list:TListObject;
  arr :TObjectArray;
  argCount:TIntObject;
  i:Int32;
begin
  argCount := Frame.Pop() as TIntObject;
  SetLength(arr, argCount.value);
  for i:=1 to argCount.value do
    arr[argCount.value-i] := Frame.Pop;

  list := TListObject.Create(arr);
  dest.ASGN(list, dest);
  Frame.Push(dest);

  SetLength(list.value, 0);
  list.Free;
end;

procedure TInterpreter.ExecuteSafe;
begin
  Execute;
end;

procedure TInterpreter.Execute;
var
  op: TOperation;
  tmp,left,right: TEpObject;
begin
  pc := 0;
  while True do
  begin
    op := Bytecode.Code[pc];
    Inc(pc);
    case op.code of
      LOAD:
        frame.Push(bytecode.Variables[op.arg]);
      LOAD_CONST:
        frame.Push(bytecode.Constants[op.arg]);
      STORE_FAST:
        bytecode.Variables[op.arg] := frame.Pop;
      DISCARD_TOP:
        frame.Pop();
      
      (* jumps *)
      JMP_IF_FALSE:
        if (not frame.pop().AsBool) then pc := op.arg;
      JUMP,JMP_BACK,JMP_FORWARD:
        pc := op.arg;

      ASGN:
        begin
          right := frame.Pop();
          frame.Pop.ASGN(right, bytecode.Variables[op.arg]);
        end;
      RASGN:
        begin
          left := frame.Pop();
          left.ASGN(frame.Pop(), bytecode.Variables[op.arg]);
        end;

      BUILD_LIST:
        BuildList(bytecode.Variables[op.arg]);

      (* Inc/Dec operators *)
      UNARY_PREINC:
        begin
          frame.Pop.PREINC(bytecode.Variables[op.arg]);
          frame.Push(bytecode.Variables[op.arg]);
        end;
      UNARY_PREDEC:
        begin
          frame.Pop.PREDEC(bytecode.Variables[op.arg]);
          frame.Push(bytecode.Variables[op.arg]);
        end;
      UNARY_POSTINC:
        begin
          frame.Pop.POSTINC(bytecode.Variables[op.arg]);
          frame.Push(bytecode.Variables[op.arg]);
        end;
      UNARY_POSTDEC:
        begin
          frame.Pop.POSTDEC(bytecode.Variables[op.arg]);
          frame.Push(bytecode.Variables[op.arg]);
        end;

      (* inplace assignment operators *)
      INPLACE_ADD:
        begin
          right := frame.Pop();
          frame.Pop.INPLACE_ADD(right);
        end;
      INPLACE_SUB:
        begin
          right := frame.Pop();
          frame.Pop().INPLACE_SUB(right);
        end;
      INPLACE_MUL:
        begin
          right := frame.Pop();
          frame.Pop().INPLACE_MUL(right);
        end;
      INPLACE_DIV:
        begin
          right := frame.Pop();
          frame.Pop().INPLACE_DIV(right);
        end;

      (* unary operations *)
      UNARY_SUB:
        begin
          Frame.Top.UNARY_SUB(bytecode.Variables[op.arg]);
          Frame.SetTop(bytecode.Variables[op.arg]);
        end;
      UNARY_NOT:
        begin
          Frame.Top.LOGIC_NOT(bytecode.Variables[op.arg]);
          Frame.SetTop(bytecode.Variables[op.arg]);
        end;
      UNARY_BINV:
        begin
          Frame.Top.UNARY_INV(bytecode.Variables[op.arg]);
          Frame.SetTop(bytecode.Variables[op.arg]);
        end;

      (* arithmetic operations *)
      BIN_ADD:
        begin
          right := frame.Pop();
          frame.Top.add(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_SUB:
        begin
          right := frame.Pop();
          frame.Top.sub(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_MUL:
        begin
          right := frame.Pop();
          frame.Top.MUL(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_DIV:
        begin
          right := frame.Pop();
          frame.Top.IDIV(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_FDIV:
        begin
          right := frame.Pop();
          frame.Top.FDIV(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_MOD:
        begin
          right := frame.Pop();
          frame.Top.MODULO(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      
      (* equality operations *)
      BIN_EQ:
        begin
          right := frame.Pop();
          frame.Top.EQ(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_NE:
        begin
          right := frame.Pop();
          frame.Top.NE(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_LT:
        begin
          right := frame.Pop();
          frame.Top.LT(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_GT:
        begin
          right := frame.Pop();
          frame.Top.GT(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_LE:
        begin
          right := frame.Pop();
          frame.Top.LE(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_GE:
        begin
          right := frame.Pop();
          frame.Top.GE(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;

      (* logical operators *)
      BIN_AND:
        begin
          right := frame.Pop();
          frame.Top.LOGIC_AND(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_OR:
        begin
          right := frame.Pop();
          frame.Top.LOGIC_OR(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;

      (* bitwise *)
      BIN_BAND:
        begin
          right := frame.Pop();
          frame.Top.BAND(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_BOR:
        begin
          right := frame.Pop();
          frame.Top.BOR(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      BIN_BXOR:
        begin
          right := frame.Pop();
          frame.Top.BXOR(right, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;

      (* array like *)
      GET_ITEM:
        begin
          left := frame.Pop(); //index
          left.GET_ITEM(frame.Top, bytecode.Variables[op.arg]);
          frame.SetTop(bytecode.Variables[op.arg]);
        end;
      SET_ITEM:
        begin
          right := frame.Pop(); //value
          tmp   := frame.Pop(); //index
          frame.Pop.SET_ITEM(tmp, right);
        end;

      (* other *)
      CALL:
        begin
          tmp := Frame.Pop();
          (*if tmp is TNativeFuncObject then
          begin
            args := *pop arguments* (count = TNativeFuncObject(tmp).Parms)
            tmp := TNativeFuncObject(tmp).value(args);
            if tmp = nil then
              Frame.Push(bytecode.Constants[0])
            else
              Frame.Push(tmp);
          end else *)
          begin
            CallStack.Push(pc);
            pc := TFuncObject(tmp).pc;
          end;
        end;

      PRINT:
        begin
          tmp := frame.Pop();
          WriteLn(tmp.AsString);
        end;

      TIMENOW:
        begin
          tmp := TFloatObject.Create(MarkTime());
          Frame.Push(tmp);
        end;

      RETURN:
        begin
          if CallStack.StackPos >= 0 then
            CallStack.Reset(pc)
          else
            Exit;
        end
      else
        raise Exception.Create('Operation not implemented');
    end;
  end;
end;

(*
function Interpret(source:String): TFrame;
begin
  bc := CompileAST(Parse(source));
  Result := Frame(bc); //for tests and later introspection
  Execute(Result, bc);
end;*)

end.
