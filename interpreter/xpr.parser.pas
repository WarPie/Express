unit xpr.parser;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  A parser... Or, that's what it's ment to be :P
}
{$I express.inc}

interface

uses
  SysUtils,
  xpr.express,
  xpr.lexer,
  xpr.AST,
  xpr.errors;

const
  ATOM       = [tk_ident, tk_int, tk_float, tk_bool, tk_char, tk_string, tk_none];
  EXPRESSION = ATOM + [tk_sum, tk_bitwise, tk_paren, tk_square];
  SEPARATORS = [tk_newline, tk_semicolon];

type
  TParser = class(TObject)
  private
    FPos: Int32;
    FTokenizer: TTokenizer;

    FLooping:Int32;

 {flags}
    OldLineInsenstive, FLineInsenstive: Boolean;
  public
    constructor Create(T:TTokenizer);
    function Parse(): TBaseNode; inline;

    function DocPos: TDocPos; inline;
    function Current: TToken; inline;
    function Peek(n:Int32=1): TToken; inline;
    procedure SkipTokens(tokens:TTokenKindSet); inline;
    procedure SkipNewline; inline;

    function Next(IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken; inline;
    function NextIf(Value:String; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean; inline; overload;
    function NextIf(Token:TTokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean; inline; overload;
    function NextIf(Token:TToken; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean; inline; overload;

    procedure RaiseException(msg:string); inline;
    procedure RaiseExceptionFmt(msg:string; fmt:array of const); {inline;}

    procedure Expect(Token:TToken); inline; overload;
    procedure Expect(Token:TTokenKind); inline; overload;
    procedure Expect(Value:String); inline; overload;
    procedure Expect(Values:array of String); overload;
    function ExpectAndInc(Token:TToken; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken; inline; overload;
    function ExpectAndInc(Token:TTokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken; inline; overload;
    function ExpectAndInc(Value:String; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken; inline; overload;
    function ExpectAndInc(Values:array of String; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken; overload;

    function OperatorPrecedence(): Int8; inline;
    function OperatorAssoc(): Int8; inline;

    function IS_UNARY(): Boolean; inline;
    function IS_ENCLOSURE(): Boolean; inline;

    function ParseReturn(): TReturn; inline;
    function ParseContinue(): TContinue; inline;
    function ParseBreak(): TBreak; inline;

    function ParseFunction(): TFunction;
    function ParseCall(callable: TBaseNode): TCall;
    function ParseIndex(indexable:TBaseNode): TIndex;
    function ParseIf(): TIf;
    function ParseWhile(): TWhile;
    function ParseRepeat(): TRepeat;
    function ParseFor(): TFor;
    function ParsePrint(): TPrint;

    function ParseVardecl: TVarDecl;
    function ParseListEnclosure(): TBaseNode;
    function ParsePostPrimary(PrimExpr:TBaseNode): TBaseNode; inline;
    function ParseAtom(): TBaseNode; inline;
    function ParsePrimary(): TBaseNode; inline;
    function RHSExpr(Left:TBaseNode; leftPrecedence:Int8=0): TBaseNode;
    function ParseExpression(ExpectSeparator:Boolean=True): TBaseNode;
    function ParseExpressionList(Insensitive:Boolean; AllowEmpty:Boolean=False): TNodeArray;
    function ParseVarList(Insensitive:Boolean): TVarArray;
    function ParseStatement: TBaseNode;
    function ParseStatements(EndKeywords:array of string; Increase:Boolean=False): TNodeArray;
  end;

function Parse(Tokenizer:TTokenizer): TBaseNode;

implementation

uses
  xpr.utils;

function Parse(Tokenizer:TTokenizer): TBaseNode;
var
  Parser:TParser;
begin
  Parser := TParser.Create(Tokenizer);
  Result := Parser.Parse();
  Parser.Free();
end;


constructor TParser.Create(T:TTokenizer);
begin
  FTokenizer := T;
  FPos := 0;
  FLooping := 0;
  FLineInsenstive := False;
end;

function TParser.Parse(): TBaseNode;
begin
  Result := TBlock.Create(ParseStatements([]), DocPos);
end;

{==============================================================================]
  The basics
[==============================================================================}
function TParser.DocPos: TDocPos;
begin
  if FPos = 0 then
    Result := FTokenizer.Tokens[0].DocPos
  else
    Result := FTokenizer.Tokens[FPos-1].DocPos;
end;

function TParser.Current: TToken;
begin
  Result := FTokenizer.Tokens[FPos];
end;

function TParser.Peek(n:Int32=1): TToken;
begin
  Result := FTokenizer.Tokens[FPos+n];
end;

procedure TParser.SkipTokens(tokens:TTokenKindSet);
begin
  while(Current.Token in Tokens) do Inc(FPos);
end;

procedure TParser.SkipNewline;
begin
  while(Current.Token = tk_newline) do Inc(FPos);
end;

function TParser.Next(IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken;
begin
  {$IFDEF loose_semicolon}if FLineInsenstive then SkipNewline;{$ENDIF}
  if IncOrder = PostInc then
  begin
    Result := FTokenizer.Tokens[FPos];
    Inc(FPos, Increment);
  end else
  begin
    Inc(FPos, Increment);
    Result := FTokenizer.Tokens[FPos];
  end;
end;

function TParser.NextIf(Value:String; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean;
begin
  {$IFDEF loose_semicolon}if FLineInsenstive then SkipNewline;{$ENDIF}
  Result := Peek(Ord(IncOrder)).value = Value;
  if Result then Inc(FPos, Increment);
end;

function TParser.NextIf(Token:TTokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean;
begin
  {$IFDEF loose_semicolon}if FLineInsenstive then SkipNewline;{$ENDIF}
  Result := Peek(Ord(IncOrder)).token = Token;
  if Result then Inc(FPos, Increment);
end;

function TParser.NextIf(Token:TToken; IncOrder:EIncOrder=PostInc; Increment:Int32=1): Boolean;
begin
  {$IFDEF loose_semicolon}if FLineInsenstive then SkipNewline;{$ENDIF}
  Result := Peek(Ord(IncOrder)) = Token;
  if Result then Inc(FPos, Increment);
end;

{==============================================================================]
  Error handling rutines
[==============================================================================}
procedure TParser.RaiseException(msg:string);
begin
  xpr.errors.RaiseException(eSyntaxError, msg, DocPos);
end;

procedure TParser.RaiseExceptionFmt(msg:string; fmt:array of const);
begin
  try
    xpr.errors.RaiseExceptionFmt(eSyntaxError, msg, fmt, DocPos);
  except
    on e:SyntaxError do
      raise SyntaxError.Create(e.Message) at get_caller_addr(get_frame);
  end;
end;

procedure TParser.Expect(Token:TToken);
begin
  if Token <> Current then
    RaiseExceptionFmt(eExpectedButFound, [Token.ToString(), Current.ToString]);
end;

procedure TParser.Expect(Token:TTokenKind);
begin
  if Token <> Current.Token then
    RaiseExceptionFmt(eExpectedButFound, [TkToString(Token), Current.ToString]);
end;

procedure TParser.Expect(Value:String);
begin
  if (Current.Token = tk_string) or (Value <> Current.Value) then
    RaiseExceptionFmt(eExpectedButFound, [Value, Current.ToString]);
end;

procedure TParser.Expect(Values:array of String);
begin
  if (Current.Token = tk_string) or (not StringContains(Values, Current.Value)) then
    RaiseExceptionFmt(eExpectedButFound, [Values[0], Current.ToString]);
end;

function TParser.ExpectAndInc(Token:TToken; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken;
begin
  {$IFDEF loose_semicolon}if FLineInsenstive then SkipNewline;{$ENDIF}
  Result := Next(IncOrder, Increment);
  if Token <> Result then
    RaiseExceptionFmt(eExpectedButFound, [Token.ToString(), Result.ToString]);
end;

function TParser.ExpectAndInc(Token:TTokenKind; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken;
begin
  {$IFDEF loose_semicolon}if FLineInsenstive then SkipNewline;{$ENDIF}
  Result := Next(IncOrder, Increment);
  if Token <> Result.Token then
    RaiseExceptionFmt(eExpectedButFound, [TkToString(Token), Result.ToString]);
end;

function TParser.ExpectAndInc(Value:String; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken;
begin
  {$IFDEF loose_semicolon}if FLineInsenstive then SkipNewline;{$ENDIF}
  Result := Next(IncOrder, Increment);
  if (Result.Token = tk_string) or (Value <> Result.Value) then
    RaiseExceptionFmt(eExpectedButFound, [Value, Result.ToString]);
end;

function TParser.ExpectAndInc(Values:array of String; IncOrder:EIncOrder=PostInc; Increment:Int32=1): TToken;
begin
  {$IFDEF loose_semicolon}if FLineInsenstive then SkipNewline;{$ENDIF}
  Result := Next(IncOrder, Increment);
  if (Result.Token = tk_string) or (not StringContains(Values, Result.Value)) then
    RaiseExceptionFmt(eExpectedButFound, [Values[0], Result.ToString]);
end;

{==============================================================================]
  Operator information
[==============================================================================}
function TParser.OperatorPrecedence(): Int8;
var
  def:TOperatorPrecedence = (prec:-1; assoc:0);
begin
  Result := PrecedenceMap.GetDef(Current.Value, def).prec;
end;

function TParser.OperatorAssoc(): Int8;
var
  def:TOperatorPrecedence = (prec:0; assoc:1);
begin
  Result := PrecedenceMap.GetDef(Current.Value, def).assoc;
end;

function TParser.IS_UNARY: Boolean;
var
  def:TOperatorPrecedence = (prec:-1; assoc:0);
begin
  Result := UnaryPrecedenceMap.GetDef(Current.Value, def).prec <> -1;
end;

function TParser.IS_ENCLOSURE: Boolean;
begin
  Result := (Current.Token = tk_square) and (Current.Value = '[');
end;

{==============================================================================]
  Here starts the actual parsing.. :)
[==============================================================================}

function TParser.ParseReturn(): TReturn;
begin
  ExpectAndInc('return');
  Result := TReturn.Create(ParseExpression(True), DocPos);
end;

function TParser.ParseContinue(): TContinue;
begin
  ExpectAndInc('continue');
  SkipTokens(SEPARATORS);
  Result := TContinue.Create(DocPos);
end;

function TParser.ParseBreak(): TBreak;
begin
  ExpectAndInc('break');
  SkipTokens(SEPARATORS);
  Result := TBreak.Create(DocPos);
end;

//print ...
function TParser.ParsePrint(): TPrint;
var exprs:TNodeArray;
begin
  ExpectAndInc('print');
  exprs := ParseExpressionList(False,False);
  Result := TPrint.Create(exprs, DocPos);
  SkipTokens(SEPARATORS);
end;

function TParser.ParseFunction(): TFunction;
var
  Name:TToken;
  Params:TVarArray = nil;
  Prog:TBlock;
begin
  ExpectAndInc('func');
  Name := ExpectAndInc(tk_ident);

  ExpectAndInc(lparen);
  Params := ParseVarList(True);
  ExpectAndInc(rparen);

  if Current = Token(tk_assign, '=>')  then
  begin
    Next;
    Prog := TBlock.Create(TReturn.Create(ParseExpression(True), DocPos), DocPos);
  end
  else
  begin
    SkipTokens(SEPARATORS);
    Prog := TBlock.Create(ParseStatements(['end'], True), DocPos);
  end;

  Result := TFunction.Create(Name.Value, Params, Prog, DocPos);
end;

function TParser.ParseCall(callable:TBaseNode): TCall;
var
  args:TNodeArray=nil;
begin
  ExpectAndInc(lparen);
  Args := ParseExpressionList(True, True);
  ExpectAndInc(rparen);
  Result := TCall.Create(callable, args, DocPos);
end;

function TParser.ParseIndex(indexable:TBaseNode): TIndex;
var
  args:TNodeArray;
begin
  ExpectAndInc(lsquare);
  OldLineInsenstive := FLineInsenstive;
  FLineInsenstive := True;
  while not NextIf(rsquare) do
  begin
    SetLength(args, Length(args) + 1);
    Args[high(args)] := ParseExpression(False);

    if NextIf(rsquare) then Break;
    ExpectAndInc(tk_comma);
  end;
  FLineInsenstive := OldLineInsenstive;
  Result := TIndex.Create(indexable, args, DocPos);
end;

(*IF STATEMENT:
  > if condition then <stmts> [end | else <stmts> end]
  > if (condition) <stmt> [else <stmt>]
*)
function TParser.ParseIf(): TIf;
var
  Condition: TBaseNode;
  Body, ElseBody: TBlock;
  has_parenthesis:Boolean;
begin
  ExpectAndInc('if');
  has_parenthesis := NextIf(lparen, PostInc);
  Condition := ParseExpression(False);
  if has_parenthesis then
    ExpectAndInc(rparen)
  else
    Expect(keyword('then'));

  ElseBody := nil;
  if NextIf(keyword('then')) then
  begin
    Body := TBlock.Create(ParseStatements(['end','else'], False), DocPos);
    if Next() = keyword('else') then
      ElseBody := TBlock.Create(ParseStatements(['end'], True), DocPos);
  end
  else
  begin
    Body := TBlock.Create(self.ParseStatement(), DocPos);
    if NextIf(keyword('else')) then
      ElseBody := TBlock.Create(self.ParseStatement(), DocPos);
  end;

  Result := TIf.Create(Condition, Body, ElseBody, DocPos);
end;

(*WHILE LOOP:
  > while condition do <stmts> end
  > while (condition) <stmt>
*)
function TParser.ParseWhile(): TWhile;
var
  Condition:TBaseNode;
  body:TBlock;
  has_parenthesis:Boolean;
begin
  ExpectAndInc('while');
  has_parenthesis := NextIf(lparen, PostInc);
  Condition := ParseExpression(False);
  if has_parenthesis then
    ExpectAndInc(rparen)
  else
    Expect(keyword('do'));

  Inc(FLooping);
  if NextIf(keyword('do')) then
    body := TBlock.Create(ParseStatements(['end'], True), DocPos)
  else
    body := TBlock.Create(self.ParseStatement(), DocPos);

  Result := TWhile.Create(Condition, body, DocPos);
  Dec(FLooping);
end;

(*REPEAT LOOP:
  > repeat <stmts> until(<condition>)
*)
function TParser.ParseRepeat(): TRepeat;
var
  condition:TBaseNode;
  body:TBlock;
begin
  ExpectAndInc('repeat');
  Inc(FLooping);
  body := TBlock.Create(ParseStatements(['until'], True), DocPos);
  Dec(FLooping);

  ExpectAndInc(lparen, PostInc);
  Condition := ParseExpression(False);
  ExpectAndInc(rparen, PostInc);

  Result := TRepeat.Create(condition, body, DocPos);
end;

(*FOR LOOP:
  > for <expr>; <condition>; <expr> do <stmts> end
  > for(<expr>; <condition>; <expr>) <stmt>
*)
function TParser.ParseFor(): TFor;
var
  stmt1,stmt2,stmt3,body:TBaseNode;
  has_parenthesis:Boolean;
begin
  ExpectAndInc('for');
  has_parenthesis := NextIf(lparen, PostInc);

  if current = Keyword('var') then
  begin
    stmt1 := ParseVardecl;
    ExpectAndInc(tk_semicolon);
  end
  else
    stmt1 := ParseExpression(True);
  stmt2 := ParseExpression(True);
  stmt3 := ParseExpression(False);
  if stmt3 <> nil then stmt3 := TStatement.Create(stmt3, DocPos);

  if has_parenthesis then
    ExpectAndInc(rparen)
  else
    Expect(Keyword('do'));

  Inc(FLooping);
  if NextIf(keyword('do')) then
    body := TBlock.Create(ParseStatements(['end'], True), DocPos)
  else
    body := TBlock.Create(ParseStatement(), DocPos);

  Result := TFor.Create(stmt1,stmt2,stmt3, body as TBlock, DocPos);
  Dec(FLooping);
end;


//meh
function TParser.ParseVardecl: TVarDecl;
var
  Right:TBaseNode;
  Left:TVarArray;
begin
  ExpectAndInc('var');
  Left := ParseVarList(False);
  Right := nil;
  if Current = Token(tk_assign, ':=') then
  begin
    Next;
    Right := ParseExpression(False);
  end;
  Result := TVarDecl.Create(left, right, DocPos)
end;

function TParser.ParseListEnclosure(): TBaseNode;
var
  args:TNodeArray;
begin
  ExpectAndInc(lsquare);
  args := ParseExpressionList(True,True);
  ExpectAndInc(rsquare);
  Result := TListExpr.Create(args, docpos);
end;

function TParser.ParseAtom(): TBaseNode;
begin
  Result := nil;
  case Current.token of
    tk_ident:
      Result := TVariable.Create(Current.value, DocPos);
    tk_none:
      Result := TConstNone.Create(DocPos);
    tk_bool:
      Result := TConstBool.Create(Current.value, DocPos);
    tk_char:
      Result := TConstChar.Create(Current.value, DocPos);
    tk_int:
      Result := TConstInt.Create(Current.value, DocPos);
    tk_float:
      Result := TConstFloat.Create(Current.value, DocPos);
    tk_string:
      Result := TConstString.Create(Current.value, DocPos);
    tk_square:
      Exit(ParseListEnclosure());
    else
      RaiseException(eUnexpected);
  end;
  Next();
end;

function TParser.ParsePostPrimary(PrimExpr:TBaseNode): TBaseNode;
begin
  while ((Current = lparen) or (Current = lsquare)) do
  begin
    while (Current = lparen) do  PrimExpr := ParseCall(PrimExpr);
    while (Current = lsquare) do PrimExpr := ParseIndex(PrimExpr);
  end;

  if Current.Token = tk_sum then
    if Current.Value = '++' then
      PrimExpr := TUnaryOp.Create(Next(PostInc), PrimExpr, True, DocPos)
    else if Current.Value = '--' then
      PrimExpr := TUnaryOp.Create(Next(PostInc), PrimExpr, True, DocPos);

  Result := PrimExpr;
end;

function TParser.ParsePrimary: TBaseNode;
var op:TToken;
begin
  if IS_UNARY then
  begin
    op := Next(PostInc);
    Result := TUnaryOp.Create(op, ParsePrimary(), False, DocPos)
  end
  else if Current = keyword('time') then
  begin
    Next();
    Result := TTimeNow.Create(DocPos);
  end
  else if (Current.Token in ATOM) or IS_ENCLOSURE then
  begin
    Result := ParseAtom();
  end
  else if Current = lparen then
  begin
    Next();
    OldLineInsenstive := FLineInsenstive;
    FLineInsenstive := True;
    Result := ParseExpression(False);
    ExpectAndInc(rparen, PostInc);
    FLineInsenstive := OldLineInsenstive;
  end
  else
    Result := nil;

  if Result <> nil then
    Result := ParsePostPrimary(Result);
end;


function TParser.RHSExpr(Left:TBaseNode; leftPrecedence:Int8=0): TBaseNode;
var
  precedence,nextPrecedence,assoc:Int8;
  Right: TBaseNode;
  op:TToken;

  function IfExpr(Left,Right:TBaseNode): TIfExpression; inline;
  var Condition:TBaseNode;
  begin
    Condition := Right;
    ExpectAndInc(Keyword('else'));
    Right := ParseExpression(False);
    Result := TIfExpression.Create(Condition,Left,Right, DocPos);
  end;

  function Operation(op:TToken; Left,Right:TBaseNode): TBaseNode; inline;
  begin
    if op.token = tk_assign then
      Result := TAssignment.Create(op, Left, Right, DocPos)
    else if op = Keyword('if') then
      Result := IfExpr(Left, Right)
    else
      Result := TBinOp.Create(op, left, right, DocPos);
  end;

begin
  while True do
  begin
    precedence := OperatorPrecedence;
    if precedence < leftPrecedence then
      Exit(Left);

    op := Next(PostInc);
    right := ParsePrimary();
    if right = nil then RaiseException(eInvalidExpression);

    nextPrecedence := OperatorPrecedence;
    if precedence < nextPrecedence then
      Right := RHSExpr(right, precedence+1)
    else if precedence = nextPrecedence then
    begin
      assoc := precedence + OperatorAssoc;
      Right := RHSExpr(right, assoc);
    end;
    Left := Operation(op, left, right);
  end;
  Result := Left;
end;

function TParser.ParseExpression(ExpectSeparator:Boolean=True): TBaseNode;
begin
  Result := ParsePrimary();
  if (Result <> nil) then
    Result := RHSExpr(Result);

  {$IFDEF loose_semicolon}
  OldLineInsenstive := FLineInsenstive;
  FLineInsenstive := False;
  if NextIf(tk_newline) then Exit;
  FLineInsenstive := OldLineInsenstive;
  {$ENDIF}

  if (ExpectSeparator) then
    ExpectAndInc(tk_semicolon, PostInc);
end;

function TParser.ParseExpressionList(Insensitive:Boolean; AllowEmpty:Boolean=False): TNodeArray;
var
  expr: TBaseNode;
  top: Int32;
begin
  if Insensitive then
  begin
    OldLineInsenstive := FLineInsenstive;
    FLineInsenstive := True;
  end;
  top := 0;
  while True do
  begin
    expr := ParseExpression(False);
    if (expr = nil) then
    begin
      if (not AllowEmpty) then RaiseException(eInvalidExpression);
      break;
    end;
    SetLength(Result, top + 1);
    Result[top] := expr;
    Inc(top);
    if not NextIf(tk_comma) then
      break;
  end;

  if Insensitive then
    FLineInsenstive := OldLineInsenstive;
end;

function TParser.ParseVarList(Insensitive:Boolean): TVarArray;
var top: Int32;
begin
  if Insensitive then
  begin
    OldLineInsenstive := FLineInsenstive;
    FLineInsenstive := True;
  end;
  top := 0;
  while Current.token = tk_ident do
  begin
    SetLength(Result, top + 1);
    Result[top] := TVariable.Create(Current.Value, DocPos);
    Inc(top);
    if (Next(PreInc).Token = tk_comma) then Next() else break;
  end;

  if Insensitive then
    FLineInsenstive := OldLineInsenstive;
end;

function TParser.ParseStatement: TBaseNode;
begin
  Result := nil;
  {$IFDEF loose_semicolon}SkipNewline;{$ENDIF}

  if Current.token in EXPRESSION then
  begin
    Result := ParseExpression(True);
    if not(Result is TAssignment) then
      Result := TStatement.Create(Result, DocPos);
  end
  else if(Current.token = tk_keyword) then
  begin
    if (Current.value = 'var') then
      Result := ParseVardecl()
    else if (Current.value = 'if') then
      Result := ParseIf()
    else if (Current.value = 'while') then
      Result := ParseWhile()
    else if (Current.value = 'repeat') then
      Result := ParseRepeat()
    else if (Current.value = 'for') then
      Result := ParseFor()

    //else if (Current.value = 'repeat') then
    //  result := ParseRepeat()
    else if (Current.value = 'continue') then
      if FLooping = 0 then
        RaiseExceptionFmt(eNotAllowedOutsideLoop, ['`continue`'])
      else
        Result := ParseContinue()
    else if (Current.value = 'break') then
      if FLooping = 0 then
        RaiseExceptionFmt(eNotAllowedOutsideLoop, ['`break`'])
      else
        Result := ParseBreak()
    (*
    else if (Current.value = 'pass') then
      Result := ParsePass()  *)
    else if (Current.value = 'return') then
      Result := ParseReturn()
    else if (Current.value = 'func') then
      Result := ParseFunction()
    else if (Current.value = 'print') then
      Result := ParsePrint()
    else
      RaiseExceptionFmt(eUnexpectedKeyword, [current.Value]);
  end
  else if not(Current.token in SEPARATORS) then
    RaiseExceptionFmt(eUnexpectedOperation, [current.ToString]);

  {$IFDEF loose_semicolon}
    SkipTokens(SEPARATORS);
  {$ELSE}
    NextIf(tk_semicolon);
  {$ENDIF}
end;

function TParser.ParseStatements(EndKeywords:array of string; Increase:Boolean=False): TNodeArray;
var
  prim:TBaseNode;
begin
  SetLength(Result, 0);

  while (Current.Token <> tk_unknown) and (not StringContains(EndKeywords, Current.value)) do
  begin
    prim := self.ParseStatement();
    {$IFDEF loose_semicolon}SkipNewline;{$ENDIF}

    if prim <> nil then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := prim;
    end;
  end;

  if Length(EndKeywords) <> 0 then
    Expect(EndKeywords);

  if Increase then
    Next;
end;

end.
