{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  A straight forward tokenizer
}
unit lexer;
{$I express.inc}

interface

uses
  Classes, SysUtils, dictionary, express;

type
  (*
    Tokenizer, used to turn text into an array of TTokens
    It's not the most efficient implementation, nor the most "easy",
    it's something in between.
  *)
  TTokenizer = record
  private
    FArrHigh: Int32;
  public
    Data: String;
    Pos,lineStart: Int32;
    Tokens: TTokenArray;
    DocPos: TDocPos;

    function Next(): Char; inline;
    function Peek(n:Int32=1): Char; inline;
    function Current: Char; inline;
    function Prev: Char; inline;
    function Next_CheckNewline: Char; inline;

    procedure Append(token:TTokenKind; value:string=''); inline;
    procedure AppendInc(token:TTokenKind; value:string=''; n:Int32=1); inline;
    procedure Extend(t:TTokenizer); inline;
    procedure AddToken(cases:array of string; token:TTokenKind);
    procedure AddIdent(); inline;
    procedure AddNumber(); inline;
    procedure AddChar(); inline;
    procedure AddString(); inline;
    procedure HandleComment(); inline;
    procedure Tokenize(expr:String);
  end;

  (*
    Types needed to store reserved words
  *)
  TKeywordMap = specialize TDictionary<string, TTokenKind>;

  (*
    Types needed to store operator precedence, and associativity
  *)
  TOperatorPrecedence = record
    prec, assoc: Int8;
  end;
  TOperatorPrecedenceMap = specialize TDictionary<string, TOperatorPrecedence>;

const
  lparen: TToken = (token:tk_paren;  value:'(');
  rparen: TToken = (token:tk_paren;  value:')');
  lsquare:TToken = (token:tk_square; value:'[');
  rsquare:TToken = (token:tk_square; value:']');
  lbrace: TToken = (token:tk_paren;  value:'{');
  rbrace: TToken = (token:tk_paren;  value:'}');

var
  KeywordMap: TKeywordMap;
  PrecedenceMap, UnaryPrecedenceMap: TOperatorPrecedenceMap;

function Tokenize(script:string): TTokenizer;
function Keyword(kw:string): TToken; inline;
function TkToString(x:TTokenKind): string; inline;

procedure TestLexer(fileName:string; printTokens:Boolean; nRounds:Int32=5);

implementation

uses
  utils;

type
  TReservedName = record
    name: string;
    tok: TTokenKind;
  end;

  TOperatorValue = record
    op: string;
    prec: Int8;
    assoc: Int8;
  end;

const
  ReservedWords: array [0..27] of TReservedName = (
    (name:'break';    tok:tk_keyword),
    (name:'case';     tok:tk_keyword),
    (name:'continue'; tok:tk_keyword),
    (name:'do';       tok:tk_keyword),
    (name:'else';     tok:tk_keyword),
    (name:'elif';     tok:tk_keyword),
    (name:'end';      tok:tk_keyword),
    (name:'for';      tok:tk_keyword),
    (name:'func';     tok:tk_keyword),
    (name:'if';       tok:tk_keyword),
    (name:'in';       tok:tk_keyword),
    (name:'time';     tok:tk_keyword),
    (name:'print';    tok:tk_keyword),
    (name:'repeat';   tok:tk_keyword),
    (name:'return';   tok:tk_keyword),
    (name:'then';     tok:tk_keyword),
    (name:'var';      tok:tk_keyword),
    (name:'while';    tok:tk_keyword),

    (name:'None';     tok:tk_None),
    (name:'True';     tok:tk_bool),
    (name:'False';    tok:tk_bool),

    (name:'div';      tok:tk_fact),
    (name:'shl';      tok:tk_bitwise),
    (name:'shr';      tok:tk_bitwise),
    (name:'in';       tok:tk_logical),
    (name:'and';      tok:tk_logical),
    (name:'or';       tok:tk_logical),
    (name:'not';      tok:tk_logical)
  );


  BinPrecedence: array [0..28] of TOperatorValue = (
    (op:':=';    prec:1;   assoc:-1),
    (op:'+=';    prec:1;   assoc:-1),
    (op:'-=';    prec:1;   assoc:-1),
    (op:'*=';    prec:1;   assoc:-1),
    (op:'/=';    prec:1;   assoc:-1),
    (op:'&=';    prec:1;   assoc:-1),
    (op:'|=';    prec:1;   assoc:-1),
    (op:'^=';    prec:1;   assoc:-1),

    (op:'and';   prec:2;   assoc:-1),
    (op:'or';    prec:2;   assoc:-1),

    (op:'=';     prec:3;   assoc:1),
    (op:'!=';    prec:3;   assoc:1),
    (op:'<';     prec:4;   assoc:1),
    (op:'>';     prec:4;   assoc:1),
    (op:'<=';    prec:4;   assoc:1),
    (op:'>=';    prec:4;   assoc:1),

    (op:'|';     prec:5;   assoc:1),
    (op:'&';     prec:5;   assoc:1),
    (op:'^';     prec:5;   assoc:1),

    (op:'in';    prec:6;   assoc:1),
    (op:'+';     prec:6;   assoc:1),
    (op:'-';     prec:6;   assoc:1),
    (op:'*';     prec:7;   assoc:1),
    (op:'div';   prec:7;   assoc:1),
    (op:'/';     prec:7;   assoc:1),
    (op:'%';     prec:7;   assoc:1),
    (op:'shl';   prec:7;   assoc:1),
    (op:'shr';   prec:7;   assoc:1),
    (op:'**';    prec:8;   assoc:1)
  );

  UnaryPrecedence: array [0..5] of TOperatorValue = (
    (op:'++';   prec:0;  assoc:0),
    (op:'--';   prec:0;  assoc:0),
    (op:'+';    prec:0;  assoc:0),
    (op:'-';    prec:0;  assoc:0),
    (op:'~';    prec:0;  assoc:0),
    (op:'not';  prec:0;  assoc:0)
  );


function Keyword(kw:string): TToken;
begin
  Result.Token := tk_keyword;
  Result.Value := kw;
end;

function TkToString(x:TTokenKind): string;
var str:String;
begin
  WriteStr(str, x);
  Result := str;
end;

function Tokenize(script:string): TTokenizer;
begin
  Result.Pos := 0;
  Result.Tokenize(script);
end;


function TTokenizer.Next(): Char;
begin
  Result := data[pos+1];
  Inc(pos);
end;

function TTokenizer.Peek(n:Int32=1): Char;
begin
  Result := data[pos+n];
end;

function TTokenizer.Current: Char;
begin
  Result := data[pos];
end;

function TTokenizer.Prev: Char;
begin
  if pos-1 >= 0 then
    Result := data[pos-1]
  else
    Result := #0;
end;

function TTokenizer.Next_CheckNewline: Char;
begin
  if (Current+Peek() = #13#10) then
  begin
    Inc(DocPos.line);
    Inc(pos);
    lineStart := pos+1;
  end else if (Current = #10) or (Current = #13) then
  begin
    Inc(DocPos.line);
    lineStart := pos+1;
  end;

  Inc(pos);
  Result := data[pos];
end;

procedure TTokenizer.Append(token:TTokenKind; value:String='');
begin
  if FArrHigh >= Length(tokens) then
    SetLength(tokens, 2 * Length(tokens));
  tokens[FArrHigh].value := value;
  tokens[FArrHigh].token := token;
  tokens[FArrHigh].docpos := DocPos;
  Inc(FArrHigh);
end;

procedure TTokenizer.AppendInc(token:TTokenKind; value:String=''; n:Int32=1);
var i:Int32;
begin
  if FArrHigh >= Length(tokens) then
    SetLength(tokens, 2 * Length(tokens));
  tokens[FArrHigh].value := value;
  tokens[FArrHigh].token := token;
  tokens[FArrHigh].docpos := DocPos;
  Inc(FArrHigh);
  
  for i:=0 to n-1 do
    Next();
end;


procedure TTokenizer.Extend(t:TTokenizer);
var i:Int32;
begin
  for i:=0 to t.FArrHigh-1 do
    self.Append(t.tokens[i].token,t.tokens[i].value);
end;


procedure TTokenizer.AddToken(cases:array of string; token:TTokenKind);
var i:Int32;
begin
  for i:=0 to High(cases) do
    if Slice(data,pos,pos+Length(cases[i])-1) = cases[i] then
    begin
      self.Append(token,cases[i]);
      Inc(pos, Length(cases[i]));
      Exit;
    end;
  raise Exception.Create('Undefined symbol');
end;


procedure TTokenizer.AddIdent();
var
  i:Int32;
  tmp:String;
  tok:TTokenKind;
begin
  i := pos;
  Inc(pos);
  while Current in ['a'..'z','A'..'Z','_','0'..'9'] do Inc(pos);
  tmp := Slice(data, i, pos-1);
  tok := KeywordMap.GetDef(tmp, tk_ident);
  self.Append(tok, tmp);
end;


procedure TTokenizer.AddNumber();
var
  i:Int32;
begin
  i := pos;
  Inc(pos);
  while (self.Current in ['0'..'9',#32]) do Inc(pos);

  if self.Current = '.' then
  begin
    Next();
    while self.Current in ['0'..'9',#32] do Inc(pos);
    self.Append(tk_float, StringReplace(Slice(data,i,pos-1), #32, '', [rfReplaceAll]));
  end else
    self.Append(tk_int, StringReplace(Slice(data,i,pos-1), #32, '', [rfReplaceAll]));
end;

procedure TTokenizer.AddChar();
var
  i:Int32;
begin
  i := pos;
  Inc(pos);
  while self.Current in ['0'..'9'] do Inc(pos);
  self.Append(tk_char, chr(StrToInt(Slice(data,i+1,pos-1))));
end;


procedure TTokenizer.AddString();
var
  i:Int32;
begin
  i := pos;
  Inc(pos);
  while (Current <> data[i]) and (Current <> #0) do Next_CheckNewline;
  self.Append(tk_string, Slice(data,i+1,pos-1));
  Inc(pos);
end;


procedure TTokenizer.HandleComment();
begin
  if data[pos] = '/' then
  begin
    while not(Current in [#10,#13,#0]) do 
      Inc(pos);
  end else
  begin
    Inc(pos);
    while (Current <> #0) and (Current+Peek <> '*)') do
      Next_CheckNewline;
    Inc(pos, 2);
  end;
end;

procedure TTokenizer.Tokenize(expr:String);
begin
  SetLength(tokens, 1);
  FArrHigh := 0;
  data := Expr + #0#0;
  pos  := 1;

  //DocPos.filename := '__main__';
  DocPos.Line := 1;
  lineStart   := 0;

  while data[pos] <> #0 do
  begin
    DocPos.column := Pos - lineStart;
    case data[pos] of
      #10:
        begin
          {$IFDEF loose_semicolon}
            self.AppendInc(tk_newline, '', 1);
          {$ELSE}
            Next;
          {$ENDIF}
          Inc(DocPos.line);
          lineStart := pos;
        end;
      #1..#9: Next;
      #11..#32: Next;
      ';': self.AppendInc(tk_semicolon, data[pos], 1);
      '.': self.AppendInc(tk_dot, data[pos], 1);
      ',': self.AppendInc(tk_comma, data[pos], 1);
      ':':
        if Peek() = '=' then
          self.AppendInc(tk_assign, data[pos]+data[pos+1], 2)
        else
          self.AppendInc(tk_colon, data[pos], 1);
      '<','>','=','!':
        self.AddToken(['<=','>=','!=','>','<','='], tk_cmp);
      '&','|','^','~': 
        if Peek() = '=' then
          self.AppendInc(tk_assign, data[pos]+data[pos+1], 2)
        else
          self.AddToken(['&','|','^','~'], tk_bitwise);
      '+','-':
        if Peek() = '=' then
          self.AppendInc(tk_assign, data[pos]+'=', 2)
        else
          self.AddToken(['++','--','+','-'], tk_sum);
      '/','*','%':
        if Peek() = '=' then
          self.AppendInc(tk_assign, data[pos]+data[pos+1], 2)
        else if (Current() = '/') and (Peek() = '/') then
          self.HandleComment()
        else
          self.AddToken(['/','*','%'], tk_fact);
      '(',')':
        if Peek() = '*' then
          self.HandleComment()
        else
          self.AddToken(['(',')'], tk_paren);
      '[',']':
        self.AddToken(['[',']'], tk_square);
      '{','}':
        self.AddToken(['{','}'], tk_brace);
      'a'..'z','A'..'Z','_':
        self.AddIdent();
      '0'..'9':
        self.AddNumber();
      '#':
        self.AddChar();
      #34,#39:
        self.AddString();
      #0:
        break;
      else
        raise Exception.Create('Invalid symbol "'+data[pos]+'" / #'+IntToStr(Ord(data[pos])));
    end;
  end;

  Self.Append(tk_unknown,'');
  SetLength(tokens, FArrHigh);
end;


procedure TestLexer(fileName:String; printTokens:Boolean; nRounds:Int32=5);
var
  t:TTokenizer;
  i:Int32;
  d,best,sum:Double;
  doc:String;

  f:TStringList;
begin
  best := $FFFFFF;

  f := TStringList.Create();
  f.LoadFromFile(fileName);
  doc := f.Text;
  f.Free();

  sum := 0;
  for i:=0 to nRounds do
  begin
    d := MarkTime();
    t.Tokenize(doc);
    d := MarkTime() - d;
    if d < best then best := d;
    sum += d;
  end;
  WriteLn(Format('best: %.4f ms | sum: %.4f ms',[best,sum]));
  WriteLn('------------------------------------');

  if printTokens then
    for i:=0 to High(t.tokens) do
      WriteLn(t.Tokens[i].ToString());
end;


procedure InitModule();
var
  i:Int32;
  myOperator:TOperatorPrecedence;
begin
  (* load keywords *)
  KeywordMap := TKeywordMap.Create(@HashStr);
  for i:=0 to High(ReservedWords) do
    KeywordMap.Add(ReservedWords[i].name, ReservedWords[i].tok);

  (* load operators: Assoc, and Precedence *)
  PrecedenceMap := TOperatorPrecedenceMap.Create(@HashStr);
  for i:=0 to High(BinPrecedence) do
  begin
    myOperator.assoc := BinPrecedence[i].assoc;
    myOperator.prec  := BinPrecedence[i].prec;
    PrecedenceMap.Add(BinPrecedence[i].op, myOperator);
  end;

  UnaryPrecedenceMap := TOperatorPrecedenceMap.Create(@HashStr);
  for i:=0 to High(UnaryPrecedence) do
  begin
    myOperator.assoc := UnaryPrecedence[i].assoc;
    myOperator.prec  := UnaryPrecedence[i].prec;
    UnaryPrecedenceMap.Add(UnaryPrecedence[i].op, myOperator);
  end;
end;

initialization
  InitModule();

end.
