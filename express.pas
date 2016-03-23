{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  General stuff
}
unit express;
{$I express.inc}

interface

uses
  SysUtils;

type
  // objects uses these
  PBoolean = Boolean;
  PepInt = ^epInt;
  epInt  = Int64;
  PepChar  = ^epChar;
  epChar   = Char;
  epString = String;
  //
  
  TPointerArray = array of Pointer;
  TStringArray  = array of string;
  TIntArray = array of Int32;  
  
  EIncOrder = (PostInc=0, PreInc=1); 

  TIntRange = record Low,High:Int32; end;

type
  TTokenKind = (
    tk_unknown,
    tk_newline,
    tk_keyword,
    
    //atoms
    tk_ident,
    tk_bool,
    tk_char,
    tk_int,
    tk_float,
    tk_string,
    tk_none,

    //operators
    tk_assign,
    tk_bitwise,
    tk_logical,
    tk_sum,
    tk_fact,
    tk_cmp,
    
    // other symbols
    tk_paren,
    tk_square,
    tk_brace,
    tk_semicolon,
    tk_dot,
    tk_comma,
    tk_colon
  );
  TTokenKindSet = set of TTokenKind;

  (*Do not store filename or any other string in this struct..*)
  TDocPos = packed record
    line, column: Int32;
    function ToString: string;
  end;
  TDocPosArray = array of TDocPos;   
  
  TToken = record
    Token: TTokenKind;
    Value: string;
    DocPos: TDocPos;
    function ToString: string;
  end;
  TTokenArray = array of TToken;

  
function Token(AToken:TTokenKind; AValue:string): TToken; inline;     
operator =  (L,R:TToken): Boolean; inline;
operator <> (L,R:TToken): Boolean; inline;  
  
implementation


function Token(AToken:TTokenKind; AValue:string): TToken;
begin
  Result.Token := AToken;
  Result.Value := AValue;
end; 

operator =  (L,R: TToken): Boolean;
begin
  Result := (L.Value = R.Value) and (R.Token = L.Token);
end;

operator <> (L,R: TToken): Boolean;
begin
  Result := (L.Value <> R.Value) or (R.Token <> L.Token);
end; 

function TDocPos.ToString(): String;
begin
  Result := Format('%d:%d', [self.line,self.column]);
end;   

function TToken.ToString(): String;
begin
  WriteStr(Result, self.token);
  Result += '("'+self.value+'")';
end;


end.
