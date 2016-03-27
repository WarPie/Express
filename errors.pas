{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Error handling
}
unit errors;
{$I express.inc}

interface

uses
  Classes, SysUtils, express;

type
  ExpressError = class(Exception)
    DocPos: TDocPos;
    constructor Create(Msg: string; ADocPos: TDocPos); overload;
  end;
  SyntaxError  = class(ExpressError);
  RuntimeError = class(ExpressError);
  
  EExceptionType = (eGeneralError, eSyntaxError, eRuntimeError);
  
resourcestring
  eExpectedButFound      = 'Expected `%s` but found `%s`';
  eExpectedVar           = 'Expected variable';
  eInvalidExpression     = 'Invalid expression';
  eExpectedArgCount      = 'Function `%s` expected `%d` arguments';
  eIndexOutOfRange       = 'Index out of range (index:%d; length:%d)';
  eNotImplemented        = 'Not implemented yet';
  eNotCompatible1        = 'Operation is not compatible with types (%s, %s)';
  eNotAllowedOutsideLoop = '`%s` is not allowed outside a loop';
  eUnexpected            = 'An unexpected error occurred';
  eUnexpectedOperation   = 'Unexpected operation `%s`';
  eUnexpectedKeyword     = 'Unexpected keyword `%s`';
  eUndefinedIdentifier   = 'Identifier `%s` is not defined';

  
procedure RaiseException(Msg:string; DocPos: TDocPos);
procedure RaiseExceptionFmt(Msg:string; Args: array of const; DocPos: TDocPos);
procedure RaiseException(Typ:EExceptionType; Msg:string; DocPos: TDocPos);
procedure RaiseExceptionFmt(Typ:EExceptionType; Msg:string; Args: array of const; DocPos: TDocPos);

implementation

constructor ExpressError.Create(Msg:string; ADocPos: TDocPos); overload;
begin
  DocPos := ADocPos;
  inherited Create(Msg);
end;
  
{$IF DEFINED(Delphi) AND (CompilerVersion <= 21.00)}
function ReturnAddress: Pointer;
asm
  MOV  EAX, [EBP+4]
end;
{$IFEND}

procedure _RaiseException(e:ExpressError); inline;
{$IFDEF FPC}
begin
  raise e at get_caller_addr(get_frame);
end;
{$ELSE}
begin
  raise e at ReturnAddress;
end;
{$ENDIF}
  
procedure RaiseException(Msg:string; DocPos: TDocPos);
begin
  _RaiseException(ExpressError.Create(Msg + ' at ' + DocPos.ToString, DocPos));
end;

procedure RaiseExceptionFmt(Msg:string; Args: array of const; DocPos: TDocPos);
begin
  _RaiseException(ExpressError.Create(Format(Msg, Args) + ' at ' + DocPos.ToString, DocPos));
end;

procedure RaiseException(Typ:EExceptionType; Msg:string; DocPos: TDocPos);
begin
  case typ of
    eGeneralError: _RaiseException(ExpressError.Create(Msg + ' at ' + DocPos.ToString, DocPos));
    eRuntimeError: _RaiseException(RuntimeError.Create(Msg + ' at ' + DocPos.ToString, DocPos));
    eSyntaxError:  _RaiseException(SyntaxError.Create(Msg  + ' at ' + DocPos.ToString, DocPos));
  end;
end;

procedure RaiseExceptionFmt(Typ:EExceptionType; Msg:string; Args: array of const; DocPos: TDocPos);
begin
  case typ of
    eGeneralError: _RaiseException(ExpressError.Create(Format(Msg, Args) + ' at ' + DocPos.ToString, DocPos));
    eRuntimeError: _RaiseException(RuntimeError.Create(Format(Msg, Args) + ' at ' + DocPos.ToString, DocPos));
    eSyntaxError:  _RaiseException(SyntaxError.Create(Format(Msg, Args)  + ' at ' + DocPos.ToString, DocPos));
  end;
end;

end.
