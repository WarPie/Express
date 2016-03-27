program main;
{$I express.inc}

uses
  Classes, SysUtils, CustApp, Math, Windows, 
  xpr.utils,
  xpr.bytecode,
  xpr.AST,
  xpr.parser,
  xpr.lexer,
  xpr.interpreter,
  xpr.errors;

type
  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;


function LoadFileContents(fileName:String): String;
var
  f:TStringList;
begin
  if FileExists(fileName) then
  begin
    f := TStringList.Create();
    f.LoadFromFile(fileName);
    Result := f.Text;
    f.Free();
  end else
    raise NotFoundError.CreateFmt('File `%s` not found!', [fileName]);
end;

procedure TestExecute2(filename:String);
var
  script:String;
  node:TBaseNode;
  code:TByteCode;
  CodeRunner:TInterpreter;
  v:Int32;
  t:Double;
begin
  script := LoadFileContents(filename);

  try
    t := marktime();
    node := Parse(Tokenize(script));
    code := CompileAST(node);
    WriteFancy('[LWHITE]Compiling took: '+ Format('%3f ms', [marktime() - t]));
  except
    on E:SyntaxError do
    begin
      WriteLn();
      WriteFancy('[LRED]Compiling failed with Error: '+#13#10+'  '+E.Message);
      WriteLn();
      Exit;
    end;
  end;

  v := StrToIntDef(ParamStr(2), 50);
  if Length(code.Code) <= v then
  begin
    WriteLn();
    WriteFancy('[GRAY]======================================');
    WriteFancy(code.ToString);
    WriteFancy('[GRAY]======================================');
    WriteLn();
  end else
  begin
    WriteLn();
    WriteFancy('[RED]Code is too long to print: '+ IntToStr(Length(Code.Code)) +' operations');
    WriteLn();
  end;

  WriteFancy('[LWHITE]Executing..: ');
  WriteLn();

  CodeRunner := TInterpreter.Create(code);
  try
    t := marktime();
    CodeRunner.ExecuteSafe;
    t := marktime() - t;
  except
    on E:RuntimeError do
    begin
      WriteLn();
      WriteFancy('[LRED]Execution failed with Error: '+#13#10+'  '+E.Message);
      WriteLn();
      CodeRunner.Free;
      Exit;
    end;
  end;

  WriteLn();
  WriteFancy('[LWHITE]Executed in: '+ Format('%3f ms', [t]));
  WriteLn();

  if CodeRunner.Frame.StackPos >= 0 then
  begin
    WriteFancy('[RED]There are variables on the stack:');
    WriteFancy('  '+ CodeRunner.Frame.StackToString);
  end;

  CodeRunner.Free;
end;

procedure TMyApplication.DoRun;
begin
  if ParamStr(1) = '' then
  begin
    TestExecute2('tests/quicksort.xpr');
    WriteLn();
    WriteFancy('[GRAY]======================================');
    WriteLn();
    WriteFancy('[LRED]Recieved no arguments, executed a test-script.');
  end;

  try
    TestExecute2(ParamStr(1));
  except
    on E:NotFoundError do
    begin
      if ParamStr(1) <> '' then WriteFancy('[RED]' + E.Message);
      WriteLn(Format('Usage: main.exe path/to/script.xpr [bc_max_print_size]', [ParamStr(0)]));
    end;
  end;

  while True do Sleep(500);
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'My Application';
  Application.Run;
  Application.Free;
end.

