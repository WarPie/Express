{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  Runtime datatype representations
}
unit xpr.none;
{$I express.inc}

interface

{$I objh.inc}

uses
  SysUtils,
  xpr.express, 
  xpr.objbase;


type
  TNoneObject = class(TEpObject)
    function Release: Boolean; override;
    function Copy(gcGen:Byte=0): TEpObject; override;
    function DeepCopy: TEpObject; override;
    
    function AsBool: Boolean; override;
    function AsInt: epInt; override;
    function AsFloat: Double; override;
    function AsString: epString; override;
    
    procedure ASGN(other:TEpObject; var dest:TEpObject); override;
    
    procedure EQ(other:TEpObject; var dest:TEpObject); override;
    procedure NE(other:TEpObject; var dest:TEpObject); override;
    procedure LT(other:TEpObject; var dest:TEpObject); override;
    procedure GT(other:TEpObject; var dest:TEpObject); override;
    procedure GE(other:TEpObject; var dest:TEpObject); override;
    procedure LE(other:TEpObject; var dest:TEpObject); override;
    
    procedure LOGIC_AND(other:TEpObject; var dest:TEpObject); override;
    procedure LOGIC_OR(other:TEpObject; var dest:TEpObject); override;
    procedure LOGIC_NOT(var dest:TEpObject); override;
  end;

implementation

uses
  xpr.utils, 
  xpr.errors, 
  xpr.mmgr,
  xpr.bool;


function TNoneObject.Release: Boolean;
begin 
  Result := False;
end;

function TNoneObject.Copy(gcGen:Byte=0): TEpObject; 
begin 
  Result := self; 
end;

function TNoneObject.DeepCopy: TEpObject; 
begin 
  Result := self; 
end;

function TNoneObject.AsBool: Boolean;
begin 
  Result := False; 
end;

function TNoneObject.AsInt: epInt;
begin 
  Result := 0; 
end;

function TNoneObject.AsFloat: Double;
begin 
  Result := 0; 
end;

function TNoneObject.AsString: epString; 
begin 
  Result := 'None'; 
end;

procedure TNoneObject.ASGN(other:TEpObject; var dest:TEpObject);
begin
  if (other is TNoneObject) then
    (* do nothing *)
  else
    inherited;
end;

procedure TNoneObject.EQ(other:TEpObject; var dest:TEpObject);
begin
  if other.ClassType = TNoneObject then SetBoolDest(dest, True)
  else SetBoolDest(dest, False);
end;

procedure TNoneObject.NE(other:TEpObject; var dest:TEpObject);
begin
  if other.ClassType = TNoneObject then SetBoolDest(dest, False)
  else SetBoolDest(dest, True);
end;

procedure TNoneObject.LT(other:TEpObject; var dest:TEpObject);
begin
  if other.ClassType = TNoneObject then SetBoolDest(dest, False)
  else SetBoolDest(dest, True);
end;

procedure TNoneObject.GT(other:TEpObject; var dest:TEpObject);
begin
  SetBoolDest(dest, False);
end;

procedure TNoneObject.LE(other:TEpObject; var dest:TEpObject);
begin
  if other.ClassType = TNoneObject then SetBoolDest(dest, True)
  else SetBoolDest(dest, False);
end;

procedure TNoneObject.GE(other:TEpObject; var dest:TEpObject);
begin
  if other.ClassType = TNoneObject then SetBoolDest(dest, True)
  else SetBoolDest(dest, False);
end;

procedure TNoneObject.LOGIC_AND(other:TEpObject; var dest:TEpObject); 
begin 
  SetBoolDest(dest, False and other.AsBool); 
end;

procedure TNoneObject.LOGIC_OR(other:TEpObject; var dest:TEpObject);  
begin 
  SetBoolDest(dest, False  or other.AsBool); 
end;

procedure TNoneObject.LOGIC_NOT(var dest:TEpObject); 
begin 
  SetBoolDest(dest, True); 
end;

end.
