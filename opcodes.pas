{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  stuff
}
unit opcodes;
{$I express.inc}

interface

uses
  SysUtils;

type
  EBytecode = (
    LOAD_CONST, 
    LOAD,
    STORE_FAST,
    DISCARD_TOP,

    JMP_IF_FALSE, 
    JMP_IF_TRUE,
    JUMP, {ALIASES:} __CONTINUE, __BREAK, __FUNC,
    JMP_BACK, 
    JMP_FORWARD,

    ASGN,
    RASGN,

    BUILD_LIST,

    UNARY_PREINC,
    UNARY_PREDEC,
    UNARY_POSTINC,
    UNARY_POSTDEC,

    INPLACE_ADD,
    INPLACE_SUB,
    INPLACE_DIV,
    INPLACE_MUL,

    UNARY_SUB,
    UNARY_NOT,
    UNARY_BINV,

    BIN_ADD, 
    BIN_SUB, 
    BIN_MUL, 
    BIN_FDIV,
    BIN_DIV,
    BIN_MOD,
    BIN_EQ, 
    BIN_NE, 
    BIN_LT, 
    BIN_GT, 
    BIN_LE, 
    BIN_GE,
    BIN_AND,
    BIN_OR,
    BIN_BAND,
    BIN_BOR,
    BIN_BXOR,

    CALL,
    GET_ITEM,
    SET_ITEM,

    PRINT,
    TIMENOW,
    RETURN
  );
  
  TOperation = record
    code: EBytecode;
    arg : Int32;
  end;
  TOperationArray = array of TOperation;


implementation
  
end.
