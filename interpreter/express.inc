(* basedefines *)
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{.$inline on}
{$macro on}

{$define loose_semicolon}
{.$define dbg_types}


(*  macros and such..:  *)
{$IF Defined(CPUI386) OR Defined(cpux86_64)}
  {$IFDEF cpux86_64}
    {$FPUTYPE SSE64}
  {$ELSE}
    {$FPUTYPE SSE2}
  {$ENDIF}
{$ENDIF}
