{/    Copyright 2011 Marvin Cohrs
//
//    This file is part of damnit.
//
//    damnit is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.}
{$mode objfpc}{$longstrings on}{$coperators on}

unit instructions;

interface

uses 
  expressions, cradle, buffer, nout, vars, cmplx, interpreter, 
  arguments;

procedure Instruction;
procedure MultipleInstructions;
function Statement: Complex;

implementation

var
  omitsc: boolean;

procedure Instruction;
var nm: string; c: Complex; expr: boolean;
begin
  if look='{' then begin
    Match('{');
    MultipleInstructions;
    Match('}');
    omitsc := true;
  end else if look='[' then
    nouttext(['Evaluate ',Nyanize(Eval)])
  else if look<>';' then begin
    nm := GetName;
    expr := false;
    if look='#' then begin
      expr := true;
      Match('#');
      SkipWhite;
    end;
    Match('=');
    if expr then
      c := Expression
    else
      c := Statement;
    SetVar(nm,c,false);
    nouttext(['Assign ',Nyanize(c),' to ',nm]);
  end
end;

procedure MultipleInstructions;
var q: boolean;
begin
  q := false;
  repeat
    SkipWhite;
    if look in ['}',#$0A] then q := true
    else begin
      Instruction;
      if look=';' then Match(';')
      else if not omitsc then q := true;
      omitsc := false;
    end;
  until q;
end;

function Statement: Complex;
var n,m: string;
begin
  n := GetName;
  if look='(' then begin
    Match('(');
    if n='eval' then
      Result := Eval
    else if n='expr' then
      Result := Expression
    else Result := RunFunc(n, Statement);
    Match(')');
  end else Result := GetVar(n);
  while look in ['+','-','*','/'] do
    case look of
    '+': begin Match('+'); Result += Statement() end;
    '-': begin Match('-'); Result -= Statement() end;
    '*': begin Match('*'); Result *= Statement() end;
    '/': begin Match('/'); Result /= Statement() end;
    end;
end;

initialization
  InitVars;
  answer := 0;
  omitsc := false;
  SetVar('tau',pi*2,true);        // alternative Kreiszahl Tau
  SetVar('i',cmplx_i,true);       // imaginäre Zahl
  Autoload(PathAutoload);
finalization
  FreeVars
end.
