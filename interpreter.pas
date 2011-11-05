{/    Copyright 2011 Marvin Cohrs
//
//    This file is part of Fu Bar.
//
//    Fu Bar is free software: you can redistribute it and/or modify
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

{$mode objfpc}{$longstrings on}

unit interpreter;

interface

uses cradle, expressions, sysutils, math, vars, cmd, sqrt, buffer, explain, cmplx, arguments;

procedure StartInterpreter;

var
    successful: boolean;

implementation

procedure AutoLoad(const fn: string);
var f: system.text; l: string;
begin
    Assign(f,fn);
    Reset(f);
    try
        while not eof(f) do begin
            Readln(f,l);
            UpdateBuffer(l);
            GetChar;
            if l <> '' then
                Eval;
        end
    finally
        Close(f);
    end;
    ClearExplanations
end;

procedure StartInterpreter;
var x,y: Complex; q: boolean; l: string;
begin
    try
        successful := false;
        q := false;
        answer := 0;
        InitVars;
        SetVar('tau',pi*2,true);        // alternative Kreiszahl Tau
        SetVar('i',cmplx_i,true);       // imaginäre Zahl
        if not ArgInstant then
          AutoLoad(PathAutoload);
        repeat
          if not ArgQuiet then
            Write('Enter a term: ');
          if ArgTerm = '' then
            Readln(l)
          else l := ArgTerm;
          UpdateBuffer(l);
          GetChar;
          SkipWhite;
          try
            if look = ':' then
              RunCommand
            else if look = '/' then
              RunPlugin
            else begin
              if look <> #10 then
                x := Eval
              else begin x := 0; q := true end;
              if look <> '=' then begin
                if look<>#10 then Expected('Linebreak',look);
                if not ArgQuiet then Write('Result: ');
                if l<>'' then
                  Writeln(Nyanize(x))
              end else begin
                Match('=');
                y := Eval;
                if look<>#10 then Expected('Linebreak',look);
                if SameValue(x,y) then begin
                  Writeln('True! L = R');
                  Writeln('Result: ',Nyanize(x))
                end else if x < y then begin
                  Writeln('False! L < R');
                  Writeln('Result for L: ',Nyanize(x));
                  Writeln('Result for R: ',Nyanize(y));
                  Writeln('Result for R-L: ',Nyanize(y-x));
                  if x<>0 then
                    Writeln('Result for R/L: ',Nyanize(y/x))
                end else if x > y then begin
                  Writeln('False! L > R');
                  Writeln('Result for L: ',Nyanize(x));
                  Writeln('Result for R: ',Nyanize(y));
                  Writeln('Result for L-R: ',Nyanize(x-y));
                  if y<>0 then
                    Writeln('Result for L/R: ',Nyanize(x/y))
                end
              end;
              Answer := x
            end;
          except
            on E: ECodeMistake do
              Writeln(StdErr,E.Message);
            on E: EDivByZero do
              Writeln(StdErr,'Illegal zero division.');
            on E: EZeroDivide do
              Writeln(StdErr,'Illegal zero division.');
            on E: EInvalidOp do
              Writeln(StdErr,'Illegal operation.');
            on E: EOverflow do
              Writeln(StdErr,'Float overflow.');
            on E: EUnderflow do
              Writeln(StdErr,'Float underflow.');
            on E: EUnknownPlugin do
              Writeln(StdErr,E.Message);
          end;
          if not ArgQuiet then
            Writeln;
          ClearExplanations;
        until q or (ArgTerm<>'');
        successful := true;
    except
        on E: ECodeMistake do
            Writeln(StdErr,E.Message);
        on E: EAccessViolation do
            Writeln(StdErr,'I got an access violation. I''m really sorry about that bug.');
        on E: EStackOverflow do
            Writeln(StdErr,'I got a stack overflow. I''m really sorry about that bug.');
        on E: Exception do
            Writeln(StdErr,E.Classname,' => ',E.Message);
    end;
    FreeVars
end;

end.
