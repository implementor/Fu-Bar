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

uses cradle, expressions, sysutils, math, vars, cmd, sqrt, buffer, explain;

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
var x,y: Extended; q: boolean; l: string;
begin
    try
        successful := false;
        q := false;
        answer := 0;
        InitVars;
        SetVar('tau',pi*2,true);        // alternative Kreiszahl Tau
        AutoLoad('autoload.dat');
        repeat
            Write('Enter a term: ');
            Readln(l);
            UpdateBuffer(l);
            GetChar;
            SkipWhite;
            if look = ':' then
                RunCommand
            else begin
                if look <> #10 then
                    x := Eval
                else begin x := 0; q := true end;
                if look <> '=' then begin
                    if look<>#10 then Expected('Linebreak',look);
                    WriteLn('Result: ',Nyanize(x))
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
            Writeln;
            ClearExplanations;
        until q;
        successful := true;
    except
        on E: ECodeMistake do
            Writeln(E.Message);
        on E: EAccessViolation do
            Writeln('Damn, that''s a fatal error. What did you do? Oo');
        on E: EDivByZero do
            Writeln('You cannot divide by zero! Shame on you!');
        on E: EZeroDivide do
            Writeln('You cannot divide by zero! Shame on you!');
        on E: EInvalidOp do
            Writeln('WTF, please tell me how to do this! Oo');
        on E: EStackOverflow do
            Writeln('Stop it! I''ve got a stack overflow! You are responsible for this!');
        on E: Exception do
            Writeln('WTF, I don''t know, what''s wrong here! => '+E.Classname);
    end;
    FreeVars
end;

end.
