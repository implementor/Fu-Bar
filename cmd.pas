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

{$mode objfpc}{$coperators on}

unit cmd;

interface

uses cradle, expressions, sysutils, math, buffer, explain;

procedure RunCommand;
procedure OutHelp(const section: string; const fn: string = 'help.dat');
procedure OutFullFile(const fn: string);

var nyanmode: boolean;

function Nyanize(x: Extended; relyonvar: boolean = true): string;

implementation

procedure OutHelp(const section: string; const fn: string = 'help.dat');
var f: system.text; s: boolean; l: string; i: byte;
begin
    Assign(f,fn);
    Reset(f);
    try
        s := false;
        i := 0;
        while not eof(f) do
        begin
            Readln(f,l);
            if s then begin
                if l = ':stop' then begin
                    Write('<Press Enter to continue>');
                    Readln
                end else if l = ':end' then
                    s := false
                else if l = ':intlink' then begin
                    Readln(f,l);
                    OutHelp(l);
                end else if l = ':extlink' then begin
                    Readln(f,l);
                    OutFullFile(l);
                end else if (length(l)=0) or (l[1]<>':') then
                    Writeln(l)
            end else if l = ':begin:'+section then begin
                Inc(i);
                s := true;
                if i > 1 then Writeln
            end else if l = ':extlink:'+section then begin
                Readln(f,l);
                OutFullFile(l);
            end;
        end;
    finally
        Close(f)
    end
end;

procedure OutFullFile(const fn: string);
var i: word; f: system.text; l: string;
begin
    Assign(f,fn);
    Reset(f);
    try
        i := 0;
        while not eof(f) do
        begin
            Readln(f,l);
            Inc(i);
            if (i mod 15)=0 then begin
                Write('<Press Enter to continue>');
                Readln
            end;
            Writeln(i:3, ' ', l)
        end;
    finally
        Close(f)
    end
end;

procedure RunCommand;
var nm: string;

procedure Nyan;
begin
    nyanmode := not nyanmode;
    if nyanmode then
        Writeln('Nyan Mode is now ON')
    else Writeln('Nyan Mode is now OFF')
end;

begin
    Match(':');
    if look = ':' then
    begin
        Match(':');
        OutHelp(look+ReadRemaining)
    end else begin
        nm := GetName;
        SkipWhite;
        if nm = 'help' then begin
            if look in [#13,#10] then begin
                OutHelp('help');
            end else
                OutHelp('help:'+look+ReadRemaining)
        end else if nm = 'explain' then begin
            Expression;
            repeat
                Writeln(ReadExplanation)
            until not ExplanationAvailable;
        end else if nm = 'nyan' then Nyan
        else Outhelp(nm);
    end
end;

function Nyanize(x: Extended; relyonvar: boolean = true): string;
begin
    if relyonvar and (not nyanmode) then begin
        if SameValue(x,pi*2) then
            Result := 'a full turn (tau, 2pi, 4eta)'
        else if SameValue(x,pi) then
            Result := 'a half turn (tau/2, pi, 2eta)'
        else if SameValue(x,pi/2) then
            Result := 'a quarter turn (tau/4, pi/2, eta)'
        else
            Result := FloatToStr(x)
    end else if (x>8999.99) and (x<9000.01) then
        Result := 'nyan thousand'
    else if x < 9000 then
        Result := 'below nyan thousand'
    else Result := 'over nyan thousand'
end;

initialization
  nyanmode := false
end.
