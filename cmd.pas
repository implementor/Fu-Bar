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

{$mode objfpc}{$coperators on}{$longstrings on}

unit cmd;

interface

uses cradle, expressions, sysutils, math, buffer, explain, cmplx, arguments, plug;

procedure RunCommand;
procedure RunPlugin(const int: boolean = false);
procedure OutHelp(const section: string; const fn: string = '$def');
procedure OutFullFile(const fn: string);

type
  EUnknownPlugin = class(Exception);

implementation

procedure OutHelp(const section: string; const fn: string = '$def');
var f: system.text; s: boolean; l: string; i: byte;
begin
    if ArgNoman then Error('Manual is disabled.');
    if fn='$def' then
      Assign(f,PathHelp)
    else Assign(f,fn);
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
        else if nm = 'invoke' then
          RunPlugin(true)
        else Outhelp(nm);
    end
end;

procedure RunPlugin(const int: boolean = false);
var nm: string; p: iplugin;
begin
  if not int then
    Match('/');
  nm := GetName;
  p := FindPlugin(nm);
  if p<>nil then begin
    SkipWhite;
    p.Transmit(look+ReadRemaining);
    p.Invoke;
  end else raise EUnknownPlugin.Create('Unknown plugin '''+nm+'''');
end;

initialization
  nyanmode := false
end.
