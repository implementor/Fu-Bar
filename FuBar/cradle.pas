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

{$mode objfpc}{$longstrings on}{$coperators on}{$inline on}

unit cradle;

interface

uses SysUtils, Math, buffer, cmplx;

var
    look: char;

type ECodeMistake = class(Exception);

procedure GetChar;
procedure Match(x: char);
function IsAlpha(x: char): Boolean;
function IsDigit(x: char): Boolean;
function IsAlnum(x: char): Boolean;
function GetName: string;
function GetNum(const nopoint: boolean = false): Extended;
function GetInt: Int64;
procedure Error(s: string);
procedure Expected(exp: string; found: string = '');
procedure Expected(exp: string; found: char);
procedure SkipWhite;
function MakeInt(a:Extended): Int64; inline;
function NumToStr(a:Extended): string; inline;
procedure StartWatch;
function StopWatch: DWord;
function Nyanize(x: Complex; relyonvar: boolean = true): string;

var nyanmode: boolean;
const WhiteSpaces: set of char = [#$20, #$09, #$0D];

implementation

var
    watch: TDateTime;

procedure GetChar;
begin
    look := ReadBuffer;
end;

procedure Match(x: char);
begin
    if x <> look then
        Expected(''''+x+'''', look);
    GetChar
end;

procedure SkipWhite;
begin
    while look in [#$20, #$09, #$0D] do
        GetChar
end;

function IsAlpha(x: char): Boolean;
begin
    Result := x in ['A'..'Z', 'a'..'z','_']
end;

function IsDigit(x: char): Boolean;
begin
    Result := x in ['0'..'9']
end;

function IsAlnum(x: char): Boolean;
begin
    Result := IsAlpha(x) or IsDigit(x)
end;

function GetName: string;
begin
    SkipWhite;
    Result := '';
    if not IsAlpha(look) then Expected('Identifier',look);
    repeat
        Result += look;
        GetChar;
    until not IsAlpha(look);
    SkipWhite
end;

function GetNum(const nopoint: boolean = false): Extended;
var c, neg: boolean; ac, base: byte;
    function Digit(const d: char): byte;
    begin
        if (d>='0') and (d<='9') then
            Result := Ord(d)-Ord('0')
        else if (d>='a') and (d<='f') then
            Result := Ord(d)-Ord('a')+10
        else if (d>='A') and (d<='F') then
            Result := Ord(d)-Ord('A')+10
    end;
begin
    SkipWhite;
    Result := 0;
    c := nopoint;
    ac := 0;
    base := 10;
    neg := look='-';
    if look in ['+','-'] then GetChar;
    if look='$' then begin
        Match('$');
        base := 16;
    end;
    if not (IsDigit(look) or (look in ['.',',']) or ((base=16)and(look in ['a'..'f','A'..'F']))) then Expected('Float',look);
    repeat
        if look in ['.',','] then c := true
        else begin
            Result := Result * base + Digit(look);
            if c and not nopoint then Inc(ac)
        end;
        GetChar;
    until not (IsDigit(look) or ((look in [',','.']) and (not c)) or ((base=16)and(look in ['a'..'f','A'..'F'])));
    while (ac > 0) do begin
        Result /= base;
        Dec(ac)
    end;
    if neg then Result := -Result;
    SkipWhite
end;

function GetInt: Int64;
begin
    Result := MakeInt(GetNum(true))
end;

procedure Error(s: string);
begin
    raise ECodeMistake.Create(s)
end;

procedure Expected(exp: string; found: string = '');
begin
    if found = '' then
        Error(exp+' expected.')
    else Error(exp+' expected, but '+found+' found.')
end;

procedure Expected(exp: string; found: char);
begin
    Expected(exp, ''''+found+'''')
end;

function MakeInt(a: Extended): Int64;
begin
    Result := Floor(a)
end;

function NumToStr(a: Extended): string;
begin
    Result := FloatToStr(a)
end;

procedure StartWatch;
begin
    Watch := Now
end;

function StopWatch: DWord;
var n: TDateTime;
begin
    n := Now;
    Result := Floor((n-watch)*3600*24*1000)
end;

function Nyanize(x: Complex; relyonvar: boolean = true): string;
begin
    if (relyonvar and (not nyanmode)) or not SameValue(x.i,0) then begin
        if SameValue(x.r,pi*2) and SameValue(x.i,0) then
            Result := 'a full turn (tau, 2pi, 4eta)'
        else if SameValue(x,pi) and SameValue(x.i,0) then
            Result := 'a half turn (tau/2, pi, 2eta)'
        else if SameValue(x,pi/2) and SameValue(x.i,0) then
            Result := 'a quarter turn (tau/4, pi/2, eta)'
        else if SameValue(x.i,0) then
            Result := FloatToStr(x.r)
        else if SameValue(x.r,0) and SameValue(x.i,1) then
            Result := 'i'
        else if SameValue(x.r,0) and SameValue(x.i,-1) then
            Result := '-i'
        else if SameValue(x.r,0) then
            Result := FloatToStr(x.i) + 'i'
        else if x.i<0 then
            Result := FloatToStr(x.r) + ' - ' + FloatToStr(-x.i) + 'i'
        else
            Result := FloatToStr(x.r) + ' + ' + FloatToStr(x.i) + 'i'
    end else if (x>8999.99) and (x<9000.01) then
        Result := 'nyan thousand'
    else if x < 9000 then
        Result := 'below nyan thousand'
    else Result := 'over nyan thousand'
end;

end.
