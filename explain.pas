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

unit explain;

interface

uses prayerlist, sysutils, cradle;

type
    TExplainList = specialize TPrayerList<string>;

function ReadExplainment: string;
function ExplainmentAvailable: Boolean;
procedure ExplainMe(const s: string);
procedure ClearExplainments;
procedure IndentExplainments;
procedure UnindentExplainments;

procedure ExplainBinCoef(const n,k: extended);
procedure ExplainAdd(const a,b:extended);
procedure ExplainSub(const a,b:extended);
procedure ExplainMul(const a,b:extended);
procedure ExplainDiv(const a,b:extended);
procedure ExplainIntDiv(const a,b:extended);
procedure ExplainMod(const a,b:extended);
procedure ExplainPower(const a,b:extended);
procedure ExplainAnd(const a,b:extended);
procedure ExplainOr(const a,b:extended);
procedure ExplainXor(const a,b:extended);
procedure ExplainConv(const a: extended; const c: byte);
procedure ExplainFuncEnter(const name,vn: string; const x: extended);
procedure ExplainFuncLeave(const name,vn: string; const x,r: extended);
procedure ExplainAbs(const a: extended);
procedure ExplainIterEnter(const x: extended);
procedure ExplainIterLeave(const x: extended);
procedure ExplainIterFix(const fix: extended);
procedure TellResult(const r: extended);

implementation

var
    fifo: TExplainList;
    count: word;
    proto: string;
    ind: word;
    
function GetIndent: string;
var i: word;
begin
    Result := '';
    for i := 1 to ind do
        Result += '  ';
end;

function ReadExplainment: string;
begin
    if count > 0 then begin
        Result := fifo[0];
        fifo.RemoveAt(0);
        Dec(count);
    end else Result := 'Nothing to explain'
end;

function ExplainmentAvailable: Boolean;
begin
    Result := count > 0;
end;

procedure ExplainMe(const s: string);
begin
    fifo.Add(s);
    Inc(count);
end;

procedure ClearExplainments;
begin
    fifo.Clear;
    count := 0;
end;

procedure IndentExplainments;
begin
    Inc(ind)
end;

procedure UnindentExplainments;
begin
    if ind > 0 then
        Dec(ind)
end;

procedure ExplainBinCoef(const n,k: extended);
begin
    proto := 'From '+IntToStr(MakeInt(n))+' choose '+IntToStr(MakeInt(k))+' = %s'
end;

procedure ExplainAdd(const a,b:extended);
begin
    proto := FloatToStr(a)+' + '+FloatToStr(b)+' = %s';
end;

procedure ExplainSub(const a,b:extended);
begin
    proto := FloatToStr(a)+' - '+FloatToStr(b)+' = %s';
end;

procedure ExplainMul(const a,b:extended);
begin
    proto := FloatToStr(a)+' * '+FloatToStr(b)+' = %s';
end;

procedure ExplainDiv(const a,b:extended);
begin
    proto := FloatToStr(a)+' / '+FloatToStr(b)+' = %s';
end;

procedure TellResult(const r: extended);
var s: string; i: word;
begin
    s := '';
    for i := 1 to ind do
        s += '  ';
    ExplainMe(Format('%s'+proto,[s,FloatToStr(r)]));
end;

procedure ExplainIntDiv(const a,b:extended);
begin
    proto := IntToStr(MakeInt(a))+' \ '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainMod(const a,b:extended);
begin
    proto := IntToStr(MakeInt(a))+' mod '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainPower(const a,b:extended);
begin
    proto := FloatToStr(a)+' ^ '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainAnd(const a,b:extended);
begin
    proto := IntToStr(MakeInt(a))+' and '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainOr(const a,b:extended);
begin
    proto := IntToStr(MakeInt(a))+' or '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainXor(const a,b:extended);
begin
    proto := IntToStr(MakeInt(a))+' xor '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainConv(const a: extended; const c: byte);
var u: string;
begin
    case c of
    0: u := 'radian(s)';
    1: u := 'degree(s)';
    2: u := 'gradian(s)';
    3: u := 'tau radian(s)';
    4: u := 'pi radian(s)';
    5: u := 'thousand radians';
    end;
    proto := 'Convert '+FloatToStr(a)+' '+u+' to %s radian(s)';
end;

procedure ExplainFuncEnter(const name,vn: string; const x: extended);
var s: string; i: word;
begin
    s := '';
    for i := 1 to ind do
        s += '  ';
    Explainme(s+'Enter function '+name+'('+vn+'='+FloatToStr(x)+')');
end;

procedure ExplainFuncLeave(const name,vn: string; const x,r: extended);
var s: string; i: word;
begin
    s := '';
    for i := 1 to ind do
        s += '  ';
    Explainme(s+'Leave function '+name+'('+vn+'='+FloatToStr(x)+'), resulting '+FloatToStr(r));
end;

procedure ExplainAbs(const a: extended);
begin
    proto := '|'+FloatToStr(a)+'| = %s';
end;

procedure ExplainIterEnter(const x: extended);
begin
    Explainme(GetIndent+'Enter iteration with x[n]='+FloatToStr(x));
end;

procedure ExplainIterLeave(const x: extended);
begin
    Explainme(GetIndent+'Leave iteration with x[n+1]='+FloatToStr(x));
end;

procedure ExplainIterFix(const fix: extended);
begin
    Explainme(GetIndent+'Reached fix point '+FloatToStr(fix));
end;

initialization
    fifo := TExplainList.Create(0, '');
    count := 0;
    ind := 0;
finalization
    fifo.free;
end.
