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

uses prayerlist, sysutils, cradle, cmplx;

type
    TExplainList = specialize TPrayerList<string>;

function ReadExplanation: string;
function ExplanationAvailable: Boolean;
procedure ExplainMe(const s: string);
procedure ClearExplanations;
procedure IndentExplanations;
procedure UnindentExplanations;

procedure ExplainBinCoef(const n,k: Complex);
procedure ExplainAdd(const a,b:Complex);
procedure ExplainSub(const a,b:Complex);
procedure ExplainMul(const a,b:Complex);
procedure ExplainDiv(const a,b:Complex);
procedure ExplainIntDiv(const a,b:Complex);
procedure ExplainMod(const a,b:Complex);
procedure ExplainPower(const a,b:Complex);
procedure ExplainAnd(const a,b:Complex);
procedure ExplainOr(const a,b:Complex);
procedure ExplainXor(const a,b:Complex);
procedure ExplainConv(const a: Complex; const c: byte);
procedure ExplainFuncEnter(const name,vn: string; const x: Complex);
procedure ExplainFuncLeave(const name,vn: string; const x,r: Complex);
procedure ExplainAbs(const a: Complex);
procedure ExplainIterEnter(const x: Complex);
procedure ExplainIterLeave(const x: Complex);
procedure ExplainIterFix(const fix: Complex);
procedure ExplainSumEnter(const i: Complex; const vn: string);
procedure ExplainProdEnter(const i: Complex; const vn: string);
procedure ExplainSumLeave(const i,r: Complex; const vn: string);
procedure ExplainProdLeave(const i,r: Complex; const vn: string);
procedure TellResult(const r: Complex);

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

function ReadExplanation: string;
begin
    if count > 0 then begin
        Result := fifo[0];
        fifo.RemoveAt(0);
        Dec(count);
    end else Result := 'Nothing to explain'
end;

function ExplanationAvailable: Boolean;
begin
    Result := count > 0;
end;

procedure ExplainMe(const s: string);
begin
    fifo.Add(s);
    Inc(count);
end;

procedure ClearExplanations;
begin
    fifo.Clear;
    count := 0;
end;

procedure IndentExplanations;
begin
    Inc(ind)
end;

procedure UnindentExplanations;
begin
    if ind > 0 then
        Dec(ind)
end;

procedure ExplainBinCoef(const n,k: Complex);
begin
    proto := 'From '+IntToStr(MakeInt(n))+' choose '+IntToStr(MakeInt(k))+' = %s'
end;

procedure ExplainAdd(const a,b:Complex);
begin
    proto := Nyanize(a)+' + '+Nyanize(b)+' = %s';
end;

procedure ExplainSub(const a,b:Complex);
begin
    proto := Nyanize(a)+' - '+Nyanize(b)+' = %s';
end;

procedure ExplainMul(const a,b:Complex);
begin
    proto := Nyanize(a)+' * '+Nyanize(b)+' = %s';
end;

procedure ExplainDiv(const a,b:Complex);
begin
    proto := Nyanize(a)+' / '+Nyanize(b)+' = %s';
end;

procedure TellResult(const r: Complex);
begin
    ExplainMe(Format('%s'+proto,[GetIndent,Nyanize(r)]));
end;

procedure ExplainIntDiv(const a,b:Complex);
begin
    proto := IntToStr(MakeInt(a))+' \ '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainMod(const a,b:Complex);
begin
    proto := IntToStr(MakeInt(a))+' mod '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainPower(const a,b:Complex);
begin
    proto := Nyanize(a)+' ^ '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainAnd(const a,b:Complex);
begin
    proto := IntToStr(MakeInt(a))+' and '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainOr(const a,b:Complex);
begin
    proto := IntToStr(MakeInt(a))+' or '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainXor(const a,b:Complex);
begin
    proto := IntToStr(MakeInt(a))+' xor '+IntToStr(MakeInt(b))+' = %s';
end;

procedure ExplainConv(const a: Complex; const c: byte);
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
    proto := 'Convert '+Nyanize(a)+' '+u+' to %s radian(s)';
end;

procedure ExplainFuncEnter(const name,vn: string; const x: Complex);
begin
    Explainme(GetIndent+'Enter function '+name+'('+vn+'='+Nyanize(x)+')');
end;

procedure ExplainFuncLeave(const name,vn: string; const x,r: Complex);
begin
    Explainme(GetIndent+'Leave function '+name+'('+vn+'='+Nyanize(x)+'), resulting '+Nyanize(r));
end;

procedure ExplainAbs(const a: Complex);
begin
    proto := '|'+Nyanize(a)+'| = %s';
end;

procedure ExplainIterEnter(const x: Complex);
begin
    Explainme(GetIndent+'Enter iteration with x[n]='+Nyanize(x));
end;

procedure ExplainIterLeave(const x: Complex);
begin
    Explainme(GetIndent+'Leave iteration with x[n+1]='+Nyanize(x));
end;

procedure ExplainIterFix(const fix: Complex);
begin
    Explainme(GetIndent+'Reached fix point '+Nyanize(fix));
end;

procedure ExplainSumEnter(const i: Complex; const vn: string);
begin
    ExplainMe(GetIndent+'Enter sum loop with '+vn+'='+Nyanize(i));
end;

procedure ExplainProdEnter(const i: Complex; const vn: string);
begin
    ExplainMe(GetIndent+'Enter product loop with '+vn+'='+Nyanize(i));
end;

procedure ExplainSumLeave(const i,r: Complex; const vn: string);
begin
    ExplainMe(GetIndent+'Leave sum loop with '+vn+'='+Nyanize(i)+', resulting '+Nyanize(r));
end;

procedure ExplainProdLeave(const i,r: Complex; const vn: string);
begin
    ExplainMe(GetIndent+'Leave product loop with '+vn+'='+Nyanize(i)+', resulting '+Nyanize(r));
end;

initialization
    fifo := TExplainList.Create(0, '');
    count := 0;
    ind := 0;
finalization
    fifo.free;
end.
