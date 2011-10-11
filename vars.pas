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

{$mode objfpc}{$longstrings on}{$coperators on}

unit vars;

interface

uses cradle, prayerlist, cmplx;

type
    TVariable = record
        Name: string;
        Value: Complex;
        IsConst: Boolean;
    end;
    PVariable = ^TVariable;
    TVarList = specialize TPrayerList<PVariable>;
    TFunction = record
        Name, Expr, vname: string;
        TakeZero, IsConst: Boolean;
    end;
    PFunction = ^TFunction;
    TFuncList = specialize TPrayerList<PFunction>;

var
    Varlist: TVarList;
    Funclist: TFuncList;

procedure InitVars;
procedure FreeVars;
function GetVar(const name: string): Complex;
procedure SetVar(const name: string; const val: Complex; const setconst: boolean = false);
function VarExists(const name: string): Boolean;
function GetFunc(const name: string): string; overload;
function GetFunc(const name: string; out takezero: boolean; out vname: string): string; overload;
procedure SetFunc(const name, expr: string; const takezero: boolean = true; const vname: string = 'x'; const setconst: boolean = false);
function FuncExists(const name: string): Boolean;

implementation

procedure InitVars;
begin
    Varlist := TVarlist.Create(0, nil);
    Funclist := TFunclist.Create(0, nil)
end;

procedure FreeVars;
var i: word;
begin
    if varlist.count > 0 then
    for i := 0 to Varlist.Count-1 do
        Dispose(Varlist[i]);
    Varlist.Free;
    if funclist.count > 0 then
    for i := 0 to Funclist.Count-1 do
        Dispose(Funclist[i]);
    Funclist.Free
end;

function VarExists(const name: string): Boolean;
var i: word;
begin
    Result := false;
    if VarList.Count > 0 then
    for i := 0 to Varlist.Count-1 do
        Result := Result or (Varlist[i]^.name=name)
end;

function GetVar(const name: string): Complex;
var i: word;
begin
    Result := 0;
    if Varlist.Count > 0 then
    for i := 0 to Varlist.Count-1 do
        if Varlist[i]^.name=name then
            Result := Varlist[i]^.value
end;

procedure SetVar(const name: string; const val: Complex; const setconst: boolean = false);
var v: PVariable; i: word; e: boolean;
begin
    e := false;
    if Varlist.Count > 0 then
    for i := 0 to Varlist.Count-1 do
        if Varlist[i]^.name = name then begin
            e := true;
            if not Varlist[i]^.isconst then
                Varlist[i]^.value := val
            else
                Error('Cannot write read-only const '''+name+'''');
            Varlist[i]^.isconst := Varlist[i]^.isconst or setconst
        end;
    if not e then begin
        New(v);
        v^.name := name;
        v^.value := val;
        v^.IsConst := setconst;
        Varlist.Add(v)
    end
end;

function GetFunc(const name: string): string;
var i: word;
begin
    Result := '';
    if Funclist.Count > 0 then
    for i := 0 to Funclist.Count-1 do
        if Funclist[i]^.name=name then
            Result := Funclist[i]^.expr
end;

function GetFunc(const name: string; out takezero: boolean; out vname: string): string;
var i: word;
begin
    Result := '';
    if Funclist.Count > 0 then
    for i := 0 to Funclist.Count-1 do
        if Funclist[i]^.name=name then
        begin
            Result := Funclist[i]^.expr;
            takezero := Funclist[i]^.takezero;
            vname := Funclist[i]^.vname
        end
end;

procedure SetFunc(const name, expr: string; const takezero: boolean = true; const vname: string = 'x'; const setconst: boolean = false);
var v: PFunction; i: word; e: boolean;
begin
    e := false;
    if Funclist.Count > 0 then
    for i := 0 to Funclist.Count-1 do
        if Funclist[i]^.name = name then begin
            e := true;
            if Funclist[i]^.isconst then
                Error('Cannot write read-only func '''+name+'''');
            Funclist[i]^.expr := expr;
            Funclist[i]^.takezero := takezero;
            Funclist[i]^.vname := vname;
            Funclist[i]^.isconst := setconst;
        end;
    if not e then begin
        New(v);
        v^.name := name;
        v^.expr := expr;
        v^.TakeZero := takezero;
        v^.vname := vname;
        v^.isconst := setconst;
        Funclist.Add(v)
    end
end;

function FuncExists(const name: string): Boolean;
var i: word;
begin
    Result := false;
    if Funclist.Count > 0 then
    for i := 0 to Funclist.Count-1 do
        Result := Result or (Funclist[i]^.name=name)
end;

end.
