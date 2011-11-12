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

unit nout;

interface

uses prayerlist, sysutils;

type
  TStringList = specialize TPrayerList<string>;
  TOutMethod = (omStd, omBuffer);
  TOutDestination = (odStd, odErr);
  TAnyType = (atString, atInt64);
  Any = record
  case at: TAnyType of
  atString: (strval: shortstring);
  atInt64:  (intval: Int64);
  end;

procedure nouttext(const s: string); overload;
procedure nouttext(const dest: TOutDestination; const s: string); overload;
procedure nouttext(var f: System.Text; const s: string); overload;
procedure nouttext(const elem: array of any); overload;
procedure nouttext(const dest: TOutDestination; const elem: array of any); overload;
procedure noutstart(const s: string);

procedure SetOutMethod(const m: TOutMethod);
function GetOutMethod: TOutMethod;
property OutMethod: TOutMethod read GetOutMethod write SetOutMethod;

operator :=(const s: string): Any;
operator :=(const i: int64): Any;

function noutreq(const dest: TOutDestination = odStd): string;
function nouteob(const dest: TOutDestination = odStd): boolean;

implementation

var
  FOutMethod: TOutMethod;
  FBuffer: array[TOutDestination] of TStringList;
  FStart: string;
  
function GetOutMethod: TOutMethod;
begin
  Result := FOutMethod;
end;
  
procedure SetOutMethod(const m: TOutMethod);
begin
  FOutMethod := m;
end;

procedure nouttext(const s: string);
begin
  nouttext(odStd,s);
end;

procedure nouttext(const dest: TOutDestination; const s: string);
begin
  case FOutMethod of
  omStd: case dest of
         odStd: Writeln(StdOut, FStart, s);
         odErr: Writeln(StdErr, FStart, s);
         end;
  omBuffer: FBuffer[dest].Add(FStart+s);
  end;
  FStart := '';
end;

procedure nouttext(var f: System.Text; const s: string);
begin
  Writeln(f, FStart, s);
end;

procedure nouttext(const elem: array of any);
begin
  nouttext(odStd, elem);
end;

procedure nouttext(const dest: TOutDestination; const elem: array of any);
var s: string; i: smallint;
begin
  s := '';
  for i := 0 to length(elem)-1 do
    case elem[i].at of
    atString: s += elem[i].strval;
    atInt64: s += IntToStr(elem[i].intval);
    end;
  nouttext(dest,s);
end;

procedure noutstart(const s: string);
begin
  case FOutMethod of
  omStd:    begin Write(FStart, s); FStart := '' end;
  omBuffer: FStart += s;
  end;
end;

operator :=(const s: string): Any;
begin
  Result.at := atString;
  Result.StrVal := s;
end;

operator :=(const i: int64): Any;
begin
  Result.at := atInt64;
  Result.IntVal := i;
end;

function noutreq(const dest: TOutDestination = odStd): string;
begin
  if not nouteob(dest) then begin
    Result := FBuffer[dest][0];
    FBuffer[dest].RemoveAt(0);
  end
end;

function nouteob(const dest: TOutDestination = odStd): boolean;
begin
  Result := FBuffer[dest].Count=0;
end;

initialization
  FOutMethod := omStd;
  FStart := '';
  FBuffer[odStd] := TStringList.Create(0,'');
  FBuffer[odErr] := TStringList.Create(0,'');
finalization
  FBuffer[odStd].Free;
  FBuffer[odErr].Free;
end.
