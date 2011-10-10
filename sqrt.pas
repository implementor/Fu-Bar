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

unit sqrt;

interface

uses sysutils, math;

function Heron(const sqr: Extended): Extended;
function Heron(const q: Extended; const k: byte): Extended;
function Power(const base: Extended; exp:int64): Extended;
function Fac(const n: byte): Int64;
function BinCoef(n, k: int64): Int64;

function gcd(a,b: Int64): Int64;
function lcm(a,b: Int64): Int64;

implementation

// Heron-Iteration
// Ermittelt Quadratwurzel
// x[n+1] = (x[n] + (a / x[n])) / 2
function Heron(const sqr: Extended): Extended;
var x, xn: extended;
begin
    if sqr<0 then raise EInvalidOp.Create('Cannot calc sqrt of '+floattostr(sqr))
    else if SameValue(sqr,0) then Result := 0
    else begin
        xn := sqr;
        repeat
            x := xn;
            xn := (x + (sqr/x))/2;
        until SameValue(x,xn);
        Result := x
    end
end;

// verallgemeinerte Heron-Iteration
// Ermittelt k-te Wurzel
// x[n+1] = ((k - 1) * x[n] ^ k + a) / (k * x[n] ^ (k - 1))
function Heron(const q: Extended; const k: byte): Extended;
var x, xn: extended;
begin
    if (q<0) and (k mod 2 = 0) then raise EInvalidOp.Create('Cannot calc '+inttostr(k)+'th root of '+floattostr(q));
    xn := q;
    repeat
        x := xn;
        xn := ((k-1)*Power(x,k)+q)/(k*Power(x,k-1));
    until Samevalue(x,xn);
    Result := x
end;

// Potenz mit ganzzahligem Exponenten
function Power(const base: Extended; exp:int64): Extended;
begin
    Result := 1;
    if exp<0 then  
        Result := 1/Power(base,-exp)
    else while exp>0 do begin
        Result *= base;
        Dec(exp)
    end
end;

// Fakultät
function Fac(const n: byte): Int64;
begin
    if n = 0 then
        Result := 1
    else
        Result := n * Fac(n-1)
end;

// Binomialkoeffizient
function BinCoef(n, k: Int64): Int64;
begin
    if n<0 then n := -n;
    if k<0 then k := -k;
    if n<k then raise EInvalidOp.Create('Cannot calc '+IntToStr(n)+' over '+IntToStr(k));
    Result := Fac(n) div (Fac(k) * Fac(n-k))
end;

// Größter gemeinsamer Teiler (ggT)
// greatest common divisor (gcd)
// Moderner euklidischer Algorithmus
function gcd(a,b: Int64): Int64;
var x: Int64;
begin
    if a<b then begin
        x := a;
        a := b;
        b := x
    end;
    while b<>0 do begin
        x := a mod b;
        a := b;
        b := x
    end;
    Result := a
end;

// Kleinstes gemeinsames Vielfaches (kgV)
// Least common multiple (lcm)
// a*b = ggT(a,b)*kgV(a,)
function lcm(a,b: Int64): Int64;
begin
    Result := (a*b) div gcd(a,b)
end;

end.
