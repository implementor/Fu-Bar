//    Copyright 2011 Marvin Cohrs
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
//    along with this program.  If not, see <http://www.gnu.org/licenses/>.

{$mode objfpc}{$coperators on}

unit cmplx;

interface

type
    Complex = record
        r, i: Extended;
    end;

operator :=(r: Extended) c: Complex;
operator +(a,b: Complex) c: Complex;
operator -(a,b: Complex) c: Complex;
operator *(a,b: Complex) c: Complex;
operator /(a,b: Complex) c: Complex;
operator -(a: Complex) c: Complex;

implementation

operator :=(r: Extended) c: Complex;
begin
    c.r := r;
    c.i := 0;
end;

operator +(a,b: Complex) c: Complex;
begin
    c.r := a.r + b.r;
    c.i := a.i + b.i;
end;

operator -(a,b: Complex) c: Complex;
begin
    c.r := a.r - b.r;
    c.i := a.i - b.i;
end;

operator *(a,b: Complex) c: Complex;
begin
    c.r := a.r * b.r - a.i * b.i;
    c.i := a.r * b.i + b.r * a.i;
end;

operator /(a,b: Complex) c: Complex;
begin
    c.r := (a.r * b.r + a.i * b.i) / (b.r * b.r + b.i * b.i);
    c.i := (a.i * b.r - a.r * b.i) / (b.r * b.r + b.i * b.i);
end;

operator -(a: Complex) c: Complex;
begin
    c.r := -a.r;
    c.i := -a.i;
end;

end.
