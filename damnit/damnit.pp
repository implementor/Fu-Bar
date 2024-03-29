{/    Copyright 2011 Marvin Cohrs
//
//    This file is part of damnit.
//
//    damnit is free software: you can redistribute it and/or modify
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

program damnit;

uses instructions, buffer, nout, cradle, arguments;

var 
  s,t: string;
  f: System.Text;

begin
  OutMethod := omStd;
  Assign(f,ArgTerm);
  Reset(f);
  try
    t := '';
    while not eof(f) do begin
      Readln(f,s);
      t += s;
    end;
    UpdateBuffer(t);
    GetChar;
    MultipleInstructions;
  finally
    Close(f);
  end
end.
