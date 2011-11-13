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

{$mode objfpc}{$longstrings on}

unit arguments;

interface

uses getopts, sysutils, nout;

var
  ArgInstant, ArgHelp, ArgQuiet, ArgSafe, ArgNoman: Boolean;
  ArgTerm, ArgTopic: string;
  PathAutoload, PathHelp, PathLibplugs: string;

implementation

const
  Options: array[1..7] of TOption = (
      (name: 'instant'; has_arg: 0; flag: nil; value: 'i'),
      (name: 'help'; has_arg: 0; flag: nil; value: #0),
      (name: 'quiet'; has_arg: 0; flag: nil; value: 'q'),
      (name: 'about'; has_arg: 1; flag: nil; value: #0),
      (name: 'term'; has_arg: 1; flag: nil; value: 't'),
      (name: 'safe'; has_arg: 0; flag: nil; value: #0),
      (name: 'noman'; has_arg: 0; flag: nil; value: #0)
    );

var c: char; idx: integer; f1, f2: file of byte; b: byte;

procedure cp(const fn1, fn2: string);
begin
  Assign(f1,fn1);
  Assign(f2,fn2);
  filemode := 0; Reset(f1); 
  filemode := 1; Rewrite(f2);
  filemode := 2;
  try while not eof(f1) do begin
    Read(f1,b); Write(f2,b);
  end finally
    Close(f1); Close(f2);
  end;
end;
    
begin
  c:=#0;
  ArgInstant := false;
  ArgHelp := false;
  ArgQuiet := false;
  ArgSafe := false;
  ArgNoman := false;
  ArgTerm := '';
  ArgTopic := '';
  PathAutoload := GetAppConfigDir(false)+PathDelim+'autoload';
  PathHelp := GetAppConfigDir(false)+PathDelim+'help';
  PathLibplugs := GetAppConfigDir(false)+PathDelim+'libplugs';
  repeat
    c:=getlongopts('iqt:',@options[1],idx);
    case c of
      #0: case idx of
           2:  ArgHelp := true;
           4:  ArgTopic := optarg;
           6:  ArgSafe := true;
           7:  ArgNoman := true;
           end;
      'i': ArgInstant := true;
      'q': ArgQuiet := true;
      't': ArgTerm := optarg;
  end;
  until c=endofoptions;
  if optind<=paramcount then
    ArgTerm := ParamStr(optind);
  ArgInstant := ArgInstant or ArgSafe;
  ArgNoman := (ArgNoman or ArgSafe) and (not (ArgHelp or (ArgTopic<>'')));
  ArgQuiet := ArgQuiet or ArgSafe or (ArgTerm<>'');
  if not ArgSafe then begin
    if (not DirectoryExists(GetAppConfigDir(false))) then
      CreateDir(GetAppConfigDir(false));
    if (not FileExists(GetAppConfigDir(false)+PathDelim+'autoload')) then begin
      if DirectoryExists(GetAppCOnfigDir(true)) and FileExists(GetAppConfigDir(true)+PathDelim+'autoload') then
      begin
        noutstart('Creating local configuration... ');
        cp(GetAppConfigDir(true)+PathDelim+'autoload',GetAppConfigDir(false)+PathDelim+'autoload');
        nouttext('Done!');
      end else ArgInstant := true;
    end;
    if (not FileExists(GetAppConfigDir(false)+PathDelim+'help')) then begin
      if DirectoryExists(GetAppCOnfigDir(true)) and FileExists(GetAppConfigDir(true)+PathDelim+'help') then
      begin
        noutstart('Creating local manual... ');
        cp(GetAppConfigDir(true)+PathDelim+'help',GetAppConfigDir(false)+PathDelim+'help');
        nouttext('Done!');
      end else ArgNoman := true;
    end;
    if (not FileExists(GetAppConfigDir(false)+PathDelim+'libplugs')) then begin
      if DirectoryExists(GetAppCOnfigDir(true)) and FileExists(GetAppConfigDir(true)+PathDelim+'libplugs') then
      begin
        noutstart('Creating local libplugs index... ');
        cp(GetAppConfigDir(true)+PathDelim+'libplugs',GetAppConfigDir(false)+PathDelim+'libplugs');
        nouttext('Done!');
      end else PathLibplugs := '';
    end;
  end;
end.
