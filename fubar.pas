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

program fubar;

uses interpreter, cmd, arguments;

begin
    ExitCode := 42;
    if ArgHelp then
      OutHelp('--help')
    else if ArgTopic<>'' then
      OutHelp('help:'+ArgTopic)
    else begin
      if not (ArgQuiet or ArgNoman) then
        OutHelp('startup');
      StartInterpreter;
    end;
    ExitCode := 0;
end.
