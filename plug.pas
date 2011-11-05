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

unit plug;

interface

uses prayerlist, sysutils, dynlibs, arguments, buffer, cradle;

type
  // predecl
  IPlugin = interface;
  TLibPlugin = class;
  
  // routines
  RInvoke = procedure; cdecl;
  RTransmit = procedure(const s: pchar); cdecl;
  
  // exceptions
  ELoadPlugin = class(Exception);

  // interface
  IPlugin = interface
    procedure Invoke;
    procedure Transmit(const s: string);
    function GetCommand: string;
  end;
  
  // classes
  TLibPlugin = class (TInterfacedObject, IPlugin)
  protected
    FLib: TLibHandle;
    FInvoke: RInvoke;
    FTransmit: RTransmit;
    FCommand: string;
  public
    constructor Create(const cmd,fn: string); virtual;
    destructor Destroy; override;
    procedure Invoke; virtual;
    procedure Transmit(const s: string); virtual;
    function GetCommand: string;
  end;

function FindPlugin(const cmd: string): IPlugin;

implementation

type
  TPluginList = specialize TPrayerList<IPlugin>;
  
var
  FPluginList: TPluginList;

constructor TLibPlugin.Create(const cmd,fn: string);
begin
  inherited Create;
  FCommand := cmd;
  FLib := LoadLibrary(fn);
  if FLib=0 then raise ELoadPlugin.Create('Unable to load library '+fn);
  FInvoke := RInvoke(GetProcedureAddress(FLib,'fubar_plugin_invoke'));
  FTransmit := RTransmit(GetProcedureAddress(FLib,'fubar_plugin_transmit'));
  if not (Assigned(FInvoke) and Assigned(FTransmit)) then
    raise ELoadPlugin.Create('Library''s interface does not implement all required procedures.');
end;

destructor TLibPlugin.Destroy;
begin
  if FLib <> 0 then
    UnloadLibrary(FLib);
  inherited;
end;

procedure TLibPlugin.Invoke;
begin
  FInvoke();
end;

procedure TLibPlugin.Transmit(const s: string);
begin
  FTransmit(@s[1]);
end;

function TLibPlugin.GetCommand: string;
begin
  Result := FCommand;
end;

function FindPlugin(const cmd: string): IPlugin;
var j: smallint;
begin
  Result := nil;
  for j := 0 to FPluginList.Count-1 do
    if FPluginList[j].GetCommand = cmd then
      Result := FPluginList[j];
end;

var i: smallint; f: system.text; t1,t2: string;
initialization
  FPluginList := TPluginList.Create(0,nil);
  if PathLibplugs <> '' then begin
    Assign(f,PathLibplugs);
    Reset(f);
    try
      while not eof(f) do begin
        Readln(f,t1);
        PushBuffer;
        UpdateBuffer(t1);
        GetChar;
        t1 := GetName;
        SkipWhite;
        Match(':');
        SkipWhite;
        t2 := look+ReadRemaining;
        PopBuffer;
        FPluginList.Add(TLibPlugin.Create(t1,t2));
      end;
    finally
      Close(f);
    end;
  end;
finalization
  for i := 0 to FPluginList.Count-1 do
    FPluginList[i] := nil;
  FPluginList.Free;
end.
