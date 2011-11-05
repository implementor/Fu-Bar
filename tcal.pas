{/  TCal - Task Based Calendar
//  (c) 2011 Marvin Cohrs}

{$mode objfpc}{$longstrings on}{$coperators on}

library tcal;

uses tcaltasks, tcaltaskimpl, cradle, sysutils, cmd, buffer;

var
  sched: TScheduler = nil;
  line, nm: string;
  d,m: byte;
  y: word;
  
function Month(const s: string): byte;
var sub: string; i: byte;
const monabbr: array[1..12] of string = ('jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec');
begin
  Result := 0;
  if length(s) >= 3 then begin
    sub := lowercase(copy(s,1,3));
    for i := 1 to 12 do
      if monabbr[i]=sub then
        Result := i;
  end;
end;

procedure transmit(const s: pchar);
begin
  line += string(s);
end;

procedure invoke;
begin
  if sched=nil then sched := TScheduler.Create;
  UpdateBuffer(line);
  line := '';
  GetChar;
  nm := GetName;
  if nm = 'queue' then begin
    nm := GetName;
    if nm='listmonth' then begin
      nm := GetName; y := GetInt;
      Writeln(month(nm));
      Writeln(y);
      sched.Schedule(TListmonthTask.Create(y,month(nm)));
    end else if nm='dayofweek' then begin
      y := GetInt; Match('-');
      m := GetInt; Match('-');
      d := GetInt;
      sched.Schedule(TDayOfWeekTask.Create(y,d,m));
    end else if nm='echo' then begin
      nm := look+ReadRemaining;
      sched.Schedule(TEchoTask.Create(nm));
      Writeln('Echo: ',nm);
    end
  end else if nm='apply' then
    try
      sched.Apply;
    except
      on E: Exception do
        Writeln(StdErr,E.ClassName,' => ',E.Message);
    end;
end;

exports
  transmit name 'fubar_plugin_transmit',
  invoke name 'fubar_plugin_invoke';

initialization
  sched := TScheduler.Create;
finalization
  sched.Free;
end.
