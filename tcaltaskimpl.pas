{/  TCal - Task Based Calendar
//  (c) 2011 Marvin Cohrs
//  Some simple tasks}

{$mode objfpc}{$longstrings on}

unit tcaltaskimpl;

interface

uses tcaltasks, cradle;

type
  TEchoTask = class (TInterfacedObject, ITask)
  protected
    FText: string;
  public
    constructor Create(const text: string);
    procedure CheckDependencies(const tc: ITaskContext);
    procedure RunTask(const tc: ITaskContext);
    function GetDesc: string;
  end;
  TListMonthTask = class (TInterfacedObject, ITask)
  protected
    FMonth: byte;
    FYear: word;
  public
    constructor Create(const y: word; const m: byte);
    procedure CheckDependencies(const tc: ITaskContext);
    procedure RunTask(const tc: ITaskContext);
    function GetDesc: string;
  end;
  TDayOfWeekTask = class (TInterfacedObject, ITask)
  protected
    FDay, FMonth: byte;
    FYear: word;
  public
    constructor Create(const y: word; const d,m: byte);
    procedure CheckDependencies(const tc: ITaskContext);
    procedure RunTask(const tc: ITaskContext);
    function GetDesc: string;
  end;
  
const
  DayNames: array[0..6] of string = ('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday');
  MonthNames: array[1..12] of string = ('January','February','March','April','May','June','July','August','September','October','November','December');
  MonthLengths: array[1..12] of byte = (31,28,31,30,31,30,31,31,30,31,30,31);
  
function IsLeapYear(const y: word): boolean;

implementation

function IsLeapYear(const y: word): boolean;
begin
  Result := ((y mod 4=0) and (y mod 100<>0)) or (y mod 400=0);
end;

///////////////////////////////////////////////////////////
// Echo Task //////////////////////////////////////////////
constructor TEchoTask.Create(const text: string);
begin
  inherited Create;
  FText := text;
end;

procedure TEchoTask.CheckDependencies(const tc: ITaskContext);
begin
  // No dependencies
end;

procedure TEchoTask.RunTask(const tc: ITaskContext);
begin
  tc.Emit(ftext);
end;

function TEchoTask.GetDesc: string;
begin
  Result := '';
end;

///////////////////////////////////////////////////////////
// List Month /////////////////////////////////////////////
constructor TListMonthTask.Create(const y: word; const m: byte);
begin
  inherited Create;
  FYear := y;
  FMonth := m;
end;

procedure TListMonthTask.CheckDependencies(const tc: ITaskContext);
begin
  tc.QueueDependency(TDayOfWeekTask.Create(FYear,1,FMonth),'_lm_dow1');
end;

procedure TListMonthTask.RunTask(const tc: ITaskContext);
var dowfirst,ml,i: byte; space: string;
begin
  dowfirst := tc.QueryResult('_lm_dow1');
  if not (FMonth in [1..12]) then Error('Invalid month.');
  ml := MonthLengths[FMonth];
  if IsLeapYear(FYear) and (FMonth=2) then
    Inc(ml);
  space := ' ';
  for i := 1 to ml do begin
    if i=10 then space := '';
    tc.Emit('| '+space+NumToStr(i)+' '+DayNames[(dowfirst+i-1)mod 7]);
  end;
end;

function TListMonthTask.GetDesc: string;
begin
  Result := MonthNames[FMonth]+' '+NumToStr(FYear);
end;

///////////////////////////////////////////////////////////
// Day Of Week ////////////////////////////////////////////
constructor TDayOfWeekTask.Create(const y: word; const d,m: byte);
begin
  inherited Create;
  FDay := d;
  FMonth := m;
  FYear := y;
end;

procedure TDayOfWeekTask.CheckDependencies(const tc: ITaskContext);
begin
  // No dependencies
end;

procedure TDayOfWeekTask.RunTask(const tc: ITaskContext);
var
  dow, m,y, ml: byte; suf: string;
begin
  if not (FMonth in [1..12]) then Error('Invalid month: '+NumToStr(FMonth));
  ml := MonthLengths[FMonth];
  if IsLeapYear(FYear) and (FMonth=2) then Inc(ml);
  if FDay>ml then begin
    if (FDay mod 10 = 1) and (FDay<>11) then suf := 'st'
    else if (FDay mod 10 = 2) and (FDay<>12) then suf := 'nd'
    else if (FDay mod 10 = 3) and (FDay<>13) then suf := 'rd'
    else suf := 'th';
    Error('Invalid day: '+MonthNames[FMonth]+' '+NumToStr(FDay)+suf);
  end;
  if FMonth in [1..2] then
  begin
    m := FMonth+10;
    y := FYear +1;
  end else begin
    m := FMonth;
    y := FYear;
  end;
  dow := (FDay+(m+1)*26 div 10 + 5*(y mod 100) div 4 + y div 400 - y div 50 -1) mod 7;
  tc.SaveResult(dow);
  tc.Emit(DayNames[dow]);
end;

function TDayOfWeekTask.GetDesc: string;
begin
  Result := NumToStr(FYear)+'-'+NumToStr(FMonth)+'-'+NumToStr(FDay);
end;

end.
