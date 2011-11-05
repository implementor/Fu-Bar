{/  TCal - Task Based Calendar
//  (c) 2011 Marvin Cohrs
//  Task Management}

{$mode objfpc}{$longstrings on}

unit tcaltasks;

interface

uses prayerlist, sysutils;

type
  // predecl
  ITaskContext = interface;
  ITask = interface;
  TScheduler = class;
  
  // Tasks
  TRunningReason = (rrUser, rrDependency, rrAuto);
  ITaskContext = interface
    procedure Emit(const s: string);
    function GetReason: TRunningReason;
    procedure QueueDependency(const task: ITask; const resv: string);
    procedure SaveResult(const val: Int64);
    function SaveAtom(const val: string): Int64; overload;
    function SaveAtom(const val: extended): Int64; overload;
    function QueryStrAtom(const atom: Int64): string;
    function QueryFloatAtom(const atom: Int64): extended;
    function QueryResult(const resv: string): Int64;
  end;
  ITask = interface
    procedure RunTask(const tc: ITaskContext);
    procedure CheckDependencies(const tc: ITaskContext);
    function GetDesc: string;
  end;
  
  // HashTables
  TKVResv = record
    name: string;
    val: Int64;
  end;
  PKVResv = ^TKVResv;
  TDictResv = specialize TPrayerList<PKVResv>;
  TAtomType = (atString, atFloat, atInvalid);
  TKVAtom = record
    id: Int64;
    at: TAtomType;
    sval: string;
    fval: extended;
  end;
  PKVAtom = ^TKVAtom;
  TDictAtom = specialize TPrayerList<PKVAtom>;
  TKVTask = record
    resv: string;
    voiced, depch: boolean;
    reason: TRunningReason;
    task: ITask;
  end;
  PKVTask = ^TKVTask;
  TDictTask = specialize TPrayerList<PKVTask>;
  TEmissionList = specialize TPrayerList<string>;
  
  // Task List
  TScheduler = class
  protected
    FResv: TDictResv;
    FAtoms: TDictAtom;
    FTasks: TDictTask;
    FEmit: TEmissionList;
    FCurrentTask: TKVTask;
    FAtomId: Int64;
    FTaskIdx: word;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Schedule(const task: ITask);
    procedure Apply;
  end;

implementation

type
  TTaskContext = class (TInterfacedObject, ITaskContext)
  private
    FSched: TScheduler;
  public
    constructor Create(const s: TScheduler);
    procedure Emit(const s: string);
    function GetReason: TRunningReason;
    procedure QueueDependency(const task: ITask; const resv: string);
    procedure SaveResult(const val: Int64);
    function SaveAtom(const val: string): Int64; overload;
    function SaveAtom(const val: extended): Int64; overload;
    function QueryStrAtom(const atom: Int64): string;
    function QueryFloatAtom(const atom: Int64): extended;
    function QueryResult(const resv: string): Int64;
  end;

////////////////////////////////////////////////////////////
//  TScheduler  ////////////////////////////////////////////
constructor TScheduler.Create;
var pr: PKVResv;
begin
  inherited;
  New(pr);
  pr^.name := 'res';
  pr^.val := 0;
  FResv := TDictResv.Create(0, nil);
  FResv.Add(pr);
  FAtoms := TDictAtom.Create(0, nil);
  FTasks := TDictTask.Create(0, nil);
  FEmit := TEmissionList.Create(0, '');
  FAtomId := not 1337;
end;

destructor TScheduler.Destroy;
var i: smallint;
begin
  for i := 0 to FResv.Count-1 do
    Dispose(FResv[i]);
  for i := 0 to FAtoms.Count-1 do
    Dispose(FAtoms[i]);
  for i := 0 to FTasks.Count-1 do
    Dispose(FTasks[i]);
  FResv.Free;
  FAtoms.Free;
  FTasks.Free;
  inherited;
end;

procedure TScheduler.Schedule(const task: ITask);
var pt: PKVTask;
begin
  New(pt);
  pt^.resv := 'res';
  pt^.voiced := true;
  pt^.reason := rrUser;
  pt^.task := task;
  pt^.depch := false;
  FTasks.Add(pt);
end;

procedure TScheduler.Apply;
var i: smallint; cpy: TDictTask; ctx: TTaskContext; d: string;
begin
  try
      Writeln('#> A');
      cpy := TDictTask.Create(0,nil);
      ctx := TTaskContext.Create(self);
      repeat
        Writeln('#> B');
        cpy.Clear;
        Writeln('#> B.1');
        for i := 0 to FTasks.Count-1 do
          cpy.Add(FTasks[i]);
        Writeln('#> B.2');
        FTaskIdx := 0;
        for i := 0 to cpy.Count-1 do
        begin
          Writeln('#> B.3');
          FCurrentTask := cpy[i]^;
          if not FCurrentTask.depch then
            FCurrentTask.Task.CheckDependencies(ctx);
          Writeln('#> B.4');
          cpy[i]^.depch := true;
          Inc(FTaskIdx);
        end;
      until cpy.Count = FTasks.Count;
      Writeln('#> C');
      cpy.Clear; cpy.Shrink;
      for i := 0 to FTasks.Count-1 do
      begin
        Writeln('#> D');
        FCurrentTask := FTasks[i]^;
        d := fCurrentTask.Task.GetDesc;
        if (d<>'') and (fCurrentTask.Reason=rrUser) then
          FEmit.Add('Now running: '+d);
        FCurrentTask.Task.RunTask(ctx);
        Dispose(FTasks[i]);
      end;
  finally
      Writeln('#> E');
      FTasks.Clear; FTasks.Shrink;
      for i := 0 to FEmit.Count-1 do
        Writeln(i:3,' ',FEmit[i]);
      FEmit.Clear; FEmit.Shrink;
      FreeAndNil(ctx);
      FreeAndNil(cpy);
  end
end;

////////////////////////////////////////////////////////////
//  TTaskContext  //////////////////////////////////////////
constructor TTaskCOntext.Create(const s: TScheduler);
begin
  inherited Create;
  FSched := s;
end;

procedure TTaskContext.Emit(const s: string);
begin
  if FSched.FCurrentTask.Voiced then
    FSched.FEmit.Add(s);
end;

function TTaskContext.GetReason: TRunningReason;
begin
  Result := FSched.FCurrentTask.Reason;
end;

procedure TTaskContext.QueueDependency(const task: ITask; const resv: string);
var pt: PKVTask; pr: PKVResv;
begin
  New(pt); New(pr);
  pt^.resv := resv;
  pt^.task := task;
  pt^.reason := rrDependency;
  pt^.voiced := false;
  pt^.depch := false;
  pr^.name := resv;
  pr^.val := 0;
  FSched.FResv.Add(pr);
  FSched.FTasks.Insert(pt, FSched.FTaskIdx);
  Inc(FSched.FTaskIdx);
end;

procedure TTaskContext.SaveResult(const val: Int64);
var i: smallint; f: boolean; p: PKVResv;
begin
  f := false;
  for i := 0 to FSched.FResv.Count-1 do
    if FSched.FResv[i]^.Name = FSched.FCurrentTask.Resv then
    begin
      FSched.FResv[i]^.Val := val;
      f := true;
    end;
  if not f then begin
    New(p);
    p^.name := FSched.FCurrentTask.Resv;
    p^.val := val;
    FSched.FResv.Add(p);
  end;
end;

function TTaskContext.SaveAtom(const val: string): Int64;
var pa: PKVAtom;
begin
  New(pa);
  pa^.id := FSched.FAtomId;
  Result := FSched.FAtomId;
  Inc(FSched.FAtomId);
  pa^.at := atString;
  pa^.sval := val;
  FSched.FAtoms.Add(pa);
end;

function TTaskContext.SaveAtom(const val: extended): Int64;
var pa: PKVAtom;
begin
  New(pa);
  pa^.id := FSched.FAtomId;
  Result := FSched.FAtomId;
  Inc(FSched.FAtomId);
  pa^.at := atFloat;
  pa^.fval := val;
  FSched.FAtoms.Add(pa);
end;

function TTaskContext.QueryStrAtom(const atom: Int64): string;
var i: smallint;
begin
  for i := 0 to FSched.FAtoms.Count-1 do
    if FSched.FAtoms[i]^.id = atom then
      Result := FSched.FAtoms[i]^.sval;
end;

function TTaskContext.QueryFloatAtom(const atom: Int64): Extended;
var i: smallint;
begin
  for i := 0 to FSched.FAtoms.Count-1 do
    if FSched.FAtoms[i]^.id = atom then
      Result := FSched.FAtoms[i]^.fval;
end;

function TTaskContext.QueryResult(const resv: string): Int64;
var i: smallint;
begin
  for i := 0 to FSched.FResv.Count-1 do
    if FSched.FResv[i]^.name = resv then
      Result := FSched.FResv[i]^.val;
end;

end.
