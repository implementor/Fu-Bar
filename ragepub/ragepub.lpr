program ragepub;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainform, welcframe, fubarframe, calframe, formabs, scriptfrm
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Rage Pub';
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

