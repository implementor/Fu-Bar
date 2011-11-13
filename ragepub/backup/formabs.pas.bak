unit formabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus;

type

  { TAbsform }

  TAbsform = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    DockEntry: TMenuItem;
    CloseEntry: TMenuItem;
    procedure CloseEntryClick(Sender: TObject);
    procedure DockEntryClick(Sender: TObject);
  private
    { private declarations }
    FFrame: TFrame;
  public
    { public declarations }
    property Frame: TFrame read FFrame write FFrame;
  end;

  TDockMethod = procedure (const f: TAbsForm) of object;

var
  DockMethod: TDockMethod;

implementation

{$R *.lfm}

{ TAbsform }

procedure TAbsform.CloseEntryClick(Sender: TObject);
begin
  Free;
end;

procedure TAbsform.DockEntryClick(Sender: TObject);
begin
  DockMethod(self);
end;

end.

