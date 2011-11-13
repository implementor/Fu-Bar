unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, ComCtrls, welcframe, fubarframe, calframe, formabs, scriptfrm;

type

  { TFormMain }

  TFormMain = class(TForm)
    MenuItem9: TMenuItem;
    NewSynTabAction: TAction;
    MenuItem8: TMenuItem;
    NewWelcTabAction: TAction;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    UndockTabAction: TAction;
    CloseTabAction: TAction;
    QuitAction: TAction;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    NewCalTabAction: TAction;
    NewCalcTabAction: TAction;
    Actions: TActionList;
    MainMenu1: TMainMenu;
    Pages: TPageControl;
    procedure CloseTabActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewCalcTabActionExecute(Sender: TObject);
    procedure NewCalTabActionExecute(Sender: TObject);
    procedure NewSynTabActionExecute(Sender: TObject);
    procedure NewWelcTabActionExecute(Sender: TObject);
    procedure QuitActionExecute(Sender: TObject);
    procedure UndockTabActionExecute(Sender: TObject);
    procedure WelcomeFrame1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure DockForm(const f: TAbsForm);
  end;
  TFrameTabSheet = class(TTabsheet)
  private
    FFrame: TFrame;
  public
    property Frame: TFrame read FFrame write FFrame;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.NewCalcTabActionExecute(Sender: TObject);
var ts: TFrameTabsheet; frm: TCalcFrame;
begin
  ts := TFrameTabsheet.Create(Pages);
  ts.PageControl := Pages;
  ts.Caption := 'fubar';
  frm := TCalcFrame.Create(self);
  frm.Name := '';
  frm.Parent := ts;
  frm.Align := alClient;
  ts.Frame := frm;
  Pages.Activepage := ts;
  frm.TermEdit.SetFocus;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ActCalc := NewCalcTabAction;
  ActMulti := NewSynTabAction;
  ActCal := NewCalTabAction;
  DockMethod := @DockForm;
  randomize;
  NewWelcTabAction.Execute;
end;

procedure TFormMain.CloseTabActionExecute(Sender: TObject);
begin
  Pages.ActivePage.Free;
end;

procedure TFormMain.NewCalTabActionExecute(Sender: TObject);
var ts: TFrameTabsheet; frm: TCalFrame;
begin
  ts := TFrameTabsheet.Create(Pages);
  ts.PageControl := Pages;
  ts.Caption := 'tcal';
  frm := TCalFrame.Create(self);
  frm.Name := '';
  frm.Parent := ts;
  frm.Align := alClient;
  ts.Frame := frm;
  Pages.Activepage := ts;
end;

procedure TFormMain.NewSynTabActionExecute(Sender: TObject);
var ts: TFrameTabsheet; frm: TScriptFrame;
begin
  ts := TFrameTabsheet.Create(Pages);
  ts.PageControl := Pages;
  ts.Caption := 'fubar (syn)';
  frm := TScriptFrame.Create(self);
  frm.Name := '';
  frm.Parent := ts;
  frm.Align := alClient;
  ts.Frame := frm;
  Pages.Activepage := ts;
end;

procedure TFormMain.NewWelcTabActionExecute(Sender: TObject);
var ts: TFrameTabsheet; frm: TWelcomeFrame;
begin
  ts := TFrameTabsheet.Create(Pages);
  ts.PageControl := Pages;
  ts.Caption := 'Welcome';
  frm := TWelcomeFrame.Create(self);
  frm.Name := '';
  frm.Parent := ts;
  frm.Align := alClient;
  ts.Frame := frm;
  Pages.Activepage := ts;
end;

procedure TFormMain.QuitActionExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.UndockTabActionExecute(Sender: TObject);
var abs: TAbsForm; frm: TFrame;
begin
  if Pages.PageCount>0 then begin
    abs := TAbsForm.Create(self);
    abs.Caption := Pages.ActivePage.Caption;
    frm := TFrameTabSheet(Pages.ActivePage).Frame;
    frm.Parent := abs;
    abs.Frame := frm;
    abs.Show;
    Pages.ActivePage.Free;
  end;
end;

procedure TFormMain.WelcomeFrame1Click(Sender: TObject);
begin

end;

procedure TFormMain.DockForm(const f: TAbsForm);
var ts: TFrameTabsheet;
begin
  ts := TFrameTabSheet.Create(Pages);
  ts.PageControl := Pages;
  ts.Caption := f.Caption;
  f.Frame.Parent := ts;
  ts.Frame := f.Frame;
  f.Free;
  Pages.ActivePage := ts;
end;

end.

