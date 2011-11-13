unit fubarframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, expressions,
  nout, buffer, cmplx, cradle, interpreter, vars, math, explain, cmd, arguments;

type

  { TCalcFrame }

  TCalcFrame = class(TFrame)
    EnterButton: TButton;
    ActionCombo: TComboBox;
    OutMemo: TMemo;
    TermEdit: TEdit;
    Panel1: TPanel;
    procedure EnterButtonClick(Sender: TObject);
    procedure TermEditKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    { public declarations }
  end;

threadvar
  Initialized: Boolean;

implementation

{$R *.lfm}

{ TCalcFrame }

procedure TCalcFrame.TermEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key in [#10,#13] then
    EnterButton.Click;
end;

procedure TCalcFrame.EnterButtonClick(Sender: TObject);
var r: Complex;
begin
  if not Initialized then begin
    nout.OutMethod := omBuffer;
    expressions.answer := 0;
    InitVars;
    Vars.SetVar('tau',pi*2,true);
    Vars.SetVar('i',cmplx_i,true);
    Interpreter.Autoload(PathAutoload);
    Initialized := true;
  end;
  try
    UpdateBuffer(TermEdit.Text);
    GetChar;
    case ActionCombo.ItemIndex of
    0: begin
         OutMemo.Lines.Add('#> '+TermEdit.Text);
         r := Expressions.Eval; Match(#10);
         expressions.answer := r;
         explain.ClearExplanations;
         OutMemo.Lines.Add('< '+Nyanize(r));
       end;
    1: begin
         OutMemo.Lines.Add('§> '+TermEdit.Text);
         r := Expressions.Eval; Match(#10);
         expressions.answer := r;
         repeat
           OutMemo.Lines.Add('< '+explain.ReadExplanation)
         until not explain.ExplanationAvailable;
       end;
    2: begin
         OutMemo.Lines.Add('?> '+TermEdit.Text);
         OutHelp('help:'+TermEdit.Text);
       end;
    end;
    while not nouteob do
      OutMemo.Lines.Add('< '+noutreq);
  except
    on E: ECodeMistake do
      OutMemo.Lines.Add('--> '+E.Message);
    on E: EInvalidOp do
      OutMemo.Lines.Add('--> '+E.Message);
    on E: EDivByZero do
      OutMemo.Lines.Add('--> '+E.Message);
    on E: EZeroDivide do
      OutMemo.Lines.Add('--> '+E.Message);
    on E: EUnderflow do
      OutMemo.Lines.Add('--> '+E.Message);
    on E: EOverflow do
      OutMemo.Lines.Add('--> '+E.Message);
  end;
  OutMemo.Lines.Add('');
  OutMemo.SelStart := Length(OutMemo.Lines.Text)-1;
  TermEdit.SelStart:=0;
  TermEdit.SelLength:=Length(TermEdit.Text);
end;

initialization
  Initialized := false;
finalization
  if Initialized then
    FreeVars;
end.

