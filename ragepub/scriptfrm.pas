unit scriptfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Buttons,
  SynEdit, SynHighlighterAny, SynCompletion, fubarframe, cmplx, expressions,
  cradle, arguments, explain, buffer, interpreter, vars, nout;

type

  { TScriptFrame }

  TScriptFrame = class(TFrame)
    Button1: TButton;
    OutMemo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    SynAnySyn1: TSynAnySyn;
    TermSyn: TSynEdit;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

{$R *.lfm}

{ TScriptFrame }

procedure TScriptFrame.Button1Click(Sender: TObject);
var r: Complex; s: string; i: smallint;
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
    s := '';
    for i := 0 to TermSyn.Lines.Count-1 do
      s += TermSyn.Lines[i]+' ';
    OutMemo.Lines.Clear;
    UpdateBuffer(s);
    GetChar;
    while look<>#10 do begin
      r := Expressions.Eval;
      Match(';');
      SkipWhite;
      answer := r;
      OutMemo.Lines.Add('< '+Nyanize(r));
      while not nouteob do
        OutMemo.Lines.Add('< '+noutreq);
    end;
    explain.ClearExplanations;
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
end;

end.

